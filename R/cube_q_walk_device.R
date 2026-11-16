# Training batches for the Q network, generated on the device.
#
# The same batches make_batch() in train_cube3_q_online.R builds on the host
# from generate_walk(pivot = TRUE, exact = FALSE, no_undo = TRUE, no_cancel):
# a cut drawn uniformly from k_min..depth-1, a random walk long enough to
# contain it, the state at the cut, and the two moves that meet there with
# their labels -- here as ag_* operations, so that a recorded training step
# needs nothing from the host but a random key. The rules and the encoding are
# carried over from src/generate_walk.cpp and src/cube_adi.cpp
# (cube_adi_encode_pieces); the random numbers are not, so batches agree with
# the C++ generator in distribution, not bit for bit.
#
# HOW THE WALK IS TAKEN WITHOUT A STATE
# -------------------------------------
# In the C++ walk the choice of the next move depends on the moves already
# made, never on the state. So the word is drawn first, as move indices only,
# and the state is computed afterwards -- and only where the encoder looks.
# A move applies as new[i] = state[p[i]] (perm_group.h), so after the word
# a_1 .. a_k the identity has become
#
#     s_k[i] = p_{a_1}[ p_{a_2}[ ... p_{a_k}[i] ... ] ],
#
# i.e. position i is followed through the word from its LAST move back to its
# first. The encoder reads one sticker per piece (the piece's first slot), so
# twenty positions are followed, not the whole state.
#
# THE RULES AS ARITHMETIC
# -----------------------
# Per state and per candidate move c the C++ scans the tail of the word: a
# meeting with the inverse of c bans c; a move that does not commute with c
# ends the scan; meeting c itself counts, and c is banned once 2(cnt + 1)
# exceeds its order. That scan has a running form, updated after each move a:
#
#     ban'[c] = [a == inv(c)]  OR  (commute(c, a) AND ban[c])
#     cnt'[c] = commute(c, a) * (cnt[c] + [a == c])
#     allowed[c] = !ban[c] AND cnt[c] <= floor(order_c / 2) - 1
#
# with [a == inv(c)] = (INV %*% onehot(a))[c] and commute(c, a) =
# (COMM %*% onehot(a))[c]. Everything stays 0/1 or a small whole number;
# step(z) = relu(z) - relu(z - 1) turns a whole number into "z >= 1" without a
# comparison op. Without no_cancel only the inverse of the last move is banned:
# ban' = [a == inv(c)].
#
# Constants that are added to a matrix are [rows, 1] columns of its own height,
# never [1, 1]: ag_add's host branch spreads a [1, 1] operand across one row
# only, which silently mis-adds on the CPU.

# ---------------------------------------------------------------------------
# Host tables
# ---------------------------------------------------------------------------

.q_walk_tables <- function(group, layout, depth, k_min = 2L, no_cancel = TRUE,
                           y_mu = (1 + depth) / 2,
                           y_sd = stats::sd(seq_len(depth))) {
  depth <- as.integer(depth); k_min <- as.integer(k_min)
  if (depth < 2L) stop("depth must be at least 2", call. = FALSE)
  if (k_min < 1L || k_min > depth - 1L)
    stop("k_min must be in 1..depth-1", call. = FALSE)

  perms <- lapply(cube_moves(layout$n_side)[group$moves], as.integer)
  n   <- length(perms[[1L]])
  n_a <- length(perms)
  id  <- seq_len(n)
  ap  <- function(s, p) s[p]                      # new[i] = state[p[i]]

  inv <- vapply(seq_len(n_a), function(a) {
    after <- ap(id, perms[[a]])
    hit <- which(vapply(perms, function(p) identical(ap(after, p), id), logical(1)))
    if (length(hit)) hit[1L] else NA_integer_
  }, integer(1))
  if (anyNA(inv))
    stop("every move needs its inverse in the alphabet: the pivot's label ",
         "steps back along it", call. = FALSE)

  comm <- matrix(0, n_a, n_a)
  for (a in seq_len(n_a)) for (b in seq_len(n_a))
    comm[a, b] <- identical(ap(ap(id, perms[[a]]), perms[[b]]),
                            ap(ap(id, perms[[b]]), perms[[a]]))
  ord <- vapply(perms, function(p) {
    s <- id
    for (k in seq_len(1000L)) { s <- ap(s, p); if (identical(s, id)) return(k) }
    0L
  }, integer(1))

  inv_m <- matrix(0, n_a, n_a)
  inv_m[cbind(seq_len(n_a), inv)] <- 1            # INV[c, b] = [b == inv(c)]

  # "cnt <= floor(order/2) - 1" as "K1 - cnt >= 1" with K1 = floor(order/2);
  # an order not found within the cap disables the count rule, as in the C++.
  k1 <- ifelse(ord > 0L, floor(ord / 2), depth + 2)

  first0 <- layout$slots[, 1L] - 1L               # 0-based positions read
  home <- layout$home; turn <- layout$turn
  l0 <- ifelse(home > 0L, (home - 1L) * layout$width + (turn - 1L), 0)

  list(n = n, n_a = n_a, depth = depth, k_min = k_min,
       no_cancel = isTRUE(no_cancel), y_mu = y_mu, y_sd = y_sd,
       n_piece = layout$n_piece, depth_dim = layout$n_piece * layout$width,
       perms = perms, inv = inv, comm = comm, order = ord,
       inv_m = inv_m, k1 = k1, first0 = first0, l0 = l0,
       pflat = unlist(lapply(perms, function(p) p - 1L)))   # [a * n + x]
}

# ---------------------------------------------------------------------------
# Device constants for a batch size
# ---------------------------------------------------------------------------

.q_walk_device <- function(tab, batch) {
  batch <- as.integer(batch)
  buf <- function(m) {
    b <- ggmlR::ag_buffer(nrow(m), ncol(m))
    ggmlR::ag_buffer_write(b, m, 0)
    b
  }
  ten <- function(m) ggmlR::ag_tensor(m)
  d <- tab$depth; na <- tab$n_a
  span <- d - tab$k_min
  lt <- outer(seq_len(d), seq_len(d), function(j, t) as.numeric(t > j))
  list(
    tab = tab, batch = batch,
    # gathered from by index
    P  = buf(matrix(as.numeric(tab$pflat), 1L)),
    L  = buf(matrix(as.numeric(tab$l0), 1L)),
    Id = buf(diag(tab$depth_dim)),
    Ia = buf(diag(na)),
    Ic = buf(diag(d)),
    # multiplied or added
    INV  = ten(tab$inv_m), COMM = ten(tab$comm),
    LT   = ten(lt),                                 # LT[j, t] = [t > j]
    K1   = ten(matrix(tab$k1, na, 1L)),
    oneA = ten(matrix(1, na, 1L)), negA = ten(matrix(-1, na, 1L)),
    rowA = ten(matrix(1, 1L, na)),
    zA   = ten(matrix(0, na, batch)),
    X0   = ten(matrix(as.numeric(tab$first0), tab$n_piece, batch)),
    zcut = ten(matrix(0, span, batch)),
    kmin = ten(matrix(tab$k_min, 1L, 1L)),
    cprv = ten(matrix(-1 - tab$y_mu, 1L, 1L)),
    cnxt = ten(matrix(1 - tab$y_mu, 1L, 1L)))
}

# The buffers and constant tensors, for ag_capture()'s `params`.
.q_walk_params <- function(dev) {
  dev[vapply(dev, function(v) inherits(v, c("ag_buffer", "ag_tensor")), logical(1))]
}

# ---------------------------------------------------------------------------
# One batch
# ---------------------------------------------------------------------------

# key: two numbers, or a [2, 1] ag tensor (an input of a recording). Returns
# x [depth_dim, n_piece * batch] (the Q net's input), y and mask [n_a, batch]
# (targets in the scaled units of the online script, and which two of them are
# labelled). debug = TRUE adds what the tests check: the word [depth, batch]
# (0-based move indices), the cut [1, batch], the final ban/cnt and the number
# of allowed moves at every step [depth, batch].
.q_walk_batch <- function(dev, key, debug = FALSE) {
  tab <- dev$tab; B <- dev$batch
  d <- tab$depth; np <- tab$n_piece
  step <- function(z) ggmlR::ag_sub(ggmlR::ag_relu(z),
                                    ggmlR::ag_relu(ggmlR::ag_add(z, dev$negA)))
  onehot_a <- function(a) ggmlR::ag_get_rows(dev$Ia, ggmlR::ag_transpose(a))

  # The cut, uniform on k_min..depth-1, and which step it falls on.
  cut <- ggmlR::ag_add(ggmlR::ag_sample_categorical(dev$zcut, key = key,
                                                    stream = d),
                       dev$kmin)                                 # [1, B]
  oh_cut <- ggmlR::ag_get_rows(dev$Ic, ggmlR::ag_transpose(cut))  # [d, B]
  # m[j] = [j < cut]: moves 0..cut-1 lead to the pivot, move `cut` leaves it.
  m <- ggmlR::ag_matmul(dev$LT, oh_cut)                           # [d, B]

  # The word.
  # A constant of the device, not a matrix made here: inside a recording a
  # host value would be an upload, which is refused.
  ban <- dev$zA
  cnt <- dev$zA
  words <- vector("list", d); ohs <- vector("list", d); nallow <- vector("list", d)
  for (j in seq_len(d)) {
    free <- ggmlR::ag_add(ggmlR::ag_scale(ban, -1), dev$oneA)
    allowed <- if (tab$no_cancel)
      ggmlR::ag_mul(free, step(ggmlR::ag_add(ggmlR::ag_scale(cnt, -1), dev$K1)))
    else free
    if (debug) nallow[[j]] <- ggmlR::ag_matmul(dev$rowA, allowed)
    logits <- ggmlR::ag_scale(ggmlR::ag_add(allowed, dev$negA), 1e4)
    a  <- ggmlR::ag_sample_categorical(logits, key = key, stream = j - 1L)
    oh <- onehot_a(a)
    hit <- ggmlR::ag_matmul(dev$INV, oh)                         # [a == inv(c)]
    if (tab$no_cancel) {
      co  <- ggmlR::ag_matmul(dev$COMM, oh)                      # commute(c, a)
      ban <- step(ggmlR::ag_add(hit, ggmlR::ag_mul(co, ban)))
      cnt <- ggmlR::ag_mul(co, ggmlR::ag_add(cnt, oh))
    } else {
      ban <- hit
    }
    words[[j]] <- a; ohs[[j]] <- oh
  }

  # The pivot state at the encoder's positions, the word followed backwards.
  x <- dev$X0                                                    # [np, B]
  n <- tab$n
  for (j in rev(seq_len(d))) {
    idx <- ggmlR::ag_reshape(ggmlR::ag_add(x, ggmlR::ag_scale(words[[j]], n)),
                             np * B, 1L)
    px  <- ggmlR::ag_reshape(ggmlR::ag_get_rows(dev$P, idx), np, B)
    mj  <- ggmlR::ag_slice_rows(m, j - 1L, 1L)
    x   <- ggmlR::ag_add(x, ggmlR::ag_mul(ggmlR::ag_sub(px, x), mj))
  }

  # Encoding: piece-in-slot -> one-hot column, [depth_dim, np * B].
  dd <- ggmlR::ag_get_rows(dev$L, ggmlR::ag_reshape(x, np * B, 1L))
  xin <- ggmlR::ag_get_rows(dev$Id, ggmlR::ag_transpose(dd))

  # Labels: the move taken at the cut (next, label cut + 1) and the inverse of
  # the one that arrived (prev, label cut - 1).
  nxt <- NULL; prv <- NULL
  for (j in seq_len(d)) {
    w <- ggmlR::ag_slice_rows(oh_cut, j - 1L, 1L)
    nxt_j <- ggmlR::ag_mul(ohs[[j]], w)
    nxt <- if (is.null(nxt)) nxt_j else ggmlR::ag_add(nxt, nxt_j)
    if (j >= 2L) {
      prv_j <- ggmlR::ag_mul(ggmlR::ag_matmul(dev$INV, ohs[[j - 1L]]), w)
      prv <- if (is.null(prv)) prv_j else ggmlR::ag_add(prv, prv_j)
    }
  }
  vprv <- ggmlR::ag_scale(ggmlR::ag_add(cut, dev$cprv), 1 / tab$y_sd)
  vnxt <- ggmlR::ag_scale(ggmlR::ag_add(cut, dev$cnxt), 1 / tab$y_sd)
  y    <- ggmlR::ag_add(ggmlR::ag_mul(prv, vprv), ggmlR::ag_mul(nxt, vnxt))
  mask <- ggmlR::ag_add(prv, nxt)

  out <- list(x = xin, y = y, mask = mask)
  if (debug) {
    stack <- function(l) Reduce(ggmlR::ag_concat_rows, l)
    out$words  <- stack(words)
    out$cut    <- cut
    out$ban    <- ban
    out$cnt    <- cnt
    out$nallow <- stack(nallow)
  }
  out
}

# ---------------------------------------------------------------------------
# Host reference, for the tests
# ---------------------------------------------------------------------------

# Does each word obey the walk's rules up to and including the move at the cut?
# A literal transcription of the C++ scan in generate_walk_cpp, step by step.
.q_walk_rules_ok <- function(tab, words, cut) {
  na <- tab$n_a
  vapply(seq_len(ncol(words)), function(i) {
    w <- words[, i] + 1L                           # 1-based moves
    for (step in seq_len(cut[i] + 1L)) {
      a <- w[step]
      if (tab$no_cancel) {
        inv_a <- tab$inv[a]; cnt <- 0L; ban <- FALSE
        if (step > 1L) for (j in (step - 1L):1L) {
          b <- w[j]
          if (b == inv_a) { ban <- TRUE; break }
          if (!tab$comm[a, b]) break
          if (b == a) cnt <- cnt + 1L
        }
        if (!ban && tab$order[a] > 0L && 2L * (cnt + 1L) > tab$order[a]) ban <- TRUE
        if (ban) return(FALSE)
      } else if (step > 1L && a == tab$inv[w[step - 1L]]) {
        return(FALSE)
      }
    }
    TRUE
  }, logical(1))
}

# The state after the first k moves of a word (1-based move indices), applied
# the way perm_group.h applies them.
.q_walk_replay <- function(tab, word, k) {
  s <- seq_len(tab$n)
  for (j in seq_len(k)) s <- s[tab$perms[[word[j]]]]
  s
}
