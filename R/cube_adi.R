#' Autodidactic Iteration on a Permutation Puzzle
#'
#' Training a network to solve a cube without ever being shown a solution.
#'
#' The difficulty with learning a distance-to-solved is that nobody knows what
#' it is. Labelling a state with the length of the scramble that produced it is
#' the obvious thing to do and it is wrong: the scramble is one path to that
#' state, not the shortest, so the label is an upper bound that gets looser the
#' deeper the scramble goes, and a network trained on it learns the looseness
#' along with the distance.
#'
#' Autodidactic iteration sidesteps this. A state's target is not its scramble
#' length but \eqn{\min_a (1 + v(child_a))} --- one move, plus whatever the
#' network currently believes about the best of its children. That looks
#' circular, and would be, except that a child which is already solved counts
#' as zero no matter what the network says. States one move from solved
#' therefore get an exact target immediately, their neighbours get an almost
#' exact one from them, and correctness spreads outward as training goes on.
#' This is why scrambles are drawn uniformly over depth and the loss carries no
#' weighting: the ordering is in the structure of the targets, not the sampling.
#'
#' @section Two networks:
#' Value and policy are separate models here, not two heads on one trunk.
#' \code{ggml_fit} trains a single output, so a shared trunk would need a
#' multi-output loss that ggmlR does not yet have. The cost is a second forward
#' pass per move at solve time; the benefit is that each network trains under
#' the loss that suits it -- mean squared error for a distance, cross-entropy
#' for a choice of move -- with no changes to ggmlR.
#'
#' @section The frozen network:
#' Targets are computed with a copy of the value network held fixed for the
#' whole batch. Without it the targets move as the network moves and training
#' chases itself; the copy is refreshed only once the running loss drops below
#' \code{loss_thresh}, which is DeepCubeA's criterion and steadier than
#' refreshing on a fixed schedule.
#'
#' @name cube_adi
#' @seealso \code{\link{cube_adi_train}}, \code{\link{cube_adi_solve}}
NULL

# ggmlR is a Suggests: everything here needs it, nothing else in the package
# does. One message, naming what to install, beats a missing-object error from
# somewhere three calls down.
adi_require_ggml <- function() {
  if (!requireNamespace("ggmlR", quietly = TRUE)) {
    stop("cube_adi needs ggmlR for the networks.\n",
         "  install.packages('ggmlR')  # or install from source",
         call. = FALSE)
  }
  invisible(TRUE)
}

# The transformer needs three layers that arrived in ggmlR 0.8.5, and 0.8.4 is
# what is on CRAN as this is written. Naming them as ggmlR::ggml_layer_... is
# what one would rather write, but R CMD check resolves every such reference
# against the installed ggmlR and reports the three as missing objects -- a
# WARNING on a machine that has 0.8.4, for code that is only reached if the
# caller asks for arch = "transformer".
#
# Looking them up by name defers the question to the call, which is where it
# belongs: the other five architectures do not need these layers and go on
# working under 0.8.4. Once 0.8.5 is the version on CRAN this can go back to
# being three ordinary ggmlR:: calls, and the DESCRIPTION can say
# ggmlR (>= 0.8.5) instead.
adi_ggml_fun <- function(name) {
  if (!exists(name, envir = asNamespace("ggmlR"), inherits = FALSE)) {
    stop("cube_adi_model(arch = \"transformer\") needs ggmlR 0.8.5 or newer",
         " -- this is ", as.character(packageVersion("ggmlR")), ".\n",
         "  The other architectures (mlp, resnet, lstm, gru) work as they are.",
         call. = FALSE)
  }
  getExportedValue("ggmlR", name)
}

# A state is a permutation of 1..96, and how it is handed to a network depends
# on how that network reads it. The embedding wants token ids, zero-based
# because that is what ggml_get_rows indexes with. The recurrent variant has no
# embedding to index into, so the positions go in as numbers, scaled to keep
# them in the range recurrent units are happy with.
adi_encode <- function(states, arch = "mlp", layout = NULL) {
  if (is.null(arch)) arch <- "mlp"   # nets built before arch existed are mlp
  if (is.null(dim(states))) states <- matrix(states, nrow = 1L)
  if (!is.null(layout)) {
    x <- adi_encode_pieces(states, layout)
    # The dense architectures take the pieces flat, as the encoder returns
    # them. The transformer reads them as a sequence -- one token per piece --
    # and that is only a change of dim: the C++ writes the array as
    # [n, n_piece, depth] flattens in column-major order, so the third axis is
    # already there in the buffer and simply not declared.
    if (identical(arch, "transformer"))
      dim(x) <- c(nrow(states), layout$n_piece,
                  layout$n_piece * layout$width)
    return(x)
  }
  if (arch %in% c("lstm", "gru")) {
    # [batch, timesteps, features]: one position per step, one number per
    # position. ggml_fit reads a c(seq_len, 1) input shape as a 3-d array, so
    # the trailing feature axis has to be there even though it is 1 wide.
    array(as.numeric(states) / ncol(states),
          dim = c(nrow(states), ncol(states), 1L))
  } else {
    matrix(as.integer(states) - 1L, nrow = nrow(states), ncol = ncol(states))
  }
}

#' The Piece Layout a Cube State Is Read Through
#'
#' Which stickers belong to which physical piece, in the form the piece
#' encoding needs. Built from \code{\link{cube_pieces}} rather than written out
#' here: which three stickers make a corner is a fact about the cube, and one
#' measured table beats a transcribed one.
#'
#' @section What it is for:
#' A cube state is a permutation of sticker positions, and handing that to a
#' network as 54 numbers makes it discover for itself that positions 1, 37 and
#' 48 always travel together. They do so because they are one corner. The
#' encoding below says that outright, which is the representation DeepCubeA
#' uses: for every piece, which slot it currently occupies and how it is turned.
#'
#' @section Centres:
#' Centres are dropped only when no move disturbs them. Under face turns alone
#' they never move, so on a 3x3x3 restricted to U/R/F/D/L/B they carry no
#' information and are left out, giving DeepCubeA's 20 pieces. Add the slice
#' moves M, E or S and the centres do permute --- each slice moves four of them
#' --- so there they are kept and the layout has all 26. The decision is taken
#' by applying the group's own moves and looking, not from the move names.
#'
#' @param group A \code{perm_group} built by \code{\link{cube_group}}
#' @return A list describing the layout: \code{slots} (integer matrix, one row
#'   per piece, its sticker positions padded with 0), \code{n_piece},
#'   \code{width} (slots per piece), \code{home} (which piece each sticker
#'   belongs to) and \code{turn} (which of its slots each sticker is)
#' @seealso \code{\link{cube_adi_model}}, \code{\link{cube_pieces}}
#' @export
#' @examples
#' g <- cube_group(3, moves = c("U", "U'", "R", "R'", "F", "F'",
#'                              "D", "D'", "L", "L'", "B", "B'"))
#' lay <- cube_piece_layout(g)
#' lay$n_piece   # 20: centres are fixed under face turns, so they are dropped
cube_piece_layout <- function(group) {
  if (!is_perm_group(group)) stop("group must be a perm_group")
  n_side <- round((group$n / 6)^(1 / 2))
  if (6L * n_side * n_side != group$n)
    stop("cube_piece_layout: state length ", group$n, " is not a cube")

  pieces <- cube_pieces(n_side)
  st <- lapply(strsplit(pieces$stickers, ",", fixed = TRUE), as.integer)

  # Which pieces actually move. A piece no move ever disturbs is a constant
  # input, and a constant input is 24 weights the network has to learn to
  # ignore. Checked by applying every move to the solved state.
  id <- group_identity(group)
  tbl <- cube_moves(n_side)
  moved <- rep(FALSE, group$n)
  for (m in group$moves) moved <- moved | (id[tbl[[m]]] != id)
  keep <- vapply(st, function(s) any(moved[s]), logical(1))
  if (!any(keep)) stop("cube_piece_layout: no piece moves under these moves")

  st <- st[keep]
  width <- max(lengths(st))
  slots <- t(vapply(st, function(s) c(s, rep(0L, width - length(s))),
                    integer(width)))

  # The reverse map: for each sticker, the piece it belongs to and which of
  # that piece's slots it is. Reading a scrambled state means looking up where
  # its stickers went and asking whose they were.
  home <- integer(group$n)
  turn <- integer(group$n)
  for (i in seq_along(st)) for (j in seq_along(st[[i]])) {
    home[st[[i]][j]] <- i
    turn[st[[i]][j]] <- j
  }

  list(slots = slots, n_piece = length(st), width = width,
       home = home, turn = turn, n_side = n_side)
}

# One-hot over "which piece is in this slot, and how is it turned". A slot of a
# cube with P pieces of width W can hold any of the P pieces in any of W
# turnings, so a slot costs P * W bits and a state costs P slots of them.
#
# Read from the slot rather than from the piece -- "slot 3 holds piece 7 turned
# once" rather than "piece 7 is in slot 3" -- because the state already says
# which stickers sit in a slot, so the lookup is direct. The two carry exactly
# the same information: one is the inverse permutation of the other.
# The first sticker of a slot is enough to name the piece now in it: all of a
# slot's stickers come from one piece, which cube_piece_layout's construction
# gives and the tests check on scrambled cubes.
#
# The filling itself is in C++. It runs on every batch of every ADI iteration,
# and in R -- an array of n * P * P * W doubles built a piece at a time -- it
# cost more than the training step it feeds.
adi_encode_pieces <- function(states, layout) {
  cube_adi_encode_pieces(states, layout$slots[, 1L], layout$home, layout$turn,
                         layout$n_piece, layout$width)
}

# The bottom of both networks, which is where the two encodings differ and the
# only place they do. Sticker input arrives as token ids and needs an embedding
# to become numbers; piece input is one-hot already, so it goes straight to the
# flatten. Everything above this is the same stack either way.
adi_input_head <- function(state_len, embed_dim, layout) {
  if (is.null(layout)) {
    inp <- ggmlR::ggml_input(shape = state_len, dtype = "int32")
    h   <- ggmlR::ggml_layer_embedding(inp, vocab_size = state_len,
                                       dim = embed_dim)
    return(list(inp = inp, h = ggmlR::ggml_layer_flatten(h)))
  }
  # Declared flat rather than as [pieces, bits-per-piece] with a flatten over
  # it. The two describe the same 1200 numbers in the same order, and the dense
  # layer above sees no difference -- but a 2-d input costs about 25 times as
  # much per state to run, measured, which is where the piece encoding's whole
  # slowness lived. Only the embedding path needs the second axis, because that
  # is what it indexes along.
  inp <- ggmlR::ggml_input(shape = layout$n_piece * layout$n_piece *
                             layout$width)
  list(inp = inp, h = inp)
}

#' Build the Value and Policy Networks
#'
#' Two models over the same input: 96 sticker positions read as tokens through
#' a shared-in-shape, separate-in-weights embedding, then a stack of dense
#' layers. The value network ends in one linear unit (a distance), the policy
#' network in one softmax unit per move.
#'
#' @param group A \code{perm_group}, used for its state length and move count
#' @param embed_dim Width of the token embedding (\code{arch = "mlp"} only)
#' @param hidden Integer vector of hidden layer widths
#' @param arch \code{"mlp"} (default) reads the state as a set of tokens
#'   through an embedding and puts a dense stack on top; \code{"resnet"} is the
#'   same input under residual blocks, which is the shape DeepCubeA uses.
#'   \code{"lstm"} and \code{"gru"} instead read the state as a sequence, one
#'   position at a time. A cube state is a set, not a sequence -- position 5
#'   does not come before position 6 in any sense the moves respect -- so the
#'   recurrent variants are here to be measured, not because the shape of the
#'   data asks for them; measured, they diverge. \code{"transformer"} reads the
#'   pieces as a sequence and attends over them --- a set, which is what
#'   attention takes, rather than the order the recurrent variants impose. It
#'   requires \code{encoding = "piece"}.
#' @param rnn_units Width of the recurrent layer (\code{"lstm"}/\code{"gru"})
#' @param n_blocks Number of blocks: residual blocks under
#'   \code{arch = "resnet"}, encoder blocks under \code{"transformer"}. For
#'   the resnet every block is \code{hidden[1]} wide, since a skip connection
#'   has to add tensors of the same shape.
#' @param d_model Model width (\code{arch = "transformer"} only). Must divide
#'   by \code{n_heads}.
#' @param n_heads Attention heads (\code{arch = "transformer"} only)
#' @param ff_dim Width of each block's feed-forward hidden layer
#'   (\code{arch = "transformer"} only). \code{NULL}, the default, means
#'   \code{4 * d_model}, the usual ratio.
#' @param encoding How a state reaches the network. \code{"sticker"} (default)
#'   reads it as 54 sticker positions through an embedding. \code{"piece"} reads
#'   it as DeepCubeA does --- one one-hot per piece slot, saying which piece is
#'   in it and how it is turned --- using \code{\link{cube_piece_layout}}, which
#'   also drops the pieces no move disturbs. It takes no embedding, being
#'   one-hot already, so \code{embed_dim} is ignored under it.
#' @param backend \code{"auto"}, \code{"cpu"} or \code{"vulkan"}
#' @return List with \code{value} and \code{policy} models, plus the group
#' @export
cube_adi_model <- function(group, embed_dim = 32L, hidden = c(1024L, 512L),
                           arch = c("mlp", "resnet", "lstm", "gru",
                                    "transformer"),
                           rnn_units = 256L, n_blocks = 4L,
                           d_model = 64L, n_heads = 4L, ff_dim = NULL,
                           encoding = c("sticker", "piece"),
                           backend = "auto") {
  arch <- match.arg(arch)
  encoding <- match.arg(encoding)
  adi_require_ggml()
  if (!is_perm_group(group)) stop("group must be a perm_group")

  state_len <- group$n
  n_moves   <- length(group$moves)

  # The piece encoding needs the layout to build the input shape, so it is
  # measured now and carried on the net: the same table the encoder reads at
  # every call, rather than one rebuilt per batch.
  layout <- NULL
  if (encoding == "piece") {
    if (arch %in% c("lstm", "gru"))
      stop("encoding = \"piece\" is for the dense architectures; ",
           "arch = \"", arch, "\" reads a sequence of positions instead")
    layout <- cube_piece_layout(group)
  }

  # The transformer's sequence is the pieces, so it needs their layout. The
  # sticker encoding cannot supply one: ggml_layer_embedding returns
  # [dim, seq_len] while ggml_layer_attention reads its first axis as the
  # sequence, so stacking them would run attention along the embedding
  # dimension and treat the 54 positions as features. The same missing reshape
  # that shapes the recurrent variants rules this out, and it is refused here
  # rather than left to build a graph that trains on the wrong axis.
  if (arch == "transformer" && is.null(layout))
    stop("arch = \"transformer\" needs encoding = \"piece\": ",
         "its sequence is the pieces, and the sticker embedding returns ",
         "its axes in the order attention cannot read")

  # The vocabulary is the set of things that can sit in a position, which for a
  # permutation state is every position -- 96 tokens, not 6 colours. Sticker
  # identity is what the moves permute, so it is what the network should see.
  build <- switch(arch,
    mlp = function(units, activation) {
      hd  <- adi_input_head(state_len, embed_dim, layout)
      h   <- hd$h
      for (w in hidden) h <- ggmlR::ggml_layer_dense(h, w, activation = "relu")
      out <- ggmlR::ggml_layer_dense(h, units, activation = activation)
      ggmlR::ggml_model(inputs = hd$inp, outputs = out)
    },
    # The recurrent variant reads the state one position at a time. It takes no
    # embedding, and that is forced rather than chosen: ggml_layer_embedding
    # returns [dim, seq_len] while ggml_layer_lstm reads its first axis as
    # time, so stacking them walks the LSTM along the embedding dimension and
    # treats the 96 positions as features -- recurrence over nothing. With no
    # reshape layer in ggmlR to swap the axes, feeding the positions directly
    # is what makes the sequence the sequence.
    lstm = function(units, activation) {
      inp <- ggmlR::ggml_input(shape = c(state_len, 1L))
      h   <- ggmlR::ggml_layer_lstm(inp, units = rnn_units)
      for (w in hidden) h <- ggmlR::ggml_layer_dense(h, w, activation = "relu")
      out <- ggmlR::ggml_layer_dense(h, units, activation = activation)
      ggmlR::ggml_model(inputs = inp, outputs = out)
    },
    # DeepCubeA's shape: one wide projection, then residual blocks of two dense
    # layers with a skip around each. The skip is what lets the stack go deep
    # without the gradient dying on the way back, which is the whole reason to
    # prefer this over simply adding more layers to the plain stack.
    resnet = function(units, activation) {
      hd  <- adi_input_head(state_len, embed_dim, layout)
      inp <- hd$inp
      h   <- ggmlR::ggml_layer_dense(hd$h, hidden[1], activation = "relu")
      # Blocks are all one width -- the skip adds the block's input to its
      # output, so the two have to match.
      for (b in seq_len(n_blocks)) {
        r <- ggmlR::ggml_layer_dense(h, hidden[1], activation = "relu")
        r <- ggmlR::ggml_layer_dense(r, hidden[1], activation = NULL)
        h <- ggmlR::ggml_layer_add(list(h, r))
      }
      out <- ggmlR::ggml_layer_dense(h, units, activation = activation)
      ggmlR::ggml_model(inputs = inp, outputs = out)
    },
    # Same shape of network with the cheaper recurrent cell: two gates rather
    # than three, so fewer weights per step and a shorter unrolled graph.
    gru = function(units, activation) {
      inp <- ggmlR::ggml_input(shape = c(state_len, 1L))
      h   <- ggmlR::ggml_layer_gru(inp, units = rnn_units)
      for (w in hidden) h <- ggmlR::ggml_layer_dense(h, w, activation = "relu")
      out <- ggmlR::ggml_layer_dense(h, units, activation = activation)
      ggmlR::ggml_model(inputs = inp, outputs = out)
    },
    # The pieces read as a sequence rather than as one flat vector. Under the
    # dense stack the fact that sixty consecutive bits describe one piece is
    # something the network has to infer from the data; here it is the shape of
    # the input, and attention compares whole pieces against each other. Which
    # is the relation the cube is made of: a corner is not twisted on its own
    # but with respect to its neighbours.
    transformer = function(units, activation) {
      depth <- layout$n_piece * layout$width
      inp <- ggmlR::ggml_input(shape = c(layout$n_piece, depth))
      # One kernel per position with shared weights: what piece sits in a slot
      # reads the same way in every slot, and the sequence axis survives.
      h <- ggmlR::ggml_layer_dense(inp, d_model, time_distributed = TRUE)
      # Attention is order-blind, and the slots are not interchangeable: with
      # no positional term the network cannot tell a twisted UFR from a
      # twisted DBL. A learned table rather than rope, because rope encodes
      # relative position and the distance between slots 3 and 7 means nothing
      # on a cube -- adjacency here is geometric, not linear.
      h <- adi_ggml_fun("ggml_layer_positional_embedding")(h)
      for (b in seq_len(n_blocks)) {
        h <- adi_ggml_fun("ggml_layer_transformer_block")(
          h, d_model, n_heads = n_heads,
          ff_dim = if (is.null(ff_dim)) 4L * d_model else ff_dim,
          # Not gelu: it has no backward rule in ggml, so the graph builds and
          # then training aborts. silu is the smooth one that has a gradient.
          activation = "silu", norm = "rms",
          name = paste0("block", b))
      }
      # Mean over the pieces, not flatten: there is no summary token and all
      # twenty pieces count equally, and pooling makes the head's width
      # d_model instead of n_piece * d_model.
      h   <- adi_ggml_fun("ggml_layer_sequence_pooling")(h, mode = "mean")
      out <- ggmlR::ggml_layer_dense(h, units, activation = activation)
      ggmlR::ggml_model(inputs = inp, outputs = out)
    }
  )

  value <- ggmlR::ggml_compile(build(1L, NULL), optimizer = "adam",
                               loss = "mse", metrics = NULL,
                               backend = backend)
  policy <- ggmlR::ggml_compile(build(n_moves, "softmax"), optimizer = "adam",
                                loss = "categorical_crossentropy",
                                metrics = NULL, backend = backend)

  structure(list(value = value, policy = policy, group = group,
                 n_moves = n_moves, state_len = state_len, arch = arch,
                 encoding = encoding, layout = layout),
            class = "cube_adi_net")
}

#' @export
print.cube_adi_net <- function(x, ...) {
  cat("<cube_adi_net>\n")
  cat("  group     :", x$group$name, "-- state length", x$state_len,
      "and", x$n_moves, "moves\n")
  cat("  arch      :", x$arch,
      switch(x$arch,
             lstm = , gru = "(one position per timestep)",
             resnet = "(embedded tokens, residual blocks)",
             transformer = "(pieces as a sequence, attention over them)",
             "(embedded tokens, dense stack)"), "\n")
  enc <- if (is.null(x$encoding)) "sticker" else x$encoding
  cat("  encoding  :", enc,
      if (identical(enc, "piece"))
        sprintf("-- %d pieces, %d slots each, %d inputs",
                x$layout$n_piece, x$layout$width,
                x$layout$n_piece * x$layout$n_piece * x$layout$width)
      else sprintf("-- %d positions as tokens", x$state_len), "\n")
  cat("  value     : 1 linear output (distance to solved)\n")
  cat("  policy    :", x$n_moves, "softmax outputs\n")
  invisible(x)
}

# A compiled model holds a graph built for one batch size, and asking it for a
# different one rebuilds that graph -- which at solve time, where the batch is
# 24 children rather than the 256 it trained on, runs out of context memory.
# So the batch is always the same size: short inputs are padded out with
# repeats of their first row and the padding is dropped from the answer.
adi_predict_padded <- function(model, x, batch_size, n_out) {
  n <- dim(x)[1L]
  if (n < batch_size) {
    idx <- c(seq_len(n), rep(1L, batch_size - n))
    # The recurrent input is [batch, steps, 1], the transformer's
    # [batch, pieces, depth] and the dense one [batch, cols]; indexing the
    # first axis and keeping the rest covers all three.
    x <- if (length(dim(x)) == 3L) x[idx, , , drop = FALSE]
         else x[idx, , drop = FALSE]
  }
  out <- ggmlR::ggml_predict(model, x, batch_size = batch_size)
  out <- matrix(as.numeric(out), ncol = n_out)
  out[seq_len(n), , drop = FALSE]
}

# Score states with the value network, returning a plain numeric vector.
adi_value_of <- function(model, states, batch_size, arch = "mlp",
                         layout = NULL) {
  as.numeric(adi_predict_padded(model, adi_encode(states, arch, layout),
                                batch_size, 1L))
}

# Move probabilities from the policy network, one row per state.
adi_policy_of <- function(model, states, batch_size, n_moves, arch = "mlp",
                          layout = NULL) {
  adi_predict_padded(model, adi_encode(states, arch, layout), batch_size,
                     n_moves)
}

#' Train Value and Policy by Autodidactic Iteration
#'
#' @param net A \code{cube_adi_net} from \code{\link{cube_adi_model}}
#' @param iterations Number of ADI iterations
#' @param batch_states States generated per iteration
#' @param max_depth Longest scramble drawn (uniform over 1..max_depth)
#' @param epochs Passes over each batch
#' @param batch_size Minibatch size for fitting and for scoring children
#' @param loss_thresh Refresh the frozen network once the batch loss on value
#'   falls below this. Set it too low and the copy never refreshes: the targets
#'   stay pinned to an untrained network, every state looks one move from
#'   solved, and the mean target sticks near 1 while the value loss looks
#'   healthy. If the reported mean target stops rising, this is the first thing
#'   to raise. Ignored when \code{refresh_every} is given.
#' @param refresh_every Refresh the frozen copy every this many iterations,
#'   instead of whenever the loss drops below \code{loss_thresh}. A threshold is
#'   awkward to set because it is compared against a loss whose scale changes as
#'   training proceeds: too low and the copy never refreshes, too high and it
#'   refreshes every iteration, which is worse. A copy refreshed every iteration
#'   is not frozen at all --- the targets are then computed from the same weights
#'   that are being fitted to them, the network converges on agreeing with
#'   itself, and the mean target stops moving while the loss looks fine. A fixed
#'   interval sidesteps the question: the copy is genuinely held for that many
#'   iterations whatever the loss happens to be doing.
#' @param verbose \code{TRUE} to report each iteration
#' @return The network, with \code{$history} recording loss per iteration
#' @export
cube_adi_train <- function(net, iterations = 100L, batch_states = 10000L,
                           max_depth = 20L, epochs = 1L, batch_size = 256L,
                           loss_thresh = 0.5, refresh_every = NULL,
                           verbose = TRUE) {
  adi_require_ggml()
  if (!inherits(net, "cube_adi_net")) stop("net must be a cube_adi_net")
  if (!is.null(refresh_every)) {
    refresh_every <- as.integer(refresh_every)
    if (is.na(refresh_every) || refresh_every < 1L)
      stop("refresh_every must be a positive whole number")
  }

  g       <- net$group
  n_moves <- net$n_moves
  history <- data.frame(iteration = integer(), value_loss = numeric(),
                        policy_loss = numeric(), refreshed = logical())

  # The frozen copy starts as the live network. Before the first fit both are
  # untrained, so the first batch of targets is noise everywhere except at the
  # solved children -- which is exactly the part that is exact, and the part
  # everything else is eventually derived from.
  frozen <- net$value

  for (it in seq_len(iterations)) {
    sc <- cube_adi_scramble(g$ptr, as.integer(batch_states),
                            as.integer(max_depth))
    ch <- cube_adi_children(g$ptr, sc$states)

    child_v <- adi_value_of(frozen, ch$children, batch_size, net$arch,
                            net$layout)
    tg      <- cube_adi_targets(child_v, ch$solved, n_moves)

    x  <- adi_encode(sc$states, net$arch, net$layout)
    nx <- dim(x)[1L]        # x may be 3-d for the recurrent architecture
    y_value  <- matrix(tg$value, ncol = 1L)
    y_policy <- matrix(0, nrow = nx, ncol = n_moves)
    y_policy[cbind(seq_len(nx), tg$policy)] <- 1

    net$value <- ggmlR::ggml_fit(net$value, x, y_value, epochs = epochs,
                                 batch_size = batch_size, verbose = 0L)
    net$policy <- ggmlR::ggml_fit(net$policy, x, y_policy, epochs = epochs,
                                  batch_size = batch_size, verbose = 0L)

    v_loss <- utils::tail(net$value$history$train_loss, 1L)
    p_loss <- utils::tail(net$policy$history$train_loss, 1L)

    # On a counter when one is given, on the loss otherwise. The loss rule was
    # written for the early iterations, where it falls fast and the copy should
    # follow it down; on a plateau it stops discriminating, and the copy then
    # either never refreshes or refreshes every time, depending on which side of
    # the threshold the plateau happens to sit.
    refreshed <- if (is.null(refresh_every)) isTRUE(v_loss < loss_thresh)
                 else it %% refresh_every == 0L
    if (refreshed) frozen <- net$value

    history <- rbind(history, data.frame(iteration = it, value_loss = v_loss,
                                         policy_loss = p_loss,
                                         refreshed = refreshed))
    if (verbose) {
      cat(sprintf("iter %4d | value %.4f | policy %.4f | mean target %.2f%s\n",
                  it, v_loss, p_loss, mean(tg$value),
                  if (refreshed) " | target refreshed" else ""))
    }
  }

  net$history <- history
  net
}

#' Solve a Cube With a Trained Network
#'
#' Walks downhill on the value network, one forward pass per move, refusing to
#' revisit a state it has already stood on. Banning only the inverse of the last
#' move is not enough -- four of the same quarter turn also return to where they
#' started -- so the walk remembers where it has been, which covers every length
#' of cycle with one rule.
#'
#' @param net A trained \code{cube_adi_net}
#' @param state Integer vector, the scrambled state
#' @param budget Most moves to try before giving up
#' @param use_policy Break ties with the policy network's ranking
#' @param batch_size Batch the networks were trained with. Scoring reuses the
#'   compiled graph at this size, padding the 24 children out to fill it.
#' @return List with \code{solved}, \code{path} (move names) and \code{values}
#' @export
cube_adi_solve <- function(net, state, budget = 200L, use_policy = TRUE,
                           batch_size = 256L) {
  adi_require_ggml()
  g       <- net$group
  cur     <- as.integer(state)
  path    <- character(0)
  values  <- numeric(0)
  seen    <- new.env(hash = TRUE, parent = emptyenv())
  assign(paste(cur, collapse = ","), TRUE, envir = seen)

  for (step in seq_len(budget)) {
    if (all(cur == seq_along(cur))) {
      return(list(solved = TRUE, path = path, values = values))
    }
    ch <- cube_adi_children(g$ptr, matrix(cur, nrow = 1L))
    v  <- adi_value_of(net$value, ch$children, batch_size = batch_size,
                       arch = net$arch, layout = net$layout)
    v[ch$solved] <- -Inf   # a solved child ends it, whatever the network says

    if (use_policy) {
      p <- adi_policy_of(net$policy, matrix(cur, nrow = 1L), batch_size,
                         net$n_moves, net$arch, net$layout)[1L, ]
      v <- v - 1e-3 * p    # policy nudges, value decides
    }

    ord  <- order(v)
    took <- FALSE
    for (a in ord) {
      cand <- ch$children[a, ]
      key  <- paste(cand, collapse = ",")
      if (!is.null(seen[[key]])) next
      assign(key, TRUE, envir = seen)
      cur    <- cand
      path   <- c(path, g$moves[a])
      values <- c(values, v[a])
      took   <- TRUE
      break
    }
    if (!took) break   # every child already visited
  }

  list(solved = all(cur == seq_along(cur)), path = path, values = values)
}

#' Save a Trained ADI Network
#'
#' Writes the network to a directory: the two ggml models as their own files,
#' and everything else as an RDS beside them.
#'
#' The group cannot be written directly --- it lives behind an external pointer
#' that means nothing in a later session. What is saved instead is the material
#' to rebuild it: the state length and, for each move, the permutation it
#' performs, read back by applying the move to the identity. Loading replays
#' those through \code{\link{perm_group}} and gets the same group, so the saved
#' network does not depend on the function that happened to build it.
#'
#' @param net A \code{cube_adi_net}
#' @param path Directory to write into; created if absent
#' @return \code{path}, invisibly
#' @seealso \code{\link{cube_adi_load}}
#' @export
cube_adi_save <- function(net, path) {
  adi_require_ggml()
  if (!inherits(net, "cube_adi_net")) stop("net must be a cube_adi_net")

  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  ggmlR::ggml_save_model(net$value,  file.path(path, "value.ggml"))
  ggmlR::ggml_save_model(net$policy, file.path(path, "policy.ggml"))

  g    <- net$group
  id   <- group_identity(g)
  # A permutation is what the move does to the identity, so this recovers the
  # generators without reaching into the C++ table.
  perms <- lapply(g$moves, function(m) as.integer(group_apply(g, id, m)))
  names(perms) <- g$moves

  saveRDS(list(
    version   = 1L,
    group     = list(name = g$name, n = g$n, perms = perms),
    n_moves   = net$n_moves,
    state_len = net$state_len,
    arch      = net$arch,
    encoding  = net$encoding,
    layout    = net$layout,
    history   = net$history
  ), file.path(path, "meta.rds"))

  invisible(path)
}

#' Load a Trained ADI Network
#'
#' Inverse of \code{\link{cube_adi_save}}. The group is rebuilt from the saved
#' permutations, so the result is usable by \code{\link{cube_adi_solve}} and
#' \code{\link{cube_adi_train}} without any of the code that first created it.
#'
#' @param path Directory written by \code{\link{cube_adi_save}}
#' @param backend Backend for the loaded models: \code{"cpu"}, \code{"vulkan"}
#'   or \code{"auto"}
#' @return A \code{cube_adi_net}
#' @seealso \code{\link{cube_adi_save}}
#' @export
cube_adi_load <- function(path, backend = "auto") {
  adi_require_ggml()
  meta_path <- file.path(path, "meta.rds")
  if (!file.exists(meta_path))
    stop("no meta.rds in ", path, " -- not a cube_adi_save() directory")
  meta <- readRDS(meta_path)
  if (!identical(meta$version, 1L))
    stop("unsupported save format version: ", meta$version)

  group <- perm_group(meta$group$perms, n = meta$group$n,
                      name = meta$group$name)

  structure(list(
    value     = ggmlR::ggml_load_model(file.path(path, "value.ggml"),
                                       backend = backend),
    policy    = ggmlR::ggml_load_model(file.path(path, "policy.ggml"),
                                       backend = backend),
    group     = group,
    n_moves   = meta$n_moves,
    state_len = meta$state_len,
    arch      = meta$arch,
    encoding  = meta$encoding,
    layout    = meta$layout,
    history   = meta$history
  ), class = "cube_adi_net")
}
