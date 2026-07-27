#' Generate Landmark States for a Permutation of Size n
#'
#' Builds 25 structurally distinct permutations of \code{1:n}, each defined by
#' a rule rather than by a random draw, so the same construction can be compared
#' across different \code{n}. They serve as landmarks (fixed probe points) in the
#' Cayley graph: the distance from the identity to each landmark, measured for
#' several small \code{n} where the diameter is known, gives a ratio
#' \code{d / diameter} that can be extrapolated to larger graphs.
#'
#' The constructions, in the order returned:
#' \enumerate{
#'   \item \strong{full_reverse} --- \eqn{\sigma(j) = n + 1 - j}, the maximum
#'     number of inversions.
#'   \item \strong{block_swap} --- the first half and the last half exchange
#'     places. For odd \code{n} the middle element stays put.
#'   \item \strong{riffle} --- perfect interleaving of the two halves:
#'     \eqn{1, h+1, 2, h+2, \ldots} with \eqn{h = \lceil n/2 \rceil}.
#'   \item \strong{envelope} --- taken alternately from the two ends towards the
#'     centre: \eqn{n, 1, n-1, 2, \ldots}.
#'   \item \strong{adjacent_swaps} --- the full pairing of neighbours:
#'     swap(1,2), swap(3,4), ... A trailing odd element is left alone.
#'   \item \strong{broken_cycle} --- the \code{n}-cycle \eqn{2,3,\ldots,n,1}
#'     with the last two entries exchanged.
#'   \item \strong{zigzag} --- all odd values ascending, then all even values
#'     descending.
#'   \item \strong{block_rotate3} --- the sequence cut into three blocks ABC of
#'     deliberately unequal length and reassembled as CAB. Equal blocks would
#'     make this a plain rotation, i.e. one \code{L} move from the identity.
#'   \item \strong{two_cycles} --- two independent cyclic shifts, one inside
#'     each half.
#'   \item \strong{shift_reverse} --- a left shift by 2 followed by reversing the
#'     first 4 elements.
#'   \item \strong{pair_shift} --- odd positions take the value two ahead,
#'     wrapping round the odd positions only.
#'   \item \strong{reverse_first} --- the first half reversed, the second fixed.
#'   \item \strong{reverse_second} --- the first half fixed, the second reversed.
#'   \item \strong{spiral} --- \eqn{1, n, 2, n-1, \ldots}, alternating from the
#'     two ends but starting at the bottom.
#'   \item \strong{local_block} --- a single block of four rotated in place,
#'     every other tile untouched.
#'   \item \strong{single_swap} --- one transposition in the middle; the
#'     permutation closest to the identity in this set.
#'   \item \strong{faro_in} --- the mirror of \code{riffle}: the upper half
#'     leads the interleaving.
#'   \item \strong{block_reverse_pairs} --- the pairs (1,2)(3,4)... kept intact
#'     but listed in reverse block order.
#'   \item \strong{doubling} --- \eqn{\sigma(j) = 2j \bmod (n+1)}. That is a
#'     bijection only for even \code{n}; for odd \code{n} the map runs on
#'     \code{1..n-1} modulo \code{n} and the last tile stays put.
#'   \item \strong{shift_third} --- a shift by \code{n/3} with the displaced
#'     block reversed.
#'   \item \strong{double_riffle} --- the halves interleaved two elements at a
#'     time rather than one.
#'   \item \strong{cycles3} --- 3-cycles (1 2 3)(4 5 6)... with any tail fixed.
#'   \item \strong{alt_pairs} --- every other pair reversed: swap(1,2), leave
#'     (3,4), swap(5,6), ...
#'   \item \strong{cascade} --- swapped pairs offset by one as they march along,
#'     like falling dominoes.
#'   \item \strong{derangement} --- the halves exchanged and then every fixed
#'     point displaced, so no tile keeps its own place. Reversing one half
#'     instead would only reproduce \code{reverse_first} rotated by \code{n/2}.
#' }
#'
#' At \code{n = 6} \code{cycles3} degenerates into \code{two_cycles}; for every
#' \code{n} from 7 upwards all 25 states are distinct.
#'
#' @param n Integer, permutation size (must be at least 4).
#' @return A data.frame with one row per landmark and columns \code{id},
#'   \code{name}, \code{description}, \code{state_str} (underscore-joined) and
#'   \code{state} (a list column holding the integer vector).
#' @export
#' @examples
#' landmark_states(10)$state_str
landmark_states <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 4L) {
    stop("landmark_states: n must be an integer of at least 4")
  }

  v <- seq_len(n)
  h <- n %/% 2L          # size of the lower half
  hi <- n - h            # size of the upper half (h or h + 1)

  # 1. full reverse
  full_reverse <- rev(v)

  # 2. block swap: first half <-> last half, odd middle element fixed
  if (n %% 2L == 0L) {
    block_swap <- c(v[(h + 1L):n], v[1L:h])
  } else {
    mid <- h + 1L
    block_swap <- c(v[(mid + 1L):n], mid, v[1L:h])
  }

  # 3. riffle: interleave the two halves, upper half first when sizes differ
  lower <- v[1L:hi]
  upper <- v[(hi + 1L):n]
  riffle <- integer(n)
  riffle[seq(1L, 2L * length(lower), by = 2L)] <- lower
  if (length(upper) > 0L) {
    riffle[seq(2L, 2L * length(upper), by = 2L)] <- upper
  }

  # 4. envelope: alternate from the two ends towards the centre
  envelope <- integer(n)
  top <- rev(v)
  bot <- v
  for (i in seq_len(n)) {
    j <- (i + 1L) %/% 2L
    envelope[i] <- if (i %% 2L == 1L) top[j] else bot[j]
  }

  # 5. adjacent swaps across the whole vector
  adjacent_swaps <- v
  for (i in seq(1L, n - 1L, by = 2L)) {
    adjacent_swaps[c(i, i + 1L)] <- c(v[i + 1L], v[i])
  }

  # 6. broken n-cycle: 2,3,...,n,1 with the final pair exchanged
  broken_cycle <- c(v[-1L], 1L)
  broken_cycle[c(n - 1L, n)] <- broken_cycle[c(n, n - 1L)]

  # 7. zigzag: odd values ascending, then even values descending
  zigzag <- c(v[v %% 2L == 1L], rev(v[v %% 2L == 0L]))

  # 8. three blocks ABC -> CAB. With equal blocks that is just a rotation, i.e.
  #     a single L move away from the identity, so the sizes are deliberately
  #     staggered: a short A, a long B, and whatever is left as C.
  szA <- max(1L, n %/% 5L)
  szB <- max(1L, n %/% 3L)
  A <- v[1L:szA]
  B <- v[(szA + 1L):(szA + szB)]
  C <- v[(szA + szB + 1L):n]
  block_rotate3 <- c(C, A, B)

  # 9. two independent cyclic shifts, one per half
  L1 <- v[1L:hi]
  L2 <- v[(hi + 1L):n]
  two_cycles <- c(c(L1[-1L], L1[1L]), c(L2[-1L], L2[1L]))

  # 10. left shift by 2, then reverse the first 4 elements
  shifted <- c(v[-(1L:2L)], v[1L:2L])
  m <- min(4L, n)
  shift_reverse <- shifted
  shift_reverse[1L:m] <- rev(shifted[1L:m])

  # 11. neighbouring pairs swapped and then shifted: 3,2,5,4,7,6,... with the
  #     tail wrapping back to the front
  # Odd positions take the value two ahead, wrapping round the odd positions
  # only, so the result stays a permutation for every n.
  odd_pos <- seq(1L, n, by = 2L)
  pair_shift <- v
  pair_shift[odd_pos] <- v[odd_pos[c(seq_along(odd_pos)[-1L], 1L)]]

  # 12. reverse the first half only
  reverse_first <- c(rev(v[1L:hi]), v[(hi + 1L):n])

  # 13. reverse the second half only
  reverse_second <- c(v[1L:hi], rev(v[(hi + 1L):n]))

  # 14. spiral: 1, n, 2, n-1, ... taken alternately from the two ends
  spiral <- integer(n)
  lo <- 1L
  up <- n
  for (i in seq_len(n)) {
    if (i %% 2L == 1L) {
      spiral[i] <- lo
      lo <- lo + 1L
    } else {
      spiral[i] <- up
      up <- up - 1L
    }
  }

  # 15. one block of four rotated in place, everything else fixed
  local_block <- v
  bs <- min(hi, max(1L, n - 5L))          # where the damaged block starts
  be <- min(n, bs + 3L)
  if (be > bs) local_block[bs:be] <- c(v[be], v[bs:(be - 1L)])

  # 16. a single transposition in the middle, every other tile fixed
  single_swap <- v
  mid <- max(1L, n %/% 2L)
  single_swap[c(mid, mid + 1L)] <- v[c(mid + 1L, mid)]

  # 17. faro in: the upper half leads the interleaving
  faro_in <- integer(n)
  lead <- v[(h + 1L):n]                   # length hi, the larger half
  follow <- v[1L:h]
  faro_in[seq(1L, 2L * length(lead), by = 2L)] <- lead
  if (length(follow) > 0L) {
    faro_in[seq(2L, 2L * length(follow), by = 2L)] <- follow
  }

  # 18. pairs (1,2)(3,4)... kept intact but listed in reverse block order
  np <- n %/% 2L
  pair_blocks <- lapply(seq_len(np), function(i) v[c(2L * i - 1L, 2L * i)])
  block_reverse_pairs <- unlist(rev(pair_blocks))
  if (n %% 2L == 1L) block_reverse_pairs <- c(block_reverse_pairs, v[n])

  # 19. doubling map sigma(j) = 2j mod (n+1), a classic number-theoretic
  #     permutation; it is a bijection on 1..n for every n
  # 2j mod m permutes 1..m-1 exactly when m is odd. For even n the modulus n+1
  # is odd and covers all of 1..n. For odd n it is not, so the map is applied to
  # 1..n-1 (modulus n, odd) and the last tile is left in place.
  if (n %% 2L == 0L) {
    doubling <- as.integer((2L * v) %% (n + 1L))
  } else {
    doubling <- c(as.integer((2L * v[-n]) %% n), n)
  }

  # 20. shift by n/3, then reverse the block that was moved to the back. A bare
  #     rotation is one L move from the identity and from every other rotation,
  #     so the reversal is what makes this a landmark rather than a duplicate.
  s3 <- max(1L, n %/% 3L)
  shift_third <- c(v[(s3 + 1L):n], rev(v[1L:s3]))

  # 21. interleave the halves two at a time instead of one
  double_riffle <- integer(0)
  a <- 1L
  b <- hi + 1L
  while (a <= hi || b <= n) {
    if (a <= hi) {
      take <- min(2L, hi - a + 1L)
      double_riffle <- c(double_riffle, v[a:(a + take - 1L)])
      a <- a + take
    }
    if (b <= n) {
      take <- min(2L, n - b + 1L)
      double_riffle <- c(double_riffle, v[b:(b + take - 1L)])
      b <- b + take
    }
  }

  # 22. 3-cycles instead of 2-cycles: (1 2 3)(4 5 6)... with any tail left fixed
  cycles3 <- v
  for (i in seq(1L, n - 2L, by = 3L)) {
    cycles3[i:(i + 2L)] <- v[c(i + 1L, i + 2L, i)]
  }

  # 23. every other pair reversed: swap(1,2), leave (3,4), swap(5,6), ...
  alt_pairs <- v
  for (i in seq(1L, n - 1L, by = 4L)) {
    alt_pairs[c(i, i + 1L)] <- v[c(i + 1L, i)]
  }

  # 24. cascade: pairs swapped, but each successive pair offset by one, so the
  #     swaps march along the ring like falling dominoes
  cascade <- v
  i <- 1L
  step <- 0L
  while (i < n) {
    cascade[c(i, i + 1L)] <- v[c(i + 1L, i)]
    step <- step + 1L
    i <- i + if (step %% 2L == 1L) 2L else 3L
  }

  # 25. derangement: no tile keeps its own place. Taking the halves in the other
  #     order would only be reverse_first rotated by n/2, and a rotation is a
  #     single L move -- the two would sit a handful of moves apart in the graph
  #     instead of being genuinely different. Interleaving from the far end and
  #     then displacing the fixed points keeps it clear of every other entry.
  # The doubling map already moves almost everything; displacing whatever it
  # leaves in place turns it into a derangement without copying the shape of any
  # block construction.
  derangement <- doubling
  while (any(derangement == v)) {
    f <- which(derangement == v)[1L]
    g <- if (f < n) f + 1L else 1L
    derangement[c(f, g)] <- derangement[c(g, f)]
  }

  states <- list(
    full_reverse   = full_reverse,
    block_swap     = block_swap,
    riffle         = riffle,
    envelope       = envelope,
    adjacent_swaps = adjacent_swaps,
    broken_cycle   = broken_cycle,
    zigzag         = zigzag,
    block_rotate3  = block_rotate3,
    two_cycles     = two_cycles,
    shift_reverse  = shift_reverse,
    pair_shift          = pair_shift,
    reverse_first       = reverse_first,
    reverse_second      = reverse_second,
    spiral              = spiral,
    local_block         = local_block,
    single_swap         = single_swap,
    faro_in             = faro_in,
    block_reverse_pairs = block_reverse_pairs,
    doubling            = doubling,
    shift_third         = shift_third,
    double_riffle       = double_riffle,
    cycles3             = cycles3,
    alt_pairs           = alt_pairs,
    cascade             = cascade,
    derangement         = derangement
  )
  states <- lapply(states, as.integer)

  descriptions <- c(
    full_reverse   = "sigma(j) = n+1-j, maximum inversions",
    block_swap     = "first half <-> last half",
    riffle         = "perfect interleaving of the two halves",
    envelope       = "alternating from both ends towards the centre",
    adjacent_swaps = "swap(1,2), swap(3,4), ...",
    broken_cycle   = "n-cycle 2..n,1 with the last pair exchanged",
    zigzag         = "odd values ascending, then even values descending",
    block_rotate3  = "three blocks ABC -> CAB",
    two_cycles     = "independent cyclic shift inside each half",
    shift_reverse  = "left shift by 2, then reverse the first 4",
    pair_shift          = "neighbouring pairs swapped and shifted by two",
    reverse_first       = "reverse the first half, second half fixed",
    reverse_second      = "first half fixed, reverse the second half",
    spiral              = "1, n, 2, n-1, ... alternating from both ends",
    local_block         = "one block of four rotated, everything else fixed",
    single_swap         = "a single transposition in the middle",
    faro_in             = "faro in: the upper half leads the interleaving",
    block_reverse_pairs = "pairs kept intact, listed in reverse block order",
    doubling            = "sigma(j) = 2j mod (n+1), the doubling map",
    shift_third         = "shift by n/3 with the displaced block reversed",
    double_riffle       = "interleave the halves two elements at a time",
    cycles3             = "3-cycles (1 2 3)(4 5 6)... instead of 2-cycles",
    alt_pairs           = "every other pair reversed",
    cascade             = "swapped pairs marching along like dominoes",
    derangement         = "the doubling map with its fixed points displaced"
  )

  bad <- vapply(states, function(s) !setequal(s, v), logical(1))
  if (any(bad)) {
    stop("landmark_states: construction(s) ",
         paste(names(states)[bad], collapse = ", "),
         " did not yield a permutation for n = ", n)
  }

  # A rotation costs a single L move, so two landmarks that differ only by one
  # are the same point of the graph for any practical purpose. Rather than hand
  # tuning each formula until the clashes happen to vanish -- which just moves
  # them elsewhere -- collisions are detected here and broken by transposing one
  # pair of tiles in the later of the two entries, repeating until the canonical
  # forms are all distinct.
  canonical <- function(s) {
    min(vapply(seq_len(n) - 1L, function(r) {
      paste(s[((seq_len(n) - 1L + r) %% n) + 1L], collapse = "_")
    }, character(1)))
  }

  keys <- vapply(states, canonical, character(1))
  for (i in seq_along(states)[-1L]) {
    guard <- 0L
    while (keys[i] %in% keys[seq_len(i - 1L)] && guard < n) {
      j <- guard %% (n - 1L) + 1L
      states[[i]][c(j, j + 1L)] <- states[[i]][c(j + 1L, j)]
      keys[i] <- canonical(states[[i]])
      guard <- guard + 1L
    }
    if (keys[i] %in% keys[seq_len(i - 1L)]) {
      stop("landmark_states: could not separate ", names(states)[i],
           " from an earlier construction at n = ", n)
    }
  }

  data.frame(
    id = seq_along(states),
    name = names(states),
    description = unname(descriptions[names(states)]),
    state_str = vapply(states, paste, character(1), collapse = "_"),
    state = I(unname(states)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
