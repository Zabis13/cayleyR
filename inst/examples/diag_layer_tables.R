#!/usr/bin/env Rscript
# The layer beside each face, measured, and checked against the tables the
# 4x4x4 centre solver carries.
#
# src/cube_centres.h needs to know which twelve centre cells make up the layer
# lying against a face: step 2 builds one and step 3 must not disturb it. Two
# of the six were written out by hand -- L, where the build happens, and D,
# where the rotation puts it -- and the other four were missing, which is what
# kept the pipeline from starting anywhere but L.
#
# An attempt to derive all six from the slice table failed, and the way it
# failed is worth recording. It tried to pick the near slice of an axis by
# asking whether the layer's cells still showed their own colours after that
# slice turned -- but turning a layer carries its cells between the faces of
# the ring, so their colours change by design and no slice can pass. Set
# membership is the invariant, not colour; and that does not separate the two
# slices of an axis either, since both map the ring onto itself.
#
# The definition, rather than a test for it: the layer of a face is what the
# WIDE turn of that face moves -- the face together with the slice against it.
# That is how the hand tables were measured in the first place, and it answers
# for all six.
#
# The slot numbering here is the C++ one, centre_slots_of: face*16 + {5,6,9,10},
# reading order. It is NOT the numbering cube_centre_structure() uses, which
# runs along the face-turn cycle, so the two tables cannot be compared cell by
# cell. The translation below goes through sticker numbers instead.

library(cayleyR)

N  <- 4L
FS <- N * N
face_names <- c("U", "R", "F", "D", "L", "B")

g  <- cube_group(N)
id <- group_identity(g)

# centre_slots_of(f)[k] as a 1-based sticker index
cpp_slot_sticker <- function(f, k) f * FS + c(5L, 6L, 9L, 10L)[k] + 1L

# and back: a sticker to its (face, slot), or NULL if it is not a centre
cpp_cell_of <- function(sticker) {
  f   <- (sticker - 1L) %/% FS
  loc <- (sticker - 1L) %% FS
  k   <- match(loc, c(5L, 6L, 9L, 10L))
  if (is.na(k)) return(NULL)
  list(face = f, slot = k)
}

wide <- c("Uw", "Rw", "Fw", "Dw", "Lw", "Bw")

layer_of <- function(f) {
  p <- group_apply(g, id, cube_expand_move(wide[f + 1L], N))
  out <- list()
  for (s in which(p != seq_along(p))) {
    cell <- cpp_cell_of(s)
    if (!is.null(cell)) out[[length(out) + 1L]] <- cell
  }
  if (!length(out)) return(data.frame(face = integer(0), slot = integer(0)))
  df <- data.frame(face = vapply(out, `[[`, integer(1), "face"),
                   slot = vapply(out, `[[`, integer(1), "slot"))
  df[order(df$face, df$slot), ]
}

show <- function(df)
  paste(sprintf("%s%d", face_names[df$face + 1L], df$slot), collapse = " ")

cat("\n== the layer of each face, by its wide turn --------------------\n\n")
layers <- lapply(0:5, layer_of)
for (f in 0:5)
  cat(sprintf("  face %d (%s) : %2d cells : %s\n",
              f, face_names[f + 1L], nrow(layers[[f + 1L]]),
              show(layers[[f + 1L]])))

# the two tables in src/cube_centres.h, transcribed
ref_L <- data.frame(  # l_slice_cells: all of L, U{1,3} F{1,3} D{1,3} B{2,4}
  face = c(4, 4, 4, 4,  0, 0,  2, 2,  3, 3,  5, 5),
  slot = c(1, 2, 3, 4,  1, 3,  1, 3,  1, 3,  2, 4))
ref_D <- data.frame(  # d_slice_cells: all of D, slots 3 and 4 of L, F, R, B
  face = c(3, 3, 3, 3,  4, 4,  2, 2,  1, 1,  5, 5),
  slot = c(1, 2, 3, 4,  3, 4,  3, 4,  3, 4,  3, 4))
ref_L <- ref_L[order(ref_L$face, ref_L$slot), ]
ref_D <- ref_D[order(ref_D$face, ref_D$slot), ]

cat("\n== against the hand-written tables -----------------------------\n\n")
ok_all <- TRUE
for (z in list(list(f = 4L, r = ref_L, nm = "l_slice_cells"),
               list(f = 3L, r = ref_D, nm = "d_slice_cells"))) {
  got  <- show(layers[[z$f + 1L]])
  want <- show(z$r)
  ok   <- identical(got, want)
  ok_all <- ok_all && ok
  cat(sprintf("  %-14s (face %d) : %s\n", z$nm, z$f,
              if (ok) "matches" else "DIFFERS"))
  if (!ok) {
    cat(sprintf("      hand     : %s\n", want))
    cat(sprintf("      measured : %s\n", got))
    a <- strsplit(got, " ")[[1]]
    b <- strsplit(want, " ")[[1]]
    cat(sprintf("      extra    : %s\n", paste(setdiff(a, b), collapse = " ")))
    cat(sprintf("      missing  : %s\n", paste(setdiff(b, a), collapse = " ")))
  }
}

cat("\n== what every layer must satisfy -------------------------------\n\n")
sizes <- vapply(layers, nrow, integer(1))
cat(sprintf("  twelve cells each      : %s (%s)\n",
            if (all(sizes == 12L)) "yes" else "NO",
            paste(sizes, collapse = " ")))

own <- vapply(0:5, function(f) sum(layers[[f + 1L]]$face == f), integer(1))
cat(sprintf("  all four of its own    : %s (%s)\n",
            if (all(own == 4L)) "yes" else "NO", paste(own, collapse = " ")))

spans <- vapply(layers, function(L) length(unique(L$face)), integer(1))
cat(sprintf("  spans five faces       : %s (%s)\n",
            if (all(spans == 5L)) "yes" else "NO", paste(spans, collapse = " ")))

cat("\n  closed under its own wide turn:\n")
closed <- logical(6)
for (f in 0:5) {
  L  <- layers[[f + 1L]]
  sk <- mapply(cpp_slot_sticker, L$face, L$slot)
  p  <- group_apply(g, id, cube_expand_move(wide[f + 1L], N))
  closed[f + 1L] <- all(match(sk, p) %in% sk)
  cat(sprintf("    %s : %s\n", face_names[f + 1L],
              if (closed[f + 1L]) "closed" else "NOT closed"))
}

cat("\n== verdict -----------------------------------------------------\n\n")
if (ok_all && all(sizes == 12L) && all(own == 4L) && all(spans == 5L) &&
    all(closed)) {
  cat("  Both hand tables reproduced and every layer well formed. This is\n")
  cat("  where the table in slice_cells_of comes from.\n\n")
} else {
  cat("  Something disagrees -- the table in slice_cells_of is in doubt.\n\n")
}
