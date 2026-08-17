#!/usr/bin/env Rscript
# Do the phases turn what their generator names say they turn?
#
# A phase holds a list of generator names, and build_spec4() parses each one
# into a piece permutation. The search then uses the permutations and never
# looks at the names again. So the names can be right -- can match twips's
# lists exactly, as they were checked to -- while the geometry underneath is
# something else entirely, and nothing about the names would show it.
#
# The check is direct. For each generator:
#
#   through the phase   apply the phase's own move table to a solved cube
#   through R           apply the same word with cube_moves(4)
#
# then read both as piece permutations and compare. They must agree: they are
# meant to be the same alphabet. A disagreement means every search that phase
# has run was over moves other than the ones it documents -- which would
# explain a phase that cannot solve a cube six moves from solved, and would
# explain it in a way no amount of table tuning ever could.
#
# Run with:  Rscript inst/examples/diag_phase_generators.R

library(cayleyR)

N  <- 4L
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

# Apply a generator word -- "U", "1x2", "U 2y U 2y" -- through the package's
# own alphabet. Powers are spelled out, since the alphabet holds only quarter
# turns: "1x2" is 1x twice, "U'" is the named inverse.
apply_word <- function(state, word) {
  for (tok in strsplit(trimws(word), " +")[[1]]) {
    if (grepl("2$", tok) && !tok %in% names(mv)) {
      base <- sub("2$", "", tok)
      state <- state[mv[[base]]]
      state <- state[mv[[base]]]
    } else {
      state <- state[mv[[tok]]]
    }
  }
  state
}

hr("setup")
cat("Each phase's generators, applied two ways and compared as piece\n")
cat("permutations: through the phase's own move table, and through\n")
cat("cube_moves(4). They must agree.\n")

id <- cube_identity(N)
all_ok <- TRUE

for (ph in 1:3) {
  g <- cayleyR:::cube_phase_generators_cpp(ph)
  hr(paste("phase", ph, "--", length(g$names), "generators"))

  bad <- character(0)
  for (m in seq_along(g$names)) {
    nm <- g$names[[m]]

    # Through R's alphabet, then read into the phases' representation.
    st <- apply_word(id, nm)
    ours <- cayleyR:::cube_to_pieces4_cpp(st)

    same_perm <- identical(as.integer(ours$perm), as.integer(g$perm[[m]]))
    same_ori  <- identical(as.integer(ours$ori),  as.integer(g$ori[[m]]))

    if (!same_perm || !same_ori) {
      bad <- c(bad, nm)
      cat(sprintf("  MISMATCH  %-14s  perm %s  ori %s\n", nm,
                  if (same_perm) "ok" else "DIFFERS",
                  if (same_ori) "ok" else "DIFFERS"))
      # Where they differ, in pieces -- the useful detail is which orbit moved
      # differently, not the whole vector.
      d <- which(as.integer(ours$perm) != as.integer(g$perm[[m]]))
      if (length(d) > 0) {
        cat("            differing piece slots: ",
            paste(head(d, 20), collapse = " "),
            if (length(d) > 20) " ..." else "", "\n", sep = "")
      }
    }
  }

  if (length(bad) == 0) {
    cat("  all ", length(g$names), " generators agree\n", sep = "")
  } else {
    all_ok <- FALSE
    cat("\n  ", length(bad), " of ", length(g$names),
        " generators do NOT match their names\n", sep = "")
  }
}

hr("verdict")
if (all_ok) {
  cat("Every generator of every phase turns what its name says. The phases\n")
  cat("and the package share one alphabet, so a phase that fails is failing\n")
  cat("for some other reason -- the moves it searches with are right.\n")
} else {
  cat("At least one phase searches with a move that is not what its name\n")
  cat("says. That is the bug: the phase explores a different graph from the\n")
  cat("one its goal and its prune table were built for, so it can miss short\n")
  cat("solutions entirely and spend any budget you give it.\n")
}
