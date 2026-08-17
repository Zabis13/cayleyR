#' Solve a 4x4x4 by reduction, falling back when the search does not pay off
#'
#' Phase 3 of the reduction is usually cheap when it succeeds and expensive when
#' it fails, and the two are told apart only afterwards. So the leash is a ladder
#' rather than a single length: every orientation is tried at a small budget
#' before any is tried at a large one. See \code{budget_steps}.
#'
#' When no orientation reduces the cube at any rung, \code{cube_solve4} finishes
#' the job. That solver always succeeds and its solutions are long -- 186 to 444
#' moves on six random cubes -- so it is the floor rather than the goal.
#'
#' @param state Integer vector of 96 stickers.
#' @param node_budget The largest number of nodes phase 3 may spend on one
#'   orientation. It is the top of the ladder rather than the amount every
#'   attempt pays -- see \code{budget_steps}. It bounds phase 3 only; phases 1
#'   and 2 have \code{prep_budget}.
#' @param prep_budget The largest number of nodes phases 1 and 2 may spend on
#'   one orientation. Separate from \code{node_budget}, which is a ladder top
#'   divided by \code{budget_steps}, where this is spent whole: sharing the one
#'   name gave phase 2 the undivided ceiling. Measured at 5e7, phase 2 usually
#'   took 0.1 to 0.5 s but spent 10.8 s on 30 million nodes on one orientation,
#'   which is what made that cube the slowest of its batch.
#' @param budget_steps The rungs of that ladder, as fractions of
#'   \code{node_budget}. Every orientation is tried at the first rung before any
#'   of them is tried at the second, so a cube that phase 3 finds cheaply is
#'   found cheaply whichever orientation it needed.
#'
#'   Why a ladder: failure costs the full budget by definition, since
#'   \code{exhausted} is a statement about the leash and not about the cube. One
#'   cube exhausted 5e7 twice, 84 s, before being found on a third orientation
#'   in 25.9 s. Sweeping every orientation low first costs a fraction of one
#'   failure high, and nothing is wasted -- the search restarts at each rung.
#'
#'   Where the rungs sit matters more than how many. A first rung of 2e6 was
#'   worse than no ladder for a cube that succeeded at 2.4 million nodes: all
#'   four orientations exhausted it first, 6.2 s to 12.7 s. Hence 0.1.
#'
#'   The rung at 1.0 was removed. Over thirty cubes it cost roughly 920 s of the
#'   run and yielded three cubes worth having (152, 134, 152 moves); of the ten
#'   it solved, eight came back at 294 to 312 moves, which \code{cube_solve4}
#'   matches for free at 258 to 396. Set \code{c(0.1, 0.3, 1)} to buy those
#'   three back at roughly triple the median time, or \code{1} for one rung at
#'   the full budget.
#'
#'   The rung at 0.3 has the same weakness in smaller form, and it is not yet
#'   measured whether it should go. Over thirty cubes it carried seven, but
#'   three of those returned 294 to 316 moves -- no shorter than the fallback --
#'   and every one of its successes landed between 5.3 and 12.3 million nodes,
#'   so the rung is never spent to its limit by a winner.
#' @param orientations Whole-cube rotations to try, as move words. Each is
#'   applied before the reduction and undone after, which changes nothing about
#'   the cube but everything about which goal the phases aim at.
#' @param max_orientations How many orientations to try before falling back.
#'   The default of 4 is a time bound, not a quality one -- trying all 24 costs
#'   twenty-four times as long for the cubes that end up falling back anyway.
#' @param phase2_solutions How many phase-2 solutions to take from each
#'   orientation. Each is a different state to start phase 3 from, and they are
#'   swept alongside the orientations, so this multiplies the candidates rather
#'   than replacing them.
#'
#'   The cheap end of the same idea the orientations serve: measured on four
#'   cubes, all four handovers were distinct on every cube and the whole set
#'   cost 0.03 to 0.24 s, against the tens of seconds a failed phase 3 costs.
#'   The candidates differ in cost by factors -- on one cube the second solution
#'   took 2.9 s where the first exhausted its budget in 43.5 -- and the sweep's
#'   job is to reach the cheap one first.
#'
#'   They do not replace the orientations: on one cube all four phase-2
#'   solutions exhausted the budget while the orientation sweep solved it.
#'   Set to 1 for the old behaviour: one candidate per orientation.
#' @param max_candidates How many candidates to try before giving the cube to
#'   \code{cube_solve4} instead. \code{NA}, the default, tries all of them.
#'
#'   The dial between a short answer and a quick one. \code{cube_solve4} needs
#'   no search and answers in a fraction of a second at 186 to 444 moves; the
#'   reduction searches for shorter and pays the full budget for every candidate
#'   that fails. Measured on one cube: sixteen candidates, 355 s, 146 moves.
#'
#'   A cap of 4 with \code{phase2_solutions = 4} means one orientation's worth
#'   of tries, then the fast answer.
#' @param shuffle_candidates Try the candidates in random order.
#'
#'   The build order is arbitrary, not a ranking: no cheap property was found
#'   that predicts a candidate's cost. Shuffling matters when the sweep is
#'   capped -- a fixed arbitrary order makes every cube's luck depend on the
#'   same accident, where a random one spreads it.
#' @param workers How many candidates to search at once. The candidates at a
#'   rung are independent -- each is a phase-3 search from its own state,
#'   sharing nothing -- so they parallelise without any coordination.
#'
#'   What it saves is bounded by where the winner sits: a cube won on the first
#'   candidate gains nothing, one won on the sixteenth gains nearly the full
#'   divisor. Read \code{candidate} in the return value before choosing.
#'
#'   The caution is the prune table, not the CPU: it is a process-wide 256MB
#'   table, shared copy-on-write while nobody writes, and a search deepening
#'   past its fill depth \emph{does} write, so N workers then cost N tables. Use
#'   several workers only with a prefilled table. Windows has no fork and
#'   degrades to sequential.
#' @param exact_centres Whether phase 3 uses the exact centre table. Measured:
#'   it removes the false zero bounds the hash table returns and solved one of
#'   six cubes that were failing, so it is on by default here.
#' @param verbose Print what each stage did, and say which orientation and
#'   budget is about to be tried before it is tried.
#' @param progress_every Nodes between progress lines from inside phase 3, or
#'   \code{0} for silence. Phase 3 is the only stage that can spend tens of
#'   seconds in one call -- measured at 43 s for a single exhausted orientation
#'   -- and with nothing printed it is indistinguishable from a hang. A value
#'   around 1e6 gives a line roughly every second at the rate that was measured.
#'
#' @return A list with \code{path} (the moves), \code{method}
#'   (\code{"reduce"}, \code{"solve4"}, \code{"already-solved"} or
#'   \code{"unsolved"}), \code{solved} (the path was replayed and checked, not
#'   assumed), \code{orientation} (the rotation that worked, empty for none),
#'   \code{candidate} (which of that orientation's phase-2 solutions it was,
#'   as \code{"1y#4"} -- the rotation alone does not say, and with several
#'   solutions per orientation the difference is most of the time a slow cube
#'   spends), \code{n_moves}, \code{seconds}, \code{attempts}, and \code{ops}.
#'
#'   \code{ops} is a data.frame with one row per operation performed, in the
#'   order performed: \code{op} (\code{"phase 1"}, \code{"phase 2"},
#'   \code{"phase 3"}, \code{"tail solve4"}, \code{"verify"} and so on),
#'   \code{secs}, \code{detail} (the orientation, and for phase 3 the rung),
#'   \code{nodes} and \code{outcome}. Unlike \code{attempts} it covers the
#'   attempts that failed as well as the one that worked, which is where most of
#'   a slow cube's seconds are: its rows sum to \code{seconds} up to a final
#'   \code{"unaccounted"} row, so nothing is left implicit.
#'
#' @details
#' The fallback is not a failure mode to be minimised away. A reduction that
#' does not finish inside its budget has not proved the cube unreachable -- the
#' outcome is \code{exhausted}, which means the budget ran out, and phase 3's
#' true distance is simply further than the leash allowed. What the cascade
#' buys is a bounded wait: an answer of some length always, rather than an
#' optimal one sometimes.
#'
#' @examples
#' \dontrun{
#' set.seed(8)
#' mv <- cube_moves(4); names(mv) <- cube_move_names(4)
#' s <- cube_identity(4)
#' for (m in sample(cube_move_names(4), 6, replace = TRUE)) s <- s[mv[[m]]]
#' r <- cube_solve4_cascade(s)
#' r$method
#' }
#' @export
cube_solve4_cascade <- function(state,
                                node_budget = 2e5,
                                prep_budget = 2e5,
                                orientations = c("", "1y", "1x", "1z"),
                                max_orientations = 4L,
                                phase2_solutions = 4L,
                                max_candidates = NA_integer_,
                                shuffle_candidates = FALSE,
                                workers = 1L,
                                budget_steps = c(0.1, 0.3),
                                exact_centres = TRUE,
                                verbose = FALSE,
                                progress_every = 0) {
  state <- as.integer(state)
  if (length(state) != 96L) {
    stop("cube_solve4_cascade: a 4x4x4 state has 96 stickers, got ",
         length(state), call. = FALSE)
  }

  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)
  apply_path <- function(s, p) {
    for (m in p) s <- s[moves[[m]]]
    s
  }

  t0 <- proc.time()[["elapsed"]]
  attempts <- list()

  # Every operation, timed and named, in the order it happened -- including the
  # attempts that failed, which on a cube that takes eight are most of the time
  # spent. `detail` carries the orientation and, for phase 3, the rung.
  ops <- list()
  record_op <- function(op, secs, detail = NA_character_, nodes = NA_real_,
                        outcome = NA_character_) {
    ops[[length(ops) + 1L]] <<- data.frame(
      op = op, secs = secs, detail = detail, nodes = nodes,
      outcome = outcome, stringsAsFactors = FALSE)
  }
  # Runs `expr`, records what it cost, hands back its value untouched.
  timed <- function(op, detail, expr) {
    tstart <- proc.time()[["elapsed"]]
    val <- force(expr)
    record_op(op, proc.time()[["elapsed"]] - tstart, detail)
    val
  }

  if (cube_is_colour_solved(state)) {
    el <- proc.time()[["elapsed"]] - t0
    return(list(path = character(0), method = "already-solved", solved = TRUE,
                orientation = "", candidate = "", n_moves = 0L,
                seconds = el,
                attempts = attempts,
                ops = .cascade_ops_frame(ops, el)))
  }

  rots <- utils::head(orientations, max_orientations)

  # The rungs, and the sweep across them, are shared with
  # cube_kociemba4_reduce() -- see R/budget_ladder.R for the measurements they
  # come from.
  steps <- .budget_rungs(node_budget, budget_steps)

  # Phases 1 and 2 do not depend on the rung, so they run once per orientation
  # and are kept. Usually 0.1 to 0.5 s, but they can spend their whole budget --
  # hence prep_budget rather than phase 3's ceiling.
  prep <- list()
  for (rot in rots) {
    # Expanded, not split on spaces. "1x" is a move in its own right -- it turns
    # the inner slice -- while the rotation is the four moves cube_expand_word()
    # gives back: L' 1x 2x R. Splitting turned one slice instead of the cube,
    # and the inverse token then undid the reduction along with it.
    #
    # The inverse comes from the expansion, not the notation, so no second
    # reading of the word can disagree with the first.
    rot_word <- if (nzchar(rot)) cube_expand_word(rot, 4L) else character(0)
    turned <- apply_path(state, rot_word)

    # Phases 1 and 2 for this orientation, returning several phase-2 solutions
    # rather than the first. Timed as one operation and then split by the phase
    # seconds the C++ side reports, so the two phases are told apart without
    # running them twice, and whatever is left over -- the turn, the replay --
    # stays visible instead of being folded into a phase that did not spend it.
    t_prep <- proc.time()[["elapsed"]]
    # prep_budget, not node_budget: the latter is phase 3's ladder top, and
    # passing it here handed phase 2 the whole ceiling undivided.
    p2res <- cube_kociemba4_phase2_solutions_cpp(turned,
                                                 n_solutions = phase2_solutions,
                                                 node_budget = prep_budget)
    prep_secs <- proc.time()[["elapsed"]] - t_prep
    lbl <- if (nzchar(rot)) rot else "(-)"
    record_op("phase 1", p2res$phase1_secs, lbl, p2res$phase1_nodes)
    record_op("phase 2", p2res$phase2_secs, lbl, p2res$phase2_nodes)
    leftover <- prep_secs - p2res$phase1_secs - p2res$phase2_secs
    if (leftover > 0) record_op("prep overhead", leftover, lbl)

    # One candidate per phase-2 solution. They share phase 1 and the rotation
    # and differ only in the phase-2 word, which is the whole of what makes
    # them different starting points for phase 3.
    for (k in seq_along(p2res$solutions)) {
      p12 <- c(p2res$phase1, p2res$solutions[[k]])
      slbl <- if (length(p2res$solutions) > 1L) sprintf("%s#%d", lbl, k) else lbl
      handed <- timed("prep replay", slbl, apply_path(turned, p12))
      prep[[length(prep) + 1L]] <- list(
        rot = rot,
        label = slbl,
        rot_word = rot_word,
        inv_word = .cube4_invert_moves(rot_word),
        p12 = p12,
        handed = handed,
        # If phase 2 cannot arrive there is nothing for phase 3 to be given, and
        # no rung will change that -- the candidate is finished at every size.
        p2ok = isTRUE(cube_at_phase_goal_cpp(handed, 2L)))
    }

    # No solution at all is the orientation's own failure, and it is recorded as
    # one candidate so the attempts log still mentions the orientation rather
    # than passing over it in silence.
    if (!length(p2res$solutions)) {
      prep[[length(prep) + 1L]] <- list(
        rot = rot, label = lbl, rot_word = rot_word,
        inv_word = .cube4_invert_moves(rot_word),
        p12 = character(0), handed = turned, p2ok = FALSE)
    }
  }

  # An orientation phase 2 could not reach is finished at every rung, so it is
  # recorded once here and kept out of the sweep entirely rather than skipped
  # inside it on each pass.
  for (pr in prep) {
    if (!pr$p2ok) {
      attempts[[length(attempts) + 1L]] <- list(
        orientation = pr$rot, candidate = pr$label, phase2_ok = FALSE,
        budget = NA_real_, outcome = "phase2-short", nodes = 0,
        p12_moves = length(pr$p12))
    }
  }
  live <- Filter(function(pr) pr$p2ok, prep)

  # Which candidates are tried, and in what order -- see the two parameters for
  # why the build order is arbitrary and why the cap is a time bound.
  if (isTRUE(shuffle_candidates) && length(live) > 1L) {
    live <- live[sample.int(length(live))]
  }
  if (!is.na(max_candidates) && max_candidates < length(live)) {
    # The ones dropped are recorded, so the log does not silently omit them:
    # a cube that fell back after four of sixteen is a different event from one
    # that fell back after all sixteen, and only this row tells them apart.
    for (pr in live[(max_candidates + 1L):length(live)]) {
      attempts[[length(attempts) + 1L]] <- list(
        orientation = pr$rot, candidate = pr$label, phase2_ok = TRUE,
        budget = NA_real_, outcome = "not-tried", nodes = 0,
        p12_moves = length(pr$p12))
    }
    live <- live[seq_len(max_candidates)]
  }

  try_one <- function(pr, budget) {
    # Said before the search rather than after it. Phase 3 is where the tens of
    # seconds go, and a line printed on the way out arrives when the waiting is
    # already over -- which is the whole of what a caller wanted to know while
    # it was still happening.
    if (verbose) {
      cat(sprintf("  %-6s budget %-11s phase3 running...\n", pr$label,
                  format(budget, big.mark = ",", scientific = FALSE)))
      flush.console()
    }
    lbl <- pr$label
    # Returned with the result rather than written to the enclosing log: under
    # `workers > 1` this runs in a fork, where an assignment to the parent's log
    # dies with the fork and the phase-3 seconds vanish from the accounting.
    my_ops <- list()
    my_record <- function(op, secs, detail = NA_character_, nodes = NA_real_,
                          outcome = NA_character_) {
      my_ops[[length(my_ops) + 1L]] <<- data.frame(
        op = op, secs = secs, detail = detail, nodes = nodes,
        outcome = outcome, stringsAsFactors = FALSE)
    }
    my_timed <- function(op, detail, expr) {
      tstart <- proc.time()[["elapsed"]]
      val <- force(expr)
      my_record(op, proc.time()[["elapsed"]] - tstart, detail)
      val
    }

    t3 <- proc.time()[["elapsed"]]
    r3 <- cube_kociemba4_phase3_cpp(pr$handed, node_budget = budget,
                                    use_exact_centres = exact_centres,
                                    progress_every = progress_every)
    my_record("phase 3",
              proc.time()[["elapsed"]] - t3,
              sprintf("%s @ %s", lbl,
                      format(budget, big.mark = ",", scientific = FALSE)),
              r3$nodes, r3$outcome)
    if (!isTRUE(r3$found)) return(list(ok = FALSE, r3 = r3, ops = my_ops))

    # Reduced. The cube is now a 3x3x3 in disguise, and cube_solve4 finishes it
    # -- the reduction is what this function was trying to reach, not the whole
    # solution.
    reduced <- apply_path(pr$handed, r3$path)
    tail_solve <- my_timed("tail solve4", lbl, cube_solve4(reduced))

    # cube_solve4 can return found = FALSE -- it gives up on some OLL and PLL
    # cases -- so a successful reduction is not on its own a solved cube. Checked
    # rather than assumed; a failed check leaves ok = FALSE and the sweep goes on.
    path <- c(pr$rot_word, pr$p12, r3$path, tail_solve$path, pr$inv_word)
    ok_final <- my_timed("verify", lbl,
                         cube_is_colour_solved(apply_path(state, path)))
    if (!ok_final) {
      if (verbose) {
        cat("  reduced, but the 3x3x3 stage did not finish; trying on\n")
        flush.console()
      }
      return(list(ok = FALSE, r3 = r3, ops = my_ops))
    }
    list(ok = TRUE, r3 = r3, ops = my_ops,
         value = list(path = path, orientation = pr$rot, candidate = pr$label))
  }

  record <- function(pr, budget, res) {
    # Appended in the order the sweep saw the results, which under workers > 1
    # is the order they finished within a chunk rather than the order started.
    for (row in res$ops) ops[[length(ops) + 1L]] <<- row
    attempts[[length(attempts) + 1L]] <<- list(
      orientation = pr$rot, candidate = pr$label, phase2_ok = TRUE,
      budget = budget, outcome = res$r3$outcome, nodes = res$r3$nodes,
      p12_moves = length(pr$p12))
    if (verbose) {
      cat(sprintf("  %-6s budget %-11s phase3 %-11s %s nodes\n", pr$label,
                  format(budget, big.mark = ",", scientific = FALSE),
                  res$r3$outcome,
                  format(res$r3$nodes, big.mark = ",", scientific = FALSE)))
      flush.console()
    }
  }

  hit <- .budget_sweep(live, steps, try_one, record, workers = workers)
  if (!is.null(hit)) {
    el <- proc.time()[["elapsed"]] - t0
    return(list(path = hit$path, method = "reduce", solved = TRUE,
                orientation = hit$orientation, candidate = hit$candidate,
                n_moves = length(hit$path),
                seconds = el,
                attempts = attempts,
                ops = .cascade_ops_frame(ops, el)))
  }

  # No orientation reduced inside the leash. The reduction has not proved
  # anything about this cube -- every outcome above is `exhausted`, which is a
  # statement about the budget -- so this is a decision to stop paying for it.
  if (verbose) { cat("  falling back to cube_solve4\n"); flush.console() }

  fb <- timed("fallback solve4", NA_character_, cube_solve4(state))
  solved <- timed("verify", NA_character_,
                  cube_is_colour_solved(apply_path(state, fb$path)))

  list(path = fb$path,
       method = if (solved) "solve4" else "unsolved",
       solved = solved,
       orientation = "",
       candidate = "",
       n_moves = length(fb$path),
       seconds = proc.time()[["elapsed"]] - t0,
       attempts = attempts,
       ops = .cascade_ops_frame(ops, proc.time()[["elapsed"]] - t0))
}

# The op log as one data.frame, with whatever the log did not account for kept
# as its own row.
#
# That last row is the point. An unaccounted remainder that is quietly dropped
# looks like a complete breakdown and is not one; kept and named, it is either
# small enough to ignore or the next thing to instrument. Either way the caller
# is told which, rather than being left to compare two totals and guess.
.cascade_ops_frame <- function(ops, total_secs) {
  if (!length(ops)) {
    return(data.frame(op = "unaccounted", secs = total_secs,
                      detail = NA_character_, nodes = NA_real_,
                      outcome = NA_character_, stringsAsFactors = FALSE))
  }
  df <- do.call(rbind, ops)
  rest <- total_secs - sum(df$secs)
  if (rest > 0.0005) {
    df <- rbind(df, data.frame(op = "unaccounted", secs = rest,
                               detail = NA_character_, nodes = NA_real_,
                               outcome = NA_character_,
                               stringsAsFactors = FALSE))
  }
  rownames(df) <- NULL
  df
}
