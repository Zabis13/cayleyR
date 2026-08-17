# The wing geometry of the 4x4x4 is measured, not tabulated: which slots are
# "primary", which wings pair into a dedge, and so on all come out of walking
# the cube rather than out of a written-down table. That is the right way round
# -- a literal like c(2, 1, 0, 3, 20, ...) cannot be checked by reading it --
# but it has one weakness. If the geometry ever shifts, the measurement quietly
# shifts with it and nothing complains.
#
# So these tests hold our measurements against someone else's constants. The
# tables below are copied verbatim from twips's
# src/lib/scramble/puzzles/cube4x4x4/wings.rs, which was written independently
# and in a different numbering (Speffz). Translating them through the Speffz
# bijection has to reproduce what we measure. If it stops doing so, either the
# geometry moved or the bijection is wrong, and both are worth a failing test.

# --- twips's tables, verbatim from wings.rs (0-based, Speffz numbering) ------

TW_PARTNER <- c(16, 12, 8, 4, 3, 11, 23, 17, 2, 15, 20, 5,
                1, 19, 21, 9, 0, 7, 22, 13, 10, 14, 18, 6)

TW_PRIMARY_IN_DEDGE <- c(0, 1, 2, 3, 3, 11, 23, 17, 2, 9, 20, 11,
                         1, 19, 21, 9, 0, 17, 22, 19, 20, 21, 22, 23)

TW_POSITION_IS_PRIMARY <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                            FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
                            FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)

geom <- cayleyR:::cube_wing_geometry_cpp()
sp <- geom$speffz_to_slot

# Translate a table written in Speffz indices into our slot numbering.
# Entry i of their table describes Speffz wing i; ours describes slot sp[i].
translate_slots <- function(tab) {
  out <- integer(24)
  for (i in seq_len(24)) out[sp[i] + 1L] <- sp[tab[i] + 1L]
  out
}
translate_flags <- function(tab) {
  out <- logical(24)
  for (i in seq_len(24)) out[sp[i] + 1L] <- tab[i]
  out
}

test_that("the Speffz map is a bijection of the 24 wing slots", {
  expect_length(sp, 24L)
  expect_setequal(sp, 0:23)
})

# The weakest of the three checks -- partner is symmetric, so a wrong map can
# survive it -- but it is the one that says the pairing itself agrees.
test_that("twips's partner table translates to our measured partners", {
  expect_identical(translate_slots(TW_PARTNER), as.integer(geom$partner))
})

# The one that carries the weight. POSITION_IS_PRIMARY is an asymmetric 12/12
# split, defined by which slots are reachable from one wing under <U, L, R, D>.
# We measure that by walking; they wrote it down. A wrong bijection would have
# to permute the two halves exactly right to pass this by accident.
test_that("twips's primary/secondary split translates to our measured one", {
  expect_identical(translate_flags(TW_POSITION_IS_PRIMARY), as.logical(geom$primary))
  expect_equal(sum(geom$primary), 12L)
})

test_that("twips's dedge table translates to our measured one", {
  expect_identical(translate_slots(TW_PRIMARY_IN_DEDGE),
                   as.integer(geom$primary_in_dedge))
})

# Properties that hold whatever the numbering, so they check the measurement
# rather than the translation.
test_that("the wing pairing is an involution with no fixed point", {
  partner <- as.integer(geom$partner)
  expect_true(all(partner >= 0 & partner <= 23))
  expect_true(all(partner != 0:23))
  expect_identical(partner[partner + 1L], 0:23)
})

test_that("every dedge has one primary wing and one secondary", {
  primary <- as.logical(geom$primary)
  partner <- as.integer(geom$partner)
  expect_true(all(primary != primary[partner + 1L]))
})

test_that("the twelve dedges each get exactly two wings", {
  expect_identical(sort(unname(table(as.integer(geom$dedge)))),
                   sort(unname(table(rep(0:11, each = 2)))))
})
