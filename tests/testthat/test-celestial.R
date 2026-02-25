test_that("convert_LRX_to_celestial returns correct structure", {
  coords <- convert_LRX_to_celestial(10, 5, 3)
  expect_type(coords, "list")
  expect_true(all(c("z", "z_bar", "theta", "phi", "omega_conformal") %in% names(coords)))
  expect_true(is.complex(coords$z))
  expect_equal(coords$omega_conformal, sqrt(10^2 + 5^2 + 3^2))
})

test_that("convert_LRX_to_celestial handles zero input", {
  coords <- convert_LRX_to_celestial(0, 0, 0)
  expect_equal(coords$omega_conformal, 0)
  expect_equal(coords$theta, 0)
  expect_equal(coords$phi, 0)
})

test_that("calculate_angular_distance_z is zero for same point", {
  c1 <- convert_LRX_to_celestial(10, 5, 3)
  dist <- calculate_angular_distance_z(c1, c1)
  expect_equal(dist, 0, tolerance = 1e-10)
})

test_that("calculate_angular_distance_z is positive for different points", {
  c1 <- convert_LRX_to_celestial(10, 5, 3)
  c2 <- convert_LRX_to_celestial(1, 1, 2)
  dist <- calculate_angular_distance_z(c1, c2)
  expect_true(dist > 0)
  expect_true(dist <= pi)
})

test_that("calculate_midpoint_z returns valid midpoint", {
  c1 <- convert_LRX_to_celestial(10, 5, 3)
  c2 <- convert_LRX_to_celestial(1, 1, 2)
  mid <- calculate_midpoint_z(c1, c2)
  expect_true(all(c("theta", "phi", "z", "omega_conformal") %in% names(mid)))
  expect_true(mid$theta >= 0 && mid$theta <= pi)
})
