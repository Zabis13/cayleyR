library(testthat)
library(cayleyR)

heavy <- c(
  "cube-build-centres"
)

# Cap OpenMP at 2 threads, as CRAN policy requires. Without this the C++ code
# picks up all cores, and check flags "CPU time N times elapsed time".
Sys.setenv(OMP_NUM_THREADS = "2")

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")

test_dir <- if (dir.exists("testthat")) "testthat" else "tests/testthat"

if (on_cran) {
  message("--- RUNNING LIGHT TESTS ONLY ---")

  all_tests <- list.files(test_dir, pattern = "^test-.*\\.R$")
  all_names <- sub("^test-(.*)\\.R$", "\\1", all_tests)
  light_tests <- setdiff(all_names, heavy)

  if (length(light_tests) == 0) {
    test_check("cayleyR")
  } else {
    # testthat applies `filter` as grepl(filter, <test name>) with no anchors,
    # so anchor each name with ^...$ to avoid partial matches.
    filter_regex <- paste0("^(", paste(light_tests, collapse = "|"), ")$")
    test_check("cayleyR", filter = filter_regex)
  }
} else {
  test_check("cayleyR")
}
