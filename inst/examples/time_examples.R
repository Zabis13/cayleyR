# Time every Rd example, one at a time.
#
# R CMD check --run-donttest runs the examples of all 180 help files in a
# single process, so one that never returns takes the whole check with it and
# says nothing about which one it was. This runs each in a process of its own
# under a timeout, so a hang is a line in the report rather than a wedged
# terminal.
#
# Usage -- from anywhere, the package root is found from the script's path:
#
#   Rscript inst/examples/time_examples.R              # every example
#   Rscript inst/examples/time_examples.R 25           # with a 25s timeout
#   Rscript inst/examples/time_examples.R 25 cube_solve4 cube_kociemba
#
# The first argument is the per-example timeout in seconds (default 60); any
# further arguments are topic names to run instead of all of them.
#
# Examples are extracted with donttest INCLUDED and dontrun excluded, which is
# what --run-donttest does. Reported per example: seconds, and one of
#
#   ok        ran to the end
#   ERROR     stopped -- the message is shown under the table
#   TIMEOUT   still running when the limit came round

args <- commandArgs(trailingOnly = TRUE)
timeout <- if (length(args) >= 1) as.numeric(args[1]) else 60
topics <- if (length(args) >= 2) args[-1] else NULL

if (is.na(timeout) || timeout <= 0)
  stop("timeout must be a positive number of seconds")

# The package root: the working directory when the script is run from there,
# otherwise found from the script's own path, so it works from anywhere.
find_root <- function() {
  if (file.exists("DESCRIPTION")) return(normalizePath("."))

  self <- sub("^--file=", "",
              grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  if (is.na(self)) return(NA_character_)

  d <- dirname(normalizePath(self))
  while (!file.exists(file.path(d, "DESCRIPTION"))) {
    up <- dirname(d)
    if (identical(up, d)) return(NA_character_)
    d <- up
  }
  d
}

pkg <- find_root()
if (is.na(pkg))
  stop("no DESCRIPTION found here or above the script -- is this a package?")

man <- file.path(pkg, "man")
rd <- list.files(man, pattern = "[.]Rd$", full.names = TRUE)
if (!length(rd)) stop("no .Rd files in ", man)

if (!is.null(topics)) {
  want <- paste0(topics, ".Rd")
  missing <- setdiff(want, basename(rd))
  if (length(missing))
    stop("no such help topic: ", paste(sub("[.]Rd$", "", missing), collapse = ", "))
  rd <- rd[basename(rd) %in% want]
}

# --- extract ---------------------------------------------------------------
# Rd2ex writes nothing for a topic with no \examples section, which is why the
# extracted count is smaller than the number of help files.

exdir <- file.path(tempdir(), "cayleyR-examples")
dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
unlink(list.files(exdir, pattern = "[.]R$", full.names = TRUE))

for (f in rd) {
  out <- file.path(exdir, sub("[.]Rd$", ".R", basename(f)))
  tools::Rd2ex(f, out, commentDonttest = FALSE, commentDontrun = TRUE)
}

ex <- sort(list.files(exdir, pattern = "[.]R$", full.names = TRUE))
if (!length(ex)) stop("no examples to run")

# The runner each child process executes. The package is loaded inside the
# child, so a segfault or a hang costs one example rather than the run.
runner <- file.path(exdir, "_run_one.R")
writeLines(c(
  'suppressMessages(pkgload::load_all(Sys.getenv("CAYLEYR_PKG"), quiet = TRUE))',
  'f <- commandArgs(trailingOnly = TRUE)[1]',
  'source(f, echo = FALSE, local = new.env(), max.deparse.length = Inf)'
), runner)

# --- run -------------------------------------------------------------------

cat(sprintf("Running %d examples, %.0fs timeout each\n\n", length(ex), timeout))

rscript <- file.path(R.home("bin"), "Rscript")
res <- data.frame(topic = character(), sec = numeric(),
                  status = character(), msg = character(),
                  stringsAsFactors = FALSE)

for (f in ex) {
  topic <- sub("[.]R$", "", basename(f))
  errfile <- tempfile()

  t0 <- proc.time()[["elapsed"]]
  rc <- system2(rscript, c(shQuote(runner), shQuote(f)),
                stdout = FALSE, stderr = errfile,
                env = paste0("CAYLEYR_PKG=", shQuote(pkg)),
                timeout = timeout)
  sec <- proc.time()[["elapsed"]] - t0

  # system2() returns 124 for a timeout on most systems and signals it with a
  # warning on others; the elapsed time settles it either way.
  msg <- ""
  status <- if (rc == 124L || sec >= timeout) {
    "TIMEOUT"
  } else if (rc != 0L) {
    lines <- if (file.exists(errfile)) readLines(errfile, warn = FALSE) else character()
    lines <- lines[nzchar(trimws(lines))]
    msg <- if (length(lines)) paste(tail(lines, 3), collapse = " | ") else "(no message)"
    "ERROR"
  } else {
    "ok"
  }
  unlink(errfile)

  cat(sprintf("%7.2fs  %-9s %s\n", sec, status, topic))
  res <- rbind(res, data.frame(topic = topic, sec = sec, status = status,
                               msg = msg, stringsAsFactors = FALSE))
}

# --- report ----------------------------------------------------------------

cat("\n", strrep("-", 60), "\n", sep = "")
cat(sprintf("total %.1fs over %d examples\n", sum(res$sec), nrow(res)))

slow <- res[res$status == "ok", ]
slow <- slow[order(-slow$sec), ]
if (nrow(slow)) {
  cat("\nslowest:\n")
  for (i in seq_len(min(15L, nrow(slow))))
    cat(sprintf("  %6.2fs  %s\n", slow$sec[i], slow$topic[i]))
}

bad <- res[res$status != "ok", ]
if (nrow(bad)) {
  cat("\n", nrow(bad), " did not finish cleanly:\n", sep = "")
  for (i in seq_len(nrow(bad))) {
    cat(sprintf("\n  %s -- %s (%.1fs)\n", bad$topic[i], bad$status[i], bad$sec[i]))
    if (nzchar(bad$msg[i])) cat("    ", bad$msg[i], "\n", sep = "")
  }
} else {
  cat("\nall examples ran clean\n")
}

# Written beside the extracted examples rather than into the package, which
# would leave a file R CMD check reports as a non-standard top-level one.
out <- file.path(exdir, "example-times.csv")
write.csv(res, out, row.names = FALSE)
cat("\nwritten to ", out, "\n", sep = "")

invisible(res)
