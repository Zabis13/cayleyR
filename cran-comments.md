## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Test environments

* local Linux Mint 22.2, R 4.3.x
* GitHub Actions (ubuntu-latest): release
* R-hub: linux-valgrind, linux-rchk

## Additional check results

Two checks report findings that originate outside the package's own code:

* **valgrind**: "possibly lost: 352 bytes in 1 blocks", allocated by
  `_dl_allocate_tls` under `pthread_create` called from `GOMP_parallel`.
  This is the thread-local storage block for the OpenMP thread pool, which
  libgomp keeps alive until process exit and therefore does not release
  before valgrind takes its snapshot. It is one block per run regardless of
  workload. "definitely lost" is 0 bytes and all tests pass.

* **rchk**: `[PB] has possible protection stack imbalance`, `[PB] has
  negative depth` and `[UP] attempt to unprotect more items (1) than
  protected (0)`, all reported in `Rcpp/include/Rcpp/protection/Armor.h`.
  These come from Rcpp's RAII protection wrapper, whose PROTECT in the
  constructor and UNPROTECT in the destructor rchk cannot pair up. No
  finding is reported in this package's own sources.

## Downstream dependencies

There are currently no downstream dependencies for this package.
