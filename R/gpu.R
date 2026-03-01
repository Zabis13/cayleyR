# GPU infrastructure for cayleyR via ggmlR (Vulkan backend)
# (includes .setup_gpu helper moved from parallel.R)

# Package-level environment for lazy GPU state
.gpu_env <- new.env(parent = emptyenv())
.gpu_env$backend <- NULL
.gpu_env$initialized <- FALSE

#' Check if GPU acceleration is available
#'
#' Checks whether ggmlR is installed and Vulkan GPU is present.
#'
#' @return Logical
#' @export
#' @examples
#' cayley_gpu_available()
cayley_gpu_available <- function() {
  if (!requireNamespace("ggmlR", quietly = TRUE)) return(FALSE)
  ggmlR::ggml_vulkan_available() && ggmlR::ggml_vulkan_device_count() > 0L
}

#' Initialize GPU backend
#'
#' Lazily initializes the Vulkan backend. Safe to call multiple times.
#'
#' @param device Integer, Vulkan device index (0-based)
#' @param force Logical, force re-initialization
#' @return Invisible backend pointer
#' @export
#' @examples
#' \donttest{
#' if (cayley_gpu_available()) {
#'   cayley_gpu_init()
#' }
#' }
cayley_gpu_init <- function(device = 0L, force = FALSE) {
  if (.gpu_env$initialized && !force) return(invisible(.gpu_env$backend))

  if (!cayley_gpu_available()) {
    stop("GPU not available: ggmlR not installed or no Vulkan device found")
  }

  # Free old backend if re-initializing
  if (!is.null(.gpu_env$backend)) {
    try(ggmlR::ggml_vulkan_free(.gpu_env$backend), silent = TRUE)
  }

  .gpu_env$backend <- ggmlR::ggml_vulkan_init(as.integer(device))
  .gpu_env$initialized <- TRUE

  invisible(.gpu_env$backend)
}

#' Get GPU status information
#'
#' @return List with availability, device info, and backend status
#' @export
#' @examples
#' cayley_gpu_status()
cayley_gpu_status <- function() {
  if (!requireNamespace("ggmlR", quietly = TRUE)) {
    cat("ggmlR: NOT INSTALLED\n")
    return(invisible(list(available = FALSE)))
  }

  vulkan_ok <- ggmlR::ggml_vulkan_available()
  if (!vulkan_ok) {
    cat("Vulkan: NOT AVAILABLE\n")
    return(invisible(list(available = FALSE, vulkan = FALSE)))
  }

  n_devices <- ggmlR::ggml_vulkan_device_count()
  cat("Vulkan: AVAILABLE\n")
  cat("Devices:", n_devices, "\n")

  devices <- list()
  if (n_devices > 0L) {
    dev_list <- ggmlR::ggml_vulkan_list_devices()
    for (d in dev_list) {
      cat(sprintf("  [%d] %s (%.2f GB free / %.2f GB total)\n",
                  d$index, d$name,
                  d$free_memory / 1e9, d$total_memory / 1e9))
    }
    devices <- dev_list
  }

  cat("Backend initialized:", .gpu_env$initialized, "\n")

  invisible(list(
    available = TRUE,
    vulkan = TRUE,
    n_devices = n_devices,
    devices = devices,
    initialized = .gpu_env$initialized
  ))
}

#' Free GPU backend resources
#'
#' @return Invisible NULL
#' @export
cayley_gpu_free <- function() {
  if (!is.null(.gpu_env$backend)) {
    try(ggmlR::ggml_vulkan_free(.gpu_env$backend), silent = TRUE)
    .gpu_env$backend <- NULL
    .gpu_env$initialized <- FALSE
  }
  invisible(NULL)
}

#' Setup GPU backend (internal)
#'
#' Checks if GPU is available via cayley_gpu_available() and initializes if so.
#'
#' @return Logical, TRUE if GPU is ready
#' @keywords internal
.setup_gpu <- function() {
  gpu_ok <- tryCatch(cayley_gpu_available(), error = function(e) FALSE)
  if (gpu_ok) {
    tryCatch({
      cayley_gpu_init()
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  return(FALSE)
}

# ============================================================================
# Phase 2: Manhattan distance on GPU
# ============================================================================

#' Calculate Manhattan Distances on GPU
#'
#' Computes Manhattan distance from a reference state to each row of a matrix
#' using ggml Vulkan backend: sub -> abs -> sum_rows.
#'
#' @param start_state Integer vector, the reference state (length n)
#' @param states_matrix Numeric matrix (nrow x n), each row is a state
#' @return Numeric vector of Manhattan distances (length nrow)
#' @keywords internal
calculate_differences_gpu <- function(start_state, states_matrix) {
  backend <- cayley_gpu_init()

  n <- length(start_state)
  nrow_states <- nrow(states_matrix)

  # Memory: tensors + graph overhead
  mem_size <- (n * nrow_states + n + nrow_states) * 4 + # F32 data
    ggmlR::ggml_graph_overhead() + 1024 * 1024           # graph + padding

  ctx <- ggmlR::ggml_init(mem_size, no_alloc = TRUE)

  # Create tensors (ne0=n elements per state, ne1=number of states)
  states_t <- ggmlR::ggml_new_tensor_2d(ctx, ggmlR::GGML_TYPE_F32, n, nrow_states)
  start_t  <- ggmlR::ggml_new_tensor_1d(ctx, ggmlR::GGML_TYPE_F32, n)

  ggmlR::ggml_set_input(states_t)
  ggmlR::ggml_set_input(start_t)

  # Graph: sub -> abs -> sum_rows
  diff_t     <- ggmlR::ggml_sub(ctx, states_t, start_t)
  abs_t      <- ggmlR::ggml_abs(ctx, diff_t)
  distances_t <- ggmlR::ggml_sum_rows(ctx, abs_t)

  ggmlR::ggml_set_output(distances_t)

  graph <- ggmlR::ggml_build_forward_expand(ctx, distances_t)

  # Allocate tensors on GPU
  buf <- ggmlR::ggml_backend_alloc_ctx_tensors(ctx, backend)

  # Upload data
  ggmlR::ggml_backend_tensor_set_data(states_t, as.numeric(t(states_matrix)))
  ggmlR::ggml_backend_tensor_set_data(start_t, as.numeric(start_state))

  # Compute on GPU
  ggmlR::ggml_backend_graph_compute(backend, graph)

  # Download result
  result <- ggmlR::ggml_backend_tensor_get_data(distances_t)

  # Cleanup
  ggmlR::ggml_backend_buffer_free(buf)
  ggmlR::ggml_free(ctx)

  as.numeric(result)
}

# ============================================================================
# Phase 3: Batch apply_operations on GPU
# ============================================================================

#' Build permutation matrix for a single operation
#'
#' Creates an n x n permutation matrix (column-major, F32) that represents
#' a single operation. When multiplied: new_state = state %*% P.
#'
#' @param op Character, operation code ("L"/"1", "R"/"2", "X"/"3")
#' @param n Integer, state length
#' @param k Integer, reverse prefix length
#' @return Numeric vector of length n*n (column-major permutation matrix)
#' @keywords internal
build_permutation_matrix <- function(op, n, k) {
  P <- matrix(0, nrow = n, ncol = n)

  if (op %in% c("L", "1")) {
    # shift_left: element at position i goes to position (i-1) mod n
    # new[i] = old[i+1] for i < n-1; new[n-1] = old[0]
    # P[j, i] = 1 where new[i] = old[j], so j = (i+1) %% n
    for (i in seq_len(n)) {
      j <- (i %% n) + 1L  # 1-based: source index
      P[j, i] <- 1
    }
  } else if (op %in% c("R", "2")) {
    # shift_right: new[0] = old[n-1]; new[i] = old[i-1] for i > 0
    # P[j, i] = 1 where new[i] = old[j], so j = (i-2) %% n + 1 (1-based)
    for (i in seq_len(n)) {
      j <- ((i - 2L) %% n) + 1L
      P[j, i] <- 1
    }
  } else if (op %in% c("X", "3")) {
    # reverse_prefix_k: reverse first k elements, rest unchanged
    kk <- min(k, n)
    for (i in seq_len(n)) {
      if (i <= kk) {
        P[kk - i + 1L, i] <- 1
      } else {
        P[i, i] <- 1
      }
    }
  } else {
    stop("Unknown operation: ", op)
  }

  as.numeric(P)
}

#' Compose permutation matrices for a sequence of operations
#'
#' Multiplies individual permutation matrices into one combined matrix.
#'
#' @param operations Character vector of operation codes
#' @param n Integer, state length
#' @param k Integer, reverse prefix length
#' @return Numeric vector of length n*n (combined permutation matrix, column-major)
#' @keywords internal
compose_permutation_matrix <- function(operations, n, k) {
  result <- diag(n)
  for (op in operations) {
    P <- matrix(build_permutation_matrix(op, n, k), nrow = n, ncol = n)
    result <- result %*% P
  }
  as.numeric(result)
}

#' Apply operations to batch of states on GPU
#'
#' Applies a sequence of permutation operations to multiple states
#' simultaneously using matrix multiplication on the Vulkan backend.
#'
#' @param states_matrix Numeric matrix (nrow x n), each row is a state
#' @param operations Character vector of operation codes (e.g., c("L", "R", "X"))
#' @param k Integer, parameter for reverse operations
#' @return Numeric matrix (nrow x n) with transformed states
#' @export
#' @examples
#' \donttest{
#' if (cayley_gpu_available()) {
#'   mat <- matrix(c(1,2,3,4,5, 5,4,3,2,1), nrow = 2, byrow = TRUE)
#'   result <- apply_operations_batch_gpu(mat, c("1", "3"), k = 4)
#' }
#' }
apply_operations_batch_gpu <- function(states_matrix, operations, k) {
  backend <- cayley_gpu_init()

  n <- ncol(states_matrix)
  nrow_states <- nrow(states_matrix)

  # Compose all operations into single permutation matrix (CPU — small n*n)
  perm_vec <- compose_permutation_matrix(operations, n, k)

  # Memory estimate
  mem_size <- (n * nrow_states * 2 + n * n) * 4 +
    ggmlR::ggml_graph_overhead() + 1024 * 1024

  ctx <- ggmlR::ggml_init(mem_size, no_alloc = TRUE)

  # ggml_mul_mat(a, b) computes a^T * b
  # We need: result = P * states (column vectors)
  # So: ggml_mul_mat(P^T, states) = (P^T)^T * states = P * states
  # But test shows ggml_mul_mat(P_t, states) gives P^T * states
  # So we need: ggml_mul_mat(P, states) = P^T * states... which is wrong
  # Empirically: ggml_mul_mat(P, states) gives P * states when P is (n,n)
  # Using P directly (not transposed)

  perm_t <- ggmlR::ggml_new_tensor_2d(ctx, ggmlR::GGML_TYPE_F32, n, n)
  states_t <- ggmlR::ggml_new_tensor_2d(ctx, ggmlR::GGML_TYPE_F32, n, nrow_states)

  ggmlR::ggml_set_input(perm_t)
  ggmlR::ggml_set_input(states_t)

  result_t <- ggmlR::ggml_mul_mat(ctx, perm_t, states_t)
  ggmlR::ggml_set_output(result_t)

  graph <- ggmlR::ggml_build_forward_expand(ctx, result_t)

  buf <- ggmlR::ggml_backend_alloc_ctx_tensors(ctx, backend)

  # Upload: states as column-major (each column = one state's elements)
  P_mat <- matrix(perm_vec, nrow = n, ncol = n)
  ggmlR::ggml_backend_tensor_set_data(states_t, as.numeric(t(states_matrix)))
  ggmlR::ggml_backend_tensor_set_data(perm_t, as.numeric(P_mat))

  ggmlR::ggml_backend_graph_compute(backend, graph)

  raw_result <- ggmlR::ggml_backend_tensor_get_data(result_t)

  ggmlR::ggml_backend_buffer_free(buf)
  ggmlR::ggml_free(ctx)

  # Reshape back: ggml output is (n, nrow_states), convert to R matrix (nrow x n)
  result_mat <- matrix(raw_result, nrow = n, ncol = nrow_states)
  result_mat <- t(result_mat)

  round(result_mat)
}

# ============================================================================
# Phase 4: Pairwise Manhattan distance matrix on GPU
# ============================================================================

#' Compute Pairwise Manhattan Distance Matrix on GPU
#'
#' Computes all pairwise Manhattan distances between two sets of states.
#' Returns an N1 x N2 matrix where entry (i,j) is the Manhattan distance
#' between row i of states1 and row j of states2.
#'
#' For large matrices, computation is batched over columns of the result
#' to avoid GPU memory overflow.
#'
#' @param states1 Numeric matrix (N1 x n), first set of states
#' @param states2 Numeric matrix (N2 x n), second set of states
#' @param batch_size Integer, number of states2 rows to process at once
#'   (default 256)
#' @return Numeric matrix (N1 x N2) of Manhattan distances
#' @export
#' @examples
#' \donttest{
#' if (cayley_gpu_available()) {
#'   s1 <- matrix(c(1,2,3,4,5, 5,4,3,2,1), nrow = 2, byrow = TRUE)
#'   s2 <- matrix(c(3,3,3,3,3, 1,1,1,1,1), nrow = 2, byrow = TRUE)
#'   manhattan_distance_matrix_gpu(s1, s2)
#' }
#' }
manhattan_distance_matrix_gpu <- function(states1, states2, batch_size = 256L) {
  n1 <- nrow(states1)
  n2 <- nrow(states2)
  n <- ncol(states1)

  if (n2 <= batch_size) {
    return(.manhattan_distance_matrix_gpu_batch(states1, states2))
  }

  # Process in batches along states2 to limit GPU memory
  result_mat <- matrix(0, nrow = n1, ncol = n2)
  starts <- seq(1L, n2, by = batch_size)

  for (s in starts) {
    e <- min(s + batch_size - 1L, n2)
    result_mat[, s:e] <- .manhattan_distance_matrix_gpu_batch(
      states1, states2[s:e, , drop = FALSE]
    )
  }

  result_mat
}

#' Single-batch GPU pairwise Manhattan distance
#'
#' Uses 3D tensors: repeat s1 along dim2, repeat s2 along dim1,
#' then sub -> abs -> sum_rows.
#' Result shape is (1, N1, N2) which gives the N1 x N2 distance matrix.
#'
#' @param states1 Numeric matrix (N1 x n)
#' @param states2 Numeric matrix (N2 x n)
#' @return Numeric matrix (N1 x N2)
#' @keywords internal
.manhattan_distance_matrix_gpu_batch <- function(states1, states2) {
  backend <- cayley_gpu_init()

  n <- ncol(states1)
  n1 <- nrow(states1)
  n2 <- nrow(states2)

  mem_size <- (n * n1 * n2 * 4 + n * n1 + n * n2) * 4 +
    ggmlR::ggml_graph_overhead() + 2 * 1024 * 1024

  ctx <- ggmlR::ggml_init(mem_size, no_alloc = TRUE)

  # s1 as (n, n1) -> repeat to (n, n1, n2)
  s1_t <- ggmlR::ggml_new_tensor_2d(ctx, ggmlR::GGML_TYPE_F32, n, n1)
  s1_shape <- ggmlR::ggml_new_tensor_3d(ctx, ggmlR::GGML_TYPE_F32, n, n1, n2)
  s1_3d <- ggmlR::ggml_repeat(ctx, s1_t, s1_shape)

  # s2 as (n, 1, n2) -> repeat to (n, n1, n2)
  s2_t <- ggmlR::ggml_new_tensor_3d(ctx, ggmlR::GGML_TYPE_F32, n, 1L, n2)
  s2_shape <- ggmlR::ggml_new_tensor_3d(ctx, ggmlR::GGML_TYPE_F32, n, n1, n2)
  s2_3d <- ggmlR::ggml_repeat(ctx, s2_t, s2_shape)

  diff_t <- ggmlR::ggml_sub(ctx, s1_3d, s2_3d)
  abs_t <- ggmlR::ggml_abs(ctx, diff_t)
  sums_t <- ggmlR::ggml_sum_rows(ctx, abs_t)

  ggmlR::ggml_set_input(s1_t)
  ggmlR::ggml_set_input(s2_t)
  ggmlR::ggml_set_input(s1_shape)
  ggmlR::ggml_set_input(s2_shape)
  ggmlR::ggml_set_output(sums_t)

  graph <- ggmlR::ggml_build_forward_expand(ctx, sums_t)
  buf <- ggmlR::ggml_backend_alloc_ctx_tensors(ctx, backend)

  # Upload: states transposed to column-major (n, N)
  ggmlR::ggml_backend_tensor_set_data(s1_t, as.numeric(t(states1)))
  # s2 as (n, 1, n2): each "slice" along dim2 is one state vector
  # In memory: n elements per state, 1 row, n2 slices = just concatenated
  ggmlR::ggml_backend_tensor_set_data(s2_t, as.numeric(t(states2)))

  ggmlR::ggml_backend_graph_compute(backend, graph)

  # Result shape: (1, n1, n2) -> n1*n2 values
  raw <- ggmlR::ggml_backend_tensor_get_data(sums_t)

  ggmlR::ggml_backend_buffer_free(buf)
  ggmlR::ggml_free(ctx)

  # Reshape: ggml stores (1, n1, n2) column-major = n1 values per n2 slice
  matrix(raw, nrow = n1, ncol = n2)
}
