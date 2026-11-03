# =============================================================================
# Module 11 - Debugging and Defensive Programming in R
# Assignment #11
# Author: Adriel
# =============================================================================
# Goal: flag rows of a numeric matrix that are outliers in EVERY column
# according to Tukey's rule (below Q1 - 1.5*IQR or above Q3 + 1.5*IQR).
# =============================================================================


# -----------------------------------------------------------------------------
# Helper: Tukey's rule for a single numeric vector
# Returns a logical vector the same length as x (TRUE = outlier)
# -----------------------------------------------------------------------------
tukey.outlier <- function(x) {
  q   <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower <- q[1] - 1.5 * iqr
  upper <- q[2] + 1.5 * iqr
  (x < lower) | (x > upper)
}


# -----------------------------------------------------------------------------
# Step 1: Reproduce the error with the ORIGINAL (buggy) function
# -----------------------------------------------------------------------------
tukey_multiple <- function(x) {
  outliers <- array(TRUE, dim = dim(x))
  for (j in 1:ncol(x)) {
    outliers[, j] <- outliers[, j] && tukey.outlier(x[, j])   # <-- BUG
  }
  outlier.vec <- vector("logical", length = nrow(x))
  for (i in 1:nrow(x)) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  return(outlier.vec)
}

set.seed(123)
test_mat <- matrix(rnorm(50), nrow = 10)

cat("=== Step 1: Running the original (buggy) function ===\n")
buggy_result <- tryCatch(
  tukey_multiple(test_mat),
  error = function(e) {
    cat("Error captured:\n")
    print(e)          # shows the exact error message
    NULL
  }
)


# -----------------------------------------------------------------------------
# Step 2: Diagnosis
# -----------------------------------------------------------------------------
# `&&` is a SCALAR (short-circuit) operator. It expects a single TRUE/FALSE on
# each side. Here both sides are length-10 logical vectors (one value per row),
# so `&&` has nothing sensible to do with them.
#   - In R >= 4.3.0 this is a hard error:
#       "'length = 10' in coercion to 'logical(1)'"
#   - In older R it only used the FIRST element and returned one value, which
#     was then silently recycled down the whole column -> wrong answers.
# What we need is the ELEMENT-WISE operator `&`, which compares row 1 with
# row 1, row 2 with row 2, etc., and returns a vector of the same length.


# -----------------------------------------------------------------------------
# Step 3 & 4: Corrected function (`&&` -> `&`) and validation
# -----------------------------------------------------------------------------
corrected_tukey <- function(x) {
  outliers <- array(TRUE, dim = dim(x))
  for (j in seq_len(ncol(x))) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])    # FIXED
  }
  outlier.vec <- logical(nrow(x))
  for (i in seq_len(nrow(x))) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  outlier.vec
}

cat("\n=== Step 4: Running the corrected function ===\n")
fixed_result <- corrected_tukey(test_mat)
print(fixed_result)
cat("Class: ", class(fixed_result), "\n")
cat("Length:", length(fixed_result), "\n")
stopifnot(is.logical(fixed_result), length(fixed_result) == nrow(test_mat))
cat("Validation passed: logical vector of length", nrow(test_mat), "\n")

# Sanity check: build a matrix where row 1 is an outlier in every column
check_mat <- test_mat
check_mat[1, ] <- 100
cat("\nSanity check (row 1 forced to 100 in every column):\n")
print(corrected_tukey(check_mat))


# -----------------------------------------------------------------------------
# Step 5: Defensive version with input checks
# -----------------------------------------------------------------------------
tukey_multiple_safe <- function(x) {
  # Allow numeric data frames by converting them, but check first
  if (is.data.frame(x)) {
    non_num <- names(x)[!vapply(x, is.numeric, logical(1))]
    if (length(non_num) > 0) {
      stop("All columns must be numeric. Non-numeric column(s): ",
           paste(non_num, collapse = ", "), call. = FALSE)
    }
    x <- as.matrix(x)
  }
  if (!is.matrix(x)) {
    stop("`x` must be a matrix (got class: ", class(x)[1], ").", call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop("`x` must be a NUMERIC matrix (got type: ", typeof(x), ").",
         call. = FALSE)
  }
  if (nrow(x) < 4) {
    stop("`x` needs at least 4 rows to compute meaningful quartiles.",
         call. = FALSE)
  }
  if (anyNA(x)) {
    warning("`x` contains NA values; they will not be flagged as outliers.",
            call. = FALSE)
  }

  # Vectorised core: apply Tukey rule to each column, then require all columns
  outliers <- apply(x, 2, tukey.outlier)
  outliers[is.na(outliers)] <- FALSE
  rowSums(outliers) == ncol(x)
}

cat("\n=== Step 5: Defensive version ===\n")
cat("Safe version on test_mat:\n")
print(tukey_multiple_safe(test_mat))
cat("Matches corrected version:",
    identical(tukey_multiple_safe(test_mat), corrected_tukey(test_mat)), "\n")

cat("\nTesting bad inputs:\n")
bad_inputs <- list(
  "numeric vector"            = rnorm(10),
  "character matrix"          = matrix(letters[1:20], nrow = 10),
  "data frame w/ text column" = data.frame(a = rnorm(10), b = letters[1:10]),
  "too few rows"              = matrix(rnorm(6), nrow = 2)
)
for (nm in names(bad_inputs)) {
  msg <- tryCatch({ tukey_multiple_safe(bad_inputs[[nm]]); "no error" },
                  error = function(e) conditionMessage(e))
  cat(sprintf("  %-26s -> %s\n", nm, msg))
}
