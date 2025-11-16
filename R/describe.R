#' Calculate Basic Descriptive Statistics
#'
#' Provides comprehensive descriptive statistics for numeric variables in a data frame.
#' Returns results in a data frame format with statistics for each variable.
#' By default, analyzes all numeric columns when no specific variables are specified.
#'
#' @param data A data frame containing the variables to analyze.
#' @param variables An optional character vector specifying which numeric variables to analyze.
#'   If NULL (default), all numeric columns in the data frame will be used.
#'
#' @return A data frame with one row per variable and the following columns:
#'   \item{variable}{The name of the variable}
#'   \item{n}{Number of non-NA observations}
#'   \item{mean}{Arithmetic mean}
#'   \item{median}{Median value}
#'   \item{sd}{Standard deviation}
#'   \item{min}{Minimum value}
#'   \item{max}{Maximum value}
#'
#' @examples
#' data(production)
#'
#' # Using default - analyze all numeric variables
#' describe(production)
#'
#' # Specify single numeric variable
#' describe(production, variables = "sales")
#'
#' # Specify group of numeric variables
#' describe(production, variables = c("sales", "capital", "labor"))
#'
#' @export
describe <- function(data, variables = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # If variables is NULL, use all numeric columns
  if (is.null(variables)) {
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) == 0) {
      stop("No numeric columns found in the data frame")
    }
    variables <- names(data)[numeric_cols]
  } else {
    # Validate specified variables
    missing_vars <- variables[!variables %in% names(data)]
    if (length(missing_vars) > 0) {
      stop(
        "The following variables were not found in the data frame: ",
        paste(missing_vars, collapse = ", ")
      )
    }

    # Check if specified columns are numeric
    non_numeric_vars <- variables[!sapply(data[variables], is.numeric)]
    if (length(non_numeric_vars) > 0) {
      stop(
        "The following variables are not numeric: ",
        paste(non_numeric_vars, collapse = ", ")
      )
    }
  }

  # Helper function to count non-NA values
  count_non_na <- function(x) {
    sum(!is.na(x))
  }

  # Calculate statistics for each variable
  results <- lapply(variables, function(var) {
    x <- data[[var]]

    # Handle case where all values are NA
    if (all(is.na(x))) {
      return(data.frame(
        variable = var,
        n = 0,
        mean = NA_real_,
        median = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        max = NA_real_
      ))
    }

    data.frame(
      variable = var,
      n = count_non_na(x),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  })

  # Combine all results into a single data frame
  result_df <- do.call(rbind, results)

  # Reset row names
  rownames(result_df) <- NULL

  return(result_df)
}
