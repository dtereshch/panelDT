#' Calculate Group Descriptive Statistics
#'
#' Provides comprehensive descriptive statistics for numeric variables grouped by
#' a single grouping variable. Returns results in a clean data frame format
#' with statistics for each variable within each group.
#'
#' @param data A data frame containing the variables to analyze.
#' @param variables A character vector specifying which numeric variables to analyze.
#'        If not specified, all numeric variables in the data frame will be used.
#' @param group A character string specifying the grouping variable.
#'
#' @return A data frame with the following columns:
#'   \item{group}{The grouping variable}
#'   \item{variable}{The name of the numeric variable}
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
#' describe_by(production, group = "year")
#'
#' # Specify single numeric variable
#' describe_by(production, variables = "sales", group = "year")
#' describe_by(production, variables = "sales", group = "firm")
#'
#' # Specify group of numeric variables
#' describe_by(production, variables = c("sales", "capital", "labor"), group = "year")
#' describe_by(production, variables = c("sales", "capital", "labor"), group = "firm")#'
#'
#' @export
describe_by <- function(data, variables, group) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # Validate group
  if (missing(group) || length(group) == 0) {
    stop("'group' must be specified")
  }

  if (length(group) > 1) {
    stop("Only one grouping variable is supported")
  }

  missing_groups <- group[!group %in% names(data)]
  if (length(missing_groups) > 0) {
    stop(
      "The following grouping variable was not found in the data frame: ",
      paste(missing_groups, collapse = ", ")
    )
  }

  # If variables not specified, use all numeric variables excluding the group variable
  if (missing(variables)) {
    numeric_vars <- sapply(data, is.numeric)
    variables <- names(data)[numeric_vars]
    variables <- variables[variables != group] # Exclude group variable
  }

  # Validate variables
  if (length(variables) == 0) {
    stop("No numeric variables found to analyze")
  }

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

  # Helper function to count non-NA values
  count_non_na <- function(x) {
    sum(!is.na(x))
  }

  # Split data by grouping variable
  group_combinations <- unique(data[[group]])
  group_combinations <- sort(group_combinations[!is.na(group_combinations)])

  results <- list()

  for (i in seq_along(group_combinations)) {
    current_group <- group_combinations[i]

    # Create subset for current group
    group_subset <- data[
      data[[group]] == current_group & !is.na(data[[group]]),
    ]

    # Calculate statistics for each variable in current group
    group_results <- lapply(variables, function(var) {
      x <- group_subset[[var]]

      # Handle case where all values are NA
      if (all(is.na(x))) {
        stats_row <- data.frame(
          n = 0,
          mean = NA_real_,
          median = NA_real_,
          sd = NA_real_,
          min = NA_real_,
          max = NA_real_
        )
      } else {
        stats_row <- data.frame(
          n = count_non_na(x),
          mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          sd = stats::sd(x, na.rm = TRUE),
          min = min(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE)
        )
      }

      # Add group information and variable name
      data.frame(group = current_group, variable = var, stats_row)
    })

    results <- c(results, group_results)
  }

  # Combine all results into a single data frame
  result_df <- do.call(rbind, results)

  # Reset row names
  rownames(result_df) <- NULL

  return(result_df)
}
