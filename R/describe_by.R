#' Calculate Group Descriptive Statistics
#'
#' Provides comprehensive descriptive statistics for numeric variables grouped by
#' one or more grouping variables. Returns results in a clean data frame format
#' with statistics for each variable within each group.
#'
#' @param data A data frame containing the variables to analyze.
#' @param variables A character vector specifying which numeric variables to analyze.
#' @param group A character vector specifying the grouping variable(s).
#'
#' @return A data frame with the following columns:
#'   \item{Grouping variables}{One column for each grouping variable}
#'   \item{variable}{The name of the numeric variable}
#'   \item{n}{Number of non-NA observations}
#'   \item{mean}{Arithmetic mean}
#'   \item{median}{Median value}
#'   \item{sd}{Standard deviation}
#'   \item{min}{Minimum value}
#'   \item{max}{Maximum value}
#'
#' @examples
#' # Single grouping variable
#' describe_by(mtcars, c("mpg", "wt"), "cyl")
#'
#' # Multiple grouping variables
#' describe_by(mtcars, c("mpg", "wt"), c("cyl", "am"))
#'
#' # Using with iris dataset
#' data(iris)
#' describe_by(iris, c("Sepal.Length", "Sepal.Width"), "Species")
#'
#' @export
describe_by <- function(data, variables, group) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # Validate variables
  if (missing(variables) || length(variables) == 0) {
    stop("'variables' must be specified")
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

  # Validate group
  if (missing(group) || length(group) == 0) {
    stop("'group' must be specified")
  }

  missing_groups <- group[!group %in% names(data)]
  if (length(missing_groups) > 0) {
    stop(
      "The following grouping variables were not found in the data frame: ",
      paste(missing_groups, collapse = ", ")
    )
  }

  # Helper function to count non-NA values
  count_non_na <- function(x) {
    sum(!is.na(x))
  }

  # Split data by grouping variables
  group_combinations <- unique(data[group])
  group_combinations <- group_combinations[
    do.call(order, group_combinations),
    ,
    drop = FALSE
  ]

  results <- list()

  for (i in seq_len(nrow(group_combinations))) {
    current_group <- group_combinations[i, , drop = FALSE]

    # Create subset for current group
    group_subset <- data
    for (group_col in group) {
      group_value <- current_group[[group_col]]
      group_subset <- group_subset[group_subset[[group_col]] == group_value, ]
    }

    # Remove rows with NA in grouping variables
    group_subset <- group_subset[complete.cases(group_subset[group]), ]

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
      cbind(current_group, data.frame(variable = var), stats_row)
    })

    results <- c(results, group_results)
  }

  # Combine all results into a single data frame
  result_df <- do.call(rbind, results)

  # Reset row names and reorder columns
  rownames(result_df) <- NULL

  # Reorder columns: grouping variables, then variable, then statistics
  col_order <- c(
    group,
    "variable",
    "n",
    "mean",
    "median",
    "sd",
    "min",
    "max"
  )
  result_df <- result_df[col_order]

  return(result_df)
}
