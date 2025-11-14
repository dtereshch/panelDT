#' Decompose Variation in Panel Data
#'
#' Inspired by Stata's xtsum command, this function calculates descriptive statistics
#' for panel data and decomposes variance into between and within components.
#' Provides insights into variation across groups and over time.
#'
#' @param data A data frame containing the panel data.
#' @param variables A character vector specifying which numeric variables to analyze.
#'   If NULL (default), all numeric variables in the dataset will be used.
#' @param group A character string specifying the group ID variable (e.g., individual, firm, country).
#'
#' @return A data frame with the following columns for each variable:
#'   \item{variable}{The name of the variable}
#'   \item{mean}{Overall mean}
#'   \item{sd_overall}{Overall standard deviation}
#'   \item{sd_between}{Between-group standard deviation}
#'   \item{sd_within}{Within-group standard deviation}
#'   \item{min}{Minimum value}
#'   \item{max}{Maximum value}
#'   \item{obs}{Number of observations}
#'
#' @export
decompose_variation <- function(data, variables = NULL, group = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # Convert to plain data frame to avoid any special class issues
  data_df <- as.data.frame(data)

  # Validate group parameter
  if (
    is.null(group) || !is.character(group) || length(group) == 0 || group == ""
  ) {
    stop("The 'group' parameter must be a non-empty character string")
  }

  if (!group %in% names(data_df)) {
    stop(
      "Group variable '",
      group,
      "' not found in data frame. Available variables: ",
      paste(names(data_df), collapse = ", ")
    )
  }

  # If variables is not specified, use all numeric variables
  if (is.null(variables)) {
    numeric_vars <- vapply(data_df, is.numeric, FUN.VALUE = logical(1))
    variables <- names(data_df)[numeric_vars]

    # Remove the group variable from variables if it's numeric
    if (group %in% variables) {
      variables <- variables[variables != group]
    }

    # Remove other potential ID variables
    id_like_vars <- c("id", "ID", "Id", "year", "time", "period", "date")
    variables <- variables[!variables %in% id_like_vars]

    if (length(variables) == 0) {
      stop("No numeric variables found in the dataset")
    }

    message(
      "Analyzing all numeric variables: ",
      paste(variables, collapse = ", ")
    )
  }

  # Validate variables
  missing_vars <- variables[!variables %in% names(data_df)]
  if (length(missing_vars) > 0) {
    stop(
      "The following variables were not found in the data frame: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Check if specified columns are numeric
  non_numeric_vars <- variables[
    !vapply(data_df[variables], is.numeric, FUN.VALUE = logical(1))
  ]
  if (length(non_numeric_vars) > 0) {
    stop(
      "The following variables are not numeric: ",
      paste(non_numeric_vars, collapse = ", ")
    )
  }

  # Check group variable
  group_vector <- data_df[[group]]
  if (length(group_vector) == 0) {
    stop("Group variable '", group, "' has zero length")
  }

  n_groups <- length(unique(group_vector))
  if (n_groups > 10000) {
    warning(
      "Large number of groups (",
      n_groups,
      "). This may impact performance."
    )
  }

  # Helper function to calculate panel statistics for one variable
  decompose_variation_1 <- function(data, varname, group) {
    # Remove rows with NA in the variable or group
    complete_cases <- complete.cases(data[[varname]], data[[group]])
    df <- data[complete_cases, , drop = FALSE]

    if (nrow(df) == 0) {
      return(data.frame(
        variable = varname,
        mean = NA_real_,
        sd_overall = NA_real_,
        sd_between = NA_real_,
        sd_within = NA_real_,
        min = NA_real_,
        max = NA_real_,
        obs = 0
      ))
    }

    # Convert group to character for consistent handling
    group_vec <- as.character(df[[group]])
    x <- df[[varname]]

    # Calculate overall statistics
    overall_mean <- mean(x, na.rm = TRUE)
    overall_sd <- sd(x, na.rm = TRUE)
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    n_obs <- length(x)

    # Calculate between variance (variation in group means)
    group_means <- tapply(x, group_vec, mean, na.rm = TRUE)
    between_sd <- sd(group_means, na.rm = TRUE)

    # Calculate within variance (variation around group means)
    # Match group means to original data using character representation
    group_means_expanded <- group_means[match(group_vec, names(group_means))]
    within_sd <- sd(x - group_means_expanded, na.rm = TRUE)

    data.frame(
      variable = varname,
      mean = overall_mean,
      sd_overall = overall_sd,
      sd_between = between_sd,
      sd_within = within_sd,
      min = min_val,
      max = max_val,
      obs = n_obs
    )
  }

  # Calculate statistics for each variable
  results <- lapply(variables, function(varname) {
    decompose_variation_1(data_df, varname, group)
  })

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Add data source information as attribute
  attr(result_df, "group_var") <- group
  attr(result_df, "n_groups") <- n_groups

  return(result_df)
}
