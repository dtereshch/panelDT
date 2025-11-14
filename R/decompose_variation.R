#' Decompose Variation in Panel Data
#'
#' Inspired by Stata's xtsum command, this function calculates descriptive statistics
#' for panel data and decomposes variance into between and within components.
#' Provides insights into variation across groups and over time.
#'
#' @param data A data frame containing the panel data. Can also be a `plm::pdata.frame`
#'   or an object with `fixest::panel` structure.
#' @param variables A character vector specifying which numeric variables to analyze.
#'   If NULL (default), all numeric variables in the dataset will be used.
#' @param group A character string specifying the group ID variable (e.g., individual, firm, country).
#'   For `plm::pdata.frame` and `fixest::panel` objects, this is automatically extracted.
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
#' @references
#' Based on Stata's xtsum command and corresponding discussion at
#' https://stackoverflow.com/questions/49282083/xtsum-command-for-r
#'
#' @examples
#' data("Crime", package = "plm")
#' vars_select <- c("crmrte", "polpc")
#' decompose_variation(Crime, vars_select, "county")
#'
#' # Using with simulated panel data
#' set.seed(123)
#' panel_df <- data.frame(
#'   id = rep(1:10, each = 5),
#'   time = rep(1:5, 10),
#'   y = rnorm(50),
#'   x = runif(50)
#' )
#' decompose_variation(panel_df, c("y", "x"), "id")
#'
#' # Using with plm::pdata.frame
#' if (require(plm)) {
#'   data("Produc", package = "plm")
#'   pdata <- plm::pdata.frame(Produc, index = c("state", "year"))
#'   decompose_variation(pdata)  # Automatically uses all numeric variables
#' }
#'
#' # Using with fixest::panel
#' if (require(fixest)) {
#'   data("base_did", package = "fixest")
#'   panel_data <- base_did
#'   # Set panel using fixest::panel
#'   decompose_variation(panel_data, group = "id")
#' }
#'
#' @export
decompose_variation <- function(data, variables = NULL, group = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame, pdata.frame, or similar object")
  }

  # Handle special panel data objects
  data_info <- extract_panel_info(data, group)
  data_df <- data_info$data
  group_var <- data_info$group_var
  data_class <- data_info$data_class

  # If variables is not specified, use all numeric variables
  if (is.null(variables)) {
    numeric_vars <- sapply(data_df, is.numeric)
    variables <- names(data_df)[numeric_vars]

    # Remove the group variable from variables if it's numeric
    if (group_var %in% variables) {
      variables <- variables[variables != group_var]
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

  missing_vars <- variables[!variables %in% names(data_df)]
  if (length(missing_vars) > 0) {
    stop(
      "The following variables were not found in the data frame: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Check if specified columns are numeric
  non_numeric_vars <- variables[!sapply(data_df[variables], is.numeric)]
  if (length(non_numeric_vars) > 0) {
    stop(
      "The following variables are not numeric: ",
      paste(non_numeric_vars, collapse = ", ")
    )
  }

  # Check for too many groups
  group_vector <- data_df[[group_var]]
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
    df <- data[complete_cases, ]

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
    overall_sd <- stats::sd(x, na.rm = TRUE)
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    n_obs <- length(x)

    # Calculate between variance (variation in group means)
    group_means <- tapply(x, group_vec, mean, na.rm = TRUE)
    between_sd <- stats::sd(group_means, na.rm = TRUE)

    # Calculate within variance (variation around group means)
    # Match group means to original data using character representation
    group_means_expanded <- group_means[group_vec]
    within_sd <- stats::sd(x - group_means_expanded, na.rm = TRUE)

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
    decompose_variation_1(data_df, varname, group_var)
  })

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Add data source information as attribute
  attr(result_df, "data_source") <- data_class
  attr(result_df, "group_var") <- group_var
  attr(result_df, "n_groups") <- n_groups

  return(result_df)
}

# Helper function to extract panel information from different data types
extract_panel_info <- function(data, group = NULL) {
  data_class <- class(data)

  # Check for plm::pdata.frame
  if ("pdata.frame" %in% data_class) {
    if (!requireNamespace("plm", quietly = TRUE)) {
      stop("Package 'plm' is required for handling pdata.frame objects")
    }

    index_attrs <- attr(data, "index")
    if (is.null(group)) {
      group_var <- as.character(index_attrs[[1]]) # First index is usually entity
    } else {
      group_var <- group
    }

    # Convert to regular data frame for processing
    data_df <- as.data.frame(data)
    data_df[[group_var]] <- index_attrs[[1]]

    return(list(
      data = data_df,
      group_var = group_var,
      data_class = "pdata.frame"
    ))
  }

  # Check for fixest panel data (look for panel attributes)
  if (
    !is.null(attr(data, "panel_info")) || !is.null(attr(data, "fixest_panel"))
  ) {
    if (is.null(group)) {
      # Try to extract group variable from fixest panel attributes
      panel_info <- attr(data, "panel_info")
      if (!is.null(panel_info$panel.id)) {
        group_var <- panel_info$panel.id[1]
      } else {
        stop("For fixest panel data, please specify the 'group' parameter")
      }
    } else {
      group_var <- group
    }

    return(list(
      data = as.data.frame(data),
      group_var = group_var,
      data_class = "fixest_panel"
    ))
  }

  # Regular data frame - group must be specified
  if (is.null(group)) {
    stop("For regular data frames, the 'group' parameter must be specified")
  }

  if (!group %in% names(data)) {
    stop("Group variable '", group, "' not found in data frame")
  }

  return(list(
    data = as.data.frame(data),
    group_var = group,
    data_class = "data.frame"
  ))
}
