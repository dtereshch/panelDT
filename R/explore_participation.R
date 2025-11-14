#' Providing a table of entities with incomplete observations (missing values)
#'
#' @param data The data frame, pdata.frame, or fixest panel object
#' @param group Entities' identifier (optional for panel data objects)
#' @param detailed Logical indicating whether to include detailed missing counts
#'        for each variable (TRUE) or just summary counts (FALSE). Default is FALSE.
#' @return A data frame containing entities with number of variables that have
#'         at least one missing value for that entity, as well as total number
#'         of missing observations. The data frame is arranged by number of
#'         variables with missing values. If detailed = TRUE, includes additional
#'         columns with NA counts for each variable.
#' @examples
#' # Using regular data frame - summary view
#' data("USSeatBelts", package = "AER")
#' explore_incomplete(USSeatBelts, "state")
#'
#' # Detailed view with variable-level NA counts
#' explore_incomplete(USSeatBelts, "state", detailed = TRUE)
#'
#' # Using pdata.frame (if available)
#' # library(plm)
#' # pdata <- pdata.frame(USSeatBelts, index = "state")
#' # explore_incomplete(pdata)
#'
#' # Using fixest panel (if available)
#' # library(fixest)
#' # pdata <- panel(USSeatBelts, ~ state + year)
#' # explore_incomplete(pdata)
#' @export
explore_incomplete <- function(data, group = NULL, detailed = FALSE) {
  # Check if data is provided
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  # Validate detailed parameter
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("Argument 'detailed' must be a single logical value (TRUE or FALSE)")
  }

  # Handle panel data objects
  panel_info <- extract_panel_info(data, group)
  data <- panel_info$data
  group <- panel_info$group

  # Validate group variable
  if (is.null(group)) {
    stop(
      "No group variable specified and could not be automatically extracted from panel data object"
    )
  }

  if (!group %in% names(data)) {
    stop("Group variable '", group, "' not found in data")
  }

  # Check if group variable has valid values
  if (length(data[[group]]) == 0) {
    stop("Group variable has no observations")
  }

  # Convert group to character for consistent handling
  group_values <- as.character(data[[group]])
  unique_groups <- unique(group_values)

  # Get variable names excluding group variable
  vars <- setdiff(names(data), group)

  if (length(vars) == 0) {
    stop("No variables to analyze (only group variable found)")
  }

  # Initialize base results
  result <- data.frame(
    group = unique_groups,
    n_vars_with_na = 0,
    total_na = 0,
    stringsAsFactors = FALSE
  )
  names(result)[1] <- group

  # If detailed = TRUE, pre-allocate columns for each variable
  if (detailed) {
    for (var in vars) {
      result[[var]] <- 0
    }
  }

  # For each group, calculate missing statistics
  for (i in seq_along(unique_groups)) {
    current_group <- unique_groups[i]
    group_data <- data[group_values == current_group, vars, drop = FALSE]

    # Count variables with at least one NA
    vars_with_na <- sum(apply(group_data, 2, function(x) any(is.na(x))))

    # Count total number of NA observations
    total_na <- sum(is.na(group_data))

    result$n_vars_with_na[i] <- vars_with_na
    result$total_na[i] <- total_na

    # If detailed = TRUE, add NA counts for each variable
    if (detailed) {
      for (var in vars) {
        result[[var]][i] <- sum(is.na(group_data[[var]]))
      }
    }
  }

  # Filter groups with any missing variables and arrange
  result <- result[result$n_vars_with_na > 0, ]

  # Sort by primary and secondary criteria
  result <- result[order(-result$n_vars_with_na, -result$total_na), ]

  # Reset row names
  rownames(result) <- NULL

  return(result)
}

# Helper function to handle panel data objects
extract_panel_info <- function(data, group) {
  # Check for pdata.frame (plm package)
  if (inherits(data, "pdata.frame")) {
    if (!requireNamespace("plm", quietly = TRUE)) {
      stop("plm package is required for handling pdata.frame objects")
    }

    index_attr <- attr(data, "index")
    if (!is.null(index_attr) && ncol(index_attr) >= 1) {
      group_var <- names(index_attr)[1]
      if (is.null(group)) {
        group <- group_var
        message("Using automatically detected group variable: ", group)
      }
      # Convert to regular data frame for processing
      data <- as.data.frame(data)
    }
  }

  # Check for fixest panel data
  if (inherits(data, "fixest_panel")) {
    if (!requireNamespace("fixest", quietly = TRUE)) {
      stop("fixest package is required for handling fixest panel objects")
    }

    panel_info <- fixest::panel_info(data)
    if (!is.null(panel_info$panel.id)) {
      group_var <- panel_info$panel.id[1]
      if (is.null(group)) {
        group <- group_var
        message("Using automatically detected group variable: ", group)
      }
    }
  }

  return(list(data = data, group = group))
}
