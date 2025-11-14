#' Identify entities with incomplete observations (missing values)
#'
#' @param data The data frame, pdata.frame, or fixest panel object
#' @param group Entities' identifier (column name as character string). If NULL and data is a panel data object, the entity ID will be extracted automatically.
#' @return A vector containing entities with missing values
#' @examples
#' # Using base R dataset
#' data("airquality")
#' # Add a group variable
#' airquality$City <- rep(c("NY", "LA"), length.out = nrow(airquality))
#' find_incomplete(airquality, "City")
#'
#' # This will now throw an error:
#' # find_incomplete(airquality, City)
#'
#' @export
find_incomplete <- function(data, group = NULL) {
  # Input validation
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  if (
    !is.data.frame(data) &&
      !inherits(data, "pdata.frame") &&
      !inherits(data, "fixest")
  ) {
    stop("'data' must be a data frame, pdata.frame, or fixest panel object")
  }

  # Handle special panel data objects
  group_name <- NULL

  if (inherits(data, "pdata.frame")) {
    if (!requireNamespace("plm", quietly = TRUE)) {
      stop("plm package is required for handling pdata.frame objects")
    }
    group_name <- names(attr(data, "index"))[1]
    group_var <- data[[group_name]]
    data <- as.data.frame(data)
  } else if (inherits(data, "fixest")) {
    if (!requireNamespace("fixest", quietly = TRUE)) {
      stop("fixest package is required for handling fixest panel objects")
    }
    panel_info <- fixest::panel_info(data)
    if (!is.null(panel_info$panel.id)) {
      group_name <- panel_info$panel.id[1]
      # Extract data from fixest object
      model_data <- stats::model.frame(data)
      if (group_name %in% names(model_data)) {
        group_var <- model_data[[group_name]]
      }
    }
  }

  # Extract group variable if not from special object
  if (is.null(group_name)) {
    if (missing(group) || is.null(group)) {
      stop("Argument 'group' is required for regular data frames")
    }

    # Validate group is a character string
    if (!is.character(group) || length(group) != 1) {
      stop(
        "'group' must be a single character string specifying the column name"
      )
    }

    group_name <- group

    # Validate group column exists
    if (!group_name %in% names(data)) {
      stop(sprintf("Group variable '%s' not found in data", group_name))
    }

    group_var <- data[[group_name]]
  }

  # Convert group variable to appropriate type for consistent handling
  if (is.factor(group_var)) {
    group_var <- as.character(group_var)
  }

  # Check if group variable has missing values itself
  if (any(is.na(group_var))) {
    warning(
      "Group variable contains missing values. These will be included in the results."
    )
  }

  # Identify complete cases by group
  group_unique <- unique(group_var)
  incomplete_entities <- character(0)

  for (entity in group_unique) {
    # Handle NA values in group variable
    if (is.na(entity)) {
      entity_data <- data[is.na(group_var), , drop = FALSE]
    } else {
      entity_data <- data[group_var == entity, , drop = FALSE]
    }

    # Check if any row for this entity has missing values
    # Remove group column from missing value check to avoid self-reference
    entity_data_subset <- entity_data[,
      setdiff(names(entity_data), group_name),
      drop = FALSE
    ]

    if (any(apply(entity_data_subset, 1, function(row) any(is.na(row))))) {
      incomplete_entities <- c(incomplete_entities, entity)
    }
  }

  # Convert back to original type if possible
  original_type <- class(data[[group_name]])
  if ("numeric" %in% original_type || "integer" %in% original_type) {
    # Handle numeric conversion carefully to avoid warnings
    numeric_entities <- suppressWarnings(as.numeric(incomplete_entities))
    if (!any(is.na(numeric_entities))) {
      incomplete_entities <- numeric_entities
    }
  } else if ("factor" %in% original_type) {
    incomplete_entities <- factor(
      incomplete_entities,
      levels = levels(data[[group_name]])
    )
  }

  return(incomplete_entities)
}
