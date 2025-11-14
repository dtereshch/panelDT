#' Describes participation patterns in an unbalanced panel
#'
#' Provides comprehensive summary statistics and pattern analysis of panel data structure
#' in text format. Supports various data object types including regular data frames,
#' plm::pdata.frame, and fixest::panel objects.
#'
#' @param data The data frame, plm::pdata.frame, or fixest::panel object
#' @param group Character string specifying the entities' identifier column name
#'              (optional for specialized panel objects)
#' @param time Character string specifying the time identifier column name
#'             (optional for specialized panel objects)
#' @param detailed Logical, whether to show detailed pattern information (default: TRUE)
#' @param max_patterns Maximum number of patterns to display in detail (default: 10)
#'
#' @return Invisible list containing detailed participation pattern statistics
#'
#' @examples
#' # Load required packages
#' if (requireNamespace("plm", quietly = TRUE)) {
#'   library(plm)
#'
#'   # Using regular data frame with quoted variable names
#'   data("Grunfeld", package = "plm")
#'   explore_participation(Grunfeld, group = "firm", time = "year")
#'
#'   # Using plm::pdata.frame (no need for group/time arguments)
#'   pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))
#'   explore_participation(pGrunfeld)
#'
#'   # Show only summary without detailed patterns
#'   explore_participation(Grunfeld, group = "firm", time = "year", detailed = FALSE)
#' }
#'
#' @export
explore_participation <- function(
  data,
  group = NULL,
  time = NULL,
  detailed = TRUE,
  max_patterns = 10
) {
  # Input validation
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  # Handle different object types
  if (inherits(data, "pdata.frame")) {
    # Extract indices from plm pdata.frame
    indices <- attr(data, "index")
    group_var <- indices[[1]]
    time_var <- indices[[2]]
    data_df <- as.data.frame(data)
    group_name <- names(indices)[1]
    time_name <- names(indices)[2]
  } else if (inherits(data, "fixest")) {
    # Extract panel info from fixest object
    panel_info <- data$panel.info
    if (is.null(panel_info)) {
      stop(
        "fixest object does not contain panel information. Use fixest::panel() to set panel structure."
      )
    }
    group_var <- data[[panel_info$panel.id[1]]]
    time_var <- data[[panel_info$panel.id[2]]]
    data_df <- as.data.frame(data)
    group_name <- panel_info$panel.id[1]
    time_name <- panel_info$panel.id[2]
  } else {
    # Regular data frame - require group and time arguments as character strings
    if (is.null(group) || is.null(time)) {
      stop(
        "For regular data frames, both 'group' and 'time' arguments are required as character strings"
      )
    }

    if (!is.character(group) || !is.character(time)) {
      stop(
        "For regular data frames, 'group' and 'time' arguments must be character strings"
      )
    }

    if (length(group) != 1 || length(time) != 1) {
      stop("'group' and 'time' must be single character strings")
    }

    data_df <- as.data.frame(data)
    group_name <- group
    time_name <- time

    # Extract variables
    group_var <- data_df[[group_name]]
    time_var <- data_df[[time_name]]

    if (is.null(group_var)) {
      stop(
        "Group variable '",
        group_name,
        "' not found in data. Available variables: ",
        paste(names(data_df), collapse = ", ")
      )
    }
    if (is.null(time_var)) {
      stop(
        "Time variable '",
        time_name,
        "' not found in data. Available variables: ",
        paste(names(data_df), collapse = ", ")
      )
    }
  }

  # Convert to character to handle different classes uniformly
  group_var <- as.character(group_var)
  time_var <- as.character(time_var)

  # Create cross-tabulation
  unique_groups <- unique(group_var)
  unique_times <- unique(time_var)

  # Create presence matrix (1 = present, 0 = missing)
  presence_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(unique_times),
    dimnames = list(unique_groups, unique_times)
  )

  # Fill matrix with 1 for present observations
  for (i in seq_along(group_var)) {
    row_idx <- which(unique_groups == group_var[i])
    col_idx <- which(unique_times == time_var[i])
    presence_matrix[row_idx, col_idx] <- 1
  }

  # Group entities by missing value patterns
  pattern_strings <- apply(presence_matrix, 1, function(x) {
    paste(x, collapse = "")
  })
  pattern_groups <- split(rownames(presence_matrix), pattern_strings)

  # Create pattern matrix with one row per pattern
  if (length(pattern_groups) == 1) {
    pattern_matrix <- matrix(
      as.numeric(strsplit(names(pattern_groups)[1], "")[[1]]),
      nrow = 1,
      ncol = ncol(presence_matrix)
    )
  } else {
    pattern_matrix <- matrix(
      0,
      nrow = length(pattern_groups),
      ncol = ncol(presence_matrix)
    )
  }

  rownames(pattern_matrix) <- paste0(
    "Pattern ",
    seq_len(length(pattern_groups))
  )
  colnames(pattern_matrix) <- colnames(presence_matrix)

  # Fill pattern matrix and count entities per pattern
  pattern_counts <- numeric(length(pattern_groups))
  pattern_pcts <- numeric(length(pattern_groups))
  total_entities <- length(unique_groups)

  for (i in seq_along(pattern_groups)) {
    if (length(pattern_groups) == 1) {
      # Already filled above for single pattern case
      pattern <- pattern_matrix[i, ]
    } else {
      pattern <- as.numeric(strsplit(names(pattern_groups)[i], "")[[1]])
      pattern_matrix[i, ] <- pattern
    }
    pattern_counts[i] <- length(pattern_groups[[i]])
    pattern_pcts[i] <- pattern_counts[i] / total_entities * 100
  }

  # Order patterns by frequency (most common first)
  pattern_order <- order(pattern_counts, decreasing = TRUE)
  pattern_matrix <- pattern_matrix[pattern_order, , drop = FALSE]
  pattern_counts <- pattern_counts[pattern_order]
  pattern_pcts <- pattern_pcts[pattern_order]
  pattern_groups <- pattern_groups[pattern_order]

  # Calculate summary statistics
  stats <- list(
    n_entities = total_entities,
    n_periods = ncol(presence_matrix),
    total_obs = sum(presence_matrix),
    balanced_obs = nrow(presence_matrix) * ncol(presence_matrix),
    missing_obs = nrow(presence_matrix) *
      ncol(presence_matrix) -
      sum(presence_matrix),
    pct_missing = (1 -
      sum(presence_matrix) / (nrow(presence_matrix) * ncol(presence_matrix))) *
      100,
    entities_with_gaps = sum(rowSums(presence_matrix) < ncol(presence_matrix)),
    pct_entities_with_gaps = (sum(
      rowSums(presence_matrix) < ncol(presence_matrix)
    ) /
      nrow(presence_matrix)) *
      100,
    n_patterns = length(pattern_groups)
  )

  # Order time periods
  if (all(grepl("^-?\\d+\\.?\\d*$", unique_times))) {
    col_order <- order(as.numeric(unique_times))
  } else {
    col_order <- order(unique_times)
  }

  ordered_matrix <- pattern_matrix[, col_order, drop = FALSE]

  # Print summary statistics to console
  cat("=== Panel Data Participation Patterns ===\n\n")

  cat("Basic Statistics:\n")
  cat(sprintf("  Number of entities: %d\n", stats$n_entities))
  cat(sprintf("  Number of time periods: %d\n", stats$n_periods))
  cat(sprintf("  Total observations: %d\n", stats$total_obs))
  cat(sprintf("  Potential observations (balanced): %d\n", stats$balanced_obs))
  cat(sprintf(
    "  Missing observations: %d (%.1f%%)\n",
    stats$missing_obs,
    stats$pct_missing
  ))
  cat(sprintf(
    "  Entities with gaps: %d (%.1f%%)\n",
    stats$entities_with_gaps,
    stats$pct_entities_with_gaps
  ))
  cat(sprintf("  Number of participation patterns: %d\n\n", stats$n_patterns))

  # Print pattern coverage statistics
  cat("Pattern Coverage:\n")
  cumulative_pct <- 0
  for (i in seq_len(min(length(pattern_counts), 5))) {
    cumulative_pct <- cumulative_pct + pattern_pcts[i]
    cat(sprintf(
      "  Top %d patterns cover: %.1f%% of entities\n",
      i,
      cumulative_pct
    ))
  }
  cat("\n")

  if (detailed) {
    # Print detailed pattern information
    cat("Detailed Pattern Information:\n")
    n_to_display <- min(length(pattern_groups), max_patterns)

    for (i in seq_len(n_to_display)) {
      pattern <- ordered_matrix[i, ]
      pattern_visual <- ifelse(pattern == 1, "X", ".")

      cat(sprintf(
        "Pattern %d (n=%d, %.1f%%): [%s] %s\n",
        i,
        pattern_counts[i],
        pattern_pcts[i],
        paste(pattern_visual, collapse = ""),
        ifelse(i == 1, "(Most Common)", "")
      ))

      # Show first few entities for this pattern
      entities_in_pattern <- pattern_groups[[i]]
      if (length(entities_in_pattern) <= 5) {
        cat(sprintf(
          "    Entities: %s\n",
          paste(entities_in_pattern, collapse = ", ")
        ))
      } else {
        cat(sprintf(
          "    Entities: %s, ... (%d more)\n",
          paste(entities_in_pattern[1:3], collapse = ", "),
          length(entities_in_pattern) - 3
        ))
      }
    }

    if (length(pattern_groups) > max_patterns) {
      cat(sprintf(
        "\n... and %d more patterns (use max_patterns = %d to see all)\n",
        length(pattern_groups) - max_patterns,
        length(pattern_groups)
      ))
    }
    cat("\n")
  }

  # Print time period coverage
  period_coverage <- colSums(presence_matrix)
  period_coverage_pct <- period_coverage / stats$n_entities * 100

  cat("Time Period Coverage:\n")
  for (j in seq_along(period_coverage)) {
    cat(sprintf(
      "  %s: %d entities (%.1f%%)\n",
      colnames(ordered_matrix)[j],
      period_coverage[j],
      period_coverage_pct[j]
    ))
  }

  # Return invisible results with detailed pattern information
  invisible(list(
    presence_matrix = presence_matrix,
    pattern_matrix = ordered_matrix,
    pattern_groups = pattern_groups,
    pattern_stats = data.frame(
      pattern_id = seq_len(length(pattern_counts)),
      pattern_string = names(pattern_groups),
      n_entities = pattern_counts,
      percent_entities = pattern_pcts,
      entities = I(pattern_groups)
    ),
    statistics = stats,
    period_coverage = data.frame(
      time_period = colnames(ordered_matrix),
      n_entities = period_coverage,
      percent_coverage = period_coverage_pct
    ),
    group_var = group_name,
    time_var = time_name
  ))
}
