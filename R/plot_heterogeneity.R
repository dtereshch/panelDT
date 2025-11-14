#' Plot Heterogeneity Among Groups
#'
#' Creates publication-ready visualizations of heterogeneity among groups using base R graphics.
#' Designed for panel data to show variation across object identifiers or time periods. Supports
#' multiple grouping variables (faceting) and automatic handling of panel data objects.
#'
#' @param data A data frame in long panel format, tidy format, plm::pdata.frame, or fixest::panel object
#' @param variable The numeric variable of interest (character string)
#' @param group The grouping variable(s) (character string or vector of character strings).
#'   If NULL and data is a panel data object, uses panel index variables.
#' @param xlab X-axis label (default: based on grouping variable)
#' @param ylab Y-axis label (default: based on variable name)
#' @param point_col Color for individual points (default: "gray50")
#' @param mean_col Color for mean line and points (default: "#0072B2") # blue
#' @param point_alpha Transparency for individual points (default: 0.6)
#' @param mean_lwd Line width for mean trend (default: 2)
#' @param cex Point size multiplier (default: 1)
#' @param las Orientation of axis labels (default: 1)
#' @param plot Logical, whether to create the plot (default: TRUE)
#' @param ncol Number of columns in facet grid (default: NULL, auto-determined)
#' @param nrow Number of rows in facet grid (default: NULL, auto-determined)
#'
#' @return Invisibly returns a list with summary statistics. Creates a base R plot.
#'
#' @examples
#' if(requireNamespace("AER", quietly = TRUE)) {
#'   data("Fatalities", package = "AER")
#'   plot_heterogeneity(Fatalities, "fatalrate", "year")
#'   plot_heterogeneity(Fatalities, "fatalrate", "state")
#'   plot_heterogeneity(Fatalities, "fatalrate", c("year", "state"))
#' }
#'
#' @export
plot_heterogeneity <- function(
  data,
  variable,
  group = NULL,
  xlab = NULL,
  ylab = NULL,
  point_col = "gray50",
  mean_col = "#0072B2",
  point_alpha = 0.6,
  mean_lwd = 2,
  cex = 1,
  las = 1,
  plot = TRUE,
  ncol = NULL,
  nrow = NULL
) {
  # Input validation
  if (
    !is.data.frame(data) &&
      !inherits(data, "pdata.frame") &&
      !inherits(data, "fixest_panel")
  ) {
    stop("'data' must be a data frame, pdata.frame, or fixest panel object")
  }

  if (nrow(data) == 0) {
    stop("'data' must have at least one row")
  }

  # Require variable to be character string
  if (!is.character(variable) || length(variable) != 1) {
    stop("'variable' must be a single character string")
  }

  # Handle panel data objects
  if (inherits(data, "pdata.frame")) {
    # Extract index from plm pdata.frame
    index_vars <- attr(data, "index")
    if (is.null(group)) {
      group <- names(index_vars)
    }
    # Convert to regular data frame for processing
    data_df <- as.data.frame(data)
  } else if (inherits(data, "fixest_panel")) {
    # Extract panel info from fixest
    panel_info <- attr(data, "panel_info")
    if (is.null(group)) {
      group <- c(panel_info$panel.id[[1]], panel_info$panel.id[[2]])
    }
    data_df <- as.data.frame(data)
  } else {
    data_df <- data
  }

  # If group is NULL for regular data frame, error
  if (is.null(group)) {
    stop("'group' must be provided for regular data frames")
  }

  # Validate group parameter
  if (!is.character(group)) {
    stop("'group' must be a character string or vector of character strings")
  }

  # Check if variables exist in data
  if (!variable %in% names(data_df)) {
    stop("Variable '", variable, "' not found in data")
  }

  missing_groups <- setdiff(group, names(data_df))
  if (length(missing_groups) > 0) {
    stop(
      "Group variable(s) '",
      paste(missing_groups, collapse = "', '"),
      "' not found in data"
    )
  }

  # Extract the main variable
  y_var <- data_df[[variable]]

  # Check variable type
  if (!is.numeric(y_var)) {
    stop("'variable' must be a numeric variable")
  }

  # Function to create single plot
  create_single_plot <- function(
    data_sub,
    group_var,
    xlab_single = NULL,
    ylab_single = NULL
  ) {
    x_var <- data_sub[[group_var]]

    # Check group variable type
    if (!is.factor(x_var) && !is.character(x_var) && !is.numeric(x_var)) {
      stop(
        "Group variable '",
        group_var,
        "' must be a factor, character, or numeric variable"
      )
    }

    # Convert grouping variable to factor for consistent handling
    if (!is.factor(x_var)) {
      x_var <- as.factor(x_var)
    }

    # Set default labels if not provided
    if (is.null(xlab_single)) {
      xlab_single <- group_var
    }
    if (is.null(ylab_single)) {
      ylab_single <- variable
    }

    # Create color with alpha
    point_col_rgb <- col2rgb(point_col) / 255
    point_col_alpha <- rgb(
      point_col_rgb[1],
      point_col_rgb[2],
      point_col_rgb[3],
      alpha = point_alpha
    )

    # Calculate group means
    group_means <- tapply(data_sub[[variable]], x_var, mean, na.rm = TRUE)

    # Create the plot
    plot(
      NA,
      xlim = c(0.5, length(levels(x_var)) + 0.5),
      ylim = range(data_sub[[variable]], na.rm = TRUE),
      xlab = xlab_single,
      ylab = ylab_single,
      main = "", # Remove title
      xaxt = "n",
      frame.plot = FALSE
    )

    # Add x-axis
    axis(1, at = seq_along(levels(x_var)), labels = levels(x_var))

    # Add individual points with jitter
    x_jitter <- jitter(as.numeric(x_var), amount = 0.2)
    points(
      x_jitter,
      data_sub[[variable]],
      col = point_col_alpha,
      pch = 16,
      cex = 0.8 * cex
    )

    # Add mean line and points
    lines(
      seq_along(group_means),
      group_means,
      col = mean_col,
      lwd = mean_lwd,
      type = "o",
      pch = 18,
      cex = 1.5 * cex
    )

    # Add grid
    grid()

    # Add legend to every plot
    legend(
      "topright",
      legend = c("Individual observations", "Group means"),
      col = c(point_col, mean_col),
      pch = c(16, 18),
      lty = c(NA, 1),
      pt.cex = c(0.8, 1.5),
      bty = "n",
      cex = 0.8 * cex # Slightly smaller legend for multi-panel plots
    )

    # Return summary statistics for this group
    list(
      means = group_means,
      sd = tapply(data_sub[[variable]], x_var, sd, na.rm = TRUE),
      n = tapply(data_sub[[variable]], x_var, function(x) sum(!is.na(x)))
    )
  }

  # Calculate overall summary statistics
  summary_stats <- list(
    overall_mean = mean(y_var, na.rm = TRUE),
    overall_sd = sd(y_var, na.rm = TRUE),
    group_stats = list()
  )

  if (plot) {
    # Set up plotting layout for multiple groups
    if (length(group) > 1) {
      # Determine grid dimensions
      n_plots <- length(group)
      if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_plots))
        nrow <- ceiling(n_plots / ncol)
      } else if (is.null(ncol)) {
        ncol <- ceiling(n_plots / nrow)
      } else if (is.null(nrow)) {
        nrow <- ceiling(n_plots / ncol)
      }

      # Save current par settings
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))

      # Set up multi-panel plot
      par(
        mfrow = c(nrow, ncol),
        mar = c(4, 4, 2, 1) + 0.1, # Tighter margins for multi-panel
        oma = c(2, 2, 2, 2), # Outer margins for overall labels
        las = las
      )
    } else {
      # Single plot setup
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mar = c(5, 4, 2, 2) + 0.1, las = las)
    }

    # Create plots for each grouping variable
    for (i in seq_along(group)) {
      group_var <- group[i]

      # Use provided labels only for single plot, otherwise use defaults
      xlab_single <- if (length(group) == 1) xlab else NULL
      ylab_single <- if (length(group) == 1) ylab else NULL

      group_stats <- create_single_plot(
        data_df,
        group_var,
        xlab_single,
        ylab_single
      )
      summary_stats$group_stats[[group_var]] <- group_stats
    }
  } else {
    # Calculate statistics without plotting
    for (group_var in group) {
      x_var <- data_df[[group_var]]
      if (!is.factor(x_var)) {
        x_var <- as.factor(x_var)
      }

      summary_stats$group_stats[[group_var]] <- list(
        means = tapply(data_df[[variable]], x_var, mean, na.rm = TRUE),
        sd = tapply(data_df[[variable]], x_var, sd, na.rm = TRUE),
        n = tapply(data_df[[variable]], x_var, function(x) sum(!is.na(x)))
      )
    }
  }

  # Return summary statistics invisibly
  invisible(summary_stats)
}
