#' Plot Heterogeneity Among Groups
#'
#' Creates publication-ready visualizations of heterogeneity among groups using base R graphics.
#' Designed for panel data to show variation across object identifiers or time periods.
#'
#' @param data A data frame in long panel format or tidy format
#' @param varname The numeric variable of interest (unquoted name or string)
#' @param by The grouping variable (unquoted name or string)
#' @param xlab X-axis label (default: based on grouping variable)
#' @param ylab Y-axis label (default: based on variable name)
#' @param point_col Color for individual points (default: "gray50")
#' @param mean_col Color for mean line and points (default: "#0072B2") # blue
#' @param point_alpha Transparency for individual points (default: 0.6)
#' @param mean_lwd Line width for mean trend (default: 2)
#' @param cex Point size multiplier (default: 1)
#' @param las Orientation of axis labels (default: 1)
#' @param plot Logical, whether to create the plot (default: TRUE)
#'
#' @return Invisibly returns a list with summary statistics. Creates a base R plot.
#'
#' @examples
#' if(requireNamespace("AER", quietly = TRUE)) {
#'   data("Fatalities", package = "AER")
#'   plot_heterogeneity(Fatalities, fatalrate, year)
#'   plot_heterogeneity(Fatalities, fatalrate, state)
#' }
#'
#' @export
plot_heterogeneity <- function(
  data,
  varname,
  by,
  xlab = NULL,
  ylab = NULL,
  point_col = "gray50",
  mean_col = "#0072B2",
  point_alpha = 0.6,
  mean_lwd = 2,
  cex = 1,
  las = 1,
  plot = TRUE
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (nrow(data) == 0) {
    stop("'data' must have at least one row")
  }

  # Convert unquoted variables to strings if necessary
  varname_str <- if (is.character(substitute(varname))) {
    varname
  } else {
    deparse(substitute(varname))
  }

  by_str <- if (is.character(substitute(by))) {
    by
  } else {
    deparse(substitute(by))
  }

  # Check if variables exist in data
  if (!varname_str %in% names(data)) {
    stop("Variable '", varname_str, "' not found in data")
  }

  if (!by_str %in% names(data)) {
    stop("Variable '", by_str, "' not found in data")
  }

  # Extract variables
  x_var <- data[[by_str]]
  y_var <- data[[varname_str]]

  # Check variable types
  if (!is.numeric(y_var)) {
    stop("'varname' must be a numeric variable")
  }

  if (!is.factor(x_var) && !is.character(x_var) && !is.numeric(x_var)) {
    stop("'by' must be a factor, character, or numeric variable")
  }

  # Convert grouping variable to factor for consistent handling
  if (!is.factor(x_var)) {
    x_var <- as.factor(x_var)
  }

  # Calculate summary statistics
  group_means <- tapply(y_var, x_var, mean, na.rm = TRUE)
  group_sd <- tapply(y_var, x_var, sd, na.rm = TRUE)
  group_n <- tapply(y_var, x_var, function(x) sum(!is.na(x)))

  summary_stats <- list(
    means = group_means,
    sd = group_sd,
    n = group_n,
    overall_mean = mean(y_var, na.rm = TRUE),
    overall_sd = sd(y_var, na.rm = TRUE)
  )

  if (plot) {
    # Set default labels if not provided
    if (is.null(xlab)) {
      xlab <- by_str
    }
    if (is.null(ylab)) {
      ylab <- varname_str
    }

    # Set up plot parameters
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    par(mar = c(5, 4, 2, 2) + 0.1, las = las) # Reduced top margin from 4 to 2

    # Create color with alpha
    point_col_rgb <- col2rgb(point_col) / 255
    point_col_alpha <- rgb(
      point_col_rgb[1],
      point_col_rgb[2],
      point_col_rgb[3],
      alpha = point_alpha
    )

    # Create the plot without title
    plot(
      NA,
      xlim = c(0.5, length(levels(x_var)) + 0.5),
      ylim = range(y_var, na.rm = TRUE),
      xlab = xlab,
      ylab = ylab,
      main = "", # Empty title
      xaxt = "n",
      frame.plot = FALSE
    )

    # Add x-axis
    axis(1, at = seq_along(levels(x_var)), labels = levels(x_var))

    # Add individual points with jitter
    x_jitter <- jitter(as.numeric(x_var), amount = 0.2)
    points(x_jitter, y_var, col = point_col_alpha, pch = 16, cex = 0.8 * cex)

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

    # Add legend (without sample size)
    legend(
      "topright",
      legend = c("Individual observations", "Group means"),
      col = c(point_col, mean_col),
      pch = c(16, 18),
      lty = c(NA, 1),
      pt.cex = c(0.8, 1.5),
      bty = "n"
    )
  }

  # Return summary statistics invisibly
  invisible(summary_stats)
}
