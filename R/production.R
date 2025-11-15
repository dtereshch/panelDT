#' Simulated Unbalanced Panel Data for Cobb-Douglas Production Function Estimation
#'
#' A simulated dataset containing firm-level panel data with entry, exit, and random missing values
#' for estimating Cobb-Douglas production functions. The data follows the structure:
#' Sales = A × Capital^α × Labor^β × exp(technology_shock), where α = 0.3 and β = 0.6.
#'
#' @format A data frame with 180 rows (30 firms × 6 years) and 5 variables:
#' \describe{
#'   \item{firm}{integer variable representing firm identifier (1 to 30)}
#'   \item{year}{integer variable representing year identifier (1 to 6)}
#'   \item{sales}{numeric variable representing firm sales/output, generated from
#'                Cobb-Douglas production function with technology shocks}
#'   \item{capital}{numeric variable representing capital input, following log-normal
#'                  distribution with firm-specific effects and time trend}
#'   \item{labor}{numeric variable representing labor input, following log-normal
#'                distribution with firm-specific effects and time trend}
#' }
#'
#' @details
#' The dataset exhibits several realistic features of firm-level panel data:
#' \itemize{
#'   \item 50\% of firms (15 firms) have complete data for all 6 years
#'   \item 50\% of firms (15 firms) have entry and exit patterns with different start and end years
#'   \item Additional random missing values (approx. 2\%) in sales, capital, and labor variables
#'   \item Firm-specific effects and time trends in capital and labor inputs
#'   \item Technology shocks affecting production output
#' }
#'
#' @source Simulated data for econometric analysis and demonstration purposes
#'
#' @examples
#' data(production)
#' head(production)
"production"
