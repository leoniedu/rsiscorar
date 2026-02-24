#' Get Current at a Specific Location
#'
#' Finds the nearest grid point to a given coordinate and returns its data.
#' Uses Euclidean distance in lon/lat space (suitable for nearby points).
#'
#' @param target_lon Numeric: target longitude (decimal degrees, negative for West).
#' @param target_lat Numeric: target latitude (decimal degrees, negative for South).
#' @param dt data.table: prediction data from [predict_currents()].
#' @param target_hour Integer: hour of day (0-23). Default: `0`.
#'
#' @return data.table with single row containing the nearest point's data.
#'
#' @examples
#' \dontrun{
#' dt <- predict_currents("2025-06-15", "guanabara")
#' result <- get_current_at_point(-43.12, -22.89, dt, target_hour = 12)
#' print(result)
#' }
#'
#' @export
get_current_at_point <- function(target_lon, target_lat, dt, target_hour = 0L) {
  dt_hour <- dt[hour == target_hour]
  distances <- (dt_hour$lon - target_lon)^2 + (dt_hour$lat - target_lat)^2
  dt_hour[which.min(distances)]
}
