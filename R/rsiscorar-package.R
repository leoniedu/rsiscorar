#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom rlang caller_env
#' @importFrom stats median
NULL

utils::globalVariables(c(
  ".", ".N", ".SD",
  "hour", "lon", "lat", "velocity_cm_s", "direction_deg",
  "u_velocity", "v_velocity", "speed_m_s",
  "col", "row", "dist", "date",
  "lon_idx", "lat_idx", "flag",
  "i_lon", "i_lat", "speed", "direction", "u", "v",
  "frequency_deg_hour", "frequency_scaled"
))

.onAttach <- function(libname, pkgname) {
  home <- tryCatch(siscorar_home(), error = function(e) NULL)
  if (is.null(home)) {
    packageStartupMessage(
      "rsiscorar: SISCORAR installation not found. Run siscorar_sitrep() for setup help."
    )
  }
}
