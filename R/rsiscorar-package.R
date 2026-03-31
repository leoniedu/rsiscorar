#' rsiscorar: Tidal Current Prediction for Brazilian Coastal Bays
#'
#' Predicts ocean currents for 5 Brazilian coastal bays using tidal harmonic
#' analysis. SISCORAR is the Brazilian Navy's (DHN/REMO) adaptation of
#' [ADCIRC](https://adcirc.org) (ADvanced CIRCulation Model).
#'
#' ## Architecture
#'
#' ADCIRC uses two separate grids:
#'
#' - **Computational mesh**: full ADCIRC domain including open-ocean boundary
#'   conditions (e.g., 16,905 nodes for Guanabara, 13,517 for Sepetiba).
#'   U.bin/V.bin store harmonic constants (13 constituents) only for nodes
#'   within the output extent (~3,000–13,000 per bay depending on domain size).
#' - **Output grid** (~90,000–290,000 regular lat/lon nodes per bay): produced
#'   by the exe by interpolating from mesh nodes using pre-computed barycentric
#'   weights stored in Interp.bin. Only accessible via the Wine/exe path.
#'
#' ## Prediction paths
#'
#' - [predict_currents()]: pure R harmonic summation at mesh nodes. No Wine or
#'   SISCORAR installation required. Uses 13 independently fitted constituents.
#'   V0+u (astronomical argument) agrees with exe output to <0.15° across a
#'   40-year span.
#' - [run_prediction()] + [read_predictions()]: shells out to the SISCORAR
#'   Windows executable (via Wine on macOS/Linux). Produces 142-constituent
#'   output (13 fitted + 129 inferred) on the dense regular output grid.
#'
#' ## Constituents
#'
#' The 13 fitted constituents (O1, S2, K1, N2, MN4, Q1, K2, M2, P1, M4, MS4,
#' M6, M8) cover ~95% of tidal energy. They were resolved from a 1-year ADCIRC
#' simulation (2017) analyzed with t_tide and validated against ADCP field
#' measurements. The exe synthesizes the remaining 129 inference constituents
#' from the 13 using fixed astronomical ratios (ConsNovas.txt); they contribute
#' roughly 1-3% of velocity magnitude.
#'
#' ## License note
#'
#' rsiscorar's R code is MIT-licensed. It is a clean-room reimplementation:
#' harmonic algorithms use Schureman (1958) formulas (public domain) and no
#' ADCIRC source code is incorporated. SISCORAR data files (U.bin, V.bin,
#' Interp.bin) are distributed by the Brazilian Navy (DHN/REMO) for free
#' scientific and maritime use. ADCIRC is LGPL-3.0 licensed.
#'
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
      "rsiscorar: data files not found. Run siscorar_download_data() to get started."
    )
  }
}
