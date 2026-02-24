#' Validate area name
#'
#' @param area Character: area name to validate.
#' @param call Calling environment for error messages.
#' @return Character: lowercased area name (invisibly).
#' @noRd
.validate_area <- function(area, call = rlang::caller_env()) {
  area <- tolower(area)
  if (!area %in% SISCORAR_AREAS) {
    cli::cli_abort(
      c("Invalid area: {.val {area}}.",
        "i" = "Must be one of: {.val {SISCORAR_AREAS}}."),
      call = call
    )
  }
  invisible(area)
}

#' Check that ncdf4 is available
#' @noRd
.check_ncdf4 <- function(call = rlang::caller_env()) {
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    cli::cli_abort(
      c("Package {.pkg ncdf4} required for this operation.",
        "i" = "Install with: {.code install.packages(\"ncdf4\")}"),
      call = call
    )
  }
}

#' Check that CDO is available
#' @noRd
.check_cdo <- function(call = rlang::caller_env()) {
  if (system2("which", "cdo", stdout = FALSE, stderr = FALSE) != 0L) {
    cli::cli_abort(
      c("CDO (Climate Data Operators) required for GRIB2 export.",
        "i" = "Install with: {.code brew install cdo eccodes}"),
      call = call
    )
  }
}

#' Check that grib_set is available
#' @noRd
.check_grib_set <- function() {
  system2("which", "grib_set", stdout = FALSE, stderr = FALSE) == 0L
}
