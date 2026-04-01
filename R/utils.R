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

#' Interpolate scattered points to a regular grid
#'
#' Uses Delaunay triangulation (interp package) when available, otherwise
#' falls back to nearest-cell snapping with averaging.
#'
#' @param lon,lat Numeric vectors of point coordinates.
#' @param values Numeric vector of values to interpolate.
#' @param grid_lons,grid_lats Numeric vectors defining the output grid axes.
#' @return Matrix of dimensions `length(grid_lons)` x `length(grid_lats)`.
#'   Cells outside the convex hull of the input points are `NA`.
#' @noRd
.interp_to_grid <- function(lon, lat, values, grid_lons, grid_lats) {
  if (requireNamespace("interp", quietly = TRUE)) {
    result <- tryCatch(
      interp::interp(
        x = lon, y = lat, z = values,
        xo = grid_lons, yo = grid_lats,
        linear = TRUE, extrap = FALSE,
        duplicate = "mean"
      ),
      error = function(e) NULL
    )
    if (!is.null(result)) return(result$z)
  } else {
    cli::cli_warn(c(
      "{.pkg interp} not installed; using nearest-cell snapping instead of Delaunay interpolation.",
      "i" = "Install with {.code install.packages('interp')} for smoother output."
    ), .frequency = "once", .frequency_id = "interp_missing")
  }

  # Fallback: snap to nearest cell and average
  lon_res <- if (length(grid_lons) > 1L) grid_lons[2] - grid_lons[1] else 1
  lat_res <- if (length(grid_lats) > 1L) grid_lats[2] - grid_lats[1] else 1
  n_lon <- length(grid_lons)
  n_lat <- length(grid_lats)

  lon_idx <- pmax(1L, pmin(n_lon, round((lon - grid_lons[1]) / lon_res) + 1L))
  lat_idx <- pmax(1L, pmin(n_lat, round((lat - grid_lats[1]) / lat_res) + 1L))

  mat <- matrix(NA_real_, nrow = n_lon, ncol = n_lat)
  counts <- matrix(0L, nrow = n_lon, ncol = n_lat)

  for (k in seq_along(values)) {
    i <- lon_idx[k]
    j <- lat_idx[k]
    if (is.na(mat[i, j])) {
      mat[i, j] <- values[k]
      counts[i, j] <- 1L
    } else {
      mat[i, j] <- mat[i, j] + values[k]
      counts[i, j] <- counts[i, j] + 1L
    }
  }
  mat[counts > 1L] <- mat[counts > 1L] / counts[counts > 1L]
  mat
}
