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

# Cache for GSHHG coastline polygons (loaded once per session)
.coast_cache <- new.env(parent = emptyenv())

# Cache for water masks (keyed by grid extent string)
.mask_cache <- new.env(parent = emptyenv())

#' Create a water mask using bundled GSHHG full-resolution coastline
#'
#' Uses the pre-cropped Brazilian coast from GSHHG (Global Self-consistent,
#' Hierarchical, High-resolution Geography Database) at ~130m resolution.
#' Properly handles bays, estuaries, and islands.
#'
#' @param grid_lons,grid_lats Numeric vectors defining the output grid axes.
#' @param area Character: bay area name (for cache key only).
#' @return Logical matrix (nlon x nlat): TRUE = water, FALSE = land.
#' @noRd
.water_mask <- function(grid_lons, grid_lats, area = NULL) {
  n_lon <- length(grid_lons)
  n_lat <- length(grid_lats)

  cache_key <- paste(area, round(grid_lons[1], 5), round(grid_lons[n_lon], 5),
                     round(grid_lats[1], 5), round(grid_lats[n_lat], 5),
                     n_lon, n_lat, sep = "_")
  if (exists(cache_key, envir = .mask_cache)) {
    return(get(cache_key, envir = .mask_cache))
  }

  pkgs_ok <- requireNamespace("terra", quietly = TRUE) &&
             requireNamespace("sf", quietly = TRUE)

  if (!pkgs_ok) {
    mask <- matrix(TRUE, nrow = n_lon, ncol = n_lat)
    assign(cache_key, mask, envir = .mask_cache)
    return(mask)
  }

  # Load bundled GSHHG coastline (cached per session)
  if (!exists("coast", envir = .coast_cache)) {
    gpkg <- system.file("extdata", "gshhg_brazil_coast.gpkg", package = "rsiscorar")
    if (!nzchar(gpkg)) {
      mask <- matrix(TRUE, nrow = n_lon, ncol = n_lat)
      assign(cache_key, mask, envir = .mask_cache)
      return(mask)
    }
    coast_sf <- tryCatch({
      sf::sf_use_s2(FALSE)
      sf::st_read(gpkg, quiet = TRUE)
    }, error = function(e) NULL)

    if (is.null(coast_sf)) {
      mask <- matrix(TRUE, nrow = n_lon, ncol = n_lat)
      assign(cache_key, mask, envir = .mask_cache)
      return(mask)
    }
    assign("coast", coast_sf, envir = .coast_cache)
  }
  coast_sf <- get("coast", envir = .coast_cache)

  # Crop to grid extent
  land_crop <- tryCatch({
    bbox <- sf::st_bbox(
      c(xmin = grid_lons[1], xmax = grid_lons[n_lon],
        ymin = grid_lats[1], ymax = grid_lats[n_lat]),
      crs = sf::st_crs(coast_sf)
    )
    sf::st_crop(coast_sf, bbox)
  }, error = function(e) NULL)

  if (is.null(land_crop) || nrow(land_crop) == 0L) {
    mask <- matrix(TRUE, nrow = n_lon, ncol = n_lat)
    assign(cache_key, mask, envir = .mask_cache)
    return(mask)
  }

  res_lon <- if (n_lon > 1L) grid_lons[2] - grid_lons[1] else 0.005
  res_lat <- if (n_lat > 1L) grid_lats[2] - grid_lats[1] else 0.005
  r <- terra::rast(
    xmin = grid_lons[1] - res_lon / 2,
    xmax = grid_lons[n_lon] + res_lon / 2,
    ymin = grid_lats[1] - res_lat / 2,
    ymax = grid_lats[n_lat] + res_lat / 2,
    nrows = n_lat, ncols = n_lon,
    crs = terra::crs(terra::vect(land_crop))
  )

  land_rast <- terra::rasterize(terra::vect(land_crop), r, field = 1, background = 0)
  land_mat <- terra::as.matrix(land_rast, wide = TRUE)

  mask <- t(land_mat) == 0
  if (n_lat > 1L && grid_lats[2] > grid_lats[1]) {
    mask <- mask[, rev(seq_len(n_lat)), drop = FALSE]
  }

  assign(cache_key, mask, envir = .mask_cache)
  mask
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
.interp_to_grid <- function(lon, lat, values, grid_lons, grid_lats, area = NULL) {
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
    if (!is.null(result)) {
      if (!is.null(area)) {
        result$z[!.water_mask(grid_lons, grid_lats, area)] <- NA
      }
      return(result$z)
    }
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
  if (!is.null(area)) {
    mat[!.water_mask(grid_lons, grid_lats, area)] <- NA
  }
  mat
}
