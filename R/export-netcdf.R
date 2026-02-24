#' Write Predictions to NetCDF File
#'
#' Exports tidal current predictions to CF-compliant NetCDF format.
#'
#' @inheritParams write_grib
#'
#' @return Invisibly returns the output file path.
#'
#' @details
#' The output NetCDF file follows CF-1.6 conventions with:
#' - Standard names: `eastward_sea_water_velocity`, `northward_sea_water_velocity`
#' - Units: m/s
#' - Coordinates: longitude, latitude, time
#'
#' @examples
#' \dontrun{
#' dt <- predict_currents("2025-06-15", "guanabara")
#' write_netcdf(dt, "currents.nc", resolution = 0.005)
#' }
#'
#' @seealso [write_grib()]
#' @export
write_netcdf <- function(dt, output_file, hours = 0:23, resolution = NULL) {
  .check_ncdf4()

  date_val <- as.Date(dt$datetime[1])
  dt_subset <- dt[hour %in% hours]

  lon_range <- range(dt_subset$lon)
  lat_range <- range(dt_subset$lat)

  if (is.null(resolution)) {
    unique_lons <- sort(unique(round(dt_subset$lon, 4)))
    if (length(unique_lons) > 1) {
      resolution <- median(diff(unique_lons))
    } else {
      resolution <- 0.001
    }
  }

  cli::cli_inform("Grid resolution: {.val {round(resolution, 4)}} degrees")

  grid_lons <- seq(
    floor(lon_range[1] / resolution) * resolution,
    ceiling(lon_range[2] / resolution) * resolution,
    by = resolution
  )
  grid_lats <- seq(
    floor(lat_range[1] / resolution) * resolution,
    ceiling(lat_range[2] / resolution) * resolution,
    by = resolution
  )

  n_lon <- length(grid_lons)
  n_lat <- length(grid_lats)
  n_hours <- length(hours)

  cli::cli_inform(
    "Output grid: {.val {n_lon}} x {.val {n_lat}} x {.val {n_hours}} hours"
  )

  cli::cli_inform("Interpolating to regular grid...")

  u_array <- array(NA_real_, dim = c(n_lon, n_lat, n_hours))
  v_array <- array(NA_real_, dim = c(n_lon, n_lat, n_hours))

  for (h_idx in seq_along(hours)) {
    h <- hours[h_idx]
    dt_h <- dt_subset[hour == h, .(lon, lat, u_velocity, v_velocity)]

    dt_h[, `:=`(
      lon_idx = pmax(1L, pmin(n_lon, round((lon - grid_lons[1]) / resolution) + 1L)),
      lat_idx = pmax(1L, pmin(n_lat, round((lat - grid_lats[1]) / resolution) + 1L))
    )]

    grid_vals <- dt_h[, .(
      u = mean(u_velocity) / 100,
      v = mean(v_velocity) / 100
    ), by = .(lon_idx, lat_idx)]

    for (r in seq_len(nrow(grid_vals))) {
      u_array[grid_vals$lon_idx[r], grid_vals$lat_idx[r], h_idx] <- grid_vals$u[r]
      v_array[grid_vals$lon_idx[r], grid_vals$lat_idx[r], h_idx] <- grid_vals$v[r]
    }
  }

  fill_val <- -9999
  u_array[is.na(u_array)] <- fill_val
  v_array[is.na(v_array)] <- fill_val

  cli::cli_inform("Writing NetCDF file...")

  ref_time <- as.POSIXct(paste(date_val, "00:00:00"), tz = "UTC")

  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", grid_lons)
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", grid_lats)
  time_dim <- ncdf4::ncdim_def(
    "time",
    paste("hours since", format(ref_time, "%Y-%m-%d %H:%M:%S")),
    hours, unlim = FALSE
  )

  u_var <- ncdf4::ncvar_def("u_current", "m/s", list(lon_dim, lat_dim, time_dim),
                             fill_val, "Eastward Sea Water Velocity", prec = "float")
  v_var <- ncdf4::ncvar_def("v_current", "m/s", list(lon_dim, lat_dim, time_dim),
                             fill_val, "Northward Sea Water Velocity", prec = "float")

  nc <- ncdf4::nc_create(output_file, list(u_var, v_var))

  ncdf4::ncvar_put(nc, u_var, u_array)
  ncdf4::ncvar_put(nc, v_var, v_array)

  ncdf4::ncatt_put(nc, 0, "title", "SISCORAR Tidal Current Predictions")
  ncdf4::ncatt_put(nc, 0, "institution", "Brazilian Navy (DHN)")
  ncdf4::ncatt_put(nc, 0, "source", "SISCORAR harmonic tidal prediction")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.6")
  ncdf4::ncatt_put(nc, 0, "history", paste("Created", Sys.time()))

  ncdf4::ncatt_put(nc, "u_current", "standard_name", "eastward_sea_water_velocity")
  ncdf4::ncatt_put(nc, "v_current", "standard_name", "northward_sea_water_velocity")

  ncdf4::nc_close(nc)

  cli::cli_inform(
    "Written: {.path {output_file}} ({.val {round(file.info(output_file)$size / 1e3, 1)}} KB)"
  )
  invisible(output_file)
}
