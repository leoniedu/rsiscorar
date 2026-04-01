#' Write Predictions to UV GeoJSON File
#'
#' Exports tidal current predictions to a compact GeoJSON FeatureCollection
#' suitable for consumption by web/native clients. Each feature represents one
#' grid point and carries 24-hour arrays of U, V, speed, and direction.
#'
#' @param dt data.table: prediction data from [predict_currents()] with columns
#'   `lat`, `lon`, `hour`, `u_velocity`, `v_velocity` (all in cm/s).
#' @param output_file Character: output file path (should end in `.geojson`).
#' @param resolution Numeric: grid resolution in degrees for downsampling.
#'   Default: `0.005` (~500 m at Baia de Todos os Santos).
#' @param hours Integer vector: which hours (UTC) to include. Default: `0:23`.
#' @param area Character: area name included in `meta`. Default: `NULL`
#'   (omitted from meta).
#'
#' @return Invisibly returns the output file path.
#'
#' @details
#' The function downsamples the prediction grid to `resolution` degrees by
#' rounding coordinates and averaging U and V within each cell per hour.
#' Speed (`s`, m/s) and direction (`d`, degrees clockwise from North,
#' oceanographic convention) are derived from the averaged U/V.
#'
#' Output GeoJSON structure:
#' ```json
#' {
#'   "type": "FeatureCollection",
#'   "meta": {"date": "2025-02-25", "resolution": 0.005},
#'   "hours_utc": [0, 1, ..., 23],
#'   "features": [{
#'     "type": "Feature",
#'     "geometry": {"type": "Point", "coordinates": [lon, lat]},
#'     "properties": {
#'       "u": [...],
#'       "v": [...],
#'       "s": [...],
#'       "d": [...]
#'     }
#'   }]
#' }
#' ```
#'
#' Requires the `jsonlite` package.
#'
#' @examples
#' \dontrun{
#' dt <- predict_currents("2025-06-15", "baiatos")
#' write_uv_geojson(dt, "baiatos_20250615_005.geojson", area = "baiatos")
#' }
#'
#' @seealso [write_grib()], [predict_currents()]
#' @export
write_uv_geojson <- function(dt, output_file, resolution = 0.005,
                              hours = 0:23, area = NULL) {
  rlang::check_installed("jsonlite")

  date_val <- as.Date(dt$datetime[1])

  dt_sub <- dt[hour %in% hours]

  lon_range <- range(dt_sub$lon)
  lat_range <- range(dt_sub$lat)
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

  # Interpolate u and v per hour, collect into long-form data.table
  grid_list <- vector("list", length(hours))
  for (h_idx in seq_along(hours)) {
    h <- hours[h_idx]
    dt_h <- dt_sub[hour == h]
    u_mat <- .interp_to_grid(dt_h$lon, dt_h$lat, dt_h$u_velocity / 100,
                             grid_lons, grid_lats)
    v_mat <- .interp_to_grid(dt_h$lon, dt_h$lat, dt_h$v_velocity / 100,
                             grid_lons, grid_lats)
    # Expand matrix to rows (lon varies fastest)
    idx <- which(!is.na(u_mat), arr.ind = TRUE)
    if (nrow(idx) > 0L) {
      grid_list[[h_idx]] <- data.table(
        lon_r = grid_lons[idx[, 1]],
        lat_r = grid_lats[idx[, 2]],
        hour = h,
        u = u_mat[idx],
        v = v_mat[idx]
      )
    }
  }
  grid <- rbindlist(grid_list)

  grid[, `:=`(
    s = sqrt(u^2 + v^2),
    d = (atan2(u, v) * 180 / pi) %% 360
  )]

  n_pts <- uniqueN(grid, by = c("lon_r", "lat_r"))
  cli::cli_inform(
    "Building GeoJSON: {.val {n_pts}} grid points x {.val {length(hours)}} hours"
  )

  grid_list <- split(grid, by = c("lon_r", "lat_r"), sorted = TRUE)

  features <- lapply(grid_list, function(pg) {
    # Ordered by hours (NA rows inserted for any missing hours)
    pg_ord <- pg[match(hours, pg$hour)]
    list(
      type     = "Feature",
      geometry = list(
        type        = "Point",
        coordinates = c(pg$lon_r[1], pg$lat_r[1])
      ),
      properties = list(
        u = round(pg_ord$u, 4),
        v = round(pg_ord$v, 4),
        s = round(pg_ord$s, 4),
        d = round(pg_ord$d, 1)
      )
    )
  })
  # Remove names so jsonlite serialises as an array, not an object
  names(features) <- NULL

  meta <- list(date = format(date_val, "%Y-%m-%d"), resolution = resolution)
  if (!is.null(area)) meta$area <- area

  geojson <- list(
    type      = "FeatureCollection",
    meta      = meta,
    hours_utc = as.integer(hours),
    features  = features
  )

  jsonlite::write_json(geojson, output_file, auto_unbox = TRUE, digits = 5)

  size_kb <- round(file.info(output_file)$size / 1e3, 1)
  cli::cli_inform(
    "Written: {.path {output_file}} ({.val {size_kb}} KB, {.val {n_pts}} points)"
  )

  invisible(output_file)
}
