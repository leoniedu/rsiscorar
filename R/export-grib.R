#' Write Predictions to GRIB2 File
#'
#' Exports tidal current predictions to GRIB2 format for use in marine
#' navigation software such as OpenCPN.
#'
#' @param dt data.table: prediction data from [predict_currents()].
#' @param output_file Character: output file path (should end in `.grib2`).
#' @param hours Integer vector: which hours to include. Default: `0:23` (all).
#' @param resolution Numeric: grid resolution in degrees. Default: auto-detect
#'   from data.
#'
#' @return Invisibly returns the output file path.
#'
#' @details
#' Requires external tools:
#' - CDO (Climate Data Operators): `brew install cdo`
#' - ecCodes: `brew install eccodes`
#' - R package ncdf4: `install.packages("ncdf4")`
#'
#' The output GRIB2 file contains U-component (eastward) and V-component
#' (northward) sea water velocity with GRIB2 discipline=10 (oceanographic),
#' category=1 (currents).
#'
#' @examples
#' \dontrun{
#' dt <- predict_currents("2025-06-15", "guanabara")
#' write_grib(dt, "guanabara_currents.grib2", hours = 0:23, resolution = 0.005)
#' }
#'
#' @seealso [write_netcdf()]
#' @export
write_grib <- function(dt, output_file, hours = 0:23, resolution = NULL) {
  .check_ncdf4()
  .check_cdo()
  grib_set_ok <- .check_grib_set()

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

  cli::cli_inform(
    "Output grid: {.val {n_lon}} x {.val {n_lat}} x {.val {length(hours)}} hours"
  )

  tmp_gribs <- character(0)

  for (h in hours) {
    dt_h <- dt_subset[hour == h, .(lon, lat, u_velocity, v_velocity)]

    dt_h[, `:=`(
      lon_idx = pmax(1L, pmin(n_lon, round((lon - grid_lons[1]) / resolution) + 1L)),
      lat_idx = pmax(1L, pmin(n_lat, round((lat - grid_lats[1]) / resolution) + 1L))
    )]

    grid_vals <- dt_h[, .(
      u = mean(u_velocity) / 100,
      v = mean(v_velocity) / 100
    ), by = .(lon_idx, lat_idx)]

    u_matrix <- matrix(NA_real_, nrow = n_lon, ncol = n_lat)
    v_matrix <- matrix(NA_real_, nrow = n_lon, ncol = n_lat)

    for (r in seq_len(nrow(grid_vals))) {
      u_matrix[grid_vals$lon_idx[r], grid_vals$lat_idx[r]] <- grid_vals$u[r]
      v_matrix[grid_vals$lon_idx[r], grid_vals$lat_idx[r]] <- grid_vals$v[r]
    }

    # Create temporary NetCDF
    nc_file <- tempfile(fileext = ".nc")
    on.exit(unlink(nc_file), add = TRUE)

    ref_time <- as.POSIXct(
      paste(date_val, sprintf("%02d:00:00", h)), tz = "UTC"
    )
    yr <- as.integer(format(ref_time, "%Y"))
    mo <- as.integer(format(ref_time, "%m"))
    dy <- as.integer(format(ref_time, "%d"))

    lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east", grid_lons)
    lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north", grid_lats)
    time_dim <- ncdf4::ncdim_def(
      "time",
      sprintf("hours since %s", format(ref_time, "%Y-%m-%d %H:%M:%S")),
      0, unlim = TRUE
    )

    fillvalue <- -9999.0
    u_var <- ncdf4::ncvar_def("uo", "m s-1", list(lon_dim, lat_dim, time_dim),
                               fillvalue, "Eastward Sea Water Velocity", prec = "float")
    v_var <- ncdf4::ncvar_def("vo", "m s-1", list(lon_dim, lat_dim, time_dim),
                               fillvalue, "Northward Sea Water Velocity", prec = "float")

    nc <- ncdf4::nc_create(nc_file, list(u_var, v_var))

    ncdf4::ncatt_put(nc, "uo", "standard_name", "eastward_sea_water_velocity")
    ncdf4::ncatt_put(nc, "vo", "standard_name", "northward_sea_water_velocity")
    ncdf4::ncatt_put(nc, "longitude", "standard_name", "longitude")
    ncdf4::ncatt_put(nc, "longitude", "axis", "X")
    ncdf4::ncatt_put(nc, "latitude", "standard_name", "latitude")
    ncdf4::ncatt_put(nc, "latitude", "axis", "Y")
    ncdf4::ncatt_put(nc, "time", "standard_name", "time")
    ncdf4::ncatt_put(nc, "time", "axis", "T")
    ncdf4::ncatt_put(nc, "time", "calendar", "standard")
    ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8")
    ncdf4::ncatt_put(nc, 0, "title", "SISCORAR Tidal Current Predictions")
    ncdf4::ncatt_put(nc, 0, "institution", "Brazilian Navy (DHN)")

    ncdf4::ncvar_put(nc, u_var, array(u_matrix, dim = c(n_lon, n_lat, 1)))
    ncdf4::ncvar_put(nc, v_var, array(v_matrix, dim = c(n_lon, n_lat, 1)))
    ncdf4::nc_close(nc)

    # Convert to GRIB2 with CDO
    tmp_grib <- tempfile(fileext = ".grib2")
    system2("cdo", c("-s", "-f", "grb2", "copy", nc_file, tmp_grib),
            stdout = FALSE, stderr = FALSE)

    if (file.exists(tmp_grib) && file.size(tmp_grib) > 0 && grib_set_ok) {
      u_tmp <- tempfile(fileext = ".grib2")
      v_tmp <- tempfile(fileext = ".grib2")
      u_fixed <- paste0(u_tmp, ".fixed")
      v_fixed <- paste0(v_tmp, ".fixed")

      system2("grib_copy", c("-w", "count=1", tmp_grib, u_tmp),
              stdout = FALSE, stderr = FALSE)
      system2("grib_set", c(
        "-s", sprintf(
          paste0("discipline=10,parameterCategory=1,parameterNumber=2,",
                 "dataDate=%04d%02d%02d,dataTime=%02d00,",
                 "centre=98,tablesVersion=28"),
          yr, mo, dy, h
        ),
        u_tmp, u_fixed
      ), stdout = FALSE, stderr = FALSE)

      system2("grib_copy", c("-w", "count=2", tmp_grib, v_tmp),
              stdout = FALSE, stderr = FALSE)
      system2("grib_set", c(
        "-s", sprintf(
          paste0("discipline=10,parameterCategory=1,parameterNumber=3,",
                 "dataDate=%04d%02d%02d,dataTime=%02d00,",
                 "centre=98,tablesVersion=28"),
          yr, mo, dy, h
        ),
        v_tmp, v_fixed
      ), stdout = FALSE, stderr = FALSE)

      hour_grib <- tempfile(fileext = ".grib2")
      system2("cat", c(u_fixed, v_fixed), stdout = hour_grib, stderr = FALSE)
      tmp_gribs <- c(tmp_gribs, hour_grib)

      unlink(c(u_tmp, v_tmp, u_fixed, v_fixed, tmp_grib))
    } else if (file.exists(tmp_grib)) {
      tmp_gribs <- c(tmp_gribs, tmp_grib)
    }

    unlink(nc_file)
  }

  # Concatenate all hours
  if (length(tmp_gribs) > 0) {
    system2("cat", tmp_gribs, stdout = output_file, stderr = FALSE)
    unlink(tmp_gribs)
  }

  if (file.exists(output_file)) {
    cli::cli_inform(
      "Written: {.path {output_file}} ({.val {round(file.info(output_file)$size / 1e3, 1)}} KB)"
    )
  } else {
    cli::cli_warn("GRIB2 creation failed.")
  }

  invisible(output_file)
}

#' Convert SISCORAR Text File to GRIB2
#'
#' Converts SISCORAR text files (idx, lat, lon, speed, direction format)
#' to GRIB2 format. The text files use the format produced by the SISCORAR
#' GUI application.
#'
#' @param input_file Path to SISCORAR text file.
#' @param output_file Output GRIB2 path. If `NULL`, derived from input filename.
#' @param resolution Grid resolution in degrees. Default: `0.001` (~111m).
#'
#' @return Invisibly returns a list with conversion metadata.
#'
#' @details
#' Expects tab-separated text files with columns: idx, lat, lon, speed, direction.
#' Filenames should follow the `YYYYMMDDHH.txt` pattern for automatic datetime
#' extraction. Times are assumed to be in America/Recife timezone (UTC-3).
#'
#' @export
siscorar_to_grib <- function(input_file, output_file = NULL, resolution = 0.001) {
  .check_ncdf4()
  .check_cdo()
  grib_set_ok <- .check_grib_set()

  if (is.null(output_file)) {
    output_file <- sub("\\.txt$", ".grib2", input_file)
  }

  # Extract datetime from filename (YYYYMMDDHH.txt)
  basename_no_ext <- tools::file_path_sans_ext(basename(input_file))
  if (nchar(basename_no_ext) == 10L && grepl("^\\d{10}$", basename_no_ext)) {
    local_time <- as.POSIXct(
      sprintf(
        "%s-%s-%s %s:00:00",
        substr(basename_no_ext, 1, 4),
        substr(basename_no_ext, 5, 6),
        substr(basename_no_ext, 7, 8),
        substr(basename_no_ext, 9, 10)
      ),
      tz = "America/Recife"
    )
    ref_time <- as.POSIXct(format(local_time, tz = "UTC"), tz = "UTC")
    cli::cli_inform("Local time (Recife): {format(local_time, '%Y-%m-%d %H:%M %Z')}")
  } else {
    cli::cli_warn("Could not parse datetime from filename, using current time.")
    ref_time <- Sys.time()
  }

  yr <- as.integer(format(ref_time, "%Y"))
  mo <- as.integer(format(ref_time, "%m"))
  dy <- as.integer(format(ref_time, "%d"))
  hr <- as.integer(format(ref_time, "%H"))

  cli::cli_inform("Processing: {.path {input_file}}")
  cli::cli_inform("Reference time: {format(ref_time, '%Y-%m-%d %H:%M')} UTC")

  dat <- fread(
    input_file,
    col.names = c("idx", "lat", "lon", "speed", "direction"),
    colClasses = c("integer", "numeric", "numeric", "numeric", "numeric")
  )

  cli::cli_inform("  Points: {.val {nrow(dat)}}")

  # Convert speed/direction to U/V
  dat[, `:=`(
    u = speed * sin(direction * pi / 180),
    v = speed * cos(direction * pi / 180)
  )]

  # Create regular grid
  lon_min <- floor(min(dat$lon) / resolution) * resolution
  lon_max <- ceiling(max(dat$lon) / resolution) * resolution
  lat_min <- floor(min(dat$lat) / resolution) * resolution
  lat_max <- ceiling(max(dat$lat) / resolution) * resolution

  lons <- seq(lon_min, lon_max, by = resolution)
  lats <- seq(lat_min, lat_max, by = resolution)
  nlon <- length(lons)
  nlat <- length(lats)

  u_matrix <- matrix(NA_real_, nrow = nlon, ncol = nlat)
  v_matrix <- matrix(NA_real_, nrow = nlon, ncol = nlat)

  dat[, `:=`(
    i_lon = round((lon - lon_min) / resolution) + 1L,
    i_lat = round((lat - lat_min) / resolution) + 1L
  )]

  for (k in seq_len(nrow(dat))) {
    i <- dat$i_lon[k]
    j <- dat$i_lat[k]
    if (i >= 1L && i <= nlon && j >= 1L && j <= nlat) {
      u_matrix[i, j] <- dat$u[k]
      v_matrix[i, j] <- dat$v[k]
    }
  }

  n_valid <- sum(!is.na(u_matrix))
  cli::cli_inform("  Grid: {.val {nlon}} x {.val {nlat}}, {.val {n_valid}} valid cells")

  # Write CF-compliant NetCDF
  nc_file <- tempfile(fileext = ".nc")
  on.exit(unlink(nc_file), add = TRUE)

  lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east", lons)
  lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north", lats)
  time_dim <- ncdf4::ncdim_def(
    "time",
    sprintf("hours since %s", format(ref_time, "%Y-%m-%d %H:%M:%S")),
    0, unlim = TRUE
  )

  fillvalue <- -9999.0
  u_var <- ncdf4::ncvar_def("uo", "m s-1", list(lon_dim, lat_dim, time_dim),
                             fillvalue, "Eastward Sea Water Velocity", prec = "float")
  v_var <- ncdf4::ncvar_def("vo", "m s-1", list(lon_dim, lat_dim, time_dim),
                             fillvalue, "Northward Sea Water Velocity", prec = "float")

  nc <- ncdf4::nc_create(nc_file, list(u_var, v_var))
  ncdf4::ncatt_put(nc, "uo", "standard_name", "eastward_sea_water_velocity")
  ncdf4::ncatt_put(nc, "vo", "standard_name", "northward_sea_water_velocity")
  ncdf4::ncatt_put(nc, "longitude", "standard_name", "longitude")
  ncdf4::ncatt_put(nc, "longitude", "axis", "X")
  ncdf4::ncatt_put(nc, "latitude", "standard_name", "latitude")
  ncdf4::ncatt_put(nc, "latitude", "axis", "Y")
  ncdf4::ncatt_put(nc, "time", "standard_name", "time")
  ncdf4::ncatt_put(nc, "time", "axis", "T")
  ncdf4::ncatt_put(nc, "time", "calendar", "standard")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncdf4::ncatt_put(nc, 0, "title", "SISCORAR Tidal Current Predictions")
  ncdf4::ncatt_put(nc, 0, "institution", "Brazilian Navy (DHN)")
  ncdf4::ncatt_put(nc, 0, "source", basename(input_file))

  ncdf4::ncvar_put(nc, u_var, array(u_matrix, dim = c(nlon, nlat, 1)))
  ncdf4::ncvar_put(nc, v_var, array(v_matrix, dim = c(nlon, nlat, 1)))
  ncdf4::nc_close(nc)

  # Convert to GRIB2
  tmp_grib <- tempfile(fileext = ".grib2")

  system2("cdo", c("-s", "-f", "grb2", "copy", nc_file, tmp_grib),
          stdout = FALSE, stderr = FALSE)

  if (file.exists(tmp_grib) && file.size(tmp_grib) > 0 && grib_set_ok) {
    u_tmp <- tempfile(fileext = ".grib2")
    v_tmp <- tempfile(fileext = ".grib2")
    u_fixed <- paste0(u_tmp, ".fixed")
    v_fixed <- paste0(v_tmp, ".fixed")

    system2("grib_copy", c("-w", "count=1", tmp_grib, u_tmp),
            stdout = FALSE, stderr = FALSE)
    system2("grib_set", c(
      "-s", sprintf(
        paste0("discipline=10,parameterCategory=1,parameterNumber=2,",
               "dataDate=%04d%02d%02d,dataTime=%02d00,",
               "centre=98,tablesVersion=28"),
        yr, mo, dy, hr
      ),
      u_tmp, u_fixed
    ), stdout = FALSE, stderr = FALSE)

    system2("grib_copy", c("-w", "count=2", tmp_grib, v_tmp),
            stdout = FALSE, stderr = FALSE)
    system2("grib_set", c(
      "-s", sprintf(
        paste0("discipline=10,parameterCategory=1,parameterNumber=3,",
               "dataDate=%04d%02d%02d,dataTime=%02d00,",
               "centre=98,tablesVersion=28"),
        yr, mo, dy, hr
      ),
      v_tmp, v_fixed
    ), stdout = FALSE, stderr = FALSE)

    system2("cat", c(u_fixed, v_fixed), stdout = output_file, stderr = FALSE)
    unlink(c(u_tmp, v_tmp, u_fixed, v_fixed, tmp_grib))

    if (!file.exists(output_file) || file.size(output_file) == 0) {
      file.rename(tmp_grib, output_file)
    }
  } else if (file.exists(tmp_grib) && file.size(tmp_grib) > 0) {
    file.rename(tmp_grib, output_file)
  } else {
    # Fall back to NetCDF
    output_file <- sub("\\.grib2$", ".nc", output_file)
    file.copy(nc_file, output_file, overwrite = TRUE)
    cli::cli_warn("CDO conversion failed. Saved as NetCDF: {.path {output_file}}")
  }

  unlink(nc_file)

  if (file.exists(output_file)) {
    cli::cli_inform("Output: {.path {output_file}}")
  }

  invisible(list(
    input = input_file,
    output = output_file,
    ref_time = ref_time,
    n_points = nrow(dat),
    grid_dims = c(nlon, nlat),
    resolution = resolution,
    valid_cells = n_valid
  ))
}

#' Get GRIB2 Tidal Current Forecast
#'
#' Downloads a cached GRIB2 file from a GitHub release or generates one locally.
#' Returns the loaded raster via [terra::rast()].
#'
#' @param date Date object or `"YYYY-MM-DD"` string.
#' @param area Character: bay name. Default: `"guanabara"`.
#' @param resolution Numeric: grid resolution in degrees. Default: `0.001`.
#' @param repo Character: GitHub repository for cached GRIBs
#'   (e.g., `"leoniedu/siscorar_gribs"`). Set to `NULL` to skip GitHub.
#' @param fallback Logical: if `TRUE` (default), generates the GRIB locally
#'   when not available on GitHub (requires Wine + SISCORAR). If `FALSE`,
#'   fails with an error when the GRIB is not cached.
#' @param dest Character: local cache directory. Default uses
#'   [tools::R_user_dir()].
#'
#' @return A `terra::SpatRaster` with U and V sea water velocity layers.
#'
#' @details
#' The function tries three sources in order:
#' 1. Local cache (`dest` directory)
#' 2. GitHub release (via [piggyback::pb_download()])
#' 3. Local generation via [predict_currents()] + [write_grib()]
#'    (only when `fallback = TRUE`)
#'
#' @examples
#' \dontrun{
#' # Download from GitHub (no Wine/SISCORAR needed)
#' grib <- get_grib("2025-07-01", "baiatos", fallback = FALSE)
#' terra::plot(grib)
#'
#' # Generate locally if not cached
#' grib <- get_grib(Sys.Date(), "guanabara")
#' }
#'
#' @seealso [write_grib()], [predict_currents()]
#' @export
get_grib <- function(date, area = "guanabara", resolution = 0.001,
                     repo = "leoniedu/siscorar_gribs",
                     fallback = TRUE, dest = NULL) {
  rlang::check_installed("terra")

  date <- as.Date(date)
  area <- .validate_area(area)
  resolution_str <- sprintf("%03d", resolution * 1000)
  release_tag <- resolution_str
  filename <- sprintf(
    "%s_%s_%s.grib2", area, format(date, "%Y%m%d"), resolution_str
  )

  if (is.null(dest)) {
    dest <- file.path(tools::R_user_dir("rsiscorar", "cache"), "gribs")
  }
  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }

  local_path <- file.path(dest, filename)

  # 1. Local cache

  if (file.exists(local_path)) {
    cli::cli_inform("Using cached: {.path {local_path}}")
    return(terra::rast(local_path))
  }

  # 2. GitHub release

  if (!is.null(repo) && requireNamespace("piggyback", quietly = TRUE)) {
    downloaded <- tryCatch(
      {
        existing <- piggyback::pb_list(repo = repo, tag = release_tag)
        if (filename %in% existing$file_name) {
          cli::cli_inform("Downloading {.file {filename}} from GitHub...")
          piggyback::pb_download(
            filename,
            dest = dest,
            repo = repo,
            tag = release_tag,
            overwrite = TRUE
          )
          file.exists(local_path)
        } else {
          FALSE
        }
      },
      error = function(e) {
        cli::cli_warn("GitHub download failed: {e$message}")
        FALSE
      }
    )
    if (isTRUE(downloaded)) {
      return(terra::rast(local_path))
    }
  }

  # 3. Local generation
  if (!fallback) {
    cli::cli_abort(c(
      "GRIB not available for {.val {area}} on {.val {date}}.",
      "i" = "Set {.arg fallback = TRUE} to generate locally."
    ))
  }

  cli::cli_inform("GRIB not cached, generating locally...")
  dt <- predict_currents(date, area)
  if (nrow(dt) == 0L) {
    cli::cli_abort(
      "Prediction returned no data for {.val {area}} on {.val {date}}."
    )
  }

  write_grib(dt, local_path, hours = 0:23, resolution = resolution)

  if (!file.exists(local_path)) {
    cli::cli_abort("Failed to generate GRIB file.")
  }

  terra::rast(local_path)
}

#' Report GRIB cache status
#'
#' Lists cached GRIB2 files with their sizes and modification dates.
#'
#' @param dest Character: cache directory. Default uses
#'   `tools::R_user_dir("rsiscorar", "cache")`.
#' @return A data frame with columns `file`, `size_mb`, `modified`, invisibly.
#' @export
grib_cache_info <- function(dest = NULL) {
  if (is.null(dest)) {
    dest <- file.path(tools::R_user_dir("rsiscorar", "cache"), "gribs")
  }
  if (!dir.exists(dest)) {
    cli::cli_inform("No GRIB cache directory found at {.path {dest}}")
    return(invisible(data.frame(
      file = character(), size_mb = numeric(),
      modified = as.POSIXct(character()),
      stringsAsFactors = FALSE
    )))
  }
  files <- list.files(dest, pattern = "\\.grib2$", full.names = TRUE)
  if (length(files) == 0L) {
    cli::cli_inform("GRIB cache is empty.")
    return(invisible(data.frame(
      file = character(), size_mb = numeric(),
      modified = as.POSIXct(character()),
      stringsAsFactors = FALSE
    )))
  }
  info <- file.info(files)
  result <- data.frame(
    file     = basename(files),
    size_mb  = round(info$size / 1e6, 2),
    modified = info$mtime,
    stringsAsFactors = FALSE
  )
  total_mb <- sum(result$size_mb)
  cli::cli_inform(
    "GRIB cache: {.val {nrow(result)}} file{?s}, {.val {round(total_mb, 1)}} MB total"
  )
  cli::cli_inform("Location: {.path {dest}}")
  invisible(result)
}

#' Clear the local GRIB cache
#'
#' Removes cached GRIB2 files, optionally filtered by area and/or date.
#'
#' @param area Character area name to filter (e.g. `"baiatos"`). `NULL`
#'   (default) matches all areas.
#' @param date Date or character date to filter. `NULL` (default) matches all.
#' @param dest Character: cache directory. Default uses
#'   `tools::R_user_dir("rsiscorar", "cache")`.
#' @return Invisible character vector of removed file paths.
#' @export
grib_clear_cache <- function(area = NULL, date = NULL, dest = NULL) {
  if (is.null(dest)) {
    dest <- file.path(tools::R_user_dir("rsiscorar", "cache"), "gribs")
  }
  if (!dir.exists(dest)) {
    cli::cli_inform("No GRIB cache directory found.")
    return(invisible(character()))
  }
  pattern <- ".*\\.grib2$"
  if (!is.null(area)) {
    pattern <- paste0("^", area, "_.*\\.grib2$")
  }
  files <- list.files(dest, pattern = pattern, full.names = TRUE)
  if (!is.null(date)) {
    date_str <- format(as.Date(date), "%Y%m%d")
    files <- files[grepl(date_str, basename(files))]
  }
  if (length(files) == 0L) {
    cli::cli_inform("No matching cached GRIB files found.")
    return(invisible(character()))
  }
  file.remove(files)
  cli::cli_inform("Removed {.val {length(files)}} cached GRIB file{?s}.")
  invisible(files)
}

#' Batch Convert SISCORAR Text Files to GRIB2
#'
#' Processes multiple SISCORAR text files into GRIB2 format.
#'
#' @param input_dir Directory containing text files.
#' @param output_dir Output directory for GRIB2 files.
#' @param pattern File pattern to match. Default: `"^\\d{10}\\.txt$"`.
#' @param ... Additional arguments passed to [siscorar_to_grib()].
#'
#' @return Invisibly returns a list of conversion results.
#' @export
batch_convert <- function(input_dir, output_dir,
                          pattern = "^\\d{10}\\.txt$", ...) {
  files <- list.files(input_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0L) {
    cli::cli_abort("No files found matching pattern {.val {pattern}} in {.path {input_dir}}")
  }

  cli::cli_inform("Found {.val {length(files)}} files to process")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  results <- lapply(files, function(f) {
    out_file <- file.path(output_dir, sub("\\.txt$", ".grib2", basename(f)))
    tryCatch(
      siscorar_to_grib(f, out_file, ...),
      error = function(e) {
        cli::cli_warn("Failed to process {.path {f}}: {e$message}")
        NULL
      }
    )
  })

  n_success <- sum(!vapply(results, is.null, logical(1)))
  cli::cli_inform("Processed {.val {n_success}} / {.val {length(files)}} files")

  invisible(results)
}
