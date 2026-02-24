#' Read Predictions from Grade.bin
#'
#' Parses the binary Grade.bin file containing tidal current predictions.
#' Returns a data.table with hourly predictions for all active grid nodes.
#'
#' @param area Character: bay name. Default: `"guanabara"`.
#' @param date Date object or `"YYYY-MM-DD"` string: the prediction date
#'   (used for the `datetime` column). Default: `Sys.Date()`.
#'
#' @return data.table with columns:
#' \describe{
#'   \item{col, row}{Grid cell indices}
#'   \item{lon, lat}{Coordinates (decimal degrees, WGS84)}
#'   \item{datetime}{POSIXct timestamp (America/Sao_Paulo)}
#'   \item{hour}{Hour of day (0-23)}
#'   \item{velocity_cm_s}{Speed in cm/s}
#'   \item{speed_m_s}{Speed in m/s}
#'   \item{direction_deg}{Direction from North (degrees, oceanographic convention)}
#'   \item{u_velocity}{Eastward component (cm/s)}
#'   \item{v_velocity}{Northward component (cm/s)}
#' }
#'
#' @details
#' The Grade.bin file uses a variable-length binary format:
#' - 20-byte header per node (col, row, lon, lat, flag)
#' - 192 additional bytes for active nodes (24 hours x 8 bytes)
#' - Each hour has velocity (float32) and direction (float32)
#'
#' For Guanabara Bay, this returns ~7 million records (290k nodes x 24 hours).
#'
#' @seealso [predict_currents()], [run_prediction()]
#'
#' @examples
#' \dontrun{
#' dt <- read_predictions("guanabara", "2025-06-15")
#' dt[, .(mean_speed = mean(velocity_cm_s), max_speed = max(velocity_cm_s)),
#'    by = hour]
#' }
#'
#' @export
read_predictions <- function(area = "guanabara", date = Sys.Date()) {
  area <- .validate_area(area)
  date <- as.Date(date)
  area_path <- get_area_path(area)
  grade_file <- file.path(area_path, "Grade.bin")

  if (!file.exists(grade_file)) {
    cli::cli_abort(c(
      "{.path Grade.bin} not found for area {.val {area}}.",
      "i" = "Run {.code run_prediction()} first."
    ))
  }

  file_size <- file.info(grade_file)$size
  cli::cli_inform("Reading {.path Grade.bin} ({.val {round(file_size / 1e6, 1)}} MB)...")
  t0 <- Sys.time()

  # Read entire file as raw bytes
  con <- file(grade_file, "rb")
  on.exit(close(con), add = TRUE)
  raw_data <- readBin(con, "raw", n = file_size)

  # First pass: scan for active nodes
  cli::cli_inform("  Scanning nodes...")
  max_nodes <- file_size %/% 20L
  node_offsets <- integer(max_nodes)
  node_flags <- integer(max_nodes)

  offset <- 0L
  node_count <- 0L

  while (offset + 20L <= file_size) {
    node_count <- node_count + 1L
    node_offsets[node_count] <- offset
    flag <- readBin(
      raw_data[(offset + 17L):(offset + 18L)],
      "integer", size = 2, signed = TRUE, endian = "little"
    )
    node_flags[node_count] <- flag
    offset <- offset + if (flag == 1L) 212L else 20L
  }

  node_offsets <- node_offsets[seq_len(node_count)]
  node_flags <- node_flags[seq_len(node_count)]
  active_idx <- which(node_flags == 1L)
  active_count <- length(active_idx)

  cli::cli_inform(
    "  Found {.val {format(node_count, big.mark = ',')}} nodes, {.val {format(active_count, big.mark = ',')}} active"
  )

  if (active_count == 0L) {
    cli::cli_warn("No prediction data. Run {.code run_prediction()} first.")
    return(data.table())
  }

  active_offsets <- node_offsets[active_idx]

  # Second pass: vectorized extraction
  cli::cli_inform("  Extracting data (vectorized)...")

  col_idx <- as.vector(outer(1:2, active_offsets, `+`))
  row_idx <- as.vector(outer(5:6, active_offsets, `+`))
  lon_idx <- as.vector(outer(9:12, active_offsets, `+`))
  lat_idx <- as.vector(outer(13:16, active_offsets, `+`))

  node_col <- readBin(raw_data[col_idx], "integer", n = active_count,
                       size = 2, signed = TRUE, endian = "little")
  node_row <- readBin(raw_data[row_idx], "integer", n = active_count,
                       size = 2, signed = TRUE, endian = "little")
  node_lon <- readBin(raw_data[lon_idx], "numeric", n = active_count,
                       size = 4, endian = "little")
  node_lat <- readBin(raw_data[lat_idx], "numeric", n = active_count,
                       size = 4, endian = "little")

  hourly_idx <- as.vector(outer(21:212, active_offsets, `+`))
  hourly_all <- readBin(raw_data[hourly_idx], "numeric",
                         n = active_count * 48L, size = 4, endian = "little")

  hourly_mat <- matrix(hourly_all, nrow = 48, ncol = active_count)
  vel_mat <- hourly_mat[seq(1, 47, 2), ]
  dir_mat <- hourly_mat[seq(2, 48, 2), ]

  # Build data.table
  cli::cli_inform("  Building data.table...")
  dt <- data.table(
    col = rep(node_col, each = 24L),
    row = rep(node_row, each = 24L),
    lon = rep(node_lon, each = 24L),
    lat = rep(node_lat, each = 24L),
    hour = rep(0:23, times = active_count),
    velocity_cm_s = as.vector(vel_mat),
    direction_deg = as.vector(dir_mat)
  )

  dt[, `:=`(
    datetime = as.POSIXct(
      sprintf("%s %02d:00:00", date, hour),
      tz = "America/Sao_Paulo"
    ),
    u_velocity = velocity_cm_s * sin(direction_deg * pi / 180),
    v_velocity = velocity_cm_s * cos(direction_deg * pi / 180),
    speed_m_s = velocity_cm_s / 100
  )]

  setcolorder(dt, c("col", "row", "lon", "lat", "datetime", "hour",
                     "velocity_cm_s", "speed_m_s", "direction_deg",
                     "u_velocity", "v_velocity"))

  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  cli::cli_inform(
    "Done! {.val {format(nrow(dt), big.mark = ',')}} records in {.val {round(elapsed, 1)}} sec"
  )

  dt
}

#' Read Grid Coordinates
#'
#' Reads the grid structure from Grade.bin without hourly prediction data.
#' Useful for understanding the spatial coverage of an area.
#'
#' @inheritParams read_predictions
#'
#' @return data.table with columns: col, row, lon, lat, active.
#'
#' @examples
#' \dontrun{
#' grid <- read_grid("guanabara")
#' grid[, .N, by = active]
#' plot(grid$lon, grid$lat, pch = ".", col = ifelse(grid$active, "blue", "gray"))
#' }
#'
#' @export
read_grid <- function(area = "guanabara") {
  area <- .validate_area(area)
  area_path <- get_area_path(area)
  grade_file <- file.path(area_path, "Grade.bin")

  if (!file.exists(grade_file)) {
    cli::cli_abort(c(
      "{.path Grade.bin} not found for area {.val {area}}.",
      "i" = "Run {.code run_prediction()} first to generate it."
    ))
  }

  file_size <- file.info(grade_file)$size
  con <- file(grade_file, "rb")
  on.exit(close(con), add = TRUE)
  raw_data <- readBin(con, "raw", n = file_size)

  max_nodes <- file_size %/% 20L
  cols <- integer(max_nodes)
  rows <- integer(max_nodes)
  lons <- numeric(max_nodes)
  lats <- numeric(max_nodes)
  flags <- integer(max_nodes)

  offset <- 0L
  n <- 0L

  while (offset + 20L <= file_size) {
    n <- n + 1L
    cols[n] <- readBin(raw_data[(offset + 1L):(offset + 2L)], "integer",
                        size = 2, signed = TRUE, endian = "little")
    rows[n] <- readBin(raw_data[(offset + 5L):(offset + 6L)], "integer",
                        size = 2, signed = TRUE, endian = "little")
    lons[n] <- readBin(raw_data[(offset + 9L):(offset + 12L)], "numeric",
                        size = 4, endian = "little")
    lats[n] <- readBin(raw_data[(offset + 13L):(offset + 16L)], "numeric",
                        size = 4, endian = "little")
    flags[n] <- readBin(raw_data[(offset + 17L):(offset + 18L)], "integer",
                          size = 2, signed = TRUE, endian = "little")
    offset <- offset + if (flags[n] == 1L) 212L else 20L
  }

  data.table(
    col = cols[seq_len(n)],
    row = rows[seq_len(n)],
    lon = lons[seq_len(n)],
    lat = lats[seq_len(n)],
    active = (flags[seq_len(n)] == 1L)
  )
}

#' Read Tidal Constituents
#'
#' Reads the 142 harmonic constituent definitions from ConsNovas.txt.
#'
#' @inheritParams read_predictions
#'
#' @return data.table with columns:
#' \describe{
#'   \item{index}{Constituent index (1-142)}
#'   \item{name}{Constituent name (e.g., "M2", "S2", "K1")}
#'   \item{frequency_scaled}{Frequency * 10^7}
#'   \item{frequency_deg_hour}{Frequency in degrees per hour}
#' }
#'
#' @examples
#' \dontrun{
#' cons <- read_constituents("guanabara")
#' cons[name %in% c("M2", "S2", "K1", "O1")]
#' }
#'
#' @export
read_constituents <- function(area = "guanabara") {
  area <- .validate_area(area)
  cons_file <- file.path(get_area_path(area), "ConsNovas.txt")

  if (!file.exists(cons_file)) {
    cli::cli_abort(
      "Constituent file not found: {.path {cons_file}}"
    )
  }

  lines <- readLines(cons_file)[-1L]
  parsed <- regmatches(lines, regexec("^\\s*(\\d+)\\s*(\\S+)\\s+(\\d+)", lines))
  parsed <- parsed[lengths(parsed) == 4L]

  dt <- data.table(
    index = as.integer(vapply(parsed, `[`, character(1), 2L)),
    name = vapply(parsed, `[`, character(1), 3L),
    frequency_scaled = as.numeric(vapply(parsed, `[`, character(1), 4L))
  )
  dt[, frequency_deg_hour := frequency_scaled / 1e7]
  dt
}

#' Display Area Information
#'
#' Prints summary information about a bay area including grid dimensions
#' and coordinate bounds.
#'
#' @inheritParams read_predictions
#'
#' @return Invisibly returns the grid data.table.
#'
#' @examples
#' \dontrun{
#' area_info("guanabara")
#' area_info("baiatos")
#' }
#'
#' @export
area_info <- function(area = "guanabara") {
  area <- .validate_area(area)
  grid <- read_grid(area)
  n_active <- sum(grid$active)

  cli::cli_h2("SISCORAR: {toupper(area)}")
  cli::cli_bullets(c(
    "*" = "Nodes: {.val {format(nrow(grid), big.mark = ',')}} total, {.val {format(n_active, big.mark = ',')}} active",
    "*" = "Lon: {.val {round(min(grid$lon), 4)}} to {.val {round(max(grid$lon), 4)}}",
    "*" = "Lat: {.val {round(min(grid$lat), 4)}} to {.val {round(max(grid$lat), 4)}}"
  ))

  invisible(grid)
}
