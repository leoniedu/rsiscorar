# Tidal harmonic constants ---------------------------------------------------

#' The 13 SISCORAR tidal constituents
#' @noRd
.CONSTITUENTS <- data.table::data.table(
  index = c(12L, 45L, 19L, 34L, 67L, 10L, 47L, 39L, 17L, 71L, 76L, 94L, 114L),
  name = c("O1", "S2", "K1", "N2", "MN4", "Q1", "K2", "M2", "P1",
           "M4", "MS4", "M6", "M8"),
  freq_deg_hr = c(13.9430356, 30.0, 15.0410686, 28.4397295, 57.4238337,
                  13.3986609, 30.0821373, 28.9841042, 14.9589314,
                  57.9682084, 58.9841042, 86.9523127, 115.9364169)
)

#' Read harmonic constants from U.bin and V.bin
#'
#' @param area Character: bay name.
#' @return data.table with columns: node, lon, lat, constituent,
#'   u_amplitude, u_phase, v_amplitude, v_phase.
#' @noRd
.read_harmonic_constants <- function(area) {
  area <- .validate_area(area)
  area_path <- get_area_path(area)

  u_file <- file.path(area_path, "U.bin")
  v_file <- file.path(area_path, "V.bin")

  if (!file.exists(u_file)) {
    cli::cli_abort("U.bin not found in {.path {area_path}}.")
  }
  if (!file.exists(v_file)) {
    cli::cli_abort("V.bin not found in {.path {area_path}}.")
  }

  u_raw <- .parse_bin_file(u_file)
  v_raw <- .parse_bin_file(v_file)

  if (nrow(u_raw) != nrow(v_raw)) {
    cli::cli_abort("U.bin and V.bin have different node counts.")
  }

  # Merge U and V on node + constituent
  dt <- data.table::merge.data.table(
    u_raw, v_raw,
    by = c("node", "lon", "lat", "constituent"),
    suffixes = c("_u", "_v")
  )

  data.table::setnames(dt, c("amplitude_u", "phase_u", "amplitude_v", "phase_v"),
                        c("u_amplitude", "u_phase", "v_amplitude", "v_phase"))

  data.table::setkey(dt, node, constituent)
  dt
}

#' Parse a single binary file (U.bin or V.bin)
#'
#' @param path Path to binary file.
#' @return data.table with columns: node, lon, lat, constituent, amplitude, phase.
#' @noRd
.parse_bin_file <- function(path) {
  bytes_per_node <- 168L # 14 triplets * 12 bytes
  file_size <- file.info(path)$size
  n_nodes <- file_size %/% bytes_per_node

  if (file_size %% bytes_per_node != 0L) {
    cli::cli_abort(
      "File {.path {path}} size ({file_size} bytes) is not a multiple of {bytes_per_node}."
    )
  }

  # Pre-allocate
  n_rows <- n_nodes * 13L
  node_vec <- integer(n_rows)
  lon_vec <- numeric(n_rows)
  lat_vec <- numeric(n_rows)
  cons_vec <- integer(n_rows)
  amp_vec <- numeric(n_rows)
  phase_vec <- numeric(n_rows)

  offset <- 9857

  raw_data <- readBin(path, "raw", n = file_size)

  for (i in seq_len(n_nodes)) {
    base <- (i - 1L) * bytes_per_node

    # Header triplet (triplet 0): int32, float32, float32
    lon <- readBin(raw_data[(base + 5L):(base + 8L)], "double", size = 4L, endian = "little")
    lat <- readBin(raw_data[(base + 9L):(base + 12L)], "double", size = 4L, endian = "little")

    # Constituent triplets 1-13
    for (j in seq_len(13L)) {
      row_idx <- (i - 1L) * 13L + j
      triplet_base <- base + j * 12L

      cons_raw <- readBin(raw_data[(triplet_base + 1L):(triplet_base + 4L)],
                          "integer", size = 4L, endian = "little")
      amp_raw <- readBin(raw_data[(triplet_base + 5L):(triplet_base + 8L)],
                         "double", size = 4L, endian = "little")
      phase_raw <- readBin(raw_data[(triplet_base + 9L):(triplet_base + 12L)],
                           "double", size = 4L, endian = "little")

      node_vec[row_idx] <- i
      lon_vec[row_idx] <- lon
      lat_vec[row_idx] <- lat
      cons_vec[row_idx] <- cons_raw - offset
      amp_vec[row_idx] <- amp_raw - offset
      phase_vec[row_idx] <- phase_raw - offset
    }
  }

  # Wrap phase to [0, 360)
  phase_vec <- phase_vec %% 360

  data.table::data.table(
    node = node_vec,
    lon = lon_vec,
    lat = lat_vec,
    constituent = cons_vec,
    amplitude = amp_vec,
    phase = phase_vec
  )
}
