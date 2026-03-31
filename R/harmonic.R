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

# Astronomical arguments ---------------------------------------------------

#' Compute 5 fundamental astronomical variables (Schureman 1958)
#'
#' @param date Date or coercible to Date.
#' @return Named numeric vector (degrees, 0-360): s, h, p, N_prime, p1.
#' @noRd
.compute_astro_args <- function(date) {
  jd <- as.numeric(as.Date(date)) + 2440587.5
  T <- (jd - 2451545.0) / 36525.0

  s <- 218.3164477 + 481267.88123421 * T - 0.0015786 * T^2 +
    T^3 / 538841.0 - T^4 / 65194000.0
  h <- 280.46646 + 36000.76983 * T + 0.0003032 * T^2
  p <- 83.3532465 + 4069.0137287 * T - 0.0103200 * T^2 -
    T^3 / 80053.0 + T^4 / 18999000.0
  N <- 125.04452 - 1934.13626 * T + 0.0020708 * T^2 + T^3 / 450000.0
  N_prime <- -N
  p1 <- 282.93735 + 1.71946 * T + 0.00045 * T^2

  args <- c(s = s, h = h, p = p, N_prime = N_prime, p1 = p1)
  args %% 360
}

#' Compute V0+u for each SISCORAR constituent
#'
#' @param date Date or coercible to Date.
#' @return Numeric vector of length 13 (degrees, 0-360) in .CONSTITUENTS order.
#' @noRd
.compute_v0u <- function(date) {
  a <- .compute_astro_args(as.Date(date))
  s <- a[["s"]]
  h <- a[["h"]]
  p <- a[["p"]]
  N <- -a[["N_prime"]]
  p1 <- a[["p1"]]
  Nr <- N * pi / 180

  # Nodal corrections xi and nu (Schureman)
  xi <- -12.94 * sin(Nr) + 0.68 * sin(2 * Nr)
  nu <- -5.09 * sin(Nr) - 0.44 * sin(2 * Nr)

  # nu' for K1
  nu_prime <- atan2(
    sin(Nr) * 0.10948,
    cos(Nr) * 0.10948 + 0.8886
  ) * 180 / pi

  # nu'' for K2
  nu_double_prime <- atan2(
    sin(2 * Nr) * 0.01164,
    cos(2 * Nr) * 0.01164 + 0.6583
  ) * 180 / pi

  v0u <- numeric(13L)
  # .CONSTITUENTS order: O1, S2, K1, N2, MN4, Q1, K2, M2, P1, M4, MS4, M6, M8
  v0u[1]  <- h - 2 * s + 90 + 2 * xi - nu          # O1
  v0u[2]  <- 0                                        # S2
  v0u[3]  <- h + 90 - nu_prime                        # K1
  v0u[4]  <- 2 * h - 3 * s + p + 2 * xi - nu         # N2
  v0u[5]  <- 4 * h - 5 * s + p + 4 * xi - 2 * nu     # MN4
  v0u[6]  <- h - 3 * s + p + 90 + 2 * xi - nu         # Q1
  v0u[7]  <- 2 * h - 2 * nu_double_prime              # K2
  v0u[8]  <- 2 * h - 2 * s + 2 * xi - 2 * nu          # M2
  v0u[9]  <- -h + 270                                  # P1
  v0u[10] <- 4 * h - 4 * s + 4 * xi - 4 * nu          # M4
  v0u[11] <- 2 * h - 2 * s + 2 * xi - 2 * nu          # MS4
  v0u[12] <- 6 * h - 6 * s + 6 * xi - 6 * nu          # M6
  v0u[13] <- 8 * h - 8 * s + 8 * xi - 8 * nu          # M8

  v0u %% 360
}

#' Compute nodal factors f for each SISCORAR constituent
#'
#' Adjusts amplitude based on the 18.6-year lunar nodal cycle.
#'
#' @param date Date or coercible to Date.
#' @return Numeric vector of length 13 in .CONSTITUENTS order.
#' @noRd
.compute_nodal_factors <- function(date) {
  a <- .compute_astro_args(as.Date(date))
  N <- -a[["N_prime"]]
  Nr <- N * pi / 180

  f <- numeric(13L)
  f_m2 <- 1.0004 - 0.0373 * cos(Nr) + 0.0002 * cos(2 * Nr)

  f[1]  <- 1.0089 + 0.1871 * cos(Nr) - 0.0147 * cos(2 * Nr) + 0.0014 * cos(3 * Nr)  # O1
  f[2]  <- 1.0                                                                          # S2
  f[3]  <- 1.0060 + 0.1150 * cos(Nr) - 0.0088 * cos(2 * Nr) + 0.0006 * cos(3 * Nr)  # K1
  f[4]  <- f_m2                                                                        # N2
  f[5]  <- f_m2^2                                                                      # MN4
  f[6]  <- f[1]                                                                        # Q1 = f(O1)
  f[7]  <- 1.0241 + 0.2863 * cos(Nr) + 0.0083 * cos(2 * Nr) - 0.0015 * cos(3 * Nr)  # K2
  f[8]  <- f_m2                                                                        # M2
  f[9]  <- 1.0                                                                          # P1
  f[10] <- f_m2^2                                                                      # M4
  f[11] <- f_m2                                                                        # MS4
  f[12] <- f_m2^3                                                                      # M6
  f[13] <- f_m2^4                                                                      # M8

  f
}
