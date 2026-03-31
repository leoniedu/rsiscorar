# Pure R Harmonic Tidal Current Prediction — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace Wine/exe-based tidal prediction with pure R harmonic computation at computational mesh nodes.

**Architecture:** New `R/harmonic.R` contains tidal astronomy (V0+u, f) and harmonic summation. `predict_currents()` calls the new engine instead of Wine+exe. Validation tests compare against exe output across 5 sampled dates spanning 2000-2050.

**Tech Stack:** R, data.table (already a dependency). No new dependencies.

**Spec:** `docs/superpowers/specs/2026-03-31-pure-r-harmonic-prediction-design.md`

---

## File Map

| Action | File | Responsibility |
|--------|------|---------------|
| Create | `R/harmonic.R` | Tidal astronomy, U.bin/V.bin parsing, harmonic summation |
| Modify | `R/predict.R` | Wire `predict_currents()` and `predict_currents_range()` to new engine |
| Create | `tests/testthat/test-harmonic.R` | Tests for new harmonic functions |
| Modify | `tests/testthat/test-predict.R` | Update for changed row counts |
| Create | `inst/scripts/generate_fixtures.R` | One-time script to generate exe reference fixtures |

---

### Task 1: Read U.bin/V.bin harmonic constants

**Files:**
- Create: `R/harmonic.R`
- Create: `tests/testthat/test-harmonic.R`

- [ ] **Step 1: Write the failing test for `.read_harmonic_constants()`**

In `tests/testthat/test-harmonic.R`:

```r
skip_if_not_installed <- function() {
  home <- tryCatch(siscorar_home(), error = function(e) "")
  skip_if_not(dir.exists(home), "SISCORAR not installed")
}

test_that(".read_harmonic_constants returns correct structure", {
  skip_if_not_installed()

  dt <- rsiscorar:::.read_harmonic_constants("sepetiba")
  expect_s3_class(dt, "data.table")
  expect_true(all(c("node", "lon", "lat", "constituent",
                     "u_amplitude", "u_phase",
                     "v_amplitude", "v_phase") %in% names(dt)))

  # sepetiba has 3093 computational nodes, 13 constituents each
  n_nodes <- length(unique(dt$node))
  expect_true(n_nodes > 1000)
  expect_equal(nrow(dt), n_nodes * 13L)

  # Constituent indices should be the known 13
  expected_indices <- c(12L, 45L, 19L, 34L, 67L, 10L, 47L, 39L, 17L, 71L, 76L, 94L, 114L)
  expect_equal(sort(unique(dt$constituent)), sort(expected_indices))

  # Coordinates should be in Brazilian coastal range
  expect_true(all(dt$lon > -50 & dt$lon < -30))
  expect_true(all(dt$lat > -30 & dt$lat < -5))

  # Amplitudes should be non-negative cm/s values
  expect_true(all(dt$u_amplitude >= 0))
  expect_true(all(dt$v_amplitude >= 0))

  # Phases should be 0-360
  expect_true(all(dt$u_phase >= 0 & dt$u_phase < 360))
  expect_true(all(dt$v_phase >= 0 & dt$v_phase < 360))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: FAIL — `.read_harmonic_constants` not found.

- [ ] **Step 3: Implement `.read_harmonic_constants()`**

In `R/harmonic.R`:

```r
# Constituent table: the 13 tidal constituents used by SISCORAR
# Index references ConsNovas.txt numbering
.CONSTITUENTS <- data.table::data.table(
  index = c(12L, 45L, 19L, 34L, 67L, 10L, 47L, 39L, 17L, 71L, 76L, 94L, 114L),
  name = c("O1", "S2", "K1", "N2", "MN4", "Q1", "K2", "M2", "P1", "M4", "MS4", "M6", "M8"),
  freq_deg_hr = c(13.9430356, 30.0, 15.0410686, 28.4397295, 57.4238337,
                  13.3986609, 30.0821373, 28.9841042, 14.9589314,
                  57.9682084, 58.9841042, 86.9523127, 115.9364169)
)

#' Read harmonic constants from U.bin and V.bin
#'
#' Parses the binary files containing per-node amplitude and phase for each
#' tidal constituent. Each node has 14 triplets of (int32, float32, float32):
#' triplet 0 is a header with (9857+1, lon, lat), triplets 1-13 are
#' (9857+constituent_index, 9857+amplitude, 9857+phase).
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

  if (!file.exists(u_file) || !file.exists(v_file)) {
    cli::cli_abort("Harmonic constant files not found in {.path {area_path}}")
  }

  u_raw <- readBin(u_file, "raw", n = file.info(u_file)$size)
  v_raw <- readBin(v_file, "raw", n = file.info(v_file)$size)

  bytes_per_node <- 168L  # 14 triplets * 12 bytes each
  n_nodes <- length(u_raw) %/% bytes_per_node

  # Pre-allocate output vectors
  total_rows <- n_nodes * 13L
  out_node <- integer(total_rows)
  out_lon <- numeric(total_rows)
  out_lat <- numeric(total_rows)
  out_cons <- integer(total_rows)
  out_u_amp <- numeric(total_rows)
  out_u_phase <- numeric(total_rows)
  out_v_amp <- numeric(total_rows)
  out_v_phase <- numeric(total_rows)

  row_idx <- 0L
  for (n in seq_len(n_nodes)) {
    base <- (n - 1L) * bytes_per_node

    # Header triplet: (int32, float_lon, float_lat)
    lon <- readBin(u_raw[(base + 5L):(base + 8L)], "numeric", size = 4, endian = "little")
    lat <- readBin(u_raw[(base + 9L):(base + 12L)], "numeric", size = 4, endian = "little")

    for (c in seq_len(13L)) {
      row_idx <- row_idx + 1L
      triplet_base <- base + c * 12L

      cons_idx <- readBin(u_raw[(triplet_base + 1L):(triplet_base + 4L)],
                          "integer", size = 4, endian = "little") - 9857L

      u_amp_raw <- readBin(u_raw[(triplet_base + 5L):(triplet_base + 8L)],
                           "numeric", size = 4, endian = "little")
      u_phase_raw <- readBin(u_raw[(triplet_base + 9L):(triplet_base + 12L)],
                             "numeric", size = 4, endian = "little")

      v_amp_raw <- readBin(v_raw[(triplet_base + 5L):(triplet_base + 8L)],
                           "numeric", size = 4, endian = "little")
      v_phase_raw <- readBin(v_raw[(triplet_base + 9L):(triplet_base + 12L)],
                             "numeric", size = 4, endian = "little")

      out_node[row_idx] <- n
      out_lon[row_idx] <- lon
      out_lat[row_idx] <- lat
      out_cons[row_idx] <- cons_idx
      out_u_amp[row_idx] <- u_amp_raw - 9857
      out_u_phase[row_idx] <- (u_phase_raw - 9857) %% 360
      out_v_amp[row_idx] <- v_amp_raw - 9857
      out_v_phase[row_idx] <- (v_phase_raw - 9857) %% 360
    }
  }

  data.table::data.table(
    node = out_node,
    lon = out_lon,
    lat = out_lat,
    constituent = out_cons,
    u_amplitude = out_u_amp,
    u_phase = out_u_phase,
    v_amplitude = out_v_amp,
    v_phase = out_v_phase
  )
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/harmonic.R tests/testthat/test-harmonic.R
git commit -m "feat: add .read_harmonic_constants() to parse U.bin/V.bin"
```

---

### Task 2: Compute astronomical arguments (V0+u)

**Files:**
- Modify: `R/harmonic.R`
- Modify: `tests/testthat/test-harmonic.R`

- [ ] **Step 1: Write the failing test for `.compute_v0u()`**

Append to `tests/testthat/test-harmonic.R`:

```r
test_that(".compute_v0u returns 13 values in 0-360 range", {
  v0u <- rsiscorar:::.compute_v0u(as.Date("2026-03-31"))
  expect_length(v0u, 13L)
  expect_true(all(v0u >= 0 & v0u < 360))

  # S2 should always be 0 at midnight (solar constituent, V0 = 0 at 00:00)
  s2_idx <- which(.CONSTITUENTS$name == "S2")
  expect_equal(v0u[s2_idx], 0, tolerance = 0.5)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: FAIL — `.compute_v0u` not found.

- [ ] **Step 3: Implement astronomical argument computation**

Append to `R/harmonic.R`. The formulas are from Schureman (1958) "Manual of Harmonic Analysis and Prediction of Tides". The 5 fundamental astronomical arguments are computed from the Julian century T since J2000.0, then V0+u for each constituent is assembled from Doodson number combinations.

```r
#' Compute fundamental astronomical arguments for a date
#'
#' Returns the 5 variables needed for tidal prediction (in degrees):
#' s = mean longitude of moon
#' h = mean longitude of sun
#' p = longitude of lunar perigee
#' N_prime = negative longitude of lunar ascending node (-N)
#' p1 = longitude of solar perigee
#'
#' Formulas from Schureman (1958), referenced to midnight UT of given date.
#'
#' @param date Date object.
#' @return Named numeric vector with s, h, p, N_prime, p1 (degrees, 0-360).
#' @noRd
.compute_astro_args <- function(date) {
  # Julian century from J2000.0 (2000-01-01 12:00 UT)
  jd <- as.numeric(as.Date(date)) + 2440587.5  # R epoch -> JD
  T <- (jd - 2451545.0) / 36525.0

  # Mean longitude of moon (s)
  s <- 218.3164477 + 481267.88123421 * T - 0.0015786 * T^2 +
    T^3 / 538841.0 - T^4 / 65194000.0

  # Mean longitude of sun (h)
  h <- 280.46646 + 36000.76983 * T + 0.0003032 * T^2

  # Longitude of lunar perigee (p)
  p <- 83.3532465 + 4069.0137287 * T - 0.0103200 * T^2 -
    T^3 / 80053.0 + T^4 / 18999000.0

  # Longitude of lunar ascending node (N)
  N <- 125.04452 - 1934.13626 * T + 0.0020708 * T^2 + T^3 / 450000.0
  N_prime <- -N  # SISCORAR uses -N convention

  # Longitude of solar perigee (p1) - very slowly varying
  p1 <- 282.93735 + 1.71946 * T + 0.00045 * T^2

  args <- c(s = s, h = h, p = p, N_prime = N_prime, p1 = p1)
  args %% 360
}

#' Compute V0+u for the 13 SISCORAR constituents
#'
#' V0 is the astronomical argument (equilibrium phase at Greenwich midnight).
#' u is the nodal correction to V0. Together, V0+u gives the phase of each
#' constituent at midnight UT on the given date.
#'
#' Each constituent's V0 is a linear combination of the astronomical arguments
#' defined by its Doodson numbers. The u correction depends on N' (longitude
#' of lunar ascending node).
#'
#' @param date Date object.
#' @return Numeric vector of length 13 (degrees, 0-360), in .CONSTITUENTS order.
#' @noRd
.compute_v0u <- function(date) {
  a <- .compute_astro_args(as.Date(date))
  s <- a[["s"]]
  h <- a[["h"]]
  p <- a[["p"]]
  N <- -a[["N_prime"]]  # N = longitude of ascending node (positive)
  p1 <- a[["p1"]]

  # Convert N to radians for trig functions in u corrections
  Nr <- N * pi / 180

  # V0+u for each constituent, from Schureman (1958) tables
  # V0 = Doodson-number linear combination of (tau, s, h, p, N', p1)
  # where tau = h - s + 180 (Greenwich hour angle of moon at midnight)
  # For midnight: tau = h - s (the +180 is already in the formulas below)

  v0u <- numeric(13L)

  # 1. O1: V0 = h - 2s + 90; u = 2*xi - nu
  #    Simplified u from Schureman Table 2
  xi <- -12.94 * sin(Nr) + 0.68 * sin(2 * Nr)
  nu <- -5.09 * sin(Nr) - 0.44 * sin(2 * Nr)
  v0u[1] <- (h - 2 * s + 90 + 2 * xi - nu) %% 360  # O1

  # 2. S2: V0 = 0; u = 0
  v0u[2] <- 0  # S2

  # 3. K1: V0 = h + 90; u = -nu_prime
  nu_prime <- atan2(
    sin(Nr) * 0.10948,
    cos(Nr) * 0.10948 + 0.8886
  ) * 180 / pi
  v0u[3] <- (h + 90 - nu_prime) %% 360  # K1

  # 4. N2: V0 = 2h - 3s + p; u = 2*xi - nu
  v0u[4] <- (2 * h - 3 * s + p + 2 * xi - nu) %% 360  # N2

  # 5. MN4: V0 = 4h - 5s + p; u = 4*xi - 2*nu
  v0u[5] <- (4 * h - 5 * s + p + 4 * xi - 2 * nu) %% 360  # MN4

  # 6. Q1: V0 = h - 3s + p + 90; u = 2*xi - nu
  v0u[6] <- (h - 3 * s + p + 90 + 2 * xi - nu) %% 360  # Q1

  # 7. K2: V0 = 2h; u = -2*nu_double_prime
  nu_double_prime <- atan2(
    sin(2 * Nr) * 0.01164,
    cos(2 * Nr) * 0.01164 + 0.6583
  ) * 180 / pi
  v0u[7] <- (2 * h - 2 * nu_double_prime) %% 360  # K2

  # 8. M2: V0 = 2h - 2s; u = 2*xi - 2*nu
  v0u[8] <- (2 * h - 2 * s + 2 * xi - 2 * nu) %% 360  # M2

  # 9. P1: V0 = -h + 270; u = 0
  v0u[9] <- (-h + 270) %% 360  # P1

  # 10. M4: V0 = 4h - 4s; u = 4*xi - 4*nu
  v0u[10] <- (4 * h - 4 * s + 4 * xi - 4 * nu) %% 360  # M4

  # 11. MS4: V0 = 2h - 2s; u = 2*xi - 2*nu (same V0 as M2+S2 combined)
  v0u[11] <- (2 * h - 2 * s + 2 * xi - 2 * nu) %% 360  # MS4

  # 12. M6: V0 = 6h - 6s; u = 6*xi - 6*nu
  v0u[12] <- (6 * h - 6 * s + 6 * xi - 6 * nu) %% 360  # M6

  # 13. M8: V0 = 8h - 8s; u = 8*xi - 8*nu
  v0u[13] <- (8 * h - 8 * s + 8 * xi - 8 * nu) %% 360  # M8

  v0u %% 360
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/harmonic.R tests/testthat/test-harmonic.R
git commit -m "feat: add .compute_v0u() for astronomical arguments"
```

---

### Task 3: Compute nodal factors (f)

**Files:**
- Modify: `R/harmonic.R`
- Modify: `tests/testthat/test-harmonic.R`

- [ ] **Step 1: Write the failing test for `.compute_nodal_factors()`**

Append to `tests/testthat/test-harmonic.R`:

```r
test_that(".compute_nodal_factors returns 13 positive values near 1", {
  f <- rsiscorar:::.compute_nodal_factors(as.Date("2026-03-31"))
  expect_length(f, 13L)
  expect_true(all(f > 0))
  # Nodal factors are typically 0.7-1.5
  expect_true(all(f > 0.5 & f < 2.0))

  # S2 and P1 nodal factors are always exactly 1.0
  s2_idx <- which(.CONSTITUENTS$name == "S2")
  p1_idx <- which(.CONSTITUENTS$name == "P1")
  expect_equal(f[s2_idx], 1.0)
  expect_equal(f[p1_idx], 1.0)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: FAIL — `.compute_nodal_factors` not found.

- [ ] **Step 3: Implement nodal factor computation**

Append to `R/harmonic.R`:

```r
#' Compute nodal factors (f) for the 13 SISCORAR constituents
#'
#' The nodal factor f adjusts the amplitude of each constituent based on the
#' 18.6-year lunar nodal cycle. f depends on N (longitude of lunar ascending
#' node). Formulas from Schureman (1958) Table 2.
#'
#' @param date Date object.
#' @return Numeric vector of length 13, in .CONSTITUENTS order.
#' @noRd
.compute_nodal_factors <- function(date) {
  a <- .compute_astro_args(as.Date(date))
  N <- -a[["N_prime"]]  # positive N
  Nr <- N * pi / 180

  f <- numeric(13L)

  # Schureman node factor formulas
  # f(O1) = sin(I)*cos^2(I/2) / 0.3800 where I depends on N
  # Simplified using Schureman's expansion:
  # f(O1) = 1.0089 + 0.1871*cos(N) - 0.0147*cos(2N) + 0.0014*cos(3N)
  f[1] <- 1.0089 + 0.1871 * cos(Nr) - 0.0147 * cos(2 * Nr) +
    0.0014 * cos(3 * Nr)  # O1

  f[2] <- 1.0  # S2 (purely solar, no nodal correction)

  # f(K1) = sqrt(0.8965*sin(2I)^2 + 0.6001*sin(2I)*cos(nu) + 0.1006)
  # Simplified:
  f[3] <- 1.0060 + 0.1150 * cos(Nr) - 0.0088 * cos(2 * Nr) +
    0.0006 * cos(3 * Nr)  # K1

  # f(N2) = f(M2)
  f_m2 <- 1.0004 - 0.0373 * cos(Nr) + 0.0002 * cos(2 * Nr)
  f[4] <- f_m2  # N2

  f[5] <- f_m2^2  # MN4 = f(M2)^2

  # f(Q1) = f(O1)
  f[6] <- f[1]  # Q1

  # f(K2) = sqrt(0.6402*cos(2N)^2 + ... ) simplified:
  f[7] <- 1.0241 + 0.2863 * cos(Nr) + 0.0083 * cos(2 * Nr) -
    0.0015 * cos(3 * Nr)  # K2

  f[8] <- f_m2  # M2

  f[9] <- 1.0  # P1 (purely solar, no nodal correction)

  f[10] <- f_m2^2  # M4 = f(M2)^2

  f[11] <- f_m2  # MS4 = f(M2) * f(S2) = f(M2) * 1.0

  f[12] <- f_m2^3  # M6 = f(M2)^3

  f[13] <- f_m2^4  # M8 = f(M2)^4

  f
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/harmonic.R tests/testthat/test-harmonic.R
git commit -m "feat: add .compute_nodal_factors() for 18.6-year cycle"
```

---

### Task 4: Implement harmonic prediction at nodes

**Files:**
- Modify: `R/harmonic.R`
- Modify: `tests/testthat/test-harmonic.R`

- [ ] **Step 1: Write the failing test for `.predict_at_nodes()`**

Append to `tests/testthat/test-harmonic.R`:

```r
test_that(".predict_at_nodes returns correct structure", {
  skip_if_not_installed()

  dt <- rsiscorar:::.predict_at_nodes(as.Date("2026-03-31"), "sepetiba")
  expect_s3_class(dt, "data.table")

  expected_cols <- c("col", "row", "lon", "lat", "datetime", "hour",
                     "velocity_cm_s", "speed_m_s", "direction_deg",
                     "u_velocity", "v_velocity")
  expect_true(all(expected_cols %in% names(dt)))

  # 3093 nodes * 24 hours for sepetiba
  n_nodes <- length(unique(dt[, paste(lon, lat)]))
  expect_true(n_nodes > 1000)
  expect_equal(nrow(dt), n_nodes * 24L)

  # Hours 0-23
  expect_equal(sort(unique(dt$hour)), 0:23)

  # Physical ranges
  expect_true(all(dt$velocity_cm_s >= 0))
  expect_true(all(dt$direction_deg >= 0 & dt$direction_deg < 360))
  expect_equal(dt$speed_m_s, dt$velocity_cm_s / 100)

  # Datetime should use Sao Paulo timezone
  expect_s3_class(dt$datetime, "POSIXct")
  expect_equal(attr(dt$datetime[1], "tzone"), "America/Sao_Paulo")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: FAIL — `.predict_at_nodes` not found.

- [ ] **Step 3: Implement `.predict_at_nodes()`**

Append to `R/harmonic.R`:

```r
#' Predict tidal currents at computational mesh nodes
#'
#' For each node and each hour (0-23), sums the 13 harmonic constituents:
#'   u(h) = sum( f_c * H_u_c * cos(omega_c * h + V0u_c - g_u_c) )
#'   v(h) = sum( f_c * H_v_c * cos(omega_c * h + V0u_c - g_v_c) )
#'
#' @param date Date object.
#' @param area Character: bay name.
#' @return data.table with same schema as read_predictions().
#' @noRd
.predict_at_nodes <- function(date, area) {
  date <- as.Date(date)
  area <- .validate_area(area)

  hc <- .read_harmonic_constants(area)
  v0u <- .compute_v0u(date)
  f <- .compute_nodal_factors(date)
  freqs <- .CONSTITUENTS$freq_deg_hr

  # Get unique nodes
  nodes <- unique(hc[, .(node, lon, lat)])
  n_nodes <- nrow(nodes)

  # Build constituent matrices (n_nodes x 13)
  # hc is ordered by node (13 rows per node)
  u_amp_mat <- matrix(hc$u_amplitude, nrow = 13L, ncol = n_nodes)
  u_phase_mat <- matrix(hc$u_phase, nrow = 13L, ncol = n_nodes)
  v_amp_mat <- matrix(hc$v_amplitude, nrow = 13L, ncol = n_nodes)
  v_phase_mat <- matrix(hc$v_phase, nrow = 13L, ncol = n_nodes)

  # Apply nodal factor to amplitudes
  u_amp_mat <- u_amp_mat * f

  v_amp_mat <- v_amp_mat * f

  # Pre-compute for all 24 hours
  hours <- 0:23
  n_hours <- 24L
  total_rows <- n_nodes * n_hours

  out_u <- numeric(total_rows)
  out_v <- numeric(total_rows)

  deg2rad <- pi / 180

  for (hi in seq_along(hours)) {
    h <- hours[hi]
    # Phase angle for each constituent at this hour (vector of 13)
    phase_h <- (freqs * h + v0u) * deg2rad

    # For each constituent c, across all nodes:
    # u_contribution = u_amp[c,] * cos(phase_h[c] - u_phase[c,] * deg2rad)
    u_sum <- numeric(n_nodes)
    v_sum <- numeric(n_nodes)
    for (c in seq_len(13L)) {
      u_sum <- u_sum + u_amp_mat[c, ] * cos(phase_h[c] - u_phase_mat[c, ] * deg2rad)
      v_sum <- v_sum + v_amp_mat[c, ] * cos(phase_h[c] - v_phase_mat[c, ] * deg2rad)
    }

    idx_start <- (hi - 1L) * n_nodes + 1L
    idx_end <- hi * n_nodes
    out_u[idx_start:idx_end] <- u_sum
    out_v[idx_start:idx_end] <- v_sum
  }

  velocity <- sqrt(out_u^2 + out_v^2)
  direction <- (atan2(out_u, out_v) * 180 / pi) %% 360

  dt <- data.table::data.table(
    col = rep(nodes$node, times = n_hours),
    row = rep(1L, total_rows),
    lon = rep(nodes$lon, times = n_hours),
    lat = rep(nodes$lat, times = n_hours),
    hour = rep(hours, each = n_nodes),
    velocity_cm_s = velocity,
    direction_deg = direction
  )

  dt[, `:=`(
    datetime = as.POSIXct(
      sprintf("%s %02d:00:00", date, hour),
      tz = "America/Sao_Paulo"
    ),
    u_velocity = out_u,
    v_velocity = out_v,
    speed_m_s = velocity_cm_s / 100
  )]

  data.table::setcolorder(dt, c("col", "row", "lon", "lat", "datetime", "hour",
                                 "velocity_cm_s", "speed_m_s", "direction_deg",
                                 "u_velocity", "v_velocity"))

  dt
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/harmonic.R tests/testthat/test-harmonic.R
git commit -m "feat: add .predict_at_nodes() harmonic summation"
```

---

### Task 5: Generate validation fixtures from exe

**Files:**
- Create: `inst/scripts/generate_fixtures.R`
- Create: `tests/testthat/fixtures/` (directory + RDS files)

- [ ] **Step 1: Write fixture generation script**

Create `inst/scripts/generate_fixtures.R`:

```r
#!/usr/bin/env Rscript
# Generate test fixtures by running SISCORAR exe for sampled dates.
# Requires Wine + SISCORAR installation. Run once, commit fixtures.
#
# Usage: Rscript inst/scripts/generate_fixtures.R

library(rsiscorar)

fixtures_dir <- file.path("tests", "testthat", "fixtures")
dir.create(fixtures_dir, showWarnings = FALSE, recursive = TRUE)

# Sample K=5 dates from 2000-2050 (monthly, fixed seed)
all_dates <- seq(as.Date("2000-01-01"), as.Date("2050-01-01"), by = "month")
set.seed(42L)
test_dates <- sort(sample(all_dates, 5L))

cat("Generating fixtures for dates:\n")
print(test_dates)

area <- "sepetiba"  # smallest area, fastest to run

for (d in test_dates) {
  d <- as.Date(d, origin = "1970-01-01")
  cat(sprintf("\n--- %s ---\n", d))

  # Run exe prediction
  success <- run_prediction(d, area)
  if (!success) {
    warning(sprintf("Prediction failed for %s", d))
    next
  }

  # Read FatNod.txt (V0+u from exe)
  fatnod_file <- file.path(get_area_path(area), "FatNod.txt")
  fatnod_lines <- readLines(fatnod_file, n = 13L)
  fatnod_parsed <- do.call(rbind, strsplit(trimws(fatnod_lines), "\\s+"))
  v0u_exe <- data.frame(
    constituent_index = as.integer(fatnod_parsed[, 1]),
    v0u_deg = as.numeric(fatnod_parsed[, 2])
  )

  # Read Grade.bin predictions
  grade_dt <- read_predictions(area, d)

  # Save fixture
  fixture <- list(
    date = d,
    area = area,
    v0u = v0u_exe,
    grade = grade_dt
  )

  fname <- sprintf("fixture_%s_%s.rds", area, format(d, "%Y%m%d"))
  saveRDS(fixture, file.path(fixtures_dir, fname))
  cat(sprintf("  Saved: %s (%d rows)\n", fname, nrow(grade_dt)))
}

# Save the test dates for easy loading
saveRDS(test_dates, file.path(fixtures_dir, "test_dates.rds"))
cat("\nDone! Fixtures saved to", fixtures_dir, "\n")
```

- [ ] **Step 2: Run the fixture generation**

Run: `r -e 'source("inst/scripts/generate_fixtures.R")'`
Expected: Creates 5 RDS files + `test_dates.rds` in `tests/testthat/fixtures/`.

- [ ] **Step 3: Verify fixtures were created**

Run: `ls -la tests/testthat/fixtures/`
Expected: 6 files (5 fixture RDS + 1 test_dates.rds)

- [ ] **Step 4: Commit fixtures**

```bash
git add inst/scripts/generate_fixtures.R tests/testthat/fixtures/
git commit -m "test: generate validation fixtures from exe (K=5, 2000-2050)"
```

---

### Task 6: Validate V0+u against exe fixtures

**Files:**
- Modify: `tests/testthat/test-harmonic.R`

- [ ] **Step 1: Write V0+u validation test using fixtures**

Append to `tests/testthat/test-harmonic.R`:

```r
test_that(".compute_v0u matches exe FatNod.txt across 2000-2050", {
  fixtures_dir <- test_path("fixtures")
  dates_file <- file.path(fixtures_dir, "test_dates.rds")
  skip_if_not(file.exists(dates_file), "Fixtures not generated")

  test_dates <- readRDS(dates_file)

  for (d in test_dates) {
    d <- as.Date(d, origin = "1970-01-01")
    fname <- sprintf("fixture_sepetiba_%s.rds", format(d, "%Y%m%d"))
    fixture <- readRDS(file.path(fixtures_dir, fname))

    our_v0u <- rsiscorar:::.compute_v0u(d)

    # Match by constituent index order (FatNod order = .CONSTITUENTS order)
    exe_v0u <- fixture$v0u$v0u_deg

    for (i in seq_along(our_v0u)) {
      # Handle wrap-around (e.g., 359.8 vs 0.2 should be ~0.4 apart)
      diff <- abs(our_v0u[i] - exe_v0u[i])
      diff <- min(diff, 360 - diff)
      expect_lt(
        diff, 0.5,
        label = sprintf("V0+u mismatch for constituent %d on %s: ours=%.2f exe=%.2f",
                        .CONSTITUENTS$index[i], d, our_v0u[i], exe_v0u[i])
      )
    }
  }
})
```

- [ ] **Step 2: Run test**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: PASS if V0+u formulas are correct. If FAIL, adjust the Schureman formulas in `.compute_v0u()` and re-run until all 5 dates × 13 constituents pass within 0.5°.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-harmonic.R
git commit -m "test: validate V0+u against exe across 2000-2050"
```

---

### Task 7: Validate predictions against exe fixtures

**Files:**
- Modify: `tests/testthat/test-harmonic.R`

- [ ] **Step 1: Write prediction validation test**

Append to `tests/testthat/test-harmonic.R`:

```r
test_that("prediction at mesh nodes matches exe Grade.bin within 5%", {
  skip_if_not_installed()

  fixtures_dir <- test_path("fixtures")
  dates_file <- file.path(fixtures_dir, "test_dates.rds")
  skip_if_not(file.exists(dates_file), "Fixtures not generated")

  test_dates <- readRDS(dates_file)

  for (d in test_dates) {
    d <- as.Date(d, origin = "1970-01-01")
    fname <- sprintf("fixture_sepetiba_%s.rds", format(d, "%Y%m%d"))
    fixture <- readRDS(file.path(fixtures_dir, fname))

    our_dt <- rsiscorar:::.predict_at_nodes(d, "sepetiba")

    # For each of our mesh nodes, find nearest Grade.bin node
    grade_dt <- fixture$grade

    # Test at hour 6 and hour 18 (mid-flood and mid-ebb, avoids slack)
    for (test_hour in c(6L, 18L)) {
      our_h <- our_dt[hour == test_hour]
      grade_h <- grade_dt[hour == test_hour]

      # Sample 50 mesh nodes for efficiency
      set.seed(123L)
      sample_idx <- sample(seq_len(nrow(our_h)), min(50L, nrow(our_h)))

      for (i in sample_idx) {
        target_lon <- our_h$lon[i]
        target_lat <- our_h$lat[i]

        # Find nearest Grade.bin node
        dists <- (grade_h$lon - target_lon)^2 + (grade_h$lat - target_lat)^2
        nearest <- which.min(dists)
        nearest_dist <- sqrt(dists[nearest])

        # Only compare if very close (< 0.001 degrees ~ 100m)
        if (nearest_dist > 0.001) next

        our_vel <- our_h$velocity_cm_s[i]
        exe_vel <- grade_h$velocity_cm_s[nearest]

        # Skip near-zero velocities (slack water, ratios meaningless)
        if (exe_vel < 1) next

        pct_diff <- abs(our_vel - exe_vel) / exe_vel
        expect_lt(
          pct_diff, 0.05,
          label = sprintf("Velocity mismatch on %s h=%d node=%d: ours=%.2f exe=%.2f (%.1f%%)",
                          d, test_hour, i, our_vel, exe_vel, pct_diff * 100)
        )

        # Direction within 5 degrees (only when velocity > 5 cm/s)
        if (exe_vel > 5) {
          our_dir <- our_h$direction_deg[i]
          exe_dir <- grade_h$direction_deg[nearest]
          dir_diff <- abs(our_dir - exe_dir)
          dir_diff <- min(dir_diff, 360 - dir_diff)
          expect_lt(
            dir_diff, 5,
            label = sprintf("Direction mismatch on %s h=%d: ours=%.1f exe=%.1f",
                            d, test_hour, our_dir, exe_dir)
          )
        }
      }
    }
  }
})
```

- [ ] **Step 2: Run test**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-harmonic.R")'`
Expected: PASS. If velocity differences exceed 5%, investigate whether the nodal factor or V0+u is the source and fix.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-harmonic.R
git commit -m "test: validate predictions against exe Grade.bin (5% tolerance)"
```

---

### Task 8: Wire predict_currents() to new engine

**Files:**
- Modify: `R/predict.R`
- Modify: `tests/testthat/test-predict.R`

- [ ] **Step 1: Update test expectations for new row counts**

Replace `tests/testthat/test-predict.R`:

```r
skip_if_not_installed <- function() {
  home <- tryCatch(siscorar_home(), error = function(e) "")
  skip_if_not(dir.exists(home), "SISCORAR not installed")
}

test_that("predict_currents returns valid data for sepetiba", {
  skip_if_not_installed()

  dt <- predict_currents(Sys.Date(), "sepetiba")
  expect_s3_class(dt, "data.table")
  expect_true(nrow(dt) > 100)
  expect_true(all(c("lon", "lat", "hour", "velocity_cm_s", "direction_deg",
                     "u_velocity", "v_velocity", "speed_m_s",
                     "datetime") %in% names(dt)))
  expect_equal(sort(unique(dt$hour)), 0:23)
  expect_s3_class(dt$datetime, "POSIXct")
})

test_that("predict_currents_range combines multiple dates", {
  skip_if_not_installed()

  dt <- predict_currents_range("2026-03-30", "2026-03-31", "sepetiba")
  expect_s3_class(dt, "data.table")
  expect_true("date" %in% names(dt))
  expect_equal(length(unique(dt$date)), 2L)
})

test_that("run_prediction still works with Wine", {
  skip_if_not_runnable <- function() {
    home <- tryCatch(siscorar_home(), error = function(e) "")
    skip_if_not(dir.exists(home), "SISCORAR not installed")
    if (.Platform$OS.type != "windows") {
      wine <- tryCatch(rsiscorar:::.detect_wine(), error = function(e) "")
      skip_if_not(nzchar(wine), "Wine not installed")
    }
  }
  skip_if_not_runnable()

  result <- run_prediction(Sys.Date(), "sepetiba")
  expect_type(result, "logical")
})
```

- [ ] **Step 2: Run test to see current state**

Run: `r -e 'devtools::test_active_file("tests/testthat/test-predict.R")'`
Expected: Tests pass but `predict_currents` still uses Wine.

- [ ] **Step 3: Modify `predict_currents()` to use harmonic engine**

In `R/predict.R`, replace the body of `predict_currents()`:

```r
predict_currents <- function(date, area = "guanabara", daylight_saving = FALSE) {
  date <- as.Date(date)
  area <- .validate_area(area)
  dt <- .predict_at_nodes(date, area)

  if (daylight_saving && nrow(dt) > 0L) {
    dt[, datetime := datetime + 3600L]
  }

  dt
}
```

- [ ] **Step 4: Modify `predict_currents_range()` to use harmonic engine**

In `R/predict.R`, replace the body of `predict_currents_range()`:

```r
predict_currents_range <- function(start_date, end_date, area = "guanabara",
                                   daylight_saving = FALSE) {
  area <- .validate_area(area)
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")

  results <- lapply(dates, function(d) {
    dt <- predict_currents(d, area, daylight_saving)
    if (nrow(dt) > 0L) dt[, date := d]
    dt
  })

  rbindlist(results)
}
```

- [ ] **Step 5: Run tests**

Run: `r -e 'devtools::test()'`
Expected: All tests pass. `predict_currents` now uses pure R, no Wine needed.

- [ ] **Step 6: Commit**

```bash
git add R/predict.R tests/testthat/test-predict.R
git commit -m "feat: wire predict_currents() to pure R harmonic engine

predict_currents() and predict_currents_range() no longer require Wine.
run_prediction() kept for direct exe access."
```

---

### Task 9: Update roxygen docs and run devtools::check()

**Files:**
- Modify: `R/predict.R` (roxygen comments)
- Modify: `R/harmonic.R` (no roxygen needed, all internal)

- [ ] **Step 1: Update roxygen for predict_currents()**

In `R/predict.R`, update the `predict_currents` roxygen to note it no longer requires Wine:

Replace the `@details` section in the `predict_currents` roxygen:

```r
#' Predict Currents for a Single Date
#'
#' Computes tidal current predictions using harmonic analysis at
#' computational mesh nodes. Does not require Wine or external executables.
#'
#' @inheritParams run_prediction
#'
#' @return data.table with prediction results. Columns: col, row, lon, lat,
#'   datetime, hour, velocity_cm_s, speed_m_s, direction_deg, u_velocity,
#'   v_velocity.
#'
#' @details
#' Predictions are computed at the computational mesh nodes (~3,000 per area)
#' using 13 tidal harmonic constituents. This is a pure R implementation that
#' does not require Wine or the SISCORAR executables.
#'
#' For the full regular-grid output (~90,000+ nodes), use [run_prediction()]
#' followed by [read_predictions()] (requires Wine on macOS/Linux).
#'
#' @examples
#' \dontrun{
#' dt <- predict_currents(Sys.Date(), "guanabara")
#' dt[which.max(velocity_cm_s)]
#' }
#'
#' @seealso [predict_currents_range()] for multiple dates
#' @export
```

- [ ] **Step 2: Run devtools::document()**

Run: `r -e 'devtools::document()'`
Expected: NAMESPACE and man pages updated.

- [ ] **Step 3: Run devtools::check()**

Run: `r -e 'devtools::check()'`
Expected: 0 errors, 0 warnings, possibly some notes.

- [ ] **Step 4: Fix any issues from check, then commit**

```bash
git add R/ man/ NAMESPACE
git commit -m "docs: update roxygen for pure R predict_currents()"
```
