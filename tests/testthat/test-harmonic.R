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

  n_nodes <- length(unique(dt$node))
  expect_true(n_nodes > 1000)
  expect_equal(nrow(dt), n_nodes * 13L)

  expected_indices <- c(12L, 45L, 19L, 34L, 67L, 10L, 47L, 39L, 17L, 71L, 76L, 94L, 114L)
  expect_equal(sort(unique(dt$constituent)), sort(expected_indices))

  expect_true(all(dt$lon > -50 & dt$lon < -30))
  expect_true(all(dt$lat > -30 & dt$lat < -5))
  expect_true(all(dt$u_amplitude >= 0))
  expect_true(all(dt$v_amplitude >= 0))
  expect_true(all(dt$u_phase >= 0 & dt$u_phase < 360))
  expect_true(all(dt$v_phase >= 0 & dt$v_phase < 360))
})

test_that(".compute_v0u returns 13 values in 0-360 range", {
  v0u <- rsiscorar:::.compute_v0u(as.Date("2026-03-31"))
  expect_length(v0u, 13L)
  expect_true(all(v0u >= 0 & v0u < 360))

  # S2 should always be 0 at midnight
  s2_idx <- which(rsiscorar:::.CONSTITUENTS$name == "S2")
  expect_equal(v0u[s2_idx], 0, tolerance = 0.5)
})

test_that(".compute_nodal_factors returns 13 positive values near 1", {
  f <- rsiscorar:::.compute_nodal_factors(as.Date("2026-03-31"))
  expect_length(f, 13L)
  expect_true(all(f > 0))
  expect_true(all(f > 0.5 & f < 2.0))

  s2_idx <- which(rsiscorar:::.CONSTITUENTS$name == "S2")
  p1_idx <- which(rsiscorar:::.CONSTITUENTS$name == "P1")
  expect_equal(f[s2_idx], 1.0)
  expect_equal(f[p1_idx], 1.0)
})

test_that(".predict_at_nodes returns correct structure", {
  skip_if_not_installed()

  dt <- rsiscorar:::.predict_at_nodes(as.Date("2026-03-31"), "sepetiba")
  expect_s3_class(dt, "data.table")

  expected_cols <- c("col", "row", "lon", "lat", "datetime", "hour",
                     "velocity_cm_s", "speed_m_s", "direction_deg",
                     "u_velocity", "v_velocity")
  expect_true(all(expected_cols %in% names(dt)))

  n_nodes <- length(unique(dt[, paste(lon, lat)]))
  expect_true(n_nodes > 1000)
  expect_equal(nrow(dt), n_nodes * 24L)
  expect_equal(sort(unique(dt$hour)), 0:23)
  expect_true(all(dt$velocity_cm_s >= 0))
  expect_true(all(dt$direction_deg >= 0 & dt$direction_deg < 360))
  expect_equal(dt$speed_m_s, dt$velocity_cm_s / 100)
  expect_s3_class(dt$datetime, "POSIXct")
  expect_equal(attr(dt$datetime[1], "tzone"), "America/Sao_Paulo")
})

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

    # FatNod order matches .CONSTITUENTS order
    exe_v0u <- fixture$v0u$v0u_deg

    for (i in seq_along(our_v0u)) {
      diff <- abs(our_v0u[i] - exe_v0u[i])
      diff <- min(diff, 360 - diff)
      expect_lt(
        diff, 0.5,
        label = sprintf("V0+u mismatch for constituent %d on %s: ours=%.2f exe=%.2f",
                        rsiscorar:::.CONSTITUENTS$index[i], d, our_v0u[i], exe_v0u[i])
      )
    }
  }
})
