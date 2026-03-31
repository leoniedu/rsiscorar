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
