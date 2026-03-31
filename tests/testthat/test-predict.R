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
  home <- tryCatch(siscorar_home(), error = function(e) "")
  skip_if_not(dir.exists(home), "SISCORAR not installed")
  if (.Platform$OS.type != "windows") {
    wine <- tryCatch(rsiscorar:::.detect_wine(), error = function(e) "")
    skip_if_not(nzchar(wine), "Wine not installed")
  }

  result <- run_prediction(Sys.Date(), "sepetiba")
  expect_type(result, "logical")
})
