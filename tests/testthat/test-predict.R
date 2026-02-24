skip_if_not_runnable <- function() {
  home <- tryCatch(siscorar_home(), error = function(e) "")
  skip_if_not(dir.exists(home), "SISCORAR not installed")

  if (.Platform$OS.type != "windows") {
    wine <- tryCatch(.detect_wine(), error = function(e) "")
    skip_if_not(nzchar(wine), "Wine not installed")
  }
}

test_that("predict_currents returns valid data for sepetiba", {
  skip_if_not_runnable()

  # sepetiba is the smallest area (~5,400 nodes)
  dt <- predict_currents(Sys.Date(), "sepetiba")
  expect_s3_class(dt, "data.table")

  if (nrow(dt) > 0) {
    expect_true(all(c("lon", "lat", "hour", "velocity_cm_s", "direction_deg",
                       "u_velocity", "v_velocity") %in% names(dt)))
    expect_true(nrow(dt) > 100)
  }
})

test_that("run_prediction returns logical", {
  skip_if_not_runnable()

  result <- run_prediction(Sys.Date(), "sepetiba")
  expect_type(result, "logical")
})
