skip_if_not_installed <- function() {
  home <- tryCatch(siscorar_home(), error = function(e) "")
  skip_if_not(dir.exists(home), "SISCORAR not installed")
}

test_that("read_grid returns valid data.table", {
  skip_if_not_installed()

  grid <- read_grid("guanabara")
  expect_s3_class(grid, "data.table")
  expect_true(all(c("col", "row", "lon", "lat", "active") %in% names(grid)))
  expect_true(nrow(grid) > 0)
  expect_true(any(grid$active))
  expect_true(any(!grid$active))
})

test_that("read_grid works for all areas", {
  skip_if_not_installed()

  for (area in siscorar_areas()) {
    area_path <- file.path(siscorar_home(), "arquivos", "areas", area)
    grade_file <- file.path(area_path, "Grade.bin")
    skip_if_not(file.exists(grade_file), paste("Grade.bin missing for", area))

    grid <- read_grid(area)
    expect_s3_class(grid, "data.table")
    expect_true(nrow(grid) > 0, label = paste(area, "has nodes"))
  }
})

test_that("read_constituents returns 142 constituents", {
  skip_if_not_installed()

  cons <- read_constituents("guanabara")
  expect_s3_class(cons, "data.table")
  expect_equal(nrow(cons), 142)
  expect_true(all(c("index", "name", "frequency_scaled", "frequency_deg_hour") %in% names(cons)))
  expect_true(all(cons$frequency_deg_hour >= 0))
})

test_that("read_predictions returns valid data.table", {
  skip_if_not_installed()

  area_path <- file.path(siscorar_home(), "arquivos", "areas", "sepetiba")
  grade_file <- file.path(area_path, "Grade.bin")
  skip_if_not(file.exists(grade_file), "Grade.bin missing for sepetiba")

  dt <- read_predictions("sepetiba")
  expect_s3_class(dt, "data.table")

  if (nrow(dt) > 0) {
    expected_cols <- c("col", "row", "lon", "lat", "datetime", "hour",
                       "velocity_cm_s", "speed_m_s", "direction_deg",
                       "u_velocity", "v_velocity")
    expect_true(all(expected_cols %in% names(dt)))
    expect_true(all(dt$hour >= 0 & dt$hour <= 23))
    expect_s3_class(dt$datetime, "POSIXct")
    expect_equal(dt$speed_m_s, dt$velocity_cm_s / 100)
  }
})

test_that("area_info runs without error", {
  skip_if_not_installed()
  expect_invisible(area_info("guanabara"))
})
