test_that("get_current_at_point returns nearest point", {
  dt <- data.table::data.table(
    lon = c(-43.1, -43.2, -43.3),
    lat = c(-22.8, -22.9, -23.0),
    hour = c(0L, 0L, 0L),
    velocity_cm_s = c(10, 20, 30),
    direction_deg = c(45, 90, 135)
  )

  result <- get_current_at_point(-43.15, -22.85, dt, target_hour = 0L)
  expect_equal(nrow(result), 1)
  expect_equal(result$lon, -43.1)
  expect_equal(result$lat, -22.8)
})

test_that("get_current_at_point filters by hour", {
  dt <- data.table::data.table(
    lon = c(-43.1, -43.1, -43.2, -43.2),
    lat = c(-22.8, -22.8, -22.9, -22.9),
    hour = c(0L, 12L, 0L, 12L),
    velocity_cm_s = c(10, 50, 20, 60),
    direction_deg = c(45, 90, 135, 180)
  )

  result0 <- get_current_at_point(-43.1, -22.8, dt, target_hour = 0L)
  result12 <- get_current_at_point(-43.1, -22.8, dt, target_hour = 12L)

  expect_equal(result0$velocity_cm_s, 10)
  expect_equal(result12$velocity_cm_s, 50)
})

test_that("get_current_at_point handles single-row data", {
  dt <- data.table::data.table(
    lon = -43.1,
    lat = -22.8,
    hour = 0L,
    velocity_cm_s = 10,
    direction_deg = 45
  )

  result <- get_current_at_point(-50.0, -10.0, dt, target_hour = 0L)
  expect_equal(nrow(result), 1)
  expect_equal(result$lon, -43.1)
})
