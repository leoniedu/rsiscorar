# Helper: build a minimal synthetic predict_currents()-style data.table
# 4 grid points (2x2), 24 hours, u/v in cm/s
make_synthetic_dt <- function() {
  base_time <- as.POSIXct("2025-02-25 00:00:00", tz = "America/Sao_Paulo")
  lons <- c(-38.54, -38.545)
  lats <- c(-12.91, -12.915)
  grid <- expand.grid(lon = lons, lat = lats)
  hours <- 0:23

  rows <- lapply(seq_len(nrow(grid)), function(i) {
    data.table::data.table(
      lon          = grid$lon[i],
      lat          = grid$lat[i],
      hour         = as.integer(hours),
      datetime     = base_time + hours * 3600,
      u_velocity   = seq(10, 10 + length(hours) - 1) * (i * 0.5),  # cm/s
      v_velocity   = seq(5,  5  + length(hours) - 1) * (i * 0.3),
      velocity_cm_s = 15,
      speed_m_s    = 0.15,
      direction_deg = 90
    )
  })
  data.table::rbindlist(rows)
}

test_that("write_uv_geojson creates a valid GeoJSON file", {
  skip_if_not_installed("jsonlite")

  dt <- make_synthetic_dt()
  tmp <- withr::local_tempfile(fileext = ".geojson")

  result <- write_uv_geojson(dt, tmp, resolution = 0.01)

  expect_equal(result, tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 0)
})

test_that("write_uv_geojson produces valid FeatureCollection structure", {
  skip_if_not_installed("jsonlite")

  dt <- make_synthetic_dt()
  tmp <- withr::local_tempfile(fileext = ".geojson")
  write_uv_geojson(dt, tmp, resolution = 0.01, area = "test")

  json <- jsonlite::read_json(tmp)

  expect_equal(json$type, "FeatureCollection")
  expect_equal(json$meta$date, "2025-02-25")
  expect_equal(json$meta$area, "test")
  expect_length(json$hours_utc, 24L)
  expect_equal(json$hours_utc[[1]], 0L)
  expect_equal(json$hours_utc[[24]], 23L)
})

test_that("each feature has 24-element u/v/s/d arrays", {
  skip_if_not_installed("jsonlite")

  dt <- make_synthetic_dt()
  tmp <- withr::local_tempfile(fileext = ".geojson")
  write_uv_geojson(dt, tmp, resolution = 0.01)

  json <- jsonlite::read_json(tmp)
  features <- json$features

  expect_gt(length(features), 0L)

  for (f in features) {
    expect_equal(f$type, "Feature")
    expect_equal(f$geometry$type, "Point")
    expect_length(f$geometry$coordinates, 2L)
    expect_length(f$properties$u, 24L)
    expect_length(f$properties$v, 24L)
    expect_length(f$properties$s, 24L)
    expect_length(f$properties$d, 24L)
  }
})

test_that("u/v values are converted from cm/s to m/s", {
  skip_if_not_installed("jsonlite")

  # Single point, single hour for easy verification
  dt <- data.table::data.table(
    lon          = rep(-38.54, 24),
    lat          = rep(-12.91, 24),
    hour         = 0:23,
    datetime     = as.POSIXct("2025-02-25", tz = "America/Sao_Paulo") + 0:23 * 3600,
    u_velocity   = rep(100, 24),   # 100 cm/s = 1 m/s
    v_velocity   = rep(50, 24),    # 50 cm/s  = 0.5 m/s
    velocity_cm_s = 100,
    speed_m_s    = 1,
    direction_deg = 90
  )
  tmp <- withr::local_tempfile(fileext = ".geojson")
  write_uv_geojson(dt, tmp, resolution = 0.01)

  json <- jsonlite::read_json(tmp)
  f <- json$features[[1]]

  expect_equal(unlist(f$properties$u)[[1]], 1.0, tolerance = 1e-4)
  expect_equal(unlist(f$properties$v)[[1]], 0.5, tolerance = 1e-4)
})

test_that("speed is sqrt(u^2 + v^2) and direction is atan2(u,v)", {
  skip_if_not_installed("jsonlite")

  # u=1 m/s (100 cm/s), v=0 → speed=1, direction=90°
  dt <- data.table::data.table(
    lon          = rep(-38.54, 24),
    lat          = rep(-12.91, 24),
    hour         = 0:23,
    datetime     = as.POSIXct("2025-02-25", tz = "America/Sao_Paulo") + 0:23 * 3600,
    u_velocity   = rep(100, 24),
    v_velocity   = rep(0, 24),
    velocity_cm_s = 100,
    speed_m_s    = 1,
    direction_deg = 90
  )
  tmp <- withr::local_tempfile(fileext = ".geojson")
  write_uv_geojson(dt, tmp, resolution = 0.01)

  json <- jsonlite::read_json(tmp)
  f <- json$features[[1]]

  s <- unlist(f$properties$s)[[1]]
  d <- unlist(f$properties$d)[[1]]

  expect_equal(s, 1.0, tolerance = 1e-3)
  expect_equal(d, 90.0, tolerance = 0.1)  # atan2(1,0)*180/pi = 90°
})

test_that("resolution downsampling merges nearby points", {
  skip_if_not_installed("jsonlite")

  # 4 raw points that all round to the same 0.01° cell
  dt <- data.table::data.table(
    lon          = rep(c(-38.541, -38.542), each = 24 * 2),
    lat          = rep(c(-12.911, -12.912), 24 * 2),
    hour         = rep(0:23, 4L),
    datetime     = rep(as.POSIXct("2025-02-25", tz = "America/Sao_Paulo") + 0:23 * 3600, 4L),
    u_velocity   = rep(100, 24 * 4),
    v_velocity   = rep(0, 24 * 4),
    velocity_cm_s = 100,
    speed_m_s    = 1,
    direction_deg = 90
  )
  tmp <- withr::local_tempfile(fileext = ".geojson")
  write_uv_geojson(dt, tmp, resolution = 0.01)

  json <- jsonlite::read_json(tmp)
  # All 4 raw points round to the same cell → 1 feature
  expect_equal(length(json$features), 1L)
})

test_that("write_uv_geojson u/v match grib-equivalent aggregation", {
  skip_if_not_installed("jsonlite")

  # Verify the GeoJSON averages u/v consistently with how write_grib does it:
  # grib: mean(u_velocity) / 100  (cm/s → m/s)
  dt <- data.table::data.table(
    lon          = rep(c(-38.540, -38.541), each = 24),
    lat          = rep(-12.91, 24 * 2),
    hour         = rep(0:23, 2),
    datetime     = rep(as.POSIXct("2025-02-25", tz = "America/Sao_Paulo") + 0:23 * 3600, 2),
    u_velocity   = c(rep(100, 24), rep(200, 24)),  # cm/s; two points per cell at 0.01° res
    v_velocity   = c(rep(0, 24),   rep(0, 24)),
    velocity_cm_s = 100,
    speed_m_s    = 1,
    direction_deg = 90
  )
  tmp <- withr::local_tempfile(fileext = ".geojson")
  write_uv_geojson(dt, tmp, resolution = 0.01)

  json <- jsonlite::read_json(tmp)
  f <- json$features[[1]]

  # Expected: mean(100, 200) / 100 = 1.5 m/s
  expect_equal(unlist(f$properties$u)[[1]], 1.5, tolerance = 1e-4)
})

test_that("write_uv_geojson works with SISCORAR predict_currents output", {
  skip_if_not_installed("jsonlite")

  home <- tryCatch(siscorar_home(), error = function(e) "")
  skip_if_not(dir.exists(home), "SISCORAR not installed")
  if (.Platform$OS.type != "windows") {
    wine <- tryCatch(rsiscorar:::.detect_wine(), error = function(e) "")
    skip_if_not(nzchar(wine), "Wine not installed")
  }

  dt <- predict_currents(as.Date("2025-02-25"), "baiatos")
  skip_if(nrow(dt) == 0, "No prediction data returned")

  tmp <- withr::local_tempfile(fileext = ".geojson")
  write_uv_geojson(dt, tmp, resolution = 0.005, area = "baiatos")

  json <- jsonlite::read_json(tmp)

  expect_equal(json$type, "FeatureCollection")
  expect_gt(length(json$features), 50L)

  # Spot-check: speeds should be physically plausible (< 5 m/s)
  for (f in json$features[seq_len(min(10, length(json$features)))]) {
    speeds <- unlist(f$properties$s)
    expect_true(all(speeds < 5, na.rm = TRUE))
  }
})
