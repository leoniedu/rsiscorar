test_that("siscorar_home respects R option", {
  withr::local_options(siscorar.home = tempdir())
  withr::local_envvar(SISCORAR_HOME = NA)
  expect_equal(siscorar_home(), normalizePath(tempdir()))
})

test_that("siscorar_home respects environment variable", {
  withr::local_options(siscorar.home = NULL)
  withr::local_envvar(SISCORAR_HOME = tempdir())
  expect_equal(siscorar_home(), normalizePath(tempdir()))
})

test_that("siscorar_home option takes precedence over envvar", {
  withr::local_options(siscorar.home = tempdir())
  withr::local_envvar(SISCORAR_HOME = "/some/other/path")
  expect_equal(siscorar_home(), normalizePath(tempdir()))
})

test_that("siscorar_home errors when path does not exist", {
  withr::local_options(siscorar.home = NULL)
  withr::local_envvar(SISCORAR_HOME = "/nonexistent/path/siscorar")
  expect_error(siscorar_home(), "not found")
})

test_that("set_siscorar_home sets option", {
  withr::local_options(siscorar.home = NULL)
  old <- set_siscorar_home(tempdir())
  expect_equal(getOption("siscorar.home"), normalizePath(tempdir()))
})

test_that("set_siscorar_home errors on missing directory", {
  expect_error(set_siscorar_home("/nonexistent"), "not found")
})

test_that("siscorar_wine returns NULL on Windows", {
  skip_on_os(c("mac", "linux", "solaris"))
  expect_null(siscorar_wine())
})
