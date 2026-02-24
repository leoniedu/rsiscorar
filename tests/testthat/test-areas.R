test_that("siscorar_areas returns all 5 areas", {
  areas <- siscorar_areas()
  expect_length(areas, 5)
  expect_true("guanabara" %in% areas)
  expect_true("sepetiba" %in% areas)
  expect_true("paranagua" %in% areas)
  expect_true("santos" %in% areas)
  expect_true("baiatos" %in% areas)
})

test_that("SISCORAR_AREAS matches siscorar_areas()", {
  expect_identical(SISCORAR_AREAS, siscorar_areas())
})

test_that(".validate_area rejects invalid area", {
  expect_error(.validate_area("invalid"), "Invalid area")
  expect_error(.validate_area(""), "Invalid area")
})

test_that(".validate_area is case-insensitive", {
  expect_equal(.validate_area("GUANABARA"), "guanabara")
  expect_equal(.validate_area("Sepetiba"), "sepetiba")
  expect_equal(.validate_area("BAIATOS"), "baiatos")
})

test_that("get_exec_name returns correct filenames", {
  withr::local_options(siscorar.home = tempdir())
  expect_equal(get_exec_name("guanabara"), "PrevMarBG.exe")
  expect_equal(get_exec_name("sepetiba"), "PrevMarSEP.exe")
  expect_equal(get_exec_name("paranagua"), "PrevMarPar.exe")
  expect_equal(get_exec_name("santos"), "PrevMarSan.exe")
  expect_equal(get_exec_name("baiatos"), "PrevMarBTS.exe")
})

test_that("get_exec_name is case-insensitive", {
  withr::local_options(siscorar.home = tempdir())
  expect_equal(get_exec_name("GUANABARA"), "PrevMarBG.exe")
})

test_that("get_exec_name errors on invalid area", {
  expect_error(get_exec_name("invalid"), "Invalid area")
})

test_that(".exec_arch identifies baiatos as 64-bit", {
  expect_equal(.exec_arch[["baiatos"]], "PE32+")
  expect_equal(.exec_arch[["guanabara"]], "PE32")
  expect_equal(.exec_arch[["sepetiba"]], "PE32")
  expect_equal(.exec_arch[["paranagua"]], "PE32")
  expect_equal(.exec_arch[["santos"]], "PE32")
})
