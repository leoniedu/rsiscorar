#' Get SISCORAR installation path
#'
#' Searches in order:
#' 1. R option `siscorar.home`
#' 2. Environment variable `SISCORAR_HOME`
#' 3. Default `~/bin/siscorar-5.0`
#'
#' @return Character scalar: path to SISCORAR installation root.
#' @export
#'
#' @examples
#' \dontrun{
#' siscorar_home()
#'
#' # Override for session
#' options(siscorar.home = "/opt/siscorar")
#' siscorar_home()
#' }
siscorar_home <- function() {
  path <- getOption(
    "siscorar.home",
    default = Sys.getenv("SISCORAR_HOME", unset = "")
  )
  if (nzchar(path)) {
    path <- normalizePath(path, mustWork = FALSE)
    if (!dir.exists(path)) {
      cli::cli_abort(c(
        "SISCORAR installation not found at {.path {path}}.",
        "i" = "Set {.envvar SISCORAR_HOME} or {.code options(siscorar.home = ...)} to the correct path.",
        "i" = "Or run {.code siscorar_download_data()} to download data files automatically."
      ))
    }
    return(path)
  }
  # Default path
  default_path <- normalizePath("~/bin/siscorar-5.0", mustWork = FALSE)
  if (dir.exists(default_path)) {
    return(default_path)
  }
  cli::cli_abort(c(
    "SISCORAR data files not found.",
    "i" = "Run {.code siscorar_download_data()} to download automatically.",
    "i" = "Or set {.envvar SISCORAR_HOME} to an existing installation directory."
  ))
}

#' Download SISCORAR Data Files
#'
#' Downloads harmonic constants (U.bin, V.bin) and supporting files
#' (ConsNovas.txt, Interp.bin) for the requested bay areas from the official
#' SISCORAR distribution ZIP hosted by the Brazilian Navy.
#'
#' This is the recommended first step for new users. Only the files needed
#' for [predict_currents()] are downloaded — the Windows prediction
#' executables are not fetched.
#'
#' @param areas Character vector of bay names to download. Defaults to all
#'   five areas. See [siscorar_areas()].
#' @param destdir Character: destination directory. Defaults to
#'   `~/bin/siscorar-5.0` (the default [siscorar_home()] path).
#' @param overwrite Logical: re-download and overwrite existing files?
#'   Default `FALSE` skips areas whose U.bin already exists.
#'
#' @return Invisibly returns the destination directory path.
#'
#' @details
#' Downloads from:
#' `https://www.marinha.mil.br/chm/sites/www.marinha.mil.br.chm/files/siscorar-5.0.zip`
#'
#' The ZIP is downloaded to a temporary file and deleted after extraction.
#' Files extracted per area: `U.bin`, `V.bin`, `ConsNovas.txt`, `Interp.bin`.
#'
#' @seealso [predict_currents()], [siscorar_sitrep()]
#'
#' @examples
#' \dontrun{
#' # Download all areas (first-time setup)
#' siscorar_download_data()
#'
#' # Download specific areas only
#' siscorar_download_data(areas = c("sepetiba", "guanabara"))
#'
#' # Download to a custom location
#' siscorar_download_data(destdir = "/opt/siscorar-5.0")
#' }
#'
#' @export
siscorar_download_data <- function(areas = SISCORAR_AREAS,
                                   destdir = normalizePath("~/bin/siscorar-5.0",
                                                           mustWork = FALSE),
                                   overwrite = FALSE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg piggyback} is required to download data files.",
      "i" = "Install with {.code install.packages('piggyback')}"
    ))
  }

  areas <- vapply(areas, .validate_area, character(1L))

  if (!overwrite) {
    already_done <- vapply(areas, function(a) {
      file.exists(file.path(destdir, "arquivos", "areas", a, "U.bin"))
    }, logical(1L))
    if (all(already_done)) {
      cli::cli_inform(
        "All requested areas already present. Use {.code overwrite = TRUE} to re-download."
      )
      return(invisible(destdir))
    }
    areas <- areas[!already_done]
  }

  repo <- "leoniedu/rsiscorar"
  tag  <- "siscorar-data"
  tmp  <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  for (area in areas) {
    asset <- paste0(area, ".tar.gz")
    cli::cli_inform("Downloading {.val {area}} data...")
    piggyback::pb_download(
      file  = asset,
      dest  = tmp,
      repo  = repo,
      tag   = tag,
      overwrite = TRUE
    )
    tarball <- file.path(tmp, asset)
    area_dir <- file.path(destdir, "arquivos", "areas", area)
    dir.create(area_dir, recursive = TRUE, showWarnings = FALSE)
    utils::untar(tarball, exdir = area_dir)
    cli::cli_alert_success("{.val {area}}: extracted to {.path {area_dir}}")
  }

  cli::cli_inform(
    "Run {.code predict_currents(Sys.Date(), {.val {areas[1L]}})} to verify."
  )
  invisible(destdir)
}

#' Set SISCORAR installation path for the session
#'
#' @param path Character scalar: path to SISCORAR root directory.
#'
#' @return Invisibly returns the previous value.
#' @export
#'
#' @examples
#' \dontrun{
#' old <- set_siscorar_home("/opt/siscorar-5.0")
#' }
set_siscorar_home <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(path)) {
    cli::cli_abort("Directory not found: {.path {path}}")
  }
  old <- getOption("siscorar.home")
  options(siscorar.home = path)
  cli::cli_inform("SISCORAR home set to {.path {path}}")
  invisible(old)
}

#' Get Wine executable path
#'
#' Searches in order:
#' 1. R option `siscorar.wine`
#' 2. Environment variable `SISCORAR_WINE`
#' 3. Auto-detection at common paths
#'
#' On Windows, returns `NULL` (Wine not needed).
#'
#' @return Character scalar: path to Wine executable, or `NULL` on Windows.
#' @export
#'
#' @examples
#' \dontrun{
#' siscorar_wine()
#'
#' # Override Wine path
#' Sys.setenv(SISCORAR_WINE = "/opt/homebrew/bin/wine64")
#' }
siscorar_wine <- function() {
  if (.Platform$OS.type == "windows") return(NULL)
  .detect_wine()
}

#' Validate SISCORAR setup
#'
#' Checks that the SISCORAR installation, area directories, executables,
#' and Wine (on non-Windows) are all properly configured. Prints a
#' diagnostic report.
#'
#' @return Invisibly returns a list with check results.
#' @export
#'
#' @examples
#' \dontrun{
#' siscorar_sitrep()
#' }
siscorar_sitrep <- function() {
  cli::cli_h1("SISCORAR Setup Report")
  checks <- list(home = FALSE, wine = FALSE, areas = list())

  # Check SISCORAR home
  home <- tryCatch(siscorar_home(), error = function(e) NULL)
  if (is.null(home)) {
    cli::cli_alert_danger("SISCORAR data files: {.emph not found}")
    cli::cli_bullets(c(
      "i" = "Run {.code siscorar_download_data()} to download automatically.",
      "i" = "Or set {.envvar SISCORAR_HOME} to an existing installation directory."
    ))
    return(invisible(checks))
  }
  cli::cli_alert_success("SISCORAR home: {.path {home}}")
  checks$home <- TRUE

  # Check Wine (non-Windows only)
  if (.Platform$OS.type != "windows") {
    wine <- tryCatch(.detect_wine(), error = function(e) NULL)
    if (is.null(wine)) {
      cli::cli_alert_danger("Wine: {.emph not found}")
      cli::cli_bullets(c(
        "i" = "Install with: {.code brew install wine-stable}",
        "i" = "Or set {.envvar SISCORAR_WINE} to the Wine executable path"
      ))
    } else {
      cli::cli_alert_success("Wine: {.path {wine}}")
      checks$wine <- TRUE
    }
  } else {
    cli::cli_alert_success("Platform: Windows (no Wine needed)")
    checks$wine <- TRUE
  }

  # Check each area
  cli::cli_h2("Areas")
  for (area in SISCORAR_AREAS) {
    area_path <- file.path(home, "arquivos", "areas", area)
    exec_name <- .exec_names[[area]]
    exec_path <- file.path(area_path, exec_name)
    grade_path <- file.path(area_path, "Grade.bin")
    cons_path <- file.path(area_path, "ConsNovas.txt")

    area_ok <- TRUE
    details <- character()

    if (!dir.exists(area_path)) {
      cli::cli_alert_danger("{.val {area}}: directory not found")
      area_ok <- FALSE
    } else {
      u_ok <- file.exists(file.path(area_path, "U.bin"))
      v_ok <- file.exists(file.path(area_path, "V.bin"))
      if (u_ok && v_ok) {
        details <- c(details, "U.bin+V.bin")
      } else {
        details <- c(details, "!U.bin/V.bin")
        area_ok <- FALSE
      }
      if (file.exists(exec_path)) {
        details <- c(details, "exe")
      }
      if (file.exists(grade_path)) {
        age <- difftime(Sys.time(), file.info(grade_path)$mtime, units = "days")
        details <- c(details, sprintf("Grade.bin (%.0fd old)", age))
      }
      if (file.exists(cons_path)) {
        details <- c(details, "ConsNovas.txt")
      }

      if (area_ok) {
        cli::cli_alert_success("{.val {area}}: {paste(details, collapse = ', ')}")
      } else {
        cli::cli_alert_warning("{.val {area}}: {paste(details, collapse = ', ')} -- run {.code siscorar_download_data({.val {area}})}")
      }
    }
    checks$areas[[area]] <- area_ok
  }

  # Check optional tools
  cli::cli_h2("Optional tools")
  cdo_ok <- system2("which", "cdo", stdout = FALSE, stderr = FALSE) == 0L
  if (cdo_ok) {
    cli::cli_alert_success("CDO: available (GRIB2 export)")
  } else {
    cli::cli_alert_info("CDO: not found (needed for GRIB2 export)")
  }

  grib_set_ok <- system2("which", "grib_set", stdout = FALSE, stderr = FALSE) == 0L
  if (grib_set_ok) {
    cli::cli_alert_success("ecCodes (grib_set): available")
  } else {
    cli::cli_alert_info("ecCodes: not found (needed for GRIB2 parameter codes)")
  }

  ncdf4_ok <- requireNamespace("ncdf4", quietly = TRUE)
  if (ncdf4_ok) {
    cli::cli_alert_success("ncdf4: installed")
  } else {
    cli::cli_alert_info("ncdf4: not installed (needed for NetCDF/GRIB2 export)")
  }

  invisible(checks)
}
