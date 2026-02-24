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
        "i" = "Download from {.url https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare}"
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
    "SISCORAR installation not found.",
    "i" = "Download from {.url https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare}",
    "i" = "Then set {.envvar SISCORAR_HOME} to the installation directory.",
    "i" = "Or install to the default location: {.path ~/bin/siscorar-5.0}"
  ))
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
    cli::cli_alert_danger("SISCORAR installation: {.emph not found}")
    cli::cli_bullets(c(
      "i" = "Download from {.url https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare}",
      "i" = "Set {.envvar SISCORAR_HOME} or {.code options(siscorar.home = \"/path/to/siscorar\")}"
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
      if (file.exists(exec_path)) {
        details <- c(details, "exec")
      } else {
        details <- c(details, "!exec")
        area_ok <- FALSE
      }
      if (file.exists(grade_path)) {
        age <- difftime(Sys.time(), file.info(grade_path)$mtime, units = "days")
        details <- c(details, sprintf("Grade.bin (%.0fd old)", age))
      }
      if (file.exists(cons_path)) {
        details <- c(details, "constituents")
      }

      if (area_ok) {
        cli::cli_alert_success("{.val {area}}: {paste(details, collapse = ', ')}")
      } else {
        cli::cli_alert_warning("{.val {area}}: {paste(details, collapse = ', ')}")
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
