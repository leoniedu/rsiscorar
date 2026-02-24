SISCORAR_AREAS <- c("guanabara", "sepetiba", "paranagua", "santos", "baiatos")

# Prediction executable names per area (actual filenames on disk)
.exec_names <- list(
  guanabara = "PrevMarBG.exe",
  sepetiba  = "PrevMarSEP.exe",
  paranagua = "PrevMarPar.exe",
  santos    = "PrevMarSan.exe",
  baiatos   = "PrevMarBTS.exe"
)

# Executable architecture (PE32 = 32-bit, PE32+ = 64-bit)
.exec_arch <- list(
  guanabara = "PE32",
  sepetiba  = "PE32",
  paranagua = "PE32",
  santos    = "PE32",
  baiatos   = "PE32+"
)

#' Available SISCORAR bay areas
#'
#' `SISCORAR_AREAS` is a character vector of the 5 Brazilian coastal bays
#' supported by SISCORAR. `siscorar_areas()` returns the same vector.
#'
#' The supported areas are:
#' - **guanabara**: Guanabara Bay, Rio de Janeiro (~290,000 nodes)
#' - **sepetiba**: Sepetiba Bay, Rio de Janeiro (~5,400 nodes)
#' - **paranagua**: Paranagua Bay, Parana (~8,700 nodes)
#' - **santos**: Santos Bay, Sao Paulo (~8,900 nodes)
#' - **baiatos**: Baia de Todos os Santos, Salvador (~6,300 nodes)
#'
#' @return Character vector of area names.
#' @rdname siscorar_areas
#' @export
#'
#' @examples
#' siscorar_areas()
#' SISCORAR_AREAS
siscorar_areas <- function() {
  SISCORAR_AREAS
}

#' @rdname siscorar_areas
#' @usage SISCORAR_AREAS
#' @export
"SISCORAR_AREAS"

#' Get filesystem path for a bay area
#'
#' @param area Character: bay name (case-insensitive). One of:
#'   `"guanabara"`, `"sepetiba"`, `"paranagua"`, `"santos"`, `"baiatos"`.
#'
#' @return Character: full path to area directory.
#' @export
get_area_path <- function(area) {
  area <- .validate_area(area)
  file.path(siscorar_home(), "arquivos", "areas", area)
}

#' Get prediction executable name for an area
#'
#' @inheritParams get_area_path
#'
#' @return Character: executable filename.
#' @export
get_exec_name <- function(area) {
  area <- .validate_area(area)
  .exec_names[[area]]
}
