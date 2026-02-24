#' Auto-detect Wine executable path
#'
#' Checks option/envvar, then `Sys.which()`, then common installation paths.
#'
#' @return Character: path to Wine executable.
#' @noRd
.detect_wine <- function() {
  # Check option/envvar first
  wine <- getOption(
    "siscorar.wine",
    default = Sys.getenv("SISCORAR_WINE", unset = "")
  )
  if (nzchar(wine) && file.exists(wine)) return(wine)

  # Try Sys.which
  wine_path <- Sys.which("wine")
  if (nzchar(wine_path)) return(unname(wine_path))

  # Common paths
  candidates <- c(
    "/usr/bin/wine",
    "/usr/local/bin/wine",
    "/opt/homebrew/bin/wine",
    "/opt/homebrew/bin/wine64",
    "/Applications/Wine Stable.app/Contents/Resources/wine/bin/wine"
  )
  for (candidate in candidates) {
    if (file.exists(candidate)) return(candidate)
  }

  cli::cli_abort(c(
    "Wine not found.",
    "i" = "Install with: {.code brew install wine-stable}",
    "i" = "Or set {.envvar SISCORAR_WINE} to the Wine executable path."
  ))
}

#' Determine the correct Wine command for an area
#'
#' baiatos uses PE32+ (64-bit), requiring wine64; others use PE32 (32-bit).
#' Modern Wine (8+) handles both via a single `wine` binary on most platforms.
#'
#' @param area Character: bay name.
#' @return Character: path to appropriate wine binary.
#' @noRd
.wine_command_for_area <- function(area) {
  if (.Platform$OS.type == "windows") return(NULL)

  area <- tolower(area)
  arch <- .exec_arch[[area]]
  wine_base <- .detect_wine()

  if (arch == "PE32+") {
    # Try wine64 first for 64-bit executables
    wine64 <- sub("wine$", "wine64", wine_base)
    if (file.exists(wine64)) return(wine64)
  }

  wine_base
}
