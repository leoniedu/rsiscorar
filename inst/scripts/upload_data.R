#!/usr/bin/env Rscript
# Upload SISCORAR harmonic data files to GitHub releases via piggyback.
# Run once whenever data files are updated (new SISCORAR version, etc.).
#
# Requires: SISCORAR installation, piggyback, GitHub PAT with repo scope.
#
# Usage: Rscript inst/scripts/upload_data.R

devtools::load_all(".")

REPO <- "leoniedu/rsiscorar"
TAG  <- "siscorar-data"
DATA_FILES <- c("U.bin", "V.bin", "ConsNovas.txt", "Interp.bin")

# Create release if it doesn't exist (requires gh CLI)
existing <- system2("gh", c("release", "view", TAG, "--repo", REPO),
                    stdout = FALSE, stderr = FALSE)
if (existing != 0L) {
  cat("Creating release", TAG, "\n")
  system2("gh", c("release", "create", TAG,
                  "--repo", REPO,
                  "--title", "SISCORAR harmonic data files",
                  "--notes", paste(
                    "Harmonic constants (U.bin, V.bin), constituent catalog",
                    "(ConsNovas.txt), and interpolation weights (Interp.bin)",
                    "for all 5 SISCORAR bay areas.\n\n",
                    "Source: SISCORAR 5.0, Brazilian Navy (DHN/REMO).",
                    "https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare"
                  )))
}

tmp <- tempfile()
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE))

for (area in SISCORAR_AREAS) {
  area_path <- get_area_path(area)
  tarball   <- file.path(tmp, paste0(area, ".tar.gz"))
  files     <- file.path(area_path, DATA_FILES)
  files     <- files[file.exists(files)]

  if (length(files) == 0L) {
    warning("No data files for area: ", area, " — skipping")
    next
  }

  cat(sprintf("Packing %s (%d files)...\n", area, length(files)))
  system2("tar", c("-czf", tarball, "-C", area_path, basename(files)))
  cat(sprintf("Uploading %s (%.1f MB)...\n", basename(tarball),
              file.info(tarball)$size / 1e6))
  system2("gh", c("release", "upload", TAG, tarball,
                  "--repo", REPO, "--clobber"))
}

cat("\nDone. Assets at https://github.com/", REPO, "/releases/tag/", TAG, "\n")
cat("Users can now run siscorar_download_data() to install.\n")
