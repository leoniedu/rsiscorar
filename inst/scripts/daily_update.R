#!/usr/bin/env Rscript
# =============================================================================
# SISCORAR Daily GRIB Update
#
# Generates rolling X-day forecast GRIBs and uploads to GitHub releases.
# Run via cron:
#   0 6 * * * Rscript /path/to/daily_update.R
# =============================================================================

library(rsiscorar)
library(piggyback)
# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

FORECAST_DAYS <- 7
LOOKBACK_DAYS <- 7
AREAS <- c("baiatos")
RESOLUTION <- 0.001
RESOLUTION_STR <- sprintf("%03d", RESOLUTION * 1000)
OUTPUT_DIR <- "output/gribs"
GITHUB_REPO <- "leoniedu/siscorar_gribs"
UPLOAD_TO_GITHUB <- nzchar(GITHUB_REPO)
CLEANUP_OLD <- TRUE
release_tag <- RESOLUTION_STR


try(piggyback::pb_new_release(repo = GITHUB_REPO, tag = release_tag), silent = TRUE)


# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

cli::cli_h1("SISCORAR Daily Update - {Sys.time()}")


if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

from <- Sys.Date() - LOOKBACK_DAYS

dates <- seq(from, Sys.Date() + FORECAST_DAYS - 1, by = "day")
cli::cli_inform(
  "Generating forecasts: {min(dates)} to {max(dates)} ({length(dates)} days)"
)
cli::cli_inform("Areas: {paste(AREAS, collapse = ', ')}")

# -----------------------------------------------------------------------------
# Check GitHub for existing files
# -----------------------------------------------------------------------------

github_files <- character(0)


if (UPLOAD_TO_GITHUB && requireNamespace("piggyback", quietly = TRUE)) {
  cli::cli_h2("Checking GitHub releases")

  tryCatch(
    {
      existing <- piggyback::pb_list(repo = GITHUB_REPO, tag = release_tag)

      if (nrow(existing) > 0) {
        github_files <- existing$file_name
        cli::cli_inform("Found {length(github_files)} file(s) on GitHub")
      }
    },
    error = function(e) {
      cli::cli_warn("Could not check GitHub: {e$message}")
    }
  )
}

# -----------------------------------------------------------------------------
# Generate GRIBs
# -----------------------------------------------------------------------------

generated_files <- character(0)

for (area in AREAS) {
  cli::cli_h2("Processing: {toupper(area)}")

  for (d in dates) {
    d <- as.Date(d, origin = "1970-01-01")
    output_file <- file.path(
      OUTPUT_DIR,
      sprintf("%s_%s_%s.grib2", area, format(d, "%Y%m%d"), RESOLUTION_STR)
    )

    # Skip if already on GitHub
    if (basename(output_file) %in% github_files) {
      cli::cli_inform("  {basename(output_file)}: already on GitHub, skipping")
      next
    }

    # Skip if local file exists
    if (file.exists(output_file)) {
      cli::cli_inform("  {basename(output_file)}: exists locally, skipping")
      generated_files <- c(generated_files, output_file)
      next
    }

    cli::cli_inform("  {format(d, '%Y-%m-%d')}: generating...")

    tryCatch(
      {
        dt <- predict_currents(d, area)

        if (nrow(dt) > 0) {
          write_grib(dt, output_file, hours = 0:23, resolution = RESOLUTION)
          generated_files <- c(generated_files, output_file)
          cli::cli_inform(
            "  {basename(output_file)}: done ({round(file.info(output_file)$size / 1024, 1)} KB)"
          )
        } else {
          cli::cli_warn("  {format(d, '%Y-%m-%d')}: no data returned")
        }
      },
      error = function(e) {
        cli::cli_warn("  {format(d, '%Y-%m-%d')}: failed - {e$message}")
      }
    )
  }
}

# -----------------------------------------------------------------------------
# Upload to GitHub Releases (optional)
# -----------------------------------------------------------------------------

if (UPLOAD_TO_GITHUB && length(generated_files) > 0) {
  cli::cli_h2("Uploading to GitHub")

  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_warn("piggyback package not installed, skipping upload")
  } else {
    tryCatch(
      {
        releases <- piggyback::pb_releases(repo = GITHUB_REPO)
        if (!release_tag %in% releases$tag_name) {
          piggyback::pb_release_create(
            repo = GITHUB_REPO,
            tag = release_tag,
            name = "GRIB Forecasts",
            body = "Rolling tidal current forecasts in GRIB2 format"
          )
        }

        n_uploaded <- 0L
        for (f in generated_files) {
          cli::cli_inform("  Uploading: {basename(f)}")
          tryCatch(
            {
              piggyback::pb_upload(
                f,
                repo = GITHUB_REPO,
                tag = release_tag,
                overwrite = TRUE
              )
              n_uploaded <- n_uploaded + 1L
            },
            error = function(e) {
              cli::cli_warn("  Failed to upload {basename(f)}: {e$message}")
            }
          )
        }
        cli::cli_alert_success(
          "Upload complete: {n_uploaded}/{length(generated_files)} files"
        )
      },
      error = function(e) {
        cli::cli_warn("Upload failed: {e$message}")
      }
    )
  }
}

# -----------------------------------------------------------------------------
# Cleanup old files
# -----------------------------------------------------------------------------

if (CLEANUP_OLD) {
  cli::cli_h2("Cleanup")

  all_gribs <- list.files(OUTPUT_DIR, pattern = "\\.grib2$", full.names = TRUE)

  for (f in all_gribs) {
    date_str <- gsub(".*_(\\d{8}).*\\.grib2$", "\\1", basename(f))
    if (nchar(date_str) == 8L) {
      file_date <- as.Date(date_str, format = "%Y%m%d")
      if (file_date < Sys.Date()) {
        cli::cli_inform("  Removing: {basename(f)}")
        unlink(f)
      }
    }
  }
}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

cli::cli_h2("Summary")
cli::cli_bullets(c(
  "*" = "Generated: {length(generated_files)} GRIB files",
  "*" = "Output: {normalizePath(OUTPUT_DIR, mustWork = FALSE)}",
  "*" = "GitHub upload: {if (UPLOAD_TO_GITHUB) 'enabled' else 'disabled'}"
))
