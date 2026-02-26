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

FORECAST_DAYS <- 90
LOOKBACK_DAYS <- 90
AREAS <- c("baiatos")
RESOLUTION <- 0.001
GEOJSON_RESOLUTION <- 0.005
RESOLUTION_STR <- sprintf("%03d", RESOLUTION * 1000)
GEOJSON_RESOLUTION_STR <- sprintf("%03d", GEOJSON_RESOLUTION * 1000)
OUTPUT_DIR <- "output/gribs"
GITHUB_REPO <- "leoniedu/siscorar_gribs"
UPLOAD_TO_GITHUB <- nzchar(GITHUB_REPO)
CLEANUP_OLD <- TRUE
release_tag <- RESOLUTION_STR
geojson_release_tag <- paste0("geojson", GEOJSON_RESOLUTION_STR)


try(
  piggyback::pb_new_release(repo = GITHUB_REPO, tag = release_tag),
  silent = TRUE
)
try(
  piggyback::pb_new_release(repo = GITHUB_REPO, tag = geojson_release_tag),
  silent = TRUE
)


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
# Check GitHub for existing files (both releases)
# -----------------------------------------------------------------------------

github_grib_files <- character(0)
github_geojson_files <- character(0)

if (UPLOAD_TO_GITHUB && requireNamespace("piggyback", quietly = TRUE)) {
  cli::cli_h2("Checking GitHub releases")

  tryCatch(
    {
      existing <- piggyback::pb_list(repo = GITHUB_REPO, tag = release_tag)
      if (nrow(existing) > 0) {
        github_grib_files <- existing$file_name
        cli::cli_inform(
          "Found {length(github_grib_files)} GRIB file(s) on GitHub [{release_tag}]"
        )
      }
    },
    error = function(e) {
      cli::cli_warn("Could not check GRIB release: {e$message}")
    }
  )

  tryCatch(
    {
      existing <- piggyback::pb_list(
        repo = GITHUB_REPO,
        tag = geojson_release_tag
      )
      if (nrow(existing) > 0) {
        github_geojson_files <- existing$file_name
        cli::cli_inform(
          "Found {length(github_geojson_files)} GeoJSON file(s) on GitHub [{geojson_release_tag}]"
        )
      }
    },
    error = function(e) {
      cli::cli_warn("Could not check GeoJSON release: {e$message}")
    }
  )
}

# -----------------------------------------------------------------------------
# Generate GRIBs and GeoJSONs
# -----------------------------------------------------------------------------

generated_grib_files <- character(0)
generated_geojson_files <- character(0)

for (area in AREAS) {
  cli::cli_h2("Processing: {toupper(area)}")

  for (d in dates) {
    d <- as.Date(d, origin = "1970-01-01")
    output_file <- file.path(
      OUTPUT_DIR,
      sprintf("%s_%s_%s.grib2", area, format(d, "%Y%m%d"), RESOLUTION_STR)
    )
    geojson_file <- file.path(
      OUTPUT_DIR,
      sprintf(
        "%s_%s_%s.geojson",
        area,
        format(d, "%Y%m%d"),
        GEOJSON_RESOLUTION_STR
      )
    )

    need_grib <- !basename(output_file) %in% github_grib_files &&
      !file.exists(output_file)
    need_geojson <- !basename(geojson_file) %in% github_geojson_files &&
      !file.exists(geojson_file)

    # Collect local-only files that still need uploading
    if (
      !need_grib &&
        file.exists(output_file) &&
        !basename(output_file) %in% github_grib_files
    ) {
      generated_grib_files <- c(generated_grib_files, output_file)
    }
    if (
      !need_geojson &&
        file.exists(geojson_file) &&
        !basename(geojson_file) %in% github_geojson_files
    ) {
      generated_geojson_files <- c(generated_geojson_files, geojson_file)
    }

    if (!need_grib && !need_geojson) {
      cli::cli_inform("  {format(d, '%Y-%m-%d')}: both files present, skipping")
      next
    }

    cli::cli_inform(
      "  {format(d, '%Y-%m-%d')}: generating (grib={need_grib}, geojson={need_geojson})..."
    )

    tryCatch(
      {
        dt <- predict_currents(d, area)

        if (nrow(dt) > 0) {
          if (need_grib) {
            write_grib(dt, output_file, hours = 0:23, resolution = RESOLUTION)
            generated_grib_files <- c(generated_grib_files, output_file)
            cli::cli_inform(
              "  {basename(output_file)}: done ({round(file.info(output_file)$size / 1024, 1)} KB)"
            )
          }

          if (need_geojson) {
            write_uv_geojson(
              dt,
              geojson_file,
              resolution = GEOJSON_RESOLUTION,
              area = area
            )
            generated_geojson_files <- c(generated_geojson_files, geojson_file)
            cli::cli_inform(
              "  {basename(geojson_file)}: done ({round(file.info(geojson_file)$size / 1024, 1)} KB)"
            )
          }
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

upload_to_release <- function(files, tag, label) {
  if (length(files) == 0L) {
    return(invisible(0L))
  }

  cli::cli_inform("  Release [{tag}]: {length(files)} {label} file(s)")
  n_uploaded <- 0L
  for (f in files) {
    cli::cli_inform("    Uploading: {basename(f)}")
    tryCatch(
      {
        piggyback::pb_upload(f, repo = GITHUB_REPO, tag = tag, overwrite = TRUE)
        n_uploaded <- n_uploaded + 1L
      },
      error = function(e) {
        cli::cli_warn("    Failed: {basename(f)}: {e$message}")
      }
    )
  }
  n_uploaded
}

n_total <- length(generated_grib_files) + length(generated_geojson_files)

if (UPLOAD_TO_GITHUB && n_total > 0L) {
  cli::cli_h2("Uploading to GitHub")

  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_warn("piggyback package not installed, skipping upload")
  } else {
    tryCatch(
      {
        n_up <- 0L
        n_up <- n_up +
          upload_to_release(generated_grib_files, release_tag, "GRIB")
        n_up <- n_up +
          upload_to_release(
            generated_geojson_files,
            geojson_release_tag,
            "GeoJSON"
          )
        cli::cli_alert_success("Upload complete: {n_up}/{n_total} files")
      },
      error = function(e) cli::cli_warn("Upload failed: {e$message}")
    )
  }
}

# -----------------------------------------------------------------------------
# Cleanup old files
# -----------------------------------------------------------------------------

if (CLEANUP_OLD) {
  cli::cli_h2("Cleanup")

  all_local <- list.files(
    OUTPUT_DIR,
    pattern = "\\.(grib2|geojson)$",
    full.names = TRUE
  )

  for (f in all_local) {
    date_str <- gsub(".*_(\\d{8})_.*$", "\\1", basename(f))
    if (nchar(date_str) == 8L) {
      file_date <- as.Date(date_str, format = "%Y%m%d")
      if (file_date < Sys.Date()) {
        is_on_github <- if (grepl("\\.geojson$", f)) {
          basename(f) %in% github_geojson_files
        } else {
          basename(f) %in% github_grib_files
        }
        if (is_on_github) {
          cli::cli_inform("  Removing: {basename(f)}")
          unlink(f)
        } else {
          cli::cli_warn("  Skipping removal (not on GitHub): {basename(f)}")
        }
      }
    }
  }
}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

cli::cli_h2("Summary")
cli::cli_bullets(c(
  "*" = "GRIBs:   {length(generated_grib_files)} file(s) -> release [{release_tag}]",
  "*" = "GeoJSON: {length(generated_geojson_files)} file(s) -> release [{geojson_release_tag}]",
  "*" = "Output: {normalizePath(OUTPUT_DIR, mustWork = FALSE)}",
  "*" = "GitHub upload: {if (UPLOAD_TO_GITHUB) 'enabled' else 'disabled'}"
))
