#!/usr/bin/env Rscript
# Generate test fixtures by running SISCORAR exe for sampled dates.
# Requires Wine + SISCORAR installation. Run once, commit fixtures.
#
# Only keeps Grade.bin rows near computational mesh nodes (within 0.001 deg)
# to keep fixture files small enough for git.
#
# Usage: Rscript inst/scripts/generate_fixtures.R

devtools::load_all(".")
library(data.table)

fixtures_dir <- file.path("tests", "testthat", "fixtures")
dir.create(fixtures_dir, showWarnings = FALSE, recursive = TRUE)

# Sample K=5 dates from 2000-2050 (monthly, fixed seed)
all_dates <- seq(as.Date("2000-01-01"), as.Date("2050-01-01"), by = "month")
set.seed(42L)
test_dates <- sort(sample(all_dates, 5L))

cat("Generating fixtures for dates:\n")
print(test_dates)

area <- "sepetiba"

# Read computational mesh node positions (static, date-independent)
hc <- .read_harmonic_constants(area)
mesh_nodes <- unique(hc[, .(lon, lat)])
cat(sprintf("Mesh nodes: %d\n", nrow(mesh_nodes)))

for (d in test_dates) {
  d <- as.Date(d, origin = "1970-01-01")
  cat(sprintf("\n--- %s ---\n", d))

  success <- run_prediction(d, area)
  if (!success) {
    warning(sprintf("Prediction failed for %s", d))
    next
  }

  # Read FatNod.txt (V0+u from exe)
  fatnod_file <- file.path(get_area_path(area), "FatNod.txt")
  fatnod_lines <- readLines(fatnod_file, n = 13L)
  fatnod_parsed <- do.call(rbind, strsplit(trimws(fatnod_lines), "\\s+"))
  v0u_exe <- data.frame(
    constituent_index = as.integer(fatnod_parsed[, 1]),
    v0u_deg = as.numeric(fatnod_parsed[, 2])
  )

  # Read Grade.bin, keep only nodes near mesh nodes
  grade_dt <- read_predictions(area, d)

  # For each mesh node, find nearest Grade.bin node at hour 0
  grade_h0 <- grade_dt[hour == 0L]
  keep_indices <- integer(0)
  for (i in seq_len(nrow(mesh_nodes))) {
    dists <- (grade_h0$lon - mesh_nodes$lon[i])^2 +
      (grade_h0$lat - mesh_nodes$lat[i])^2
    nearest <- which.min(dists)
    if (sqrt(dists[nearest]) < 0.001) {
      keep_indices <- c(keep_indices, nearest)
    }
  }
  keep_indices <- unique(keep_indices)

  # Keep all hours for the matched nodes (identified by col+row)
  matched <- grade_h0[keep_indices, .(col, row)]
  grade_slim <- grade_dt[matched, on = .(col, row)]

  fixture <- list(
    date = d,
    area = area,
    v0u = v0u_exe,
    grade = grade_slim
  )

  fname <- sprintf("fixture_%s_%s.rds", area, format(d, "%Y%m%d"))
  saveRDS(fixture, file.path(fixtures_dir, fname))
  cat(sprintf("  Saved: %s (%d rows, trimmed from %d)\n",
              fname, nrow(grade_slim), nrow(grade_dt)))
}

saveRDS(test_dates, file.path(fixtures_dir, "test_dates.rds"))
cat("\nDone! Fixtures saved to", fixtures_dir, "\n")
