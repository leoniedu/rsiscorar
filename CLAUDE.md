# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

rsiscorar is an R package that wraps the Brazilian Navy's SISCORAR tidal current prediction system. It runs Windows `.exe` prediction binaries (via Wine on macOS/Linux), reads their binary output files, and exports to GRIB2/NetCDF/GeoJSON for marine navigation software. Covers 5 Brazilian coastal bays with 142 tidal harmonics.

## Development Commands

```bash
# Tests
r -e 'devtools::test()'
r -e 'devtools::test_active_file("tests/testthat/test-read.R")'

# Documentation (after editing roxygen)
r -e 'devtools::document()'

# Full check
r -e 'devtools::check()'

# Setup diagnostics
r -e 'rsiscorar::siscorar_sitrep()'
```

## Architecture

The pipeline is: **configure → run prediction exe → read binary → query/export**.

| File | Role |
|------|------|
| `R/config.R` | Locates SISCORAR install (`siscorar_home()`) and Wine (`siscorar_wine()`). Cascading lookup: R option → env var → default path. |
| `R/wine.R` | Wine detection and 32/64-bit selection. baiatos uses PE32+ (64-bit), others PE32. |
| `R/areas.R` | Area registry (`SISCORAR_AREAS`), executable names, grid metadata (`area_info()`). |
| `R/predict.R` | `run_prediction()` shells out to Wine+exe; `predict_currents()` combines run+read; `predict_currents_range()` does multi-day. |
| `R/read.R` | `read_predictions()` parses Grade.bin (variable-length binary: 20-byte header per node, 192 bytes for 24h of velocity/direction). `read_grid()` and `read_constituents()` parse other binary/text files. |
| `R/query.R` | `get_current_at_point()` — spatial lookup by lon/lat at a target hour. |
| `R/export-grib.R` | `write_grib()` — exports to GRIB2 via ncdf4+CDO pipeline with caching. |
| `R/export-netcdf.R` | `write_netcdf()` — exports to NetCDF4 via ncdf4. |
| `R/export-geojson.R` | `write_uv_geojson()` — exports UV vectors as GeoJSON. |

## Key Data Flow

- Predictions return `data.table` with columns: `col`, `row`, `lon`, `lat`, `datetime`, `hour`, `velocity_cm_s`, `speed_m_s`, `direction_deg`, `u_velocity`, `v_velocity`.
- Grade.bin binary format: per node = 20-byte header (col, row, lon, lat, flag as 4×float32 + 1×int32), then 192 bytes if active (24 hours × 2×float32 for velocity+direction).
- Guanabara Bay has ~290k nodes → ~7M rows per day.

## System Dependencies

- **SISCORAR 5.0**: Default install path `~/bin/siscorar-5.0`. Area data lives under `arquivos/areas/{area}/`.
- **Wine**: Required on macOS/Linux to run the `.exe` prediction binaries.
- **CDO + ecCodes**: Optional, for GRIB2 export.
- **ncdf4**: Optional R package for NetCDF/GRIB2 export.

## Daily Update Script

`inst/scripts/daily_update.R` generates rolling 90-day GRIB2+GeoJSON forecasts and uploads to GitHub releases on `leoniedu/siscorar_gribs` via the `gh` CLI. Configured via launchd plist in `inst/launchd/`.
