# rsiscorar

R interface to the Brazilian Navy's [SISCORAR](https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare) (Sistema de Correntes e Mares) tidal current prediction system.

SISCORAR predicts ocean currents for 5 Brazilian coastal bays. It is based on [ADCIRC](https://adcirc.org) (ADvanced CIRCulation Model), an unstructured finite-element coastal ocean model developed by Luettich and Westerink and widely used by NOAA and the US Army Corps of Engineers.

## Two prediction paths

| Path | Function | Nodes | Constituents | Requires |
|------|----------|-------|-------------|---------|
| Pure R | `predict_currents()` | ~3,000 mesh nodes | 13 (independently fit) | nothing |
| Exe | `run_prediction()` + `read_predictions()` | ~90,000 regular grid | 142 (13 fit + 129 inferred) | Wine + SISCORAR install |

**`predict_currents()` is recommended for most use cases.** It runs the harmonic summation in pure R at the ADCIRC computational mesh nodes, with no external dependencies. Accuracy is characterized by V0+u (astronomical argument) agreement with the exe to <0.15° across a 40-year span.

The exe path produces denser output by interpolating from the ~3,000 mesh nodes to a regular output grid using pre-computed barycentric weights (Interp.bin). The extra 129 inference constituents it synthesizes contribute roughly 1–3% of velocity magnitude.

## Coverage

The ADCIRC mesh for each bay extends beyond the output region (open-ocean boundary conditions). U.bin stores harmonic constants only for the nodes within the output extent.

| Area | Location | ADCIRC mesh nodes | U.bin nodes | Exe grid nodes |
|------|----------|------------------|-------------|---------------|
| guanabara | Guanabara Bay, Rio de Janeiro | 16,905 | ~12,758 | ~290,000 |
| sepetiba | Sepetiba Bay, Rio de Janeiro | 13,517 | ~3,093 | ~91,000 |
| paranagua | Paranagua Bay, Parana | — | ~4,971 | ~70,000 |
| santos | Santos Bay, Sao Paulo | — | ~5,107 | ~90,000 |
| baiatos | Baia de Todos os Santos, Salvador | — | ~3,620 | ~145,000 |

## Installation

```r
# install.packages("pak")
pak::pak("leoniedu/rsiscorar")
```

### Prerequisites

`predict_currents()` has no system dependencies beyond R itself.

For the exe path (`run_prediction()` / `read_predictions()`):

1. **SISCORAR** -- Download from the Brazilian Navy:
   <https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare>

   Install to `~/bin/siscorar-5.0` (default) or any other directory.

2. **Wine** (macOS/Linux only) -- Required to run the prediction executables:

   ```bash
   # macOS
   brew install wine-stable

   # Ubuntu/Debian
   sudo apt install wine
   ```

3. **CDO + ecCodes** (optional, for GRIB2 export):

   ```bash
   brew install cdo eccodes
   ```

4. **ncdf4** (optional, for NetCDF/GRIB2 export):

   ```r
   install.packages("ncdf4")
   ```

## Configuration

The package looks for the SISCORAR installation in this order:

1. R option: `options(siscorar.home = "/path/to/siscorar-5.0")`
2. Environment variable: `SISCORAR_HOME=/path/to/siscorar-5.0`
3. Default: `~/bin/siscorar-5.0`

For Wine (non-Windows):

1. R option: `options(siscorar.wine = "/path/to/wine")`
2. Environment variable: `SISCORAR_WINE=/path/to/wine`
3. Auto-detection (Sys.which, common paths)

Add to your `~/.Renviron` for persistence:

```
SISCORAR_HOME=/path/to/siscorar-5.0
```

Check your setup with:

```r
library(rsiscorar)
siscorar_sitrep()
```

## Usage

```r
library(rsiscorar)

# View available areas
siscorar_areas()

# Area information
area_info("guanabara")

# Predict currents for a date
dt <- predict_currents("2025-06-15", "guanabara")
print(dt)

# Query current at a specific location
result <- get_current_at_point(-43.15, -22.85, dt, target_hour = 12)
print(result)

# Multi-day predictions
dt_week <- predict_currents_range("2025-06-01", "2025-06-07", "guanabara")

# Export to GRIB2 for OpenCPN
write_grib(dt, "currents.grib2", hours = 0:23, resolution = 0.005)

# Export to NetCDF
write_netcdf(dt, "currents.nc", resolution = 0.005)

# Read grid metadata
grid <- read_grid("guanabara")
constituents <- read_constituents("guanabara")
```

## Output Format

Predictions return a `data.table` with columns:

| Column | Description |
|--------|-------------|
| `col` | Node ID (mesh node for `predict_currents()`; grid column for exe path) |
| `row` | Always 1 for `predict_currents()` (mesh nodes are unstructured); grid row for exe path |
| `lon`, `lat` | Coordinates (WGS84, decimal degrees) |
| `datetime` | POSIXct timestamp (America/Sao_Paulo) |
| `hour` | Hour of day (0-23) |
| `velocity_cm_s` | Current speed (cm/s) |
| `speed_m_s` | Current speed (m/s) |
| `direction_deg` | Direction from North (degrees, oceanographic) |
| `u_velocity` | Eastward component (cm/s) |
| `v_velocity` | Northward component (cm/s) |

## Credits

- **SISCORAR** system by the Brazilian Navy Hydrographic Center (DHN) / Oceanographic Modelling and Observational Network (REMO), in cooperation with Petrobras.
  Official distribution: <https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare>
  Technical references: [Cruz et al. (2018)](https://www.marinha.mil.br/dhn/sites/www.marinha.mil.br.dhn/files/anais/Anais_Hidrograficos_2018_0.pdf), [Referência Técnica SISCORAR 2.0](https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare)

- **ADCIRC** (ADvanced CIRCulation Model for Oceanic, Coastal and Estuarine Waters) by R. Luettich (UNC) and J. Westerink (Notre Dame). ADCIRC is licensed under LGPL-3.0.
  rsiscorar is a clean-room R reimplementation: it reads SISCORAR data files but does not incorporate any ADCIRC source code.

- **Harmonic analysis** uses Schureman (1958) formulas (public domain) for astronomical arguments and nodal corrections. The 1-year ADCIRC simulation (2017) was analyzed with [t_tide](https://www.eoas.ubc.ca/~rich/t_tide/t_tide_v1.3beta.html) to derive the 13 constituent harmonic constants stored in U.bin/V.bin.

## License

MIT — see [LICENSE.md](LICENSE.md).

rsiscorar's R code is MIT-licensed. The SISCORAR data files (U.bin, V.bin, Interp.bin, etc.) are distributed separately by the Brazilian Navy for free scientific and maritime use. ADCIRC, on which SISCORAR is based, is LGPL-3.0 licensed.
