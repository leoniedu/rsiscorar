# rsiscorar

R interface to the Brazilian Navy's [SISCORAR](https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare) (Sistema de Correntes e Mares) tidal current prediction system.

SISCORAR predicts ocean currents for 5 Brazilian coastal bays using harmonic tidal analysis with 142 constituents:

| Area | Location | Grid Nodes |
|------|----------|-----------|
| guanabara | Guanabara Bay, Rio de Janeiro | ~290,000 |
| sepetiba | Sepetiba Bay, Rio de Janeiro | ~5,400 |
| paranagua | Paranagua Bay, Parana | ~8,700 |
| santos | Santos Bay, Sao Paulo | ~8,900 |
| baiatos | Baia de Todos os Santos, Salvador | ~6,300 |

## Installation

```r
# install.packages("pak")
pak::pak("leoniedu/rsiscorar")
```

### Prerequisites

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
| `col`, `row` | Grid cell indices |
| `lon`, `lat` | Coordinates (WGS84, decimal degrees) |
| `datetime` | POSIXct timestamp (America/Sao_Paulo) |
| `hour` | Hour of day (0-23) |
| `velocity_cm_s` | Current speed (cm/s) |
| `speed_m_s` | Current speed (m/s) |
| `direction_deg` | Direction from North (degrees, oceanographic) |
| `u_velocity` | Eastward component (cm/s) |
| `v_velocity` | Northward component (cm/s) |

## Credits

- **SISCORAR** system by the Brazilian Navy (DHN/REMO)
- Official distribution: <https://www.marinha.mil.br/chm/dados-do-smm/corrente-de-mare>

## License

MIT
