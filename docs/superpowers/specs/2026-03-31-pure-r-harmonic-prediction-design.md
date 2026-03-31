# Pure R Harmonic Tidal Current Prediction

**Date:** 2026-03-31
**Goal:** Eliminate Wine/exe dependency by implementing tidal harmonic prediction natively in R.

## Scope

Approach B: predict at computational mesh nodes only (~3K nodes per area). No Interp.bin spatial interpolation to regular grid. The output data.table has the same columns as today but fewer rows.

## Reverse-Engineered Data Formats

### U.bin / V.bin (per-node harmonic constants)

168 bytes per node = 14 triplets of (int32, float32, float32):

- **Triplet 0 (header):** `(9857 + node_count_or_1, longitude, latitude)`
- **Triplets 1-13:** `(9857 + constituent_index, 9857 + amplitude_cm_s, 9857 + phase_deg)`

Where `constituent_index` references ConsNovas.txt, `amplitude_cm_s` is the local harmonic amplitude, and `phase_deg` is the local phase lag (g).

### ConsNovas.txt (constituent catalog)

- Line 1: count (142)
- Remaining lines: fixed-width Fortran format
  - Cols 0-2: index (1-142)
  - Col 3: flag ('1' = major constituent used in prediction)
  - Cols 4-11: constituent name
  - Cols 11+: frequency scaled by 1e7 (degrees/hour)
  - Remaining cols: Doodson number differences (optional)

Only the 13 constituents referenced by U.bin are used:

| Index | Name | Frequency (deg/hr) | Species |
|-------|------|-------------------|---------|
| 12    | O1   | 13.9430356        | Diurnal |
| 45    | S2   | 30.0000000        | Semidiurnal |
| 19    | K1   | 15.0410686        | Diurnal |
| 34    | N2   | 28.4397295        | Semidiurnal |
| 67    | MN4  | 57.4238337        | Quarter-diurnal |
| 10    | Q1   | 13.3986609        | Diurnal |
| 47    | K2   | 30.0821373        | Semidiurnal |
| 39    | M2   | 28.9841042        | Semidiurnal |
| 17    | P1   | 14.9589314        | Diurnal |
| 71    | M4   | 57.9682084        | Quarter-diurnal |
| 76    | MS4  | 58.9841042        | Quarter-diurnal |
| 94    | M6   | 86.9523127        | Sixth-diurnal |
| 114   | M8   | 115.9364169       | Eighth-diurnal |

### FatNod.txt (exe output, used for validation only)

Written by the exe each run. 13 lines per node (all nodes identical):
```
constituent_index  V0_plus_u_degrees
```

This file is NOT read by the R implementation. It's used to validate our V0+u computation against the exe.

## Prediction Formula

For each computational node n, at each hour h (0-23):

```
u(n,h) = Σ_{c=1..13} f_c * H_u(n,c) * cos(ω_c * h + V0u_c - g_u(n,c))
v(n,h) = Σ_{c=1..13} f_c * H_v(n,c) * cos(ω_c * h + V0u_c - g_v(n,c))
```

Where:
- `f_c` = nodal factor for constituent c (depends on date, ~18.6-year cycle)
- `H_u, H_v` = amplitude from U.bin/V.bin (cm/s)
- `ω_c` = frequency from ConsNovas.txt (deg/hr)
- `V0u_c` = astronomical argument V0+u at midnight of prediction date
- `g_u, g_v` = phase lag from U.bin/V.bin (degrees)

Then:
- `velocity = sqrt(u^2 + v^2)` (cm/s)
- `direction = atan2(u, v)` (degrees from north, oceanographic convention)
- `speed_m_s = velocity / 100`

## Astronomical Arguments (V0+u) and Nodal Factors (f)

Computed from 5 fundamental astronomical variables for the prediction date:

- `s` = mean longitude of moon
- `h` = mean longitude of sun
- `p` = longitude of lunar perigee
- `N'` = negative longitude of lunar ascending node
- `p1` = longitude of solar perigee (effectively constant)

Standard Schureman (1958) formulas. Each constituent's V0+u is a linear combination of these variables (Doodson numbers). The nodal factor f is computed from N' using constituent-specific correction formulas.

### Validation

V0+u values will be validated against the exe's FatNod.txt output. Tolerance: <0.5 degrees. This was empirically verified: the exe's V0+u advances by exactly `omega * 24` degrees per day.

## New File: R/harmonic.R

### Internal functions

- `.compute_astro_args(date)` — returns named list of s, h, p, N_prime, p1 for a date
- `.compute_v0u(date)` — returns V0+u for each of the 13 constituents (degrees)
- `.compute_nodal_factors(date)` — returns f for each of the 13 constituents
- `.read_harmonic_constants(area)` — parses U.bin + V.bin, returns data.table with columns: node, lon, lat, constituent, u_amplitude, u_phase, v_amplitude, v_phase
- `.predict_at_nodes(date, area)` — full harmonic prediction, returns data.table matching current output schema

### Constants

Hardcoded table of the 13 constituents with:
- ConsNovas index
- Name
- Frequency (deg/hr)
- Doodson numbers (for V0 computation)
- Nodal factor formula coefficients

## Changes to Existing Functions

### `predict_currents(date, area, daylight_saving)`

**Before:** calls `run_prediction()` (Wine + exe) then `read_predictions()` (Grade.bin)
**After:** calls `.predict_at_nodes(date, area)` directly

Wine, exe, and Grade.bin are no longer involved.

DST adjustment: shift datetime column by +1 hour if `daylight_saving = TRUE`.

### `predict_currents_range(from, to, area, daylight_saving)`

Same change — loops `.predict_at_nodes()` instead of calling `run_prediction()` + `read_predictions()`.

### Kept as-is

- `run_prediction()` — still available for users who want the exe
- `read_predictions()` — still available for reading existing Grade.bin files
- `read_grid()`, `read_constituents()`, `area_info()`, `get_current_at_point()`
- All export functions (GRIB, NetCDF, GeoJSON)
- Config functions (`siscorar_home()`, `siscorar_wine()`, etc.)

## Breaking Changes

- **Row count**: output has ~3K rows per hour (computational nodes) instead of ~90K (regular grid). Users filtering by `lon`/`lat` may get different nearest-neighbor results.
- **Wine no longer required** for the default workflow. `siscorar_wine()` and Wine detection only needed if using `run_prediction()` directly.

## Testing Strategy

Tests validate against the exe's output:

1. **V0+u validation**: For each area, run the exe for a known date, compare our `.compute_v0u()` against FatNod.txt. Tolerance: <0.5 degrees.

2. **Prediction validation**: For each area, run the exe, read Grade.bin, find output nodes nearest to computational mesh nodes, compare velocity and direction. Tolerance: velocity within 5% (accounts for minor interpolation effects), direction within 5 degrees.

3. **Existing tests**: Current tests in `test-predict.R`, `test-read.R`, `test-query.R` should pass (may need minor adjustments for changed row counts).

Tests requiring Wine/exe are skipped via `testthat::skip_if_not()` when Wine is unavailable (CI environments).
