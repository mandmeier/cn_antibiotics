# Manuscript figure panels

One R script per figure panel. Open the `cn_antibiotics` RStudio project (working directory = package root), then source a script or run it line by line.

```r
# Example (from cn_antibiotics/)
source("R/figures/fig02a_harmonization_uniques.R")
```

| Script | Panel | Needs pipeline outputs |
|--------|-------|------------------------|
| `fig01_pipeline_schematic.R` | Fig. 1 (whole) | primary CSV files (live box stats) |
| `fig02a_harmonization_uniques.R` | Fig. 2A | R/01–R/07 outputs + raw inputs |
| `fig02b_env_qc_flow.R` | Fig. 2B | `data/intermediate/validation/flow_stages.csv` (from R/01) |
| `fig02c_env_measurement_map.R` | Fig. 2C | `supporting/env_records.csv` |
| `fig03a_antibiotic_coverage.R` | Fig. 3A | env_records, env_province, resistance_province |
| `fig03b_env_carss_venn.R` | Fig. 3B | env_records, resistance_province |
| `fig03c_env_antibiotics_map.R` | Fig. 3C | env_province |

PNG/PDF outputs land in `figures/` (next to the editable `figures.odp`).

The LibreOffice schematic in `figures/figures.odp` is the manuscript layout. These scripts recreate each **panel** from curated tables for the code archive.
