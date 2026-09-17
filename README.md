# cn_antibiotics

Pipeline to turn literature-derived raw environmental antibiotic concentrations, CARSS clinical resistance data, and China statistical yearbook variables into harmonized, cleaned, internally consistent datasets. 

## Setup

1. Open the project at the repository root (or set the working directory there). All paths are root-relative.
2. Restore the R package library:

```r
renv::restore()
```

3. Packages load via [`.Rprofile`](.Rprofile) → [`R/setup.R`](R/setup.R). On macOS with Homebrew, `setup.R` sets GDAL / PROJ / udunits paths needed by `sf` for map plots.

Run scripts with:

```bash
Rscript R/01_clean_environmental_data.R
```

## Directory layout

| Path | Role |
|------|------|
| `data/raw/` | inputs |
| `data/intermediate/` | Cleaned / harmonized tables produced by cleaning scripts |
| `data/output/` | Analysis-ready tables, province clusters, and figures |
| `R/01`–`R/06` | Pipeline scripts in run order |
| `R/utils/` | Shared helpers (units, antibiotic classes, yearbook metrics, maps) |
| `R/qa/` | Optional verification scripts (not required to reproduce outputs) |

### Raw inputs

- `data/raw/environmental_data/Zhang_2022.xls` — literature compilation of environmental measurements compiled by Zhang et.al.
- `data/raw/environmental_data/China_Environmental_Supplemental.csv` — supplemental records from recent literature
- `data/raw/environmental_data/Data_Sources.csv` — publication ID → citation
- `data/raw/resistance_data/carss_drug_resistance_full.csv` — CARSS resistance panel
- `data/raw/yearbook_data/yearbook_data_combined.csv` — CHina statistical yearbook metrics
- `data/raw/reference/antibiotic_group_lookup.csv` — antibiotic → pharmacological class
- `data/raw/reference/province_metadata.csv` — NBS province metadata (no cluster labels)
- `data/raw/cn_shp/cn.*` — China province shapefile (for maps)

## Reproduce the pipeline

Run from the repository root, in order:

```bash
Rscript R/01_clean_environmental_data.R   # → data/intermediate/environmental_cleaned.csv (+ validation/)
Rscript R/02_clean_resistance_data.R      # → data/intermediate/resistance_clean.csv
Rscript R/03_clean_yearbook_data.R        # → data/intermediate/yearbook_clean.csv
Rscript R/04_env_abx_per_province.R       # → data/output/env_abx_per_province.csv
Rscript R/05_create_china_metrics.R       # → data/output/antibiotic_metrics_china.csv
Rscript R/06_province_groups_kmeans_pca.R # → data/output/province_groups.csv + figures/
```

Steps 01–03 are independent of each other and can be run in any order. Steps 04–06 must follow as above.

### Main outputs

| File | Description |
|------|-------------|
| `data/intermediate/environmental_cleaned.csv` | Harmonized env concentrations, matrices, classes |
| `data/intermediate/resistance_clean.csv` | Cleaned CARSS (31 provinces; combo drugs dropped except TMP-SMX) |
| `data/intermediate/yearbook_clean.csv` | Harmonized yearbook metric names and units |
| `data/output/env_abx_per_province.csv` | Median concentration per sample type × province × antibiotic |
| `data/output/antibiotic_metrics_china.csv` | Combined env + resistance metrics vs China mean |
| `data/output/province_groups.csv` | Province metadata plus `k2_groups` / `k3_groups` / `k4_groups` |
| `data/output/figures/` | Elbow/silhouette diagnostics, PCA + China map |

Clustering uses `set.seed(42)` and `kmeans(..., nstart = 25)` so labels are reproducible. If `data/output/province_groups.csv` already exists, step 06 aligns new cluster IDs to the previous numbering when possible.

Yearbook cleaning is part of the curated data products. k-means uses only environmental and resistance metrics.

## Optional QA

```bash
Rscript R/qa/verify_antibiotic_names.R
Rscript R/qa/verify_antibiotic_groups.R
Rscript R/qa/build_antibiotic_group_lookup.R
```
