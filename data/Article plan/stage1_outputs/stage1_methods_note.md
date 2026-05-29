# Stage 1 Methods Note

This folder contains the Step 1 core province tables for the manuscript plan.

## AMR outputs

- Source: `Article plan/cleaned/resistance_clean.csv`
- Province scope: 31 provinces only (`National` excluded)
- `AMR_2024_snapshot.csv`:
  - one row per province
  - `amr_burden_score_2024` = simple mean across the curated sentinel endpoints below
  - includes curated endpoint columns for quick mapping/figure work
- `AMR_2024_endpoints_long.csv`:
  - one row per province × species × antibiotic for 2024
- `AMR_trends_2019_2024.csv`:
  - one row per province × species × antibiotic
  - slope estimated by simple OLS over annual province-level resistance percentages

Curated sentinel endpoints used for the province burden score:

- `E. coli | Ciprofloxacin`
- `E. coli | Levofloxacin`
- `E. coli | Trimethoprim/Sulfamethoxazole`
- `K. pneumoniae | Ciprofloxacin`
- `K. pneumoniae | Levofloxacin`
- `P. aeruginosa | Ciprofloxacin`
- `A. baumannii | Levofloxacin`
- `S. aureus | Erythromycin`

These were chosen because they are high-coverage clinical endpoints and overlap reasonably with the environmental class structure (fluoroquinolones, sulfonamides, macrolides).

## Environmental outputs

- Source: `Article plan/cleaned/environmental_cleaned.csv`
- `EnvCoverage_by_province.csv`:
  - row counts, unique references, unique antibiotics, classes, matrices, and year span by province
- `EnvBurden_by_province_class_matrix.csv`:
  - burden is separated from coverage
  - `primary_concentration` = `mean_concentration` when available, otherwise `max_concentration`
  - reported medians are on `log10` scale for comparability
  - separate columns are retained for mean-based and max-based medians

## Driver output

- Source: `Article plan/cleaned/yearbook_province_year_panel.csv`
- Year used: 2024
- `DriverIndices_by_province.csv`:
  - raw metric values
  - `log1p__*` transformed columns
  - percentile-rank columns `rankpct__*`
  - three composite indices:
    - `WastewaterHealthIndex`
    - `LivestockAquacultureIndex`
    - `EconomicScaleIndex`

Metric sets:

- Wastewater / health:
  - sewage disposal capacity
  - sewage pipe length
  - urban environmental infrastructure investment
  - hospitals
  - hospital beds
- Livestock / aquaculture:
  - hogs
  - large animals
  - pork
  - milk
  - total aquatic products
  - freshwater aquatic products
  - chemical fertilizers
- Economic scale:
  - GRP
  - per-capita GRP
  - population
  - urban share
