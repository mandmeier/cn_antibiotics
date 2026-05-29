# Appendix: Yearbook data to add (2015–2024)

## Purpose

Enable the **primary causal event-study** (wastewater infrastructure → hospital AMR) using a province-year panel from the China Statistical Yearbook (CSY).

## Years and provinces

- **Years:** 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024
- **Provinces:** all 31 CARSS provinces (same names as `resistance_clean.csv`)
- **Exclude:** `National Total`, `Central and State Organs`, `Not Classified by Region`

## Folder structure

```text
data/raw/yearbook_data/panel/
  README.md
  by_table/
    25-8_municipal_sewage.csv
    8-33_env_infrastructure.csv
    2-5_population.csv
    2-6_urban_share.csv
    3-9_gdp.csv
    22-6_hospital_beds.csv
    22-1_health_institutions.csv
    ...
```

## CSV format (required columns)

| Column | Example |
|--------|---------|
| province | Beijing |
| year | 2019 |
| metric | E25_08_Daily_Disposal_Capacity_of_City_Sewage |
| value | 751 |
| unit | 10000_cu_m |

## Tier A — must extract (primary causal module)

| File | CSY table | Metrics to include |
|------|-----------|-------------------|
| 25-8_municipal_sewage.csv | 25-8 | Daily_Disposal_Capacity_of_City_Sewage; Length_of_City_Sewage_Pipes |
| 8-33_env_infrastructure.csv | 8-33 | Investment_in_Urban_Environmental_Infrastructure |
| 2-5_population.csv | 2-5 | Population_at_year_end |
| 2-6_urban_share.csv | 2-6 | Urban_Population_Proportion |
| 3-9_gdp.csv | 3-9 | Gross_Regional_Product; Per_Capita_Gross_Regional_Product |
| 22-6_hospital_beds.csv | 22-6 | Total; Hospitals |
| 22-1_health_institutions.csv | 22-1 | Hospitals; Total |

## Tier B — secondary pathway (livestock)

| File | CSY table | Metrics |
|------|-----------|---------|
| 12-13_livestock_heads.csv | 12-13 | Hogs_year_end; Large_Animals_year_end |
| 12-14_livestock_products.csv | 12-14 | Pork; Milk |
| 12-15_aquaculture.csv | 12-15 | Total_Aquatic_Products; Freshwater_Aquatic_Products |
| 12-5_agriculture.csv | 12-5 | Irrigated_Area_of_Cultivated_Land; Consumption_of_Chemical_Fertilizers |

## Tier C — optional robustness

7-6 (environment budget expenditure), 8-12 (air pollutants), 20-7 (R&D), 25-5 (tap water), 8-17 (garbage treatment).

## QC checklist before rebuild

- [ ] Province names harmonized to CARSS
- [ ] No duplicate province-year-metric rows
- [ ] Units consistent across years (flag changes in README)
- [ ] At least 6 years of data for 25-8 sewage capacity metrics
- [ ] Re-run `build_yearbook_panel_2015_2024.R` and `run_causal_event_study_amr.R`

## Pipeline commands

```bash
Rscript sandbox/linkage_pipeline/R/wrangling/manuscript/seed_yearbook_panel_from_repo.R
Rscript sandbox/linkage_pipeline/R/wrangling/manuscript/build_yearbook_panel_2015_2024.R
Rscript sandbox/linkage_pipeline/R/wrangling/manuscript/run_causal_event_study_amr.R
```
