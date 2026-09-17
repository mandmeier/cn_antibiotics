# cn_antibiotics curated data deposit (v1.0.0)

Harmonized environmental antibiotic concentrations (China), CARSS clinical resistance panels, and China Statistical Yearbook covariates for province-level One Health reuse.

**License:** [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

**Software / pipeline:** https://github.com/mandmeier/cn_antibiotics (MIT). Software DOI pending GitHub→Zenodo release archive.

**Data DOI:** https://doi.org/10.5281/zenodo.22807175 (version 1.0.0; files restricted until acceptance). Concept DOI for all versions: https://doi.org/10.5281/zenodo.22807174.

## Recommended starting files

| Use case | Start with |
|---|---|
| Province joins (env × CARSS × covariates) | `env_abx_per_province.csv`, `resistance_clean.csv`, `yearbook_core.csv` |
| Site / season reuse | `environmental_by_site.csv` or `env_abx_per_site.csv` |
| Full yearbook appendix (764 metrics) | `yearbook_full.csv` |
| Column definitions | `codebook.csv` |
| Join rules (provinces, shared compounds, units, classes) | `join_key.md` + `join_key_antibiotic_classes.csv` |

## File descriptions

| File | Description |
|---|---|
| `environmental_cleaned.csv` | Harmonized env concentrations (province-oriented collapse) |
| `environmental_by_site.csv` | Same harmonization with `location`, `season`, `lon`, `lat` retained |
| `resistance_clean.csv` | Cleaned CARSS resistance panel (31 provinces) |
| `env_abx_per_province.csv` | Median concentration per sample type × province × antibiotic |
| `env_abx_per_site.csv` | Median concentration per sample type × location × season × antibiotic |
| `antibiotic_metrics_china.csv` | Combined env + resistance metrics vs China mean |
| `province_groups.csv` | Province metadata plus k-means group labels |
| `yearbook_core.csv` | 24 One Health yearbook covariates (recommended join file) |
| `yearbook_core_manifest.csv` | Core metric list with theme tags and definitions |
| `yearbook_full.csv` | All 764 cleaned yearbook metrics (appendix) |
| `codebook.csv` | Variable dictionary for curated tables |
| `join_key.md` | One-page join key |
| `join_key_antibiotic_classes.csv` | Antibiotic → pharmacological class |
| `Data_Sources.csv` | Environmental literature publication ID → citation |
| `FILES.md` | Manifest with file sizes |
| `README_deposit.md` | This file |

## Provenance notes

- Environmental measurements are derived from a literature compilation (Zhang et al. 2022) plus supplemental records; source citations are in `Data_Sources.csv`.
- Resistance data are derived from the China Antimicrobial Resistance Surveillance System (CARSS).
- Yearbook covariates are derived from China Statistical Yearbook tables.
- Raw third-party source dumps and GIS shapefiles are **not** included in this deposit; they remain in the GitHub repository for reproducibility under their original terms.
- Reproduce tables from the GitHub pipeline (`R/01`–`R/09`) after `renv::restore()`.

## Citation

Cite the data deposit (https://doi.org/10.5281/zenodo.22807175) and the software DOI (GitHub–Zenodo archive) once minted. Until the software DOI exists, cite the GitHub repository and data deposit version (`v1.0.0`).
