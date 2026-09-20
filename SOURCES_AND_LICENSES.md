# Sources and licenses

This repository and the Zenodo data deposit (https://doi.org/10.5281/zenodo.22807174) provide **harmonized, derived tables**. Upstream providers retain rights in their original works. The deposit package is released under **CC BY 4.0** for our curation layer only.

| Source | What it is | Cite / attribute | In Zenodo deposit? | In this GitHub repo? |
|---|---|---|---|---|
| Zhang et al. 2022 | Literature compilation of environmental antibiotic occurrences | Figshare https://doi.org/10.6084/m9.figshare.19692241.v1 ; paper https://doi.org/10.1038/s41597-022-01384-5 | **No** raw workbook — derived env tables only | Working copy `data/raw/environmental_data/Zhang_2022.xls` for reproducibility; **do not treat as our primary data** |
| Supplemental env literature | Extra rows beyond Zhang | Citations in `Data_Sources.csv` | Derived rows only | `China_Environmental_Supplemental.csv` |
| CARSS | Clinical resistance surveillance | https://www.carss.cn/ under CARSS terms | Derived `resistance_province.csv` only | Working extract under `data/raw/resistance_data/` |
| NBS China Statistical Yearbook | Official statistics | https://www.stats.gov.cn/ | Derived yearbook tables only | Working extract under `data/raw/yearbook_data/` |
| SimpleMaps / Pareto China provinces | Admin boundaries | **CC BY 4.0** — attribute https://simplemaps.com (`data/raw/cn_shp/license_gis.txt`) | **No** shapefile | `data/raw/cn_shp/` |
| This curation + R code | Harmonized tables + pipeline | Data DOI; software DOI https://doi.org/10.5281/zenodo.22807986 ; MIT for code | Yes (curated CSVs) | Yes |

**Attribution rule for Zhang:** environmental concentration *measurements* in derived tables that originate from Zhang’s compilation must be credited to Zhang et al. (Figshare + paper). This project’s contribution is cleaning, harmonization, supplemental additions, and join-ready products—not generation of those primary occurrence records.

Full user-facing wording ships in [`deposit/README_deposit.md`](deposit/README_deposit.md) (copied into the Zenodo zip).
