# cn_antibiotics curated data deposit (v1.0.0)

Harmonized environmental antibiotic concentrations (China), CARSS clinical resistance panels, and China Statistical Yearbook covariates for province-level One Health reuse.

**License (this curated package):** [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/). The CC BY 4.0 grant covers **our harmonization, cleaning, derived tables, codebook, and join keys**. It does **not** replace the licenses or terms of upstream sources.

**Software / pipeline:** https://github.com/mandmeier/cn_antibiotics (MIT). Software DOI: https://doi.org/10.5281/zenodo.22807986 (GitHub release v1.0.1).

**Data DOI (concept; all versions):** https://doi.org/10.5281/zenodo.22807174 (resolves to the latest Zenodo version; files restricted until acceptance). Do not hard-code a version DOI in this file — it would go stale on the next upload.

## Layout

```
primary/       province join set (start here)
supporting/    finer-grain or full tables
meta/          codebook, join key, class lookup, env_sources
```

## Primary (`primary/`)

| File | Description |
|---|---|
| `env_province.csv` | Median concentration per sample type × province × antibiotic |
| `resistance_province.csv` | Cleaned CARSS resistance panel (31 provinces) |
| `yearbook_province.csv` | 24 One Health yearbook covariates with `year` (population / urban share 2015–2024; other metrics 2024 vintage) |

| Use case | Start with |
|---|---|
| Province joins (env × CARSS × covariates) | `env_province.csv`, `resistance_province.csv`, `yearbook_province.csv` |

## Supporting (`supporting/`)

| File | Description |
|---|---|
| `env_records.csv` | Harmonized env concentrations (province-oriented; feeds `env_province`) |
| `env_site_records.csv` | Same harmonization with `location`, `season`, `lon`, `lat` retained |
| `env_site.csv` | Median concentration per sample type × location × season × antibiotic |
| `yearbook_full.csv` | All 764 cleaned yearbook metrics |
| `yearbook_province_manifest.csv` | Core metric list with theme tags and definitions |

## Meta (`meta/`)

| File | Description |
|---|---|
| `codebook.csv` | Variable dictionary for curated tables |
| `join_key.md` | One-page join key |
| `antibiotic_classes.csv` | Antibiotic → pharmacological class |
| `env_sources.csv` | Environmental literature publication ID → citation |

## Sources, licensing, and attribution

This Zenodo package contains **derived / curated tables only**. We do **not** redistribute the original Zhang Figshare workbook, raw CARSS extracts, NBS yearbook workbooks, or the SimpleMaps shapefile here. Obtain those from the upstream links below (or from the GitHub repo only where redistribution of a working copy is allowed under the source terms).

### Environmental concentrations (Zhang et al. 2022 + supplemental)

- **Upstream dataset (cite this for the compiled measurement rows):** Zhang, Q. et al. A dataset of distribution of antibiotic occurrence in solid environmental matrices in China. *figshare* https://doi.org/10.6084/m9.figshare.19692241.v1 (2022).
- **Data Descriptor:** Zhang, Q. et al. *Sci Data* **9**, 276 (2022). https://doi.org/10.1038/s41597-022-01384-5
- **What we did:** Harmonized names/units/matrices, added sparse supplemental literature rows, and built province- and site-level summaries. **We did not generate the Zhang 2022 occurrence measurements**; those rows remain attributed to Zhang et al. and their cited primary studies (`meta/env_sources.csv`).
- **Not in this deposit:** the original Figshare / `Zhang_2022.xls` file.

### Clinical resistance (CARSS)

- **Upstream:** China Antimicrobial Resistance Surveillance System (CARSS) provincial antimicrobial susceptibility surveillance products (https://www.carss.cn/).
- **What we did:** Cleaned and reshaped a CARSS-derived panel into `resistance_province.csv` for province-level joins.
- **Not in this deposit:** raw CARSS distribution files; obtain current CARSS products from the official CARSS channels under their terms of use.

### Yearbook covariates (NBS)

- **Upstream:** National Bureau of Statistics of China (NBS) *China Statistical Yearbook* / related statistical tables (https://www.stats.gov.cn/).
- **What we did:** Harmonized metric names/units and curated `yearbook_province.csv` (with `year`) / `yearbook_full.csv` for reuse.
- **Not in this deposit:** original NBS yearbook workbooks; obtain official tables from NBS.

### Administrative boundaries (SimpleMaps / Pareto) — GitHub maps only

- **Upstream shapefile:** GIS files © Pareto Software, LLC, released under **CC BY 4.0**. Please attribute **https://simplemaps.com** whenever possible (see `data/raw/cn_shp/license_gis.txt` in the GitHub repository).
- **Not in this deposit:** the shapefile is excluded from the Zenodo zip. Province labels in tabular products do not redistribute the geometry.

### How to cite

1. **This curated deposit** — https://doi.org/10.5281/zenodo.22807174  
2. **The R pipeline** — https://doi.org/10.5281/zenodo.22807986  
3. **Zhang et al. 2022 Figshare** (and/or the *Scientific Data* article) for environmental occurrence rows derived from their compilation  
4. **CARSS** and **NBS** as appropriate for resistance and yearbook-derived covariates  
5. **SimpleMaps** (https://simplemaps.com) if you use the GitHub shapefile or maps built from it  

Reproduce curated tables from the GitHub pipeline (`R/01`–`R/08`) after `renv::restore()`, with upstream inputs obtained as above.
