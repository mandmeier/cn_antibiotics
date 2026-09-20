# Manuscript tables

Scripts that build Tables 1–4 for the Scientific Data manuscript. Open the
`cn_antibiotics` RStudio project (working directory = package root), then:

```r
source("R/tables/tables_build.R")
```

| Script | Table | Needs pipeline outputs |
|--------|-------|------------------------|
| `tables_build.R` | Tables 1, 2, 4 (+ Table 3 md companion) | primary / supporting / meta CSVs + `data/intermediate/validation/` |
| `table03_select_spotcheck_papers.py` | Table 3 sample | `supporting/env_records.csv` + `Data_Sources.csv` |

CSV/Markdown outputs land in `tables/`. `table03_spotcheck.csv` is preserved
when already filled; `table04_unit_flags.csv` is rebuilt from the pipeline audit.

Spot-check paper folders (PDFs, open-links, per-paper subsets) stay outside this
package under `../figures_sandbox/table03_spotcheck_papers/`. Rebuild them with:

```bash
python3 R/tables/table03_select_spotcheck_papers.py
```
