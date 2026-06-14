# Pathway filter summary

Figure 3 simple screening logic (aligned with `build_figure3_simple_screening_block.R`).

- Primary scenario: `all_years`
- Matrices: `surface water`, `soil`, `sludge`, `sediment`
- Exclude `other`, `multiple classes`
- Class-concordant pairs only
- `n_provinces >= 12`
- Rank by `n_provinces`, then `|env_amr_rho|`, then `|partial_env_amr_rho|`, then LOO label
- Top `3` per matrix **(disabled; full pool exported)**

- All candidates: **2604**
- Class-concordant (after env/matrix gates): **60**
- Filter pool (+ 12+ provinces): **60**
- Filtered pathways: **60**

## Survivors per matrix

- sediment: **15**
- sludge: **15**
- soil: **15**
- surface water: **15**

