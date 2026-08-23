# Pathway filter summary

Figure 3 simple screening logic (aligned with `build_figure3_simple_screening_block.R`).

- Primary scenario: `all_years`
- Matrices: `surface water`, `soil`, `sludge`, `sediment`
- Exclude `other`, `multiple classes`
- Class-concordant pairs via unified lookup (`assign_antibiotic_group` / `antibiotic_group_lookup.csv`)
- Quinolones merged into `fluoroquinolones` for env-class aggregation
- `Trimethoprim/Sulfamethoxazole` retained in AMR resistance data
- `n_provinces >= 10`
- Rank by `n_provinces`, then `|env_amr_rho|`, then `|partial_env_amr_rho|`, then LOO label
- Top `3` per matrix **(disabled; full pool exported)**

- All candidates: **2697**
- Class-concordant (after env/matrix gates): **125**
- Filter pool (+ 10+ provinces): **83**
- Filtered pathways: **83**

## Survivors per matrix

- sediment: **23**
- sludge: **19**
- soil: **22**
- surface water: **19**

