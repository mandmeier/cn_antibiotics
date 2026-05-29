#!/usr/bin/env python3

from __future__ import annotations

from pathlib import Path
import math
import numpy as np
import pandas as pd


ROOT = Path("/Users/kali/Projects/antibiotics_in_wwt")
ARTICLE_PLAN = ROOT / "Article plan"
CLEANED = ARTICLE_PLAN / "cleaned"
OUTDIR = ARTICLE_PLAN / "stage1_outputs"


CURATED_AMR_ENDPOINTS = [
    ("E. coli", "Ciprofloxacin"),
    ("E. coli", "Levofloxacin"),
    ("E. coli", "Trimethoprim/Sulfamethoxazole"),
    ("K. pneumoniae", "Ciprofloxacin"),
    ("K. pneumoniae", "Levofloxacin"),
    ("P. aeruginosa", "Ciprofloxacin"),
    ("A. baumannii", "Levofloxacin"),
    ("S. aureus", "Erythromycin"),
]


WASTEWATER_HEALTH_METRICS = {
    "E25_08_Daily_Disposal_Capacity_of_City_Sewage": "sewage_disposal_capacity_2024",
    "E25_08_Length_of_City_Sewage_Pipes": "sewage_pipe_length_2024",
    "E08_33_Investment_in_Urban_Environmental_Infrastructure": "urban_env_infra_investment_2024",
    "E22_01_Hospitals": "hospitals_count_2024",
    "E22_06_Hospitals": "hospital_beds_2024",
}

LIVESTOCK_AQUA_METRICS = {
    "E12_13_Hogs_year_end": "hogs_year_end_2024",
    "E12_13_Large_Animals_year_end": "large_animals_year_end_2024",
    "E12_14_Pork": "pork_output_2024",
    "E12_14_Milk": "milk_output_2024",
    "E12_15_Total_Aquatic_Products": "total_aquatic_products_2024",
    "E12_15_Freshwater_Aquatic_Products": "freshwater_aquatic_products_2024",
    "E12_05_Consumption_of_Chemical_Fertilizers": "chemical_fertilizers_2024",
}

ECONOMIC_SCALE_METRICS = {
    "E03_09_Gross_Regional_Product": "grp_2024",
    "E03_09_Per_Capita_Gross_Regional_Product": "grp_per_capita_2024",
    "E02_05_Population_at_year_end": "population_2024",
    "E02_06_Urban_Population_Proportion": "urban_share_2024",
}


def safe_slug(text: str) -> str:
    return (
        text.lower()
        .replace(".", "")
        .replace("/", "_")
        .replace("-", "_")
        .replace(" ", "_")
    )


def antibiotic_class(antibiotic: str) -> str:
    a = antibiotic.lower()
    if any(x in a for x in ["ciprofloxacin", "levofloxacin", "ofloxacin", "norfloxacin"]):
        return "fluoroquinolones"
    if "erythromycin" in a:
        return "macrolides"
    if "trimethoprim/sulfamethoxazole" in a:
        return "sulfonamides"
    return "other"


def percentile_rank(series: pd.Series) -> pd.Series:
    numeric = pd.to_numeric(series, errors="coerce")
    out = pd.Series(np.nan, index=series.index, dtype=float)
    mask = numeric.notna()
    if mask.any():
        out.loc[mask] = numeric.loc[mask].rank(method="average", pct=True)
    return out


def log1p_if_nonnegative(series: pd.Series) -> pd.Series:
    numeric = pd.to_numeric(series, errors="coerce")
    return numeric.apply(lambda x: math.log1p(x) if pd.notna(x) and x >= 0 else np.nan)


def build_amr_outputs(resistance: pd.DataFrame, provinces: list[str]) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    df = resistance[resistance["province"] != "National"].copy()
    df["curated_endpoint"] = list(zip(df["bacteria_species"], df["antibiotic"]))
    curated_set = set(CURATED_AMR_ENDPOINTS)
    df["curated_endpoint"] = df["curated_endpoint"].isin(curated_set)
    df["antibiotic_class"] = df["antibiotic"].apply(antibiotic_class)
    df["endpoint_label"] = df["bacteria_species"] + " | " + df["antibiotic"]

    snapshot_long = df[df["year"] == 2024].copy()
    snapshot_long = snapshot_long[
        [
            "province",
            "bacteria_species",
            "antibiotic",
            "antibiotic_class",
            "endpoint_label",
            "curated_endpoint",
            "total_n_strains",
            "resistant_percent",
            "resistant_n_strains",
            "intermediate_percent",
            "sensitive_percent",
        ]
    ].sort_values(["province", "bacteria_species", "antibiotic"])

    province_summary = pd.DataFrame({"province": provinces})
    all_summary = (
        snapshot_long.groupby("province", as_index=False)
        .agg(
            mean_resistance_all_endpoints_2024=("resistant_percent", "mean"),
            total_strains_all_endpoints_2024=("total_n_strains", "sum"),
            n_endpoints_2024=("endpoint_label", "nunique"),
        )
    )
    curated_summary = (
        snapshot_long[snapshot_long["curated_endpoint"]]
        .groupby("province", as_index=False)
        .agg(
            amr_burden_score_2024=("resistant_percent", "mean"),
            n_curated_endpoints_2024=("endpoint_label", "nunique"),
            total_strains_curated_2024=("total_n_strains", "sum"),
        )
    )

    sentinel = snapshot_long[snapshot_long["curated_endpoint"]].copy()
    sentinel["endpoint_col"] = sentinel.apply(
        lambda r: f"res_{safe_slug(r['bacteria_species'])}__{safe_slug(r['antibiotic'])}_2024",
        axis=1,
    )
    sentinel_wide = sentinel.pivot(index="province", columns="endpoint_col", values="resistant_percent").reset_index()

    snapshot = (
        province_summary.merge(all_summary, on="province", how="left")
        .merge(curated_summary, on="province", how="left")
        .merge(sentinel_wide, on="province", how="left")
        .sort_values("province")
    )

    trend_rows = []
    for (province, species, antibiotic), g in df.groupby(["province", "bacteria_species", "antibiotic"]):
        g = g.sort_values("year")
        if g["year"].nunique() < 2:
            continue
        x = g["year"].to_numpy(dtype=float)
        y = pd.to_numeric(g["resistant_percent"], errors="coerce").to_numpy(dtype=float)
        mask = np.isfinite(x) & np.isfinite(y)
        x = x[mask]
        y = y[mask]
        if len(x) < 2:
            continue
        slope, intercept = np.polyfit(x, y, 1)
        trend_rows.append(
            {
                "province": province,
                "bacteria_species": species,
                "antibiotic": antibiotic,
                "antibiotic_class": antibiotic_class(antibiotic),
                "endpoint_label": f"{species} | {antibiotic}",
                "curated_endpoint": (species, antibiotic) in curated_set,
                "n_years": int(len(x)),
                "first_year": int(x.min()),
                "last_year": int(x.max()),
                "first_resistant_percent": float(y[0]),
                "last_resistant_percent": float(y[-1]),
                "absolute_change_2019_2024": float(y[-1] - y[0]),
                "slope_percentage_points_per_year": float(slope),
                "intercept": float(intercept),
                "mean_total_strains": float(pd.to_numeric(g["total_n_strains"], errors="coerce").mean()),
            }
        )
    trends = pd.DataFrame(trend_rows).sort_values(["province", "bacteria_species", "antibiotic"])

    return snapshot, trends, snapshot_long


def build_env_outputs(env: pd.DataFrame, provinces: list[str]) -> tuple[pd.DataFrame, pd.DataFrame]:
    df = env.copy()
    df["mean_concentration"] = pd.to_numeric(df["mean_concentration"], errors="coerce")
    df["max_concentration"] = pd.to_numeric(df["max_concentration"], errors="coerce")
    df["sample_year"] = pd.to_numeric(df["sample_year"], errors="coerce")
    df["primary_concentration"] = df["mean_concentration"].where(df["mean_concentration"].notna(), df["max_concentration"])
    df["primary_statistic"] = np.where(df["mean_concentration"].notna(), "mean", np.where(df["max_concentration"].notna(), "max", pd.NA))

    coverage = (
        pd.DataFrame({"province": provinces})
        .merge(
            df.groupby("province", as_index=False).agg(
                env_rows=("antibiotic", "size"),
                env_unique_references=("reference_number", "nunique"),
                env_unique_antibiotics=("antibiotic", "nunique"),
                env_unique_classes=("group_of_antibiotic", "nunique"),
                env_unique_matrices=("matrix", "nunique"),
                env_unique_locations=("location", "nunique"),
                env_rows_with_mean=("mean_concentration", lambda s: int(s.notna().sum())),
                env_rows_with_max=("max_concentration", lambda s: int(s.notna().sum())),
                sample_year_min=("sample_year", "min"),
                sample_year_max=("sample_year", "max"),
            ),
            on="province",
            how="left",
        )
        .sort_values("province")
    )

    burden_rows = []
    for (province, group, matrix), g in df.groupby(["province", "group_of_antibiotic", "matrix"]):
        primary = g["primary_concentration"]
        primary = primary[primary.notna() & (primary > 0)]
        mean_vals = g["mean_concentration"]
        mean_vals = mean_vals[mean_vals.notna() & (mean_vals > 0)]
        max_vals = g["max_concentration"]
        max_vals = max_vals[max_vals.notna() & (max_vals > 0)]
        burden_rows.append(
            {
                "province": province,
                "group_of_antibiotic": group,
                "matrix": matrix,
                "n_rows": int(len(g)),
                "n_rows_mean_based": int(g["mean_concentration"].notna().sum()),
                "n_rows_max_based": int(g["max_concentration"].notna().sum()),
                "median_log10_primary_concentration": float(np.median(np.log10(primary))) if len(primary) else np.nan,
                "median_log10_mean_concentration": float(np.median(np.log10(mean_vals))) if len(mean_vals) else np.nan,
                "median_log10_max_concentration": float(np.median(np.log10(max_vals))) if len(max_vals) else np.nan,
                "median_primary_concentration_raw": float(np.median(primary)) if len(primary) else np.nan,
                "concentration_unit_mode": g["concentration_unit"].mode().iloc[0] if g["concentration_unit"].notna().any() else pd.NA,
                "sample_year_min": float(g["sample_year"].min()) if g["sample_year"].notna().any() else np.nan,
                "sample_year_max": float(g["sample_year"].max()) if g["sample_year"].notna().any() else np.nan,
            }
        )
    burden = pd.DataFrame(burden_rows).sort_values(["province", "group_of_antibiotic", "matrix"])
    return coverage, burden


def build_driver_output(yearbook_panel: pd.DataFrame, provinces: list[str]) -> pd.DataFrame:
    df = yearbook_panel[yearbook_panel["year"] == 2024].copy()
    df["value"] = pd.to_numeric(df["value"], errors="coerce")

    metric_map = {}
    metric_map.update(WASTEWATER_HEALTH_METRICS)
    metric_map.update(LIVESTOCK_AQUA_METRICS)
    metric_map.update(ECONOMIC_SCALE_METRICS)

    subset = df[df["metric"].isin(metric_map)].copy()
    wide = (
        subset.assign(metric_name=subset["metric"].map(metric_map))
        .pivot(index="province", columns="metric_name", values="value")
        .reset_index()
    )
    wide = pd.DataFrame({"province": provinces}).merge(wide, on="province", how="left")

    for col in [c for c in wide.columns if c != "province"]:
        wide[f"log1p__{col}"] = log1p_if_nonnegative(wide[col])
        wide[f"rankpct__{col}"] = percentile_rank(wide[f"log1p__{col}"])

    wastewater_rank_cols = [f"rankpct__{v}" for v in WASTEWATER_HEALTH_METRICS.values()]
    livestock_rank_cols = [f"rankpct__{v}" for v in LIVESTOCK_AQUA_METRICS.values()]
    economic_rank_cols = [f"rankpct__{v}" for v in ECONOMIC_SCALE_METRICS.values()]

    wide["WastewaterHealthIndex"] = wide[wastewater_rank_cols].mean(axis=1, skipna=True)
    wide["LivestockAquacultureIndex"] = wide[livestock_rank_cols].mean(axis=1, skipna=True)
    wide["EconomicScaleIndex"] = wide[economic_rank_cols].mean(axis=1, skipna=True)

    return wide.sort_values("province")


def write_methods_note() -> None:
    text = """# Stage 1 Methods Note

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
"""
    (OUTDIR / "stage1_methods_note.md").write_text(text, encoding="utf-8")


def main() -> None:
    OUTDIR.mkdir(exist_ok=True)

    resistance = pd.read_csv(CLEANED / "resistance_clean.csv")
    env = pd.read_csv(CLEANED / "environmental_cleaned.csv")
    yearbook_panel = pd.read_csv(CLEANED / "yearbook_province_year_panel.csv")
    provinces = sorted(pd.read_csv(CLEANED / "province_groups.csv")["province"].dropna().unique().tolist())

    amr_snapshot, amr_trends, amr_long = build_amr_outputs(resistance, provinces)
    env_coverage, env_burden = build_env_outputs(env, provinces)
    driver_indices = build_driver_output(yearbook_panel, provinces)

    amr_snapshot.to_csv(OUTDIR / "AMR_2024_snapshot.csv", index=False)
    amr_trends.to_csv(OUTDIR / "AMR_trends_2019_2024.csv", index=False)
    amr_long.to_csv(OUTDIR / "AMR_2024_endpoints_long.csv", index=False)
    env_coverage.to_csv(OUTDIR / "EnvCoverage_by_province.csv", index=False)
    env_burden.to_csv(OUTDIR / "EnvBurden_by_province_class_matrix.csv", index=False)
    driver_indices.to_csv(OUTDIR / "DriverIndices_by_province.csv", index=False)
    write_methods_note()


if __name__ == "__main__":
    main()
