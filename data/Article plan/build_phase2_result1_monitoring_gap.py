from __future__ import annotations

import os
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/private/tmp/matplotlib")

import matplotlib
matplotlib.use("Agg")
from matplotlib import colors as mcolors
import matplotlib.pyplot as plt
import pandas as pd
import geopandas as gpd


ROOT = Path(__file__).resolve().parent
STAGE1 = ROOT / "stage1_outputs"
OUT = ROOT / "phase2_result1_monitoring_gap"
MAP_PATH = ROOT / "china_provinces.json"
OUT.mkdir(exist_ok=True)


def rank_pct(series: pd.Series) -> pd.Series:
    return series.rank(method="average", pct=True)


def classify_quadrant(row: pd.Series, x_mid: float, y_mid: float) -> str:
    high_amr = row["amr_burden_percentile"] >= y_mid
    low_env = row["env_evidence_percentile"] < x_mid
    if high_amr and low_env:
        return "High AMR / Low Env"
    if high_amr and not low_env:
        return "High AMR / High Env"
    if (not high_amr) and low_env:
        return "Low AMR / Low Env"
    return "Low AMR / High Env"


def main() -> None:
    amr = pd.read_csv(STAGE1 / "AMR_2024_snapshot.csv")[
        ["province", "amr_burden_score_2024", "total_strains_curated_2024"]
    ]
    env = pd.read_csv(STAGE1 / "EnvCoverage_by_province.csv")[
        [
            "province",
            "env_rows",
            "env_unique_references",
            "env_unique_matrices",
            "sample_year_min",
            "sample_year_max",
        ]
    ]

    df = amr.merge(env, on="province", how="inner")
    df["env_year_span"] = df["sample_year_max"] - df["sample_year_min"] + 1

    df["amr_burden_percentile"] = rank_pct(df["amr_burden_score_2024"])
    df["env_rows_percentile"] = rank_pct(df["env_rows"])
    df["env_matrices_percentile"] = rank_pct(df["env_unique_matrices"])
    df["env_year_span_percentile"] = rank_pct(df["env_year_span"])

    df["env_evidence_percentile"] = df[
        ["env_rows_percentile", "env_matrices_percentile", "env_year_span_percentile"]
    ].mean(axis=1)
    df["env_monitoring_weakness"] = 1 - df["env_evidence_percentile"]

    # Product score only becomes large when both high AMR and weak env evidence are true.
    df["monitoring_gap_priority_score"] = (
        df["amr_burden_percentile"] * df["env_monitoring_weakness"]
    )

    x_mid = df["env_evidence_percentile"].median()
    y_mid = df["amr_burden_percentile"].median()
    df["quadrant"] = df.apply(classify_quadrant, axis=1, x_mid=x_mid, y_mid=y_mid)
    df["top_quartile_amr"] = df["amr_burden_percentile"] >= 0.75
    df["bottom_quartile_env"] = df["env_evidence_percentile"] <= 0.25
    df["strict_h1_priority"] = df["top_quartile_amr"] & df["bottom_quartile_env"]

    df = df.sort_values(
        ["monitoring_gap_priority_score", "amr_burden_percentile"],
        ascending=[False, False],
    ).reset_index(drop=True)
    df["priority_rank"] = range(1, len(df) + 1)

    full_out = OUT / "monitoring_gap_atlas_2024.csv"
    df.to_csv(full_out, index=False)

    priority_cols = [
        "priority_rank",
        "province",
        "amr_burden_score_2024",
        "amr_burden_percentile",
        "env_rows",
        "env_unique_matrices",
        "env_year_span",
        "env_evidence_percentile",
        "env_monitoring_weakness",
        "monitoring_gap_priority_score",
        "quadrant",
        "strict_h1_priority",
    ]
    top10 = df[priority_cols].head(10).copy()
    top10.to_csv(OUT / "priority_provinces_top10.csv", index=False)

    hyp_rho = df["amr_burden_percentile"].corr(
        df["env_evidence_percentile"], method="spearman"
    )
    strict_n = int(df["strict_h1_priority"].sum())
    high_amr_n = int(df["top_quartile_amr"].sum())

    lines = [
        "# Phase 2 Result 1: Monitoring Gap Atlas",
        "",
        "## Definition",
        "",
        "- `AMR burden percentile`: percentile rank of `amr_burden_score_2024`.",
        "- `Environmental evidence percentile`: mean of percentile ranks for `env_rows`, `env_unique_matrices`, and `env_year_span`.",
        "- `Environmental monitoring weakness`: `1 - environmental evidence percentile`.",
        "- `Monitoring gap priority score`: `AMR burden percentile × environmental monitoring weakness`.",
        "",
        "This score rises only when a province is simultaneously high in clinical AMR burden and weak in environmental evidence.",
        "",
        "## Hypothesis Check",
        "",
        "Hypothesis IG1: high AMR provinces are low-monitored environmentally.",
        "",
        f"- Spearman correlation between AMR burden percentile and environmental evidence percentile: `{hyp_rho:.3f}`.",
        f"- Provinces meeting the strict rule `top quartile AMR + bottom quartile environmental evidence`: `{strict_n}` of `{high_amr_n}` high-AMR provinces.",
        "",
        "Interpretation:",
    ]

    if hyp_rho < 0:
        lines.append(
            "- The direction is consistent with IG1: provinces with higher AMR tend to have weaker environmental monitoring."
        )
    else:
        lines.append(
            "- The direction is not consistent with a blanket IG1 pattern: higher-AMR provinces do not generally have weaker environmental evidence; if anything, the province-level relationship is slightly positive."
        )

    if strict_n == 0:
        lines.append(
            "- No province met the strict high-AMR/low-monitoring rule, so IG1 is not supported under a top-quartile vs bottom-quartile definition."
        )
    elif strict_n == 1:
        lines.append(
            "- Only one province met the strict high-AMR/low-monitoring rule, so IG1 is best described as a targeted blind-spot problem rather than a national rule."
        )
    else:
        lines.append(
            f"- {strict_n} provinces met the strict high-AMR/low-monitoring rule, supporting a focused monitoring-gap pattern."
        )

    lines += [
        "",
        "## Priority Provinces",
        "",
        "Top provinces by monitoring-gap priority score:",
        "",
        "```",
        top10.to_string(index=False),
        "```",
        "",
        "## Readout",
        "",
        "- Provinces such as Inner Mongolia and Jilin rise to the top because they combine high AMR percentiles with weak environmental evidence.",
        "- Provinces such as Liaoning, Henan, Beijing, and Shanghai remain high-AMR but are not top monitoring-gap priorities because their environmental evidence base is not especially weak.",
        "- The atlas therefore points to selective surveillance blind spots rather than a universal inverse relationship between AMR and monitoring.",
    ]

    (OUT / "monitoring_gap_result1_note.md").write_text("\n".join(lines))

    # Figure 1
    plt.style.use("default")
    fig, (ax1, ax2) = plt.subplots(
        1, 2, figsize=(13, 7), gridspec_kw={"width_ratios": [1.2, 1]}
    )

    quad_colors = {
        "High AMR / Low Env": "#c0392b",
        "High AMR / High Env": "#e67e22",
        "Low AMR / Low Env": "#2980b9",
        "Low AMR / High Env": "#95a5a6",
    }

    gdf = gpd.read_file(MAP_PATH)
    gdf = gdf.rename(columns={"name": "province_map"})
    gdf["province"] = gdf["province_map"].replace(
        {
            "Neimenggu": "Inner Mongolia",
            "Xizang": "Tibet",
        }
    )
    data_gdf = gdf[gdf["province"].isin(df["province"])].copy()
    island_gdf = gdf[gdf["province_map"].isin(["Taiwan", "HongKong", "Aomen"])].copy()
    map_df = data_gdf.merge(df, on="province", how="left")

    norm = mcolors.Normalize(
        vmin=map_df["monitoring_gap_priority_score"].min(),
        vmax=map_df["monitoring_gap_priority_score"].max(),
    )
    cmap = plt.get_cmap("Reds")

    map_df.plot(
        column="monitoring_gap_priority_score",
        cmap=cmap,
        linewidth=0.6,
        edgecolor="white",
        ax=ax1,
        legend=True,
        norm=norm,
        legend_kwds={
            "label": "Monitoring Gap Priority Score",
            "shrink": 0.72,
        },
    )
    island_gdf.plot(
        ax=ax1,
        color="#f2f2f2",
        edgecolor="#9a9a9a",
        linewidth=0.5,
    )

    top_label_map = map_df[map_df["priority_rank"] <= 8].copy()
    top_label_map["label_pt"] = top_label_map.geometry.representative_point()
    offsets = {
        "Inner Mongolia": (1.0, 1.5),
        "Jilin": (1.2, 0.2),
        "Gansu": (-1.5, 0.3),
        "Anhui": (0.5, -0.8),
        "Shaanxi": (-1.0, -0.6),
        "Hubei": (0.2, -0.8),
        "Heilongjiang": (1.4, 1.0),
        "Henan": (0.4, -0.5),
    }
    for _, row in top_label_map.iterrows():
        pt = row["label_pt"]
        dx, dy = offsets.get(row["province"], (0.6, 0.6))
        ax1.text(
            pt.x + dx,
            pt.y + dy,
            row["province"],
            fontsize=7.5,
            color="#222",
            ha="center",
            va="center",
            bbox={"facecolor": "white", "edgecolor": "none", "alpha": 0.75, "pad": 1.5},
        )

    # Mark the strict IG1 province explicitly.
    strict_map = map_df[map_df["strict_h1_priority"]].copy()
    if not strict_map.empty:
        strict_map["label_pt"] = strict_map.geometry.representative_point()
        ax1.scatter(
            strict_map["label_pt"].x,
            strict_map["label_pt"].y,
            marker="*",
            s=120,
            c="#2c3e50",
            edgecolors="white",
            linewidths=0.8,
            zorder=5,
        )

    ax1.text(
        0.02,
        0.03,
        "Star = strict IG1 province",
        transform=ax1.transAxes,
        ha="left",
        va="bottom",
        fontsize=8,
        color="#333",
        bbox={"facecolor": "white", "edgecolor": "none", "alpha": 0.8, "pad": 1.5},
    )

    ax1.set_title("Fig 1A. Monitoring Gap Atlas")
    ax1.set_axis_off()

    # South / island inset using Taiwan, Hong Kong, Macao, and the South China Sea islands
    # embedded in the Hainan geometry.
    inset_ax = ax1.inset_axes([0.62, 0.02, 0.30, 0.30])
    inset_names = ["Hainan", "Guangdong", "Guangxi", "Fujian"]
    inset_data = map_df[map_df["province"].isin(inset_names)].copy()
    inset_nodata = island_gdf.copy()

    inset_data.plot(
        column="monitoring_gap_priority_score",
        cmap=cmap,
        linewidth=0.5,
        edgecolor="white",
        ax=inset_ax,
        norm=norm,
    )
    inset_nodata.plot(
        ax=inset_ax,
        color="#f2f2f2",
        edgecolor="#9a9a9a",
        linewidth=0.5,
    )
    inset_ax.set_xlim(105, 125)
    inset_ax.set_ylim(2, 27)
    inset_ax.set_xticks([])
    inset_ax.set_yticks([])
    inset_ax.set_facecolor("white")
    inset_ax.set_title("Taiwan & southern islands", fontsize=7, pad=2)
    for spine in inset_ax.spines.values():
        spine.set_edgecolor("#7f8c8d")
        spine.set_linewidth(0.8)

    plot_top = top10.iloc[::-1]
    bar_colors = [
        cmap(norm(df.set_index("province").loc[p, "monitoring_gap_priority_score"]))
        for p in plot_top["province"]
    ]
    ax2.barh(
        plot_top["province"],
        plot_top["monitoring_gap_priority_score"] * 100,
        color=bar_colors,
        edgecolor="none",
    )
    ax2.set_xlabel("Priority Score (0-100)")
    ax2.set_title("Fig 1B. Priority Provinces")
    for y, (_, row) in enumerate(plot_top.iterrows()):
        ax2.text(
            row["monitoring_gap_priority_score"] * 100 + 1,
            y,
            f"AMR {row['amr_burden_percentile']*100:.0f} | Env {row['env_evidence_percentile']*100:.0f}",
            va="center",
            fontsize=8,
        )

    fig.suptitle(
        "Phase 2 Result 1: Clinical AMR Burden vs Environmental Monitoring Weakness",
        fontsize=14,
        y=0.98,
    )
    fig.tight_layout(rect=[0, 0, 1, 0.96])
    fig.savefig(OUT / "fig1_monitoring_gap_atlas.png", dpi=220, bbox_inches="tight")
    plt.close(fig)


if __name__ == "__main__":
    main()
