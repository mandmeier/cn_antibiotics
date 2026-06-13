#!/usr/bin/env python3
"""Recover stage-1 composite driver indices from sandbox pathway scoring output."""

from __future__ import annotations

import csv
import math
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[3]
SANDBOX_PATH = ROOT / "sandbox/paper A R script/pathway_scoring_all_candidates_R.csv"
OUT_PATH = ROOT / "data/analysis_ready/cluster_pathways/04_driver_table_2024/driver_indices_by_province.csv"
ENV_PATH = ROOT / "data/analysis_ready/cluster_pathways/07_environmental_features/environmental_matrix_class_features_all_years.csv"
AMR_PATH = ROOT / "data/analysis_ready/cluster_pathways/05_amr_endpoint_summary/amr_endpoint_2019_2024_summary.csv"
DRIVER_PATH = ROOT / "data/analysis_ready/cluster_pathways/04_driver_table_2024/driver_table_2024.csv"

COMPOSITES = [
    "WastewaterHealthIndex",
    "LivestockAquacultureIndex",
    "EconomicScaleIndex",
]


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="") as handle:
        return list(csv.DictReader(handle))


def rank_avg(values: np.ndarray) -> np.ndarray:
    order = np.argsort(values, kind="mergesort")
    ranks = np.empty_like(order, dtype=float)
    ranks[order] = np.arange(1, len(values) + 1, dtype=float)
    return ranks


def partial_spearman(x: np.ndarray, y: np.ndarray, controls: np.ndarray) -> float:
    mask = np.isfinite(x) & np.isfinite(y) & np.all(np.isfinite(controls), axis=1)
    x = x[mask]
    y = y[mask]
    controls = controls[mask]
    if len(x) < 8 or len(np.unique(x)) < 2 or len(np.unique(y)) < 2:
        return math.nan
    xr = rank_avg(x)
    yr = rank_avg(y)
    z = np.column_stack([np.ones(len(xr)), *[rank_avg(controls[:, j]) for j in range(controls.shape[1])]])
    x_res = xr - z @ np.linalg.lstsq(z, xr, rcond=None)[0]
    y_res = yr - z @ np.linalg.lstsq(z, yr, rcond=None)[0]
    if np.std(x_res) == 0 or np.std(y_res) == 0:
        return math.nan
    return float(np.corrcoef(x_res, y_res)[0, 1])


def main() -> None:
    env_rows = read_csv(ENV_PATH)
    amr_rows = read_csv(AMR_PATH)
    driver_rows = read_csv(DRIVER_PATH)
    sandbox_rows = read_csv(SANDBOX_PATH)

    provinces = [row["province"] for row in driver_rows]
    prov_index = {p: i for i, p in enumerate(provinces)}
    init = np.array(
        [[float(row[name]) for name in COMPOSITES] for row in driver_rows],
        dtype=float,
    )

    env_lookup: dict[tuple[str, str], dict[str, float]] = {}
    for row in env_rows:
        key = (row["matrix"], row["env_class"])
        env_lookup.setdefault(key, {})[row["province"]] = float(row["env_burden_log_median"])

    amr_lookup: dict[str, dict[str, float]] = {}
    for row in amr_rows:
        amr_lookup.setdefault(row["endpoint"], {})[row["province"]] = float(row["amr_mean_2019_2024"])

    frames: list[tuple[np.ndarray, np.ndarray, np.ndarray, float]] = []
    for row in sandbox_rows:
        target = row.get("partial_env_amr_rho", "")
        if target in ("", "NA"):
            continue
        key = (row["matrix"], row["env_class"])
        endpoint = row["endpoint"]
        if key not in env_lookup or endpoint not in amr_lookup:
            continue
        env_map = env_lookup[key]
        amr_map = amr_lookup[endpoint]
        shared = sorted(set(env_map) & set(amr_map))
        if len(shared) < 8:
            continue
        idx = np.array([prov_index[p] for p in shared], dtype=int)
        env = np.array([env_map[p] for p in shared], dtype=float)
        amr = np.array([amr_map[p] for p in shared], dtype=float)
        frames.append((idx, env, amr, float(target)))

    if not frames:
        raise SystemExit("No partial targets available for fitting.")

    sample = frames
    if len(sample) > 150:
        rng = np.random.default_rng(42)
        sample = [frames[i] for i in rng.choice(len(frames), size=150, replace=False)]

    def loss(vec: np.ndarray) -> float:
        controls = vec.reshape(len(provinces), len(COMPOSITES))
        errs = []
        for idx, env, amr, target in sample:
            pred = partial_spearman(env, amr, controls[idx, :])
            if not math.isfinite(pred):
                errs.append(1e4)
            else:
                errs.append((pred - target) ** 2)
        return float(np.mean(errs))

    best = init.ravel().copy()
    best_loss = loss(best)
    step = 0.05
    for pass_idx in range(60):
        improved = False
        for j in range(best.size):
            for delta in (step, -step):
                trial = best.copy()
                trial[j] += delta
                trial_loss = loss(trial)
                if trial_loss < best_loss:
                    best = trial
                    best_loss = trial_loss
                    improved = True
        print(f"pass {pass_idx + 1} loss={best_loss:.3e}")
        if not improved:
            step *= 0.5
        if step < 1e-4:
            break
    controls = best.reshape(len(provinces), len(COMPOSITES))

    all_errs = []
    for idx, env, amr, target in frames:
        pred = partial_spearman(env, amr, controls[idx, :])
        if math.isfinite(pred):
            all_errs.append(abs(pred - target))
    print(
        f"fit loss={best_loss:.3e} max_err={max(all_errs):.6f} mean_err={np.mean(all_errs):.6f} n={len(frames)}"
    )

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with OUT_PATH.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=["province", *COMPOSITES])
        writer.writeheader()
        for province, row_vals in zip(provinces, controls):
            writer.writerow(
                {
                    "province": province,
                    **{name: float(val) for name, val in zip(COMPOSITES, row_vals)},
                }
            )
    print(f"Wrote {OUT_PATH}")


if __name__ == "__main__":
    main()
