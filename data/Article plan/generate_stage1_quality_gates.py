#!/usr/bin/env python3

from __future__ import annotations

from pathlib import Path
import pandas as pd


ROOT = Path("/Users/kali/Projects/antibiotics_in_wwt")
ARTICLE_PLAN = ROOT / "Article plan"
CLEANED = ARTICLE_PLAN / "cleaned"
REPORTS = ROOT / "reports and articles"


def gate_row(gate: str, status: str, rule: str, observed: str, meaning: str) -> dict[str, str]:
    return {
        "gate": gate,
        "status": status,
        "rule": rule,
        "observed": observed,
        "meaning": meaning,
    }


def main() -> None:
    resistance = pd.read_csv(CLEANED / "resistance_clean.csv")
    env = pd.read_csv(CLEANED / "environmental_cleaned.csv")
    yearbook_panel = pd.read_csv(CLEANED / "yearbook_province_year_panel.csv")
    yearbook_clean = pd.read_csv(CLEANED / "yearbook_clean.csv")
    province_groups = pd.read_csv(CLEANED / "province_groups.csv")

    canonical_provinces = sorted(province_groups["province"].dropna().unique().tolist())
    canonical_set = set(canonical_provinces)

    resistance_non_national = resistance[resistance["province"] != "National"].copy()
    resistance_province_set = set(resistance_non_national["province"].dropna().unique())
    env_province_set = set(env["province"].dropna().unique())
    panel_province_set = set(yearbook_panel["province"].dropna().unique())
    clean_province_set = set(yearbook_clean["province"].dropna().unique())

    resistance_dupes = int(
        resistance.duplicated(["year", "province", "bacteria_species", "antibiotic"]).sum()
    )
    panel_dupes = int(yearbook_panel.duplicated(["province", "year", "metric"]).sum())

    env_key = [
        "sample_type",
        "matrix",
        "province",
        "location",
        "antibiotic",
        "group_of_antibiotic",
        "concentration_unit",
        "sample_year",
        "season",
        "reference_number",
    ]
    env_dupes = int(env.duplicated(env_key).sum())

    env_coverage_n = len(env_province_set)
    env_missing = sorted(canonical_set - env_province_set)

    join_ok = (
        resistance_province_set == canonical_set
        and env_province_set == canonical_set
        and panel_province_set == canonical_set
        and clean_province_set == canonical_set
    )

    yearbook_images_in_scope = False
    fei_zhao_machine_extractable = False

    qi_zhao_supp = (
        REPORTS
        / "Papers with required data"
        / "Sup 2023 Qi Zhao Current status and trends in antimicrobial use in food animals in China, 2018–2020.docx"
    )
    fei_zhao_main = (
        REPORTS
        / "Papers with required data"
        / "2026 Fei Zhao Trends in Antibiotic Consumption and Antimicrobial Resistance in China- An Ecological Analysis from 2016 to 2022.pdf"
    )
    fei_zhao_supp = (
        REPORTS
        / "Papers with required data"
        / "Sup 2026 Fei Zhao Trends in Antibiotic Consumption and Antimicrobial Resistance in China- An Ecological Analysis from 2016 to 2022.pdf"
    )

    gates: list[dict[str, str]] = []

    gates.append(
        gate_row(
            "Resistance duplicate keys",
            "PASS" if resistance_dupes == 0 else "FAIL",
            "0 duplicate province-year-species-antibiotic rows",
            str(resistance_dupes),
            "Clinical AMR table is structurally safe for province-level aggregation.",
        )
    )

    gates.append(
        gate_row(
            "Environmental duplicate keys",
            "PASS" if env_dupes == 0 else "FAIL",
            "0 duplicate rows on the cleaned environmental key",
            str(env_dupes),
            "Environmental evidence table will not double-count harmonised records silently.",
        )
    )

    gates.append(
        gate_row(
            "Yearbook duplicate keys",
            "PASS" if panel_dupes == 0 else "FAIL",
            "0 duplicate province-year-metric rows",
            str(panel_dupes),
            "Driver panel is structurally safe for joins and panel summaries.",
        )
    )

    gates.append(
        gate_row(
            "Province-name alignment",
            "PASS" if join_ok else "FAIL",
            "All cleaned tables align to the same 31-province set",
            (
                f"resistance={len(resistance_province_set)}/31, "
                f"env={len(env_province_set)}/31, "
                f"yearbook_panel={len(panel_province_set)}/31, "
                f"yearbook_clean={len(clean_province_set)}/31"
            ),
            "Broken joins and silent province drops are unlikely.",
        )
    )

    gates.append(
        gate_row(
            "Env ≥ 30/31 provinces",
            "PASS" if env_coverage_n >= 30 else "FAIL",
            "At least 30 of the 31 CARSS provinces have environmental rows",
            f"{env_coverage_n}/31; missing={env_missing if env_missing else 'none'}",
            "Gap-map comparisons are fair at national province level.",
        )
    )

    gates.append(
        gate_row(
            "Yearbook source-field integrity",
            "PASS" if int(yearbook_panel['source_file'].isna().sum()) == 0 else "FAIL",
            "0 missing source_file rows in yearbook panel",
            str(int(yearbook_panel["source_file"].isna().sum())),
            "Every driver cell in the panel still points back to a documented source path.",
        )
    )

    gates.append(
        gate_row(
            "10-cell yearbook spot-check",
            "PENDING" if not yearbook_images_in_scope else "READY",
            "All 10 sampled province-year-metric cells match source yearbook tables",
            "Pending manual spot-check: source images are outside the current scoped folders.",
            "This is the manual extraction/ OCR sanity gate before Results claims about drivers.",
        )
    )

    gates.append(
        gate_row(
            "CARSS vs Fei Zhao rank rho",
            "PENDING" if not fei_zhao_machine_extractable else "READY",
            "Spearman rho > 0.7 between province ranks in CARSS and Fei Zhao external AMR source",
            (
                "Pending manual extraction: Fei Zhao source PDFs are present but not "
                "machine-readable with current in-scope tools."
            ),
            "This is the external sanity check on provincial AMR pattern concordance.",
        )
    )

    vetabx_inputs_ready = {
        "E12_13_Hogs_year_end",
        "E12_14_Milk",
        "E12_15_Total_Aquatic_Products",
    }.issubset(set(yearbook_panel["metric"].unique()))

    gates.append(
        gate_row(
            "VetABX within 2x national",
            "PENDING",
            "Constructed VetABX proxy calibrated to national reference and no absurd province scaling",
            (
                "Inputs ready in yearbook panel; Qi Zhao 2023 supplement available at "
                f"{qi_zhao_supp.name}; MARA-side class-weight lock still needs manual definition."
                if vetabx_inputs_ready
                else "Yearbook proxy inputs incomplete."
            ),
            "Prevents a scaling bug from making one province look larger than a plausible national benchmark.",
        )
    )

    summary = pd.DataFrame(gates)
    out_dir = ARTICLE_PLAN / "stage1_quality_gates"
    out_dir.mkdir(exist_ok=True)
    summary.to_csv(out_dir / "stage1_quality_gates_summary.csv", index=False)

    pass_n = int((summary["status"] == "PASS").sum())
    fail_n = int((summary["status"] == "FAIL").sum())
    pending_n = int((summary["status"] == "PENDING").sum())

    lines: list[str] = []
    lines.append("# Stage 1 Pre-Results Quality Gates\n")
    lines.append("This report locks the Stage 1 data-quality gates before any manuscript Results text.\n")
    lines.append("## Status summary\n")
    lines.append(f"- PASS: {pass_n}")
    lines.append(f"- FAIL: {fail_n}")
    lines.append(f"- PENDING: {pending_n}\n")
    lines.append("## Gate table\n")
    for row in gates:
        lines.append(f"### {row['gate']}")
        lines.append(f"- Status: `{row['status']}`")
        lines.append(f"- Rule: {row['rule']}")
        lines.append(f"- Observed: {row['observed']}")
        lines.append(f"- Why it matters: {row['meaning']}\n")

    lines.append("## Current interpretation\n")
    lines.append(
        "- The cleaned Article-plan tables pass the key structural gates needed to start province-level analysis: duplicate-key safety, province-name alignment, environmental coverage, and yearbook source-field completeness."
    )
    lines.append(
        "- The remaining blockers are source-dependent validation gates, not table-integrity gates: (1) manual 10-cell yearbook spot-check against source tables, (2) external Fei Zhao province-rank extraction, and (3) final VetABX class-weight lock."
    )
    lines.append(
        "- That means Stage 1 can proceed for internal exploratory summaries, but manuscript-strength Results should wait until those three pending gates are resolved."
    )
    lines.append("\n## Source files used for pending gates\n")
    lines.append(f"- Fei Zhao main: `{fei_zhao_main}`")
    lines.append(f"- Fei Zhao supplement: `{fei_zhao_supp}`")
    lines.append(f"- Qi Zhao supplement: `{qi_zhao_supp}`")

    (out_dir / "stage1_quality_gates_report.md").write_text("\n".join(lines), encoding="utf-8")


if __name__ == "__main__":
    main()
