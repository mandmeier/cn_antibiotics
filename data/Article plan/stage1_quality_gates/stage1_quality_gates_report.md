# Stage 1 Pre-Results Quality Gates

This report locks the Stage 1 data-quality gates before any manuscript Results text.

## Status summary

- PASS: 6
- FAIL: 0
- PENDING: 3

## Gate table

### Resistance duplicate keys
- Status: `PASS`
- Rule: 0 duplicate province-year-species-antibiotic rows
- Observed: 0
- Why it matters: Clinical AMR table is structurally safe for province-level aggregation.

### Environmental duplicate keys
- Status: `PASS`
- Rule: 0 duplicate rows on the cleaned environmental key
- Observed: 0
- Why it matters: Environmental evidence table will not double-count harmonised records silently.

### Yearbook duplicate keys
- Status: `PASS`
- Rule: 0 duplicate province-year-metric rows
- Observed: 0
- Why it matters: Driver panel is structurally safe for joins and panel summaries.

### Province-name alignment
- Status: `PASS`
- Rule: All cleaned tables align to the same 31-province set
- Observed: resistance=31/31, env=31/31, yearbook_panel=31/31, yearbook_clean=31/31
- Why it matters: Broken joins and silent province drops are unlikely.

### Env ≥ 30/31 provinces
- Status: `PASS`
- Rule: At least 30 of the 31 CARSS provinces have environmental rows
- Observed: 31/31; missing=none
- Why it matters: Gap-map comparisons are fair at national province level.

### Yearbook source-field integrity
- Status: `PASS`
- Rule: 0 missing source_file rows in yearbook panel
- Observed: 0
- Why it matters: Every driver cell in the panel still points back to a documented source path.

### 10-cell yearbook spot-check
- Status: `PENDING`
- Rule: All 10 sampled province-year-metric cells match source yearbook tables
- Observed: Pending manual spot-check: source images are outside the current scoped folders.
- Why it matters: This is the manual extraction/ OCR sanity gate before Results claims about drivers.

### CARSS vs Fei Zhao rank rho
- Status: `PENDING`
- Rule: Spearman rho > 0.7 between province ranks in CARSS and Fei Zhao external AMR source
- Observed: Pending manual extraction: Fei Zhao source PDFs are present but not machine-readable with current in-scope tools.
- Why it matters: This is the external sanity check on provincial AMR pattern concordance.

### VetABX within 2x national
- Status: `PENDING`
- Rule: Constructed VetABX proxy calibrated to national reference and no absurd province scaling
- Observed: Inputs ready in yearbook panel; Qi Zhao 2023 supplement available at Sup 2023 Qi Zhao Current status and trends in antimicrobial use in food animals in China, 2018–2020.docx; MARA-side class-weight lock still needs manual definition.
- Why it matters: Prevents a scaling bug from making one province look larger than a plausible national benchmark.

## Current interpretation

- The cleaned Article-plan tables pass the key structural gates needed to start province-level analysis: duplicate-key safety, province-name alignment, environmental coverage, and yearbook source-field completeness.
- The remaining blockers are source-dependent validation gates, not table-integrity gates: (1) manual 10-cell yearbook spot-check against source tables, (2) external Fei Zhao province-rank extraction, and (3) final VetABX class-weight lock.
- That means Stage 1 can proceed for internal exploratory summaries, but manuscript-strength Results should wait until those three pending gates are resolved.

## Source files used for pending gates

- Fei Zhao main: `/Users/kali/Projects/antibiotics_in_wwt/reports and articles/Papers with required data/2026 Fei Zhao Trends in Antibiotic Consumption and Antimicrobial Resistance in China- An Ecological Analysis from 2016 to 2022.pdf`
- Fei Zhao supplement: `/Users/kali/Projects/antibiotics_in_wwt/reports and articles/Papers with required data/Sup 2026 Fei Zhao Trends in Antibiotic Consumption and Antimicrobial Resistance in China- An Ecological Analysis from 2016 to 2022.pdf`
- Qi Zhao supplement: `/Users/kali/Projects/antibiotics_in_wwt/reports and articles/Papers with required data/Sup 2023 Qi Zhao Current status and trends in antimicrobial use in food animals in China, 2018–2020.docx`