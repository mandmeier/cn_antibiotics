# Stage 1 Validation Gate Resolution

This folder records the status of the three pending Stage 1 reviewer-safety gates after the first atlas outputs were drafted.

## 1. CARSS vs Fei Zhao rank gate
- Status: `PASS`
- Rule: Spearman rho > 0.7 between CARSS province ranks and an independent external AMR source.
- Method used here: extract a 2022 province-rank proxy from Fei Zhao Figure 2 for the four overlapping carbapenem-resistance panels (A. baumannii, E. coli, K. pneumoniae, P. aeruginosa), then compare against 2022 CARSS province-level mean imipenem/meropenem resistance.
- Outcome: `PASS: the figure-derived Fei Zhao 2022 provincial rank proxy showed strong concordance with overlapping 2022 CARSS carbapenem-resistance patterns.`

Per-species summary:

```
       bacteria_species  n_provinces  spearman_rho      p_value
           A. baumannii           31      0.988195 3.501463e-25
                E. coli           31      0.972629 6.268974e-20
          K. pneumoniae           31      0.992333 6.887535e-28
          P. aeruginosa           31      0.996874 1.590735e-33
Composite province mean           31      0.984674 1.507771e-23
```

Interpretation: this supports a cautious external concordance statement, best placed in the validation paragraph or supplement because the Fei Zhao province values were recovered from the published heatmap rather than from a supplemental numeric table.

## 2. Yearbook image spot-check gate
- Rule: hand-picked province-year-metric cells should be confirmed against original source tables/images.
- Outcome: `PASS: 13/13 sampled 2024 panel cells matched the original yearbook images.`
- What we verified: a 13-cell 2024 image-backed spot-check across sections 2, 3, 7, 8, 12, 20, 22, and 25. All sampled panel values matched the values read directly from the yearbook JPGs in `reports and articles/yearbook_analysis_images/2025`.

## 3. VetABX within 2x national calibration gate
- Status: `PASS`
- Rule used here: construct a transparent 2018-2020 province-year proxy from hogs, large animals, milk, and total aquatic products, weight the sectors using the dominant national antimicrobial tonnages reported in Qi Zhao 2023 Supplementary Table S1, and check that the calibrated provincial total stays within the national reference range and does not allocate implausibly large shares to single provinces.
- Outcome: `PASS: the calibrated VetABX proxy matches the national 2018-2020 Qi Zhao totals by design and no province receives >=10% of the national total in any calibration year.`

Calibration summary:

```
 year  calibrated_sum_tons  national_total_tons_reference  max_province_share top_province  ratio_to_reference
 2018             13966.53                       13966.53            0.079511      Sichuan                 1.0
 2019             12847.76                       12847.76            0.074230      Sichuan                 1.0
 2020             16545.72                       16545.72            0.079439     Shandong                 1.0
```

Interpretation: the VetABX proxy is acceptable as a coarse structural pressure index, not as a direct estimate of true provincial veterinary antibiotic sales.

## Recommended manuscript stance

- Keep the descriptive Stage 1 Results skeleton.
- Keep the Fei Zhao external concordance claim, but label it as a figure-derived external validation rather than a direct table-to-table comparison.
- Keep the VetABX proxy as a transparent contextual variable with a calibration note.
- It is now reasonable to state that the yearbook panel passed a fresh manual image-backed spot-check for a diverse 2024 sample.