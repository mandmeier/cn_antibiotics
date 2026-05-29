# Stage 1 Results Skeleton for Atlas Paper

This draft is grounded in the Stage 1 outputs in `Article plan/stage1_outputs`. It is meant to be a manuscript scaffold, not final polished prose.

## 1. Provincial clinical AMR burden in 2024 was heterogeneous across China

Across the 31 provinces, the 2024 composite AMR burden score had a median of 35.80, with an interquartile range of 34.16 to 40.57. The highest burden provinces were Liaoning (47.87), Henan (46.19), Jilin (44.73), Beijing (43.35), and Shanghai (42.67). The lowest burden provinces were Tibet (26.92), Ningxia (32.04), Xinjiang (32.32), Qinghai (32.49), and Zhejiang (32.92).

This pattern indicates that the national clinical resistance landscape is not a simple east-west or coastal-inland split. Several northeastern provinces ranked near the top of the burden distribution, while several western provinces remained at the lower end.

Suggested display items:
- Figure 1A: 2024 provincial AMR burden map
- Figure 1B: ranked lollipop plot of `amr_burden_score_2024`
- Source table: `stage1_outputs/AMR_2024_snapshot.csv`

## 2. Recent AMR trends were not uniform across provinces

When the curated sentinel endpoints were traced across 2019-2024, province-level average slopes showed both worsening and improving trajectories. Jilin had the largest positive mean slope (+0.44 percentage points per year), followed by Heilongjiang (+0.16), Chongqing (+0.14), Beijing (+0.08), and Gansu (+0.07). In contrast, Henan showed the steepest decline (-1.91 percentage points per year), followed by Jiangsu (-0.89), Hubei (-0.83), Anhui (-0.78), and Jiangxi (-0.74).

These results suggest that cross-sectional burden in 2024 and recent trend direction are not interchangeable. Some provinces remain high-burden but are improving, whereas others are moderate-burden but trending upward.

Suggested display items:
- Figure 2A: slope map for curated endpoints
- Figure 2B: burden-versus-trend quadrant plot
- Source table: `stage1_outputs/AMR_trends_2019_2024.csv`

## 3. Environmental antibiotic evidence was nationwide but highly uneven in density

After harmonization, all 31 provinces had at least one environmental antibiotic record. However, evidence density varied sharply. The median province had 133 environmental rows, with an interquartile range of 74.5 to 252.5. Guangdong had by far the largest evidence base (1,745 rows), followed by Jiangsu (679), Tianjin (517), Hebei (395), Shanghai (310), Liaoning (305), Shandong (297), and Zhejiang (295). At the other extreme, Inner Mongolia had 1 row, Tibet 2, Ningxia 5, Qinghai 27, Xinjiang 28, Gansu 36, and Yunnan 41.

This means that nationwide environmental presence is not the same thing as nationwide evidentiary balance. Province-level comparisons are possible, but they must separate environmental coverage from inferred environmental burden.

Suggested display items:
- Figure 3A: map of `env_rows`
- Figure 3B: references-versus-records scatter
- Source table: `stage1_outputs/EnvCoverage_by_province.csv`

## 4. High-concentration environmental signatures were concentrated in specific province-class-matrix combinations

The strongest environmental concentration signatures were not evenly distributed across matrices or antibiotic classes. Among province-class-matrix cells with at least 3 records, the highest median primary concentrations were observed for tetracyclines in Hainan soil, fluoroquinolones in Jiangsu sludge, fluoroquinolones in Tianjin sludge, sulfonamides in Hebei sludge, and fluoroquinolones in Beijing sludge. Additional high-signal combinations included tetracyclines in Hunan sludge and fluoroquinolones in Liaoning sludge.

These early patterns support a matrix-specific reading of environmental burden rather than a single province-level concentration score. Sludge and soil appear especially important in several high-signal provinces.

Suggested display items:
- Figure 4A: province-class-matrix heatmap of median log10 concentrations
- Figure 4B: top burden signatures table
- Source table: `stage1_outputs/EnvBurden_by_province_class_matrix.csv`

## 5. Clinical burden tended to rise with environmental evidence density and structural driver indices, but only modestly

The 2024 AMR burden score showed positive but moderate Spearman associations with environmental evidence density (`env_rows`, rho = 0.34), wastewater-health infrastructure (`WastewaterHealthIndex`, rho = 0.35), economic scale (`EconomicScaleIndex`, rho = 0.33), livestock-aquaculture intensity (`LivestockAquacultureIndex`, rho = 0.29), and unique environmental references (`env_unique_references`, rho = 0.27).

These effect sizes are strong enough to justify deeper modeling, but not strong enough to claim that any single structural driver explains provincial AMR burden by itself. At the atlas stage, the safe interpretation is that clinical burden, environmental evidence, and province-level infrastructure pressures partially align but remain distinct dimensions.

Suggested display items:
- Figure 5A: AMR burden versus env coverage
- Figure 5B: AMR burden versus driver index panels
- Source tables:
  - `stage1_outputs/AMR_2024_snapshot.csv`
  - `stage1_outputs/EnvCoverage_by_province.csv`
  - `stage1_outputs/DriverIndices_by_province.csv`

## 6. Suggested cautious take-home paragraph

Taken together, the Stage 1 atlas results indicate that provincial clinical AMR burden in China is heterogeneous, recent trend directions differ across provinces, and environmental antibiotic evidence is nationwide but strongly imbalanced in density. Environmental concentration signals cluster in specific province-class-matrix combinations rather than appearing uniformly across settings. Structural wastewater, livestock-aquaculture, and economic indices show only moderate concordance with clinical burden, which supports a cautious interpretation: these are candidate contextual drivers, not yet demonstrated determinants.

## 7. Claims that are safe now versus claims that still need validation

Safe now:
- 31-province AMR burden ranking for 2024
- 2019-2024 directional trend summaries for curated endpoints
- 31/31 province environmental coverage after harmonization
- strong imbalance in environmental evidence density
- descriptive province-level driver context for 2024

Still conditional pending validation:
- external concordance claim against Fei Zhao 2026
- yearbook extraction sanity claim based on a fresh 10-cell spot-check
- calibrated VetABX proxy claim

## 8. Files this skeleton is based on

- `Article plan/stage1_outputs/AMR_2024_snapshot.csv`
- `Article plan/stage1_outputs/AMR_trends_2019_2024.csv`
- `Article plan/stage1_outputs/EnvCoverage_by_province.csv`
- `Article plan/stage1_outputs/EnvBurden_by_province_class_matrix.csv`
- `Article plan/stage1_outputs/DriverIndices_by_province.csv`
