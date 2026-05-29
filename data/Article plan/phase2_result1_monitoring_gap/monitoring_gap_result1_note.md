# Phase 2 Result 1: Monitoring Gap Atlas

## Definition

- `AMR burden percentile`: percentile rank of `amr_burden_score_2024`.
- `Environmental evidence percentile`: mean of percentile ranks for `env_rows`, `env_unique_matrices`, and `env_year_span`.
- `Environmental monitoring weakness`: `1 - environmental evidence percentile`.
- `Monitoring gap priority score`: `AMR burden percentile × environmental monitoring weakness`.

This score rises only when a province is simultaneously high in clinical AMR burden and weak in environmental evidence.

## Hypothesis Check

Hypothesis IG1: high AMR provinces are low-monitored environmentally.

- Spearman correlation between AMR burden percentile and environmental evidence percentile: `0.215`.
- Provinces meeting the strict rule `top quartile AMR + bottom quartile environmental evidence`: `1` of `8` high-AMR provinces.

Interpretation:
- The direction is not consistent with a blanket IG1 pattern: higher-AMR provinces do not generally have weaker environmental evidence; if anything, the province-level relationship is slightly positive.
- Only one province met the strict high-AMR/low-monitoring rule, so IG1 is best described as a targeted blind-spot problem rather than a national rule.

## Priority Provinces

Top provinces by monitoring-gap priority score:

```
 priority_rank       province  amr_burden_score_2024  amr_burden_percentile  env_rows  env_unique_matrices  env_year_span  env_evidence_percentile  env_monitoring_weakness  monitoring_gap_priority_score            quadrant  strict_h1_priority
             1 Inner Mongolia              41.211845               0.838710         1                    1            1.0                 0.053763                 0.946237                       0.793618  High AMR / Low Env                True
             2          Jilin              44.730299               0.935484        79                    3            7.0                 0.258065                 0.741935                       0.694069  High AMR / Low Env               False
             3          Gansu              37.618068               0.580645        36                    1            1.0                 0.107527                 0.892473                       0.518210  High AMR / Low Env               False
             4          Anhui              38.710310               0.645161        85                    3            7.0                 0.279570                 0.720430                       0.464794  High AMR / Low Env               False
             5        Shaanxi              39.997005               0.709677       122                    4           10.0                 0.435484                 0.564516                       0.400624  High AMR / Low Env               False
             6          Hubei              37.716890               0.612903       164                    2            9.0                 0.376344                 0.623656                       0.382241  High AMR / Low Env               False
             7   Heilongjiang              41.098100               0.806452        82                    6           10.0                 0.532258                 0.467742                       0.377211 High AMR / High Env               False
             8          Henan              46.188209               0.967742       210                    5           12.0                 0.645161                 0.354839                       0.343392 High AMR / High Env               False
             9         Yunnan              35.743661               0.483871        41                    2           12.0                 0.306452                 0.693548                       0.335588   Low AMR / Low Env               False
            10       Liaoning              47.870800               1.000000       305                    5           12.0                 0.677419                 0.322581                       0.322581 High AMR / High Env               False
```

## Readout

- Provinces such as Inner Mongolia and Jilin rise to the top because they combine high AMR percentiles with weak environmental evidence.
- Provinces such as Liaoning, Henan, Beijing, and Shanghai remain high-AMR but are not top monitoring-gap priorities because their environmental evidence base is not especially weak.
- The atlas therefore points to selective surveillance blind spots rather than a universal inverse relationship between AMR and monitoring.