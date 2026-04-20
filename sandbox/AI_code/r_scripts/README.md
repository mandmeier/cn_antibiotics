R analysis bundle for independent verification

Included scripts:
- analyze_k3_relationships.R
- analyze_k3_source_attribution.R
- analyze_dataset_novelty.R
- analyze_k3_source_groups.R
- analyze_saureus_clindamycin_adjustment.R
- common_utils.R

Required R packages:
- ggplot2
- nnet
- randomForest
- cluster

Usage examples:
- Rscript analyze_k3_relationships.R --dataset_dir ../Dataset --environment_csv ../artifacts/article_quality_table/article_quality_table.csv --output_dir ../artifacts/k3_relationships_r
- Rscript analyze_k3_source_attribution.R --dataset_dir ../Dataset --environment_csv ../artifacts/article_quality_table/article_quality_table.csv --output_dir ../artifacts/k3_source_attribution_r
- Rscript analyze_dataset_novelty.R --dataset_dir ../Dataset --environment_csv ../artifacts/article_quality_table/article_quality_table.csv --output_dir ../artifacts/dataset_novelty_r
- Rscript analyze_k3_source_groups.R --dataset_dir ../Dataset --source_attribution_csv ../artifacts/k3_source_attribution_article_quality/province_source_attribution.csv --output_dir ../artifacts/k3_source_groups_r
- Rscript analyze_saureus_clindamycin_adjustment.R --feature_table ../artifacts/k3_relationships_article_quality/feature_table.csv --source_attribution ../artifacts/k3_source_attribution_article_quality/province_source_attribution.csv --output_dir ../artifacts/k3_relationships_adjusted_r

Notes:
- These are R-native ports of the core Python analyses used in the publication workflow.
- They preserve the same inputs, output file structure where practical, and the same high-level statistical procedures.
- Some model-fitting internals differ slightly because the implementations are R-native rather than direct Python bindings.