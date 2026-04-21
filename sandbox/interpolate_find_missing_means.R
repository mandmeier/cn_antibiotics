

env_abx <- read_csv("data/raw/environmental_antibiotics_data_manual.csv")



# harmonize units
env_abx_harmonized <- env_abx %>%
  select(-`...15`, -`...16`) %>%
  mutate(concentration_unit = ifelse(grepl("water", sample_type), "ng/L", "μg/kg dw"))




#
# minmax <- test %>%
#   select(mean_concentration, max_concentration) %>%
#   filter(!is.na(mean_concentration) & !is.na(max_concentration)) %>%
#   mutate(maxtomin = max_concentration/mean_concentration) %>%
#   #remove measurements that are both zero
#   filter(mean_concentration > 0 & max_concentration > 0)
#


#
# ### interpolate mean values
# # train model from cases that have BOTH min and max
#
# library(lme4)
#
#
# # =========================
# # 1) INPUT DATA
# # =========================
#
# df <- env_abx_harmonized  # <-- replace this
#
#
# # =========================
# # 2) SAFE CLEANING (log(0) fix)
# # =========================
#
# eps <- 1e-6
#
# df$mean_concentration[df$mean_concentration <= 0] <- NA
# df$max_concentration[df$max_concentration <= 0]   <- NA
#
# df <- df[!is.na(df$max_concentration), ]
#
# df_train <- df[!is.na(df$mean_concentration), ]
# df_pred  <- df[is.na(df$mean_concentration), ]
#
#
# df_train$log_mean <- log(df_train$mean_concentration + eps)
# df_train$log_max  <- log(df_train$max_concentration + eps)
# df_pred$log_max   <- log(df_pred$max_concentration + eps)
#
#
# # =========================
# # 3) FORCE CONSISTENT FACTORS
# # =========================
#
# df_train$sample_type <- factor(df_train$sample_type)
# df_train$antibiotic  <- factor(df_train$antibiotic)
# df_train$province    <- factor(df_train$province)
#
# df_pred$sample_type <- factor(df_pred$sample_type,
#                               levels = levels(df_train$sample_type))
#
# df_pred$antibiotic <- factor(df_pred$antibiotic,
#                              levels = levels(df_train$antibiotic))
#
# df_pred$province <- factor(df_pred$province,
#                            levels = levels(df_train$province))
#
#
# # =========================
# # 4) DROP UNMAPPABLE ROWS (prevents matrix errors)
# # =========================
#
# df_pred <- df_pred[
#   !is.na(df_pred$sample_type) &
#     !is.na(df_pred$antibiotic) &
#     !is.na(df_pred$province) &
#     !is.na(df_pred$log_max),
# ]
#
#
# # =========================
# # 5) FIT MIXED MODEL
# # =========================
#
# fit <- lmer(
#   log_mean ~ log_max + sample_type +
#     (1 | antibiotic) +
#     (1 | province),
#   data = df_train,
#   REML = FALSE
# )
#
#
# # =========================
# # 6) PREDICT (SAFE)
# # =========================
#
# pred_log <- predict(
#   fit,
#   newdata = df_pred,
#   allow.new.levels = TRUE
# )
#
# df_pred$mean_estimated <- exp(pred_log) - eps
#
#
# # =========================
# # 7) MERGE BACK INTO FULL DATASET
# # =========================
#
# df$mean_filled <- df$mean_concentration
# df$mean_filled[is.na(df$mean_filled)] <- df_pred$mean_estimated
#
#
# # =========================
# # 8) QUICK CHECK
# # =========================
#
# train_pred <- predict(fit, allow.new.levels = TRUE)
#
# cor(
#   exp(train_pred),
#   df_train$mean_concentration,
#   use = "complete.obs"
# )
#
#
# #### fill gaps in mean data
#
# with_est_mean <- df %>%
#   select(-mean_concentration) %>%
#   rename(mean_concentration = mean_filled)
#
#
#
# env_abx_filled <- env_abx_harmonized %>%
#   filter(is.na(max_concentration)) %>%
#   bind_rows(with_est_mean)
#
#






#### Select median measured value for each sample_type, province, antibiotic
# goal is to get ONE representative measurement per sample_type, antibiotic and province
# median does not exclude data, and as opposed to using mean or max, we are not susceptible to one-off high measurements from heavily polluted sites

env_abx_cleaned <- env_abx_harmonized %>%
  filter(!is.na(mean_concentration)) %>%
  select(sample_type, province, location, antibiotic, mean_concentration, concentration_unit, sample_year, season, full_reference_name, reference_number) %>%
  unique() %>%
  group_by(sample_type, province, antibiotic) %>%

  # get median reported concentration for all specimens taken in the province
  # need to treat zero values in a defensible way. Likely not true zeroes, but below limit of detection.
  # estimating LOD as "lowest measured value among all measurements in a group / 2"
  ## in cases where NO values are reported > 0 we assume a real NA value (no data measured). These cases are removed
  mutate(
    lod_est = {
      positives <- mean_concentration[mean_concentration > 0]

      if (length(positives) == 0) {
        NA_real_
      } else {
        min(positives, na.rm = TRUE)
      }
    }
  ) %>%
  # replace with LLOD/2 if reported as zero
  mutate(
    mean_concentration = case_when(
      mean_concentration == 0 & !is.na(lod_est) ~ lod_est/2,
      mean_concentration == 0 & is.na(lod_est)  ~ NA_real_,
      TRUE ~ mean_concentration
    )
  ) %>%
  # calculate median
  mutate(median_concentration = median(mean_concentration)) %>%
  select(-mean_concentration) %>%
  # remove true NA measurements (nothing measured for this antibiotic in this province)
  filter(!is.na(median_concentration)) %>%
  unique() %>%
  add_tally() %>%
  # in case of tie use first publication with the reported value
  slice_min(reference_number, n = 1, with_ties = FALSE) %>%
  arrange(sample_type, province, antibiotic) %>%
  select(-n) %>%
  relocate(median_concentration, .after = "antibiotic") %>%
  ungroup() %>%
  select(-lod_est)


write_csv(env_abx_cleaned, "data/cleaned/env_abx_cleaned.csv")
