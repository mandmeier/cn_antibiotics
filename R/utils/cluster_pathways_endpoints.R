# AMR endpoint and environmental antibiotic-class mapping helpers.

source("R/utils/antibiotic_classes.R")

endpoint_lookup_class <- function(antibiotic) {
  abx <- as.character(antibiotic)
  env_class_norm(assign_antibiotic_group(abx))
}

endpoint_class <- function(antibiotic) {
  endpoint_lookup_class(antibiotic)
}

env_class_norm <- function(value) {
  vapply(value, function(one) {
    x <- tolower(trimws(as.character(one)))
    aliases <- c(
      "quinolones" = "fluoroquinolones",
      "qns" = "fluoroquinolones",
      "sulfonamide" = "sulfonamides",
      "macrolide" = "macrolides",
      "tetracycline" = "tetracyclines",
      "fluoroquinolones" = "fluoroquinolones",
      "sulfonamides" = "sulfonamides",
      "tetracyclines" = "tetracyclines",
      "macrolides" = "macrolides",
      "phenicols" = "phenicols",
      "lincosamides" = "lincosamides",
      "beta-lactams" = "beta-lactams",
      "diaminopyrimidines" = "diaminopyrimidines"
    )
    if (x %in% names(aliases)) unname(aliases[[x]]) else x
  }, character(1), USE.NAMES = FALSE)
}

endpoint_matches_env_class <- function(endpoint, env_class) {
  antibiotic <- sub("^.* \\| ", "", endpoint)
  endpoint_lookup_class(antibiotic) == env_class_norm(env_class)
}
