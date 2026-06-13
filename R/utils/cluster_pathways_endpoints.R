# AMR endpoint and environmental antibiotic-class mapping helpers.

endpoint_class <- function(antibiotic) {
  a <- tolower(as.character(antibiotic))
  if (grepl("ciprofloxacin|levofloxacin", a)) return("fluoroquinolones")
  if (grepl("erythromycin|azithromycin|clarithromycin", a)) return("macrolides")
  if (grepl("sulfamethoxazole|sulfonamide", a)) return("sulfonamides")
  if (grepl("tetracycline|tigecycline", a)) return("tetracyclines")
  if (grepl("ceph|cef", a)) return("cephalosporins")
  if (grepl("imipenem|meropenem|ertapenem", a)) return("carbapenems")
  if (grepl("gentamicin|amikacin|tobramycin", a)) return("aminoglycosides")
  "other"
}

env_class_norm <- function(value) {
  vapply(value, function(one) {
    x <- tolower(trimws(as.character(one)))
    aliases <- c(
      "quinolones" = "fluoroquinolones",
      "qns" = "fluoroquinolones",
      "sulfonamide" = "sulfonamides",
      "macrolide" = "macrolides",
      "tetracycline" = "tetracyclines"
    )
    if (x %in% names(aliases)) unname(aliases[[x]]) else x
  }, character(1), USE.NAMES = FALSE)
}

endpoint_matches_env_class <- function(endpoint, env_class) {
  antibiotic <- if (grepl(" \\| ", endpoint)) {
    strsplit(endpoint, " \\| ")[[1]][2]
  } else {
    endpoint
  }
  endpoint_class(antibiotic) == env_class_norm(env_class)
}
