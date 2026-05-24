# Import and combine environmental antibiotic measurements from raw Excel files.

read_environmental_excel <- function(file, sheet = 1) {
  df <- read_excel(file, sheet = sheet, col_types = "text")

  if (!"article_link" %in% names(df)) {
    return(df)
  }

  wb <- wb_load(file)
  hlinks <- wb$worksheets[[sheet]]$hyperlinks
  rels <- wb$worksheets_rels[[sheet]]

  if (length(hlinks) == 0) {
    return(df)
  }

  refs <- str_match(hlinks, "ref=\"([^\"]+)\"")[, 2]
  rids <- str_match(hlinks, "r:id=\"([^\"]+)\"")[, 2]
  rel_xml <- if (length(rels) == 1) rels else paste(rels, collapse = "")
  rel_map <- setNames(
    str_match_all(rel_xml, "Target=\"([^\"]+)\"")[[1]][, 2],
    str_match_all(rel_xml, "Id=\"([^\"]+)\"")[[1]][, 2]
  )
  targets <- rel_map[rids]
  link_col_letter <- int2col(match("article_link", names(df)))
  link_cols <- str_match(refs, "^([A-Z]+)")[, 2]
  link_rows <- as.integer(str_match(refs, "(\\d+)$")[, 2])
  in_link_col <- !is.na(link_cols) & link_cols == link_col_letter

  if (!any(in_link_col)) {
    return(df)
  }

  url_by_row <- setNames(
    targets[in_link_col],
    as.character(link_rows[in_link_col])
  )
  excel_rows <- as.character(seq_len(nrow(df)) + 1L)
  df$article_link <- coalesce(url_by_row[excel_rows], df$article_link)
  df
}

data_dir <- "data/raw/environmental_data"

files <- sort(list.files(
  data_dir,
  pattern = "[.](xlsx|xls)$",
  full.names = TRUE,
  ignore.case = TRUE
))

combined <- bind_rows(lapply(files, read_environmental_excel))

environmental_data_combined <- combined %>%
  unique()

write_csv(environmental_data_combined, "data/raw/environmental_data/environmental_data_combined.csv")
