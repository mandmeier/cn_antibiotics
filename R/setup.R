#setup

# use china mirror for faster package install
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
options(scipen = 999)

suppressPackageStartupMessages({
  library(cluster)
  library(conflicted)
  library(factoextra)
  library(ggplot2)
  library(ggpubr)
  library(Hmisc)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)

  library(dplyr) # load last for dplyr::filter

})



conflict_prefer("filter", "dplyr")

