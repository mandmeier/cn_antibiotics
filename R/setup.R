#setup

# use china mirror for faster package install
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
options(scipen = 999)

suppressPackageStartupMessages({
  library(conflicted)
  library(dplyr)
  library(factoextra)
  library(ggplot2)
  library(ggpubr)
  library(ggrepel)
  library(Hmisc)
  library(patchwork)
  library(readr)
  library(openxlsx2)
  library(readxl)
  library(sf)
  library(stringr)
  library(tibble)
  library(tidyr)


})



conflicts_prefer(dplyr::filter)
conflicts_prefer(plotly::layout)


# set paths to install units package (dependency of sf package)
Sys.setenv(
  UDUNITS2_INCLUDE = "/opt/homebrew/include",
  UDUNITS2_LIBS = "/opt/homebrew/lib"
)

# paths to GDAL software (dependency of sf package)
Sys.setenv(
  PATH = paste("/opt/homebrew/bin", Sys.getenv("PATH"), sep=":"),
  GDAL_CONFIG = "/opt/homebrew/bin/gdal-config",
  PROJ_LIB = "/opt/homebrew/share/proj"
)
