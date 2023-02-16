libs <- c("ggplot2",
          "ISOweek",
          "purrr",
          "dplyr",
          "tidyr",
          "yaml",
          "glue",
          "httr",
          "readr",
          "eurostat",
          "gridExtra",
          "stringr",
          "ungroup",
          "lubridate",
          "openxlsx",
          "abind",
          "StMoMo",
          "tidyverse",
          "ggflags",
          "scales")

install.packages(
  libs,
  repos = c(getOption('repos')),
  dep = TRUE
)
