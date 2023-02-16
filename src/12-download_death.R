# Download data on death counts
#
# (1) Download weekly death count data by age, sex, and region from
#     STMF input file

# Init ------------------------------------------------------------

library(yaml)
library(httr)
library(purrr); library(dplyr); library(readr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  # STMF input data files
  url_stmf = 'https://www.mortality.org/File/GetDocument/Public/STMF/Inputs/STMFinput.zip'
)
paths$output <- list(
  stmf = './dat/12-deaths.rds',
  stmf_zip = './tmp/12-stmf.zip'
)

# global configuration
config <- read_yaml(paths$input$config)

# list containers for analysis artifacts
dat <- list()

# Download STMF data ----------------------------------------------

# download international weekly death counts by age and sex from STMF
dat$stmf_zip <-
  GET(url = paths$input$url_stmf, progress())

# Preliminary format STMF data ------------------------------------

# save downloaded zip to file
writeBin(
  object = content(dat$stmf_zip, 'raw'),
  con = paths$output$stmf_zip
)

# list all files in archive
dat$stmf_filenames <- unzip(
  paths$output$stmf_zip,
  list = TRUE
)[['Name']]

# bind all .csv files in stmf zip archive into single file
dat$stmf <-
  dat$stmf_filenames %>%
  map(~{
    unz(paths$output$stmf_zip, filename = .x) %>%
      read_csv(
        col_names = c('PopCode', 'Area', 'Year', 'Week', 'Sex', 'Age',
                      'AgeInterval', 'Deaths', 'Type', 'Access'),
        col_types = 'ciiccccdcc',
        skip = 1,
        na = '.'
      )
  }) %>%
  bind_rows()

# Export ----------------------------------------------------------

saveRDS(dat$stmf, file = paths$output$stmf)
