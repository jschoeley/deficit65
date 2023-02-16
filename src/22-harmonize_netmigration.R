# Harmonize data on net-migration

# Init ------------------------------------------------------------

library(dplyr); library(lubridate); library(stringr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = './src/00-global.R',
  netmigration = 'dat/13-netmigration.rds',
  skeleton = './tmp/10-harmonized_skeleton.rds'
)
paths$output <- list(
  netmigration = 'tmp/22-harmonized_netmigration.rds'
)

netmigration <- list()

# Functions -------------------------------------------------------

source(paths$input$global)

# Load data -------------------------------------------------------

skeleton <- readRDS(paths$input$skeleton)

netmigration$raw <- readRDS(paths$input$netmigration)

# Harmonize -------------------------------------------------------

netmigration$clean <-
  netmigration$raw %>%
  mutate(
    sex = factor(
      sex,
      levels = c('F', 'M', 'T'),
      labels = c('Female', 'Male', 'Totals')
    ),
    age = case_when(
      age == 'Y_LT1' ~ 'Y0',
      age == 'Y_GE100' ~ 'Y100',
      TRUE ~ age
    )
  ) %>%
  filter(
    grepl('^Y[[:digit:]]+$', age),
    projection == 'BSL', # baseline scenario
  ) %>%
  transmute(
    sex = sex,
    region = geo,
    year = year(time),
    age = as.integer(str_sub(age, 2, 4)),
    netmigration_eurostat = values,
    netmigration_source = 'eurostat_proj_19nanmig',
    id = GenerateRowID(region, sex, year, age)
  )

netmigration$ready_for_export <-
  left_join(skeleton, netmigration$clean, by = 'id') %>%
  select(id, netmigration_eurostat, netmigration_source)

# Export ----------------------------------------------------------

saveRDS(netmigration$ready_for_export, file = paths$output$netmigration)
