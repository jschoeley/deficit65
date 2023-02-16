# Harmonize data on net-migration

# Init ------------------------------------------------------------

library(glue)
library(dplyr); library(lubridate); library(stringr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = './src/00-global.R',
  lifeexpectancy = 'dat/14-lifeexpectancy.rds',
  skeleton = './tmp/10-harmonized_skeleton.rds'
)
paths$output <- list(
  lifeexpectancy = 'tmp/23-harmonized_lifeexpectancy.rds'
)

lifeexpectancy <- list()

# Functions -------------------------------------------------------

source(paths$input$global)

# Load data -------------------------------------------------------

skeleton <- readRDS(paths$input$skeleton)

lifeexpectancy$raw <- readRDS(paths$input$lifeexpectancy)

# Harmonize -------------------------------------------------------

lifeexpectancy$clean <-
  lifeexpectancy$raw %>%
  mutate(
    sex = factor(
      sex,
      levels = c('F', 'M', 'T'),
      labels = c('Female', 'Male', 'Totals')
    ),
    age = case_when(
      age == 'Y_LT1' ~ 'Y00',
      age == 'Y_GE85' ~ 'Y85',
      TRUE ~ age
    )
  ) %>%
  filter(
    grepl('^Y[[:digit:]]+$', age)
  ) %>%
  transmute(
    sex = sex,
    region = geo,
    year = year(time),
    age = as.integer(str_sub(age, 2, 4)),
    lifeexpectancy_eurostat = values,
    lifeexpectancy_source = 'eurostat_demo_mlexpec',
    id = GenerateRowID(region, sex, year, age)
  )

lifeexpectancy$ready_for_export <-
  left_join(skeleton, lifeexpectancy$clean, by = 'id') %>%
  select(id, lifeexpectancy_eurostat, lifeexpectancy_source)

# Export ----------------------------------------------------------

saveRDS(lifeexpectancy$ready_for_export, file = paths$output$lifeexpectancy)
