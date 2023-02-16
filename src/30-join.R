# Assemble data basis for pop projection

# Init ------------------------------------------------------------

library(dplyr); library(readr); library(openxlsx)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config = './cfg/config.yaml',
  skeleton = './tmp/10-harmonized_skeleton.rds',
  global = './src/00-global.R',
  population = './tmp/20-harmonized_population.rds',
  death = './tmp/21-harmonized_death.rds',
  netmigration = './tmp/22-harmonized_netmigration.rds',
  lifeexpectancy = './tmp/23-harmonized_lifeexpectancy.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  out_rds = './out/30-projectioninput.rds',
  out_csv = './out/30-projectioninput.csv',
  out_xlsx = './out/30-projectioninput.xlsx'
)

# list containers for analysis artifacts
dat <- list()

# Functions -------------------------------------------------------

source(paths$input$global)

# Data ------------------------------------------------------------

dat$skeleton <- readRDS(paths$input$skeleton)
dat$population <- readRDS(paths$input$population)
dat$death <- readRDS(paths$input$death)
dat$netmigration <- readRDS(paths$input$netmigration)
dat$lifeexpectancy <- readRDS(paths$input$lifeexpectancy)

# Join ------------------------------------------------------------

dat$projectioninput <-
  dat$skeleton %>% 
  mutate(nweeks_year = ifelse(YearHasIsoWeek53(year), 53L, 52L)) %>%
  left_join(dat$death, by = 'id') %>%
  left_join(dat$population, by = 'id') %>%
  left_join(dat$netmigration, by = 'id') %>%
  left_join(dat$lifeexpectancy, by = 'id')

# Export ----------------------------------------------------------

saveRDS(dat$projectioninput, file = paths$output$out_rds)

write_csv(dat$projectioninput, file = paths$output$out_csv)

write.xlsx(dat$projectioninput, file = paths$output$out_xlsx,
           keepNA = TRUE, na.string = '.',
           firstRow = TRUE, firstCol = TRUE, overwrite = TRUE)
