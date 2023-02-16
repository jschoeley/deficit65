# Download data on population & fertility estimates and projections
#
# We download yearly population midyear, january 1st population, and
# fertility rate estimates and projections from WPP by sex, age and
# region.

# Init ------------------------------------------------------------

library(httr); library(yaml); library(readr)
library(dplyr); library(tidyr); library(purrr); library(glue)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config = './cfg/config.yaml',
  region_meta = './cfg/region_metadata.csv',
  # WPP january 1st population estimates
  url_wpp_jan1st_estimates = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip',
  url_wpp_jan1st_projections = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip',
  zipfilename_wpp_jan1st_estimates = 'WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv',
  zipfilename_wpp_jan1st_projections = 'WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv',
  # WPP midyear 1st population estimates
  url_wpp_midyear_estimates = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationBySingleAgeSex_Medium_1950-2021.zip',
  url_wpp_midyear_projections = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationBySingleAgeSex_Medium_2022-2100.zip',
  zipfilename_wpp_midyear_estimates = 'WPP2022_PopulationBySingleAgeSex_Medium_1950-2021.csv',
  zipfilename_wpp_midyear_projections = 'WPP2022_PopulationBySingleAgeSex_Medium_2022-2100.csv',
  # WPP age specific fertility rates
  url_wpp_fertility_projections = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Fertility_by_Age1.zip',
  zipfilename_wpp_fertility_projections = 'WPP2022_Fertility_by_Age1.csv',
  # WPP constant mortality population projections
  url_wpp_midyear_projectionsconstantmortality = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationBySingleAgeSex_Constant%20mortality_2022-2100.zip',
  zipfilename_wpp_midyear_projectionsconstantmortality = 'WPP2022_PopulationBySingleAgeSex_Constant mortality_2022-2100.csv'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  wpp = './dat/11-population.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# meta data on regions
region_meta <- read_csv(paths$input$region_meta, na = '.')

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  # lookup table for region codes
  # only countries defined in skeleton
  region_lookup = 
    region_meta %>%
    filter(region_code_iso3166_2 %in% config$skeleton$region) %>%
    select(region_code_iso3166_2, region_code_wpp) %>%
    drop_na()
  # first year in harmonized data set
  skeleton_first_year = config$skeleton$year$start
  # last year in harmonized data set
  skeleton_final_year = config$skeleton$year$end
})

# list containers for analysis artifacts
dat <- list()

# Download WPP data -----------------------------------------------

# download World Population Prospects
# - single year-age population estimates 1950-2022
#   (january 1st & midyear)
# - single year-age population projections 2023-2100
#   (january 1st & midyear)
# - single year-age fertility rate projections 1950-2100
dat$wpp_get <- map(
  c(
    wpp_popjan1st_estimates =
      paths$input$url_wpp_jan1st_estimates,
    wpp_popjan1st_projections =
      paths$input$url_wpp_jan1st_projections,
    wpp_popmidyear_estimates =
      paths$input$url_wpp_midyear_estimates,
    wpp_popmidyear_projections =
      paths$input$url_wpp_midyear_projections,
    wpp_popmidyearcnstmo_projections =
      paths$input$url_wpp_midyear_projectionsconstantmortality,
    wpp_fertility_projections =
      paths$input$url_wpp_fertility_projections
  ), ~{
    GET(url = .x, progress())
  })

# save downloaded zip to file
iwalk(dat$wpp_get, ~{
  writeBin(
    object = content(.x, 'raw'),
    con = glue('{paths$input$tmpdir}/11-{.y}.zip')
  )
})

# Preliminary format of WPP data ----------------------------------

# long format data frame of WPP population data
dat$wpp_unzip <-
  map2(
    .x = names(dat$wpp_get),
    .y = c(paths$input$zipfilename_wpp_jan1st_estimates,
           paths$input$zipfilename_wpp_jan1st_projections,
           paths$input$zipfilename_wpp_midyear_estimates,
           paths$input$zipfilename_wpp_midyear_projections,
           paths$input$zipfilename_wpp_midyear_projectionsconstantmortality,
           paths$input$zipfilename_wpp_fertility_projections),
    ~{
      pth <- paste0(paths$input$tmpdir, '/11-', .x, '.zip')
      # unzip
      unz(pth, .y) %>%
        read_csv() %>%
        filter(
          # subset to regions of interest
          LocID %in% cnst$region_lookup$region_code_wpp,
          # subset to years of interest
          Time %in% seq(cnst$skeleton_first_year, cnst$skeleton_final_year, 1)
        ) %>%
        mutate(population_source = .x)
    })
names(dat$wpp_unzip) <- names(dat$wpp_get)

# bind all WPP data in a long dataframe
dat$wpp_df <- bind_rows(
  dat$wpp_unzip$wpp_popjan1st_estimates %>%
    filter(Variant %in% c('Medium')) %>%
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  dat$wpp_unzip$wpp_popjan1st_projections %>%
    filter(Variant %in% c('Medium')) %>%
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  dat$wpp_unzip$wpp_popmidyear_estimates %>%
    filter(Variant %in% c('Medium')) %>%
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  dat$wpp_unzip$wpp_popmidyear_projections %>%
    filter(Variant %in% c('Medium')) %>%
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  dat$wpp_unzip$wpp_popmidyearcnstmo_projections %>%
    filter(Variant %in% c('Constant mortality')) %>%
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  dat$wpp_unzip$wpp_fertility_projections %>%
    filter(Variant %in% c('Medium')) %>%
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = ASFR) %>%
    mutate(Male = NA)
)

# Export ----------------------------------------------------------

saveRDS(dat$wpp_df, file = paths$output$wpp)
