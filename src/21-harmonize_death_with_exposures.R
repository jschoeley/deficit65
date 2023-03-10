# Harmonize data on death counts
#
# We harmonize death counts data from:
#
# (1): The HMD-STMF input data base for weekly death counts by age
#      and sex for numerous countries. This is our main source. The data
#      is updated weekly and includes the most recent information on
#      death counts.
#
# We then perform a model based ungrouping (doi:10.1093/aje/kwv020)
# of deaths into single ages for each region-year-sex stratum. During
# this ungrouping we also derive person-weeks of exposure from the
# mid-year population counts for each age within a stratum.
#
# Exposures are adjusted for the length of the observation period within
# a year, taking into weeks missing from the input data and leap-weeks.
#
# Because the age grouping scheme may vary within a year, the
# ungrouping is separately applied to each
# region x sex x year x age pattern combination in the input data.
# The ungrouped STMF deaths and exposures are then aggregated into
# annual death counts by age.

# Init ------------------------------------------------------------

library(glue)
library(yaml); library(readr)
library(dplyr); library(purrr); library(tidyr); library(stringr); library(ISOweek)
library(ungroup)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  region_meta = './cfg/region_metadata.csv',
  
  # path to global objects
  global = './src/00-global.R',
  # path to pclm output
  pclm = './tmp',
  # path to logfile for pclm
  pclm_log = './tmp/21-pclm_log.txt',
  # skeleton path
  skeleton = './tmp/10-harmonized_skeleton.rds',
  # path to stmf data
  stmf = './dat/12-deaths.rds',
  # path to harmonized population
  population = './tmp/20-harmonized_population.rds'
)
paths$output <- list(
  # harmonized deaths
  harmonized = './tmp/21-harmonized_death.rds',
  fig = './out'
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
    select(region_code_iso3166_2, region_code_stmf) %>%
    drop_na()
  
  stmf_regions_reporting_iso_week_year =
    region_meta %>%
    filter(calendar_stmf == 'iso_week_date') %>%
    pull(region_code_iso3166_2)
  
  # lookup table for sex codes by source
  code_sex_stmf =
    c(m = config$skeleton$sex$Male, f = config$skeleton$sex$Female)
  # pclm life-table closeout age
  pclm_highest_age = 110
  
})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# Functions -------------------------------------------------------

# global functions
source(paths$input$global)

MidyearPop2Exposures <- function (midyearpop, nweeks_observed) {
  midyearpop*nweeks_observed/52
}

MakeAgeGroupPattern <- function (age_start) {
  paste0(age_start, collapse = '-')
}

ConstructReadyForUngroup <- function (
  df,
  region, sex, year, agegroup_pattern, age_start,
  age_width, deaths, nweeksobserved, nweeksyear, source
) {
  require(dplyr)
  df %>%
    arrange(region, sex, year, age_start) %>%
    mutate(source = source) %>%
    select(
      region, sex, year, agegroup_pattern, age_start,
      deaths, nweeksobserved, nweeksyear, source
    ) %>%
    nest(
      'deaths_{source}' := c(
        agegroup_pattern, age_start, deaths,
        nweeksobserved, nweeksyear, source
      )
    )
}

# Load data -------------------------------------------------------

# skeleton
dat$skeleton <- readRDS(paths$input$skeleton)

# harmonized population
dat$population <- readRDS(paths$input$population)

# stmf death counts
dat$stmf <- readRDS(paths$input$stmf)

# STMF prepare for ungroup ----------------------------------------

dat$stmf_harmonized_labels <-
  dat$stmf %>%
  # harmonize variable names
  select(
    region_code_stmf = PopCode,
    year = Year, iso_week = Week,
    sex = Sex, age_start = Age, age_width = AgeInterval,
    deaths = Deaths
  ) %>%
  # subset to data of interest
  filter(
    # ignore total sex category
    sex != 'b',
    # ignore total age category
    # ignore deaths with unknown age for now
    !(age_start %in% c('TOT', 'UNK')),
    # subset to years of interest
    year %in% seq(config$skeleton$year$start, config$skeleton$year$end, 1)
  ) %>%
  mutate(
    # harmonize sex to common format
    sex = as.character(factor(
      sex, levels = names(cnst$code_sex_stmf),
      labels = cnst$code_sex_stmf)
    ),
    # add iso region codes
    region = as.character(factor(
      region_code_stmf,
      levels = region_meta$region_code_stmf,
      labels = region_meta$region_code_iso3166_2
    )),
    # convert week variable to numeric
    # deaths with unknown week are coded as week 0
    iso_week = as.numeric(ifelse(iso_week == 'UNK', '0', iso_week)),
    # convert age variables to numeric
    age_start = as.numeric(age_start),
    # if open age group, code width as Inf
    age_width = as.numeric(ifelse(age_width == '+', 'Inf', age_width))
  ) %>%
  select(region, sex, year, everything(), -region_code_stmf)

# (1) within each region-sex-year-week stratum, check what kind of
# age grouping pattern is used and give it a unique label
# (2) within each region-sex-year stratum, disregard the weeks and
# just iterate over the unique age grouping patterns, creating
# age-specific yearly death counts by unique age grouping pattern

# region, sex, year: strata
# agegroup_pattern: the age grouping pattern, possibly multiple per stratum
# age_start, age_width
# deaths: summed over weeks of year into for weeks reporting same
#         agegroup_pattern
# nweeksobserved: number of completely observed weeks contained in the
#                 summed death counts
# nweeksyear: the number of weeks in the year, may vary according to the
#             calendar used by the data provider
# source: where does the data originate?
dat$stmf_ready_for_ungroup <-
  dat$stmf_harmonized_labels %>%
  arrange(region, sex, year, iso_week, age_start) %>%
  group_by(region, sex, year, iso_week) %>%
  # the STMF input files can be unreliable, therefore we check for
  # missing data within each week before we sum over weeks.
  # for each week within a region-sex-year stratum determine
  # - occurrence of any missing values in death count column, i.e.
  #   missing deaths within any age groups, or implicitly missing
  #   age groups
  mutate(
    # explicit missing deaths in any age
    any_explicit_missing_deaths =
      any(is.na(deaths)),
    # implicitly missing age groups whenever age_start + age_width
    # don't align with the following age_start
    any_implicit_missing_deaths =
      any(head(age_start + age_width, -1) != age_start[-1]),
    any_missing_deaths =
      any_explicit_missing_deaths | any_implicit_missing_deaths,
  ) %>%
  # drop a week from further consideration if it features missing deaths
  filter(!any_missing_deaths) %>%
  # this is the age grouping scheme used in a particular week
  # it may change between weeks
  mutate(agegroup_pattern = MakeAgeGroupPattern(age_start)) %>%
  # now sum deaths over weeks within each
  # region-sex-year-age_pattern stratum
  group_by(region, sex, year, agegroup_pattern, age_start) %>%
  summarise(
    deaths = sum(deaths),
    # number of completely observed weeks being summed
    nweeksobserved = length(unique(iso_week[iso_week != 0]))
  ) %>%
  ungroup() %>%
  # - number of weeks in reporting year, i.e. if the deaths are reported
  #   over the Gregorian calendar, there would be approximately 52 weeks
  #   in every year, for an ISO-week-date calendar there would either be
  #   52 or 53 weeks in a year. 
  mutate(
    nweeksyear = case_when(
      # if STMF region reports over iso-week-date and year has leap-week
      # use 53 weeks as reporting length
      (region %in% cnst$stmf_regions_reporting_iso_week_year) & 
        YearHasIsoWeek53(year) ~ 53,
      # for all else use 52 weeks
      TRUE ~ 52
    )
  ) %>%
  ConstructReadyForUngroup(
    region, sex, year, agegroup_pattern, age_start,
    deaths, nweeksobserved, nweeksyear, source = 'stmf'
  )

# Choose which sources to use -------------------------------------

dat$all_ready_for_ungroup <-
  expand_grid(
    region = config$skeleton$region,
    sex = unlist(config$skeleton$sex),
    year = seq(config$skeleton$year$start, config$skeleton$year$end, 1)
  ) %>%
  left_join(dat$stmf_ready_for_ungroup) %>%
  select(region, sex, year, deaths = deaths_stmf) %>%
  unnest(deaths)

# PCLM age harmonization ------------------------------------------

# prepare midyear population for pclm exposures
dat$midyearpop_for_pclm <- left_join(dat$skeleton, dat$population)

dat$ungrouped_by_unique_age_pattern <-
  dat$all_ready_for_ungroup %>%
  group_by(region, sex, year, agegroup_pattern) %>%
  group_modify(~{
    
    # number of age groups
    nageraw <- length(.x$age_start)
    # start of open age group
    openageraw <- tail(.x$age_start, 1)
    # width of the last age group
    nlast <- cnst$pclm_highest_age-openageraw+1
    
    # characteristics of this data series
    # number of observed weeks making up these counts
    nweeksobserved <- .x$nweeksobserved[1]
    # number of weeks that should be reported this year
    nweeksyear <- .x$nweeksyear[1]
    # data source
    source <- .x$source[1]

    # get the corresponding midyear population
    midyearpop <-
      dat$midyearpop_for_pclm %>%
      filter(region == .y$region, sex == .y$sex, year == .y$year) %>%
      pull(popmidyear)

    # adjust midyearpop for the fraction of the observed year and
    # for eventual leap weeks
    population_py <- MidyearPop2Exposures(midyearpop, nweeksobserved)
        
    cat('Try PCLM on', .y$region, .y$sex, .y$year, .y$agegroup_pattern, Sys.time(), '\n')
    # ungroup deaths via PCLM in specific country, sex, year and
    # age grouping pattern; catch any errors
    ungrouped_deaths <- tryCatch({
      
      # ungroup deaths with exposures, output is deathrates
      fit_pclm <-
        pclm(
          y = .x$deaths, x = .x$age_start,
          nlast = nlast, out.step = 1
        )
      ungrouped_deaths <- fitted.values(fit_pclm)[1:100]
      ungrouped_deaths[101] <- sum(fitted.values(fit_pclm)[101:111])
      
      # PCLM return
      ungrouped_deaths <- tibble(
        age_start = 0:100,
        deaths = round(ungrouped_deaths, 2),
        population_py = round(population_py, 2),
        lambda = fit_pclm$smoothPar[1],
        error = FALSE,
        message = as.character(NA),
        nweeksobserved,
        nweeksyear,
        nageraw,
        openageraw,
        source
      )
      
    },
    
    # any unexpected errors are trapped here
    error = function(e) {
      cat('Error: Exception in ', .y$region, .y$sex, .y$year, .y$agegroup_pattern, geterrmessage(), '\n')
      # error return
      tibble(
        age_start = 0:100,
        deaths = as.numeric(NA),
        population_py = population_py,
        lambda = as.numeric(NA),
        error = TRUE,
        message = geterrmessage(),
        nweeksobserved = 0,
        nweeksyear = nweeksyear,
        nageraw,
        openageraw,
        source = source
      )
    })# End of tryCatch()
    
    return(ungrouped_deaths)
    
  }) %>% # End of group_modify() looping over sex, age, year, grouping pattern
  ungroup()

# sum deaths and exposures over eventual multiple
# age patterns observed within a year and derive some
# data quality metrics
dat$ungrouped <-
  dat$ungrouped_by_unique_age_pattern %>%
  group_by(region, sex, year, age_start) %>%
  summarise(
    nweeksyear = nweeksyear[1],
    deaths = sum(deaths),
    population_py = sum(population_py),
    nweeksobserved = sum(nweeksobserved),
    nweeksmiss = nweeksyear - nweeksobserved,
    minnageraw = min(nageraw),
    maxnageraw = max(nageraw),
    minopenageraw = min(openageraw),
    maxopenageraw = max(openageraw),
    source = source[1]
  ) %>%
  mutate(
    id = GenerateRowID(region, sex, year, age_start)
  ) %>%
  ungroup() %>%
  select(
    id, death = deaths,
    population_py,
    death_nweeksmiss = nweeksmiss,
    death_minnageraw = minnageraw,
    death_maxnageraw = maxnageraw,
    death_minopenageraw = minopenageraw,
    death_maxopenageraw = maxopenageraw,
    death_source = source
  )

# join with skeleton
dat$ungrouped <-
  dat$skeleton %>%
  left_join(dat$ungrouped, by = 'id')

# prepare the data for export
dat$death <-
  dat$ungrouped %>%
  select(
    id, death,
    population_py,
    death_nweeksmiss,
    death_minnageraw,
    death_maxnageraw,
    death_minopenageraw,
    death_maxopenageraw,
    death_source
  )

# Diagnostic plots ------------------------------------------------

dat$all_ready_for_ungroup

walk(cnst$region_lookup$region_code_iso3166_2, ~{
  grouped <-
    dat$all_ready_for_ungroup %>%
    filter(region == .x) %>%
    arrange(region, sex, year, agegroup_pattern, age_start) %>%
    mutate(
      age_width = c(diff(age_start), -1),
      age_width = ifelse(age_width < 0,
                         cnst$pclm_highest_age-age_start, age_width),
      deaths = deaths/age_width
    )
  ungrouped <-
    dat$ungrouped_by_unique_age_pattern %>%
    filter(region == .x)
  fig$pclm[[glue('death_pclm_{.x}')]] <<-
    ungrouped %>%
    ggplot(aes(x = age_start, y = deaths, color = sex)) +
    geom_rect(aes(
      xmin = age_start, xmax = age_start+age_width,
      ymin = 0, ymax = deaths,
      fill = sex
    ),
    alpha = 0.4, color = NA,
    data = grouped) +
    geom_line() +
    facet_grid(year ~ agegroup_pattern) +
    theme_minimal() +
    #scale_color_manual(values = fig_spec$sex_colors) +
    #scale_fill_manual(values = fig_spec$sex_colors) +
    labs(subtitle = glue('PCLM estimated singe age death counts in {.x}')) +
    MyGGplotTheme()
})

# diagnostic plots for year to year age distribution of deaths
dat$deathplot <-
  dat$ungrouped %>%
  filter(sex == 'Male') %>%
  mutate(is2020 = ifelse(year >= 2020, TRUE, FALSE))

fig$death_pclm <-
  dat$deathplot %>%
  ggplot() +
  geom_line(
    aes(
      x = age_start, y = death, color = is2020,
      group = interaction(year), size = is2020
    )
  ) +
  scale_size_manual(values = c(0.1, 0.3)) +
  scale_color_manual(values = c(`FALSE` = 'grey', `TRUE` = 'red')) +
  facet_wrap(~region, scales = 'free_y') +
  guides(color = 'none', size = 'none') +
  MyGGplotTheme() +
  labs(
    title = 'Ungrouped male death counts by age and country',
    subtitle = 'Year >2020 is red, prior years grey',
    x = '',
    y = ''
  )
fig$death_pclm

fig$hazard_pclm <-
  dat$deathplot %>%
  ggplot() +
  geom_line(
    aes(
      x = age_start, y = death/population_py, color = is2020,
      group = interaction(year), size = is2020
    )
  ) +
  scale_size_manual(values = c(0.1, 0.3)) +
  scale_color_manual(values = c(`FALSE` = 'grey', `TRUE` = 'red')) +
  facet_wrap(~region, scales = 'free_y') +
  guides(color = 'none', size = 'none') +
  MyGGplotTheme() +
  labs(
    title = 'Ungrouped male death rates by age and country',
    subtitle = 'Year >2020 is red, prior years grey',
    x = '',
    y = ''
  ) +
  scale_y_log10()
fig$hazard_pclm

# Export ----------------------------------------------------------

saveRDS(dat$death, file = paths$output$harmonized)

ExportFigure(
  fig$death_pclm, path = paths$output$fig, scale = 1.5,
  filename = '21-death_pclm', device = 'pdf'
)
ExportFigure(
  fig$hazard_pclm, path = paths$output$fig, scale = 1.5,
  filename = '21-hazard_pclm', device = 'pdf'
)

library(gridExtra)
ggsave(
  filename = paste0(paths$output$fig, '/21-pclm_death_ungroup_diagnostics.pdf'),
  plot = marrangeGrob(fig$pclm, nrow=1, ncol=1), 
  width = 6, height = 12
)
