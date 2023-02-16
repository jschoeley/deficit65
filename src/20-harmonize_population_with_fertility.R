# Harmonize WPP Population and fertility data
#
# (1) Jan 1st population estimates for all nation states from
#     World Population Prospects

# Init ------------------------------------------------------------

library(yaml)
library(dplyr); library(tidyr)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  skeleton = './tmp/10-harmonized_skeleton.rds',
  population = './dat/11-population.rds'
)
paths$output <- list(
  harmonized_population = './tmp/20-harmonized_population.rds',
  out = './out'
)

# global configuration
config <- read_yaml(paths$input$config)

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  # translation of wpp sex code to harmonized sex code
  code_sex_wpp =
    c(`male` = config$skeleton$sex$Male,
      `female` = config$skeleton$sex$Female)
  # first year in harmonized data set
  skeleton_first_year = config$skeleton$year$start
  # last year in harmonized data set
  skeleton_final_year = config$skeleton$year$end
})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# Functions -------------------------------------------------------

# global functions
source(paths$input$global)

#' Make a Grid of Population Pyramids
PopPyramids <- function(
    dat, population, age, sex, year, highlight, facet, title
) {
  require(ggplot2); require(dplyr)
  dat %>%
    transmute(
      pop = ifelse(sex == 'Male', -{{population}}, {{population}}),
      age = {{age}}, sex = {{sex}}, year = {{year}}, hl = {{highlight}},
      fct = {{facet}}
    ) %>%
    ggplot(
      aes(x = age, y = pop, color = hl,
          group = interaction(sex, year, hl))
    ) +
    geom_line(show.legend = FALSE) +
    annotate('text', x = 90, y = -Inf, label = '\u2642',
             hjust = -1, size = 6) +
    annotate('text', x = 90, y = Inf, label = '\u2640',
             hjust = 1, size = 6) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(
      breaks = seq(0, 100, 20),
      labels = function (x) ifelse(x == 100, '100+', x)
    ) +
    scale_y_continuous(
      labels = function(x) {formatC(abs(x*1e-3), format = 'd')}
    ) +
    coord_flip() +
    facet_wrap(~fct, scales = 'free_x') +
    labs(x = 'Age', y = 'Population in 1000s')
}

# Load data -------------------------------------------------------

dat$skeleton <- readRDS(paths$input$skeleton)

dat$population <- readRDS(paths$input$population)

# Harmonize WPP population ----------------------------------------

dat$wpp_clean <-
  dat$population %>%
  # select columns of interest
  select(
    population_source,
    region = ISO2_code,
    iso_year = Time, age = AgeGrpStart,
    male = Male, female = Female
  ) %>%
  # sex to long format
  pivot_longer(
    cols = c(female, male),
    names_to = 'sex',
    values_to = 'value'
  ) %>%
  # ensure proper names of factor variables
  mutate(
    sex =
      factor(sex, levels = names(cnst$code_sex_wpp),
             labels = cnst$code_sex_wpp) %>%
      as.character()
  ) %>%
  # add row id
  mutate(id = GenerateRowID(region, sex, iso_year, age)) %>%
  # make popjan1st, popmidyear, fertility different columns
  separate(
    population_source, into = c('population_source', 'measure', 'timeframe'),
    sep = '_'
  ) %>%
  select(-timeframe, -population_source) %>%
  pivot_wider(
    names_from = measure, values_from = value
  ) %>%
  mutate(
    # WPP scales popnumbers in 1000's, so we scale back
    popjan1st = popjan1st*1000,
    popmidyear = popmidyear*1000,
    popmidyearcnstmo = popmidyearcnstmo*1000,
    # set fertility to 0 where we know it to be essentially 0
    # also remove the x1000 scaling
    fertility = ifelse(sex == 'Male' | age < 15 | age > 49,
                       0, fertility/1000),
    population_source = 'wpp22'
  )

# check if data makes sense
fig$wpp_population <-
  PopPyramids(
    dat = dat$wpp_clean,
    population = popmidyear,
    age = age, sex = sex, year = iso_year, highlight = iso_year,
    facet = region
  ) +
  scale_color_viridis_c() +
  labs(title = 'WPP midyear population estimates 1990-2030') +
  MyGGplotTheme(scaler = 0.8, panel_border = TRUE)
fig$wpp_population

# Join population with skeleton -----------------------------------

# join the different sources of population count data
# with the skeleton
dat$pop_joined <-
  dat$skeleton %>% 
  left_join(
    dat$wpp_clean,
    by = 'id'
  ) %>%
  select(
    id,
    popjan1st,
    popmidyear,
    popmidyearcnstmo,
    fertility,
    population_source
  )

# Export ----------------------------------------------------------

saveRDS(dat$pop_joined, file = paths$output$harmonized_population)

ExportFigure(
  fig$wpp_population, path = paths$output$out, device = 'pdf',
  filename = '20-wpp_population', scale = 2
)
