# Plot pandemic population deficit age 65 plus

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr)
library(openxlsx)
library(ggplot2)
library(ggflags)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  region = './cfg/region_metadata.csv',
  projections = './out/40-projections.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  fig = './out/',
  rds_deficit65p = './out/50-deficit65p.rds',
  xlsx_deficit65p = './out/50-deficit65p.xlsx'
)

# global configuration
config <- read_yaml(paths$input$config)

# global objects and functions
global <- source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  region = filter(
    read_csv(paths$input$region),
    region_code_iso3166_2 %in% config$skeleton$regions
  )
})

dat <- list()

# Load data -------------------------------------------------------

dat$proj <- readRDS(paths$input$projections)

deficit65p <- list()

deficit65p$data <-
  dat$proj %>%
  group_by(year, sex, region, nsim) %>%
  summarise(
    expected_total = sum(expected),
    expected_65p = sum(expected[age>=65]),
    observed_total = sum(observed),
    observed_65p = sum(observed[age>=65]),
    delta_absolute = -(observed_65p-expected_65p),
    delta_relative = delta_absolute/expected_65p
  ) %>%
  group_by(year, sex, region) %>%
  summarise(
    delta_relative_Q05 = quantile(delta_relative, p = 0.05),
    delta_relative_Q50 = quantile(delta_relative, p = 0.50),
    delta_relative_Q95 = quantile(delta_relative, p = 0.95),
    delta_absolute_Q05 = quantile(delta_absolute, p = 0.05),
    delta_absolute_Q50 = quantile(delta_absolute, p = 0.50),
    delta_absolute_Q95 = quantile(delta_absolute, p = 0.95),
    expected_65p_Q50 = quantile(expected_65p, p = 0.50),
    observed_65p_Q50 = quantile(observed_65p, p = 0.50)
  ) %>%
  ungroup() %>%
  filter(year == 2022, sex == 'Total') %>%
  mutate(
    region_ggflag = tolower(region),
    region_rank = rank(delta_relative_Q50)
  ) %>%
  left_join(cnst$region, by = c('region' = 'region_code_iso3166_2'))

deficit65p$fig <-
  deficit65p$data %>%
  ggplot(aes(y = region_rank, yend = region_rank)) +
  geom_vline(aes(xintercept = 0), size = 0.5, color = 'grey80') +
  geom_segment(aes(x = delta_relative_Q05, xend = delta_relative_Q95),
               size = 1, color = 'grey70') +
  geom_text(
    aes(
      x = delta_relative_Q50,
      label = region_name_de
    ),
    position = position_nudge(y = -0.25, x = -0.0015), hjust = 1,
    size = 2, color = 'grey60'
  ) +
  geom_text(
    aes(
      x = delta_relative_Q50,
      label = paste0(
        formatC(delta_relative_Q50*100, digits = 1, format = 'f',
                decimal.mark = ','), '%'
      )
    ),
    position = position_nudge(y = +0.25, x = -0.0015), hjust = 1,
    size = 2, color = 'grey60'
  ) +
  geom_text(
    aes(
      x = delta_relative_Q50,
      label = paste0(
        formatC(delta_absolute_Q50, format = 'd',
                big.mark = ' ')
      )
    ),
    position = position_nudge(y = +0.25, x = +0.0015), hjust = 0,
    size = 2, color = 'grey60'
  ) +
  geom_point(aes(x = delta_relative_Q50), size = 5.5) +
  geom_flag(
    aes(x = delta_relative_Q50, country = region_ggflag), size = 5
  ) +
  scale_x_continuous(labels = ~scales::percent(.x, decimal.mark = ','), breaks = seq(0, 0.05, 0.005)) +
  scale_y_continuous(breaks = NULL) +
  MyGGplotTheme(grid = 'x', axis = 'x') +
  labs(
    y = NULL,
    x = 'Pandemisches Bevölkerungsdefizit Alter 65+ Ende 2022'
  )

deficit65p$fig

# Export ----------------------------------------------------------

ExportFigure(
  deficit65p$fig, paths$output$fig, filename = '50-deficit65p',
  width = 81.5, height = 160, dpi = 300, device = 'pdf'
)

write.xlsx(deficit65p$data, file = paths$output$xlsx_deficit65p,
           keepNA = TRUE, na.string = '.',
           firstRow = TRUE, firstCol = TRUE, overwrite = TRUE)
