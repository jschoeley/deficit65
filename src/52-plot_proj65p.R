# Plot constant mortality population projection age 65+

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(tidyr)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  region = './cfg/region_metadata.csv',
  projectioninput = './out/30-projectioninput.rds'
)
paths$output <- list(
  fig = './out/',
  rds_proj65p = './out/52-proj65p.rds'
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

projectioninput <- readRDS(paths$input$projectioninput)

# Plot population projections -------------------------------------

proj65p <- list()

proj65p$data <-
  projectioninput %>%
  select(year, age_start, sex, region, popmidyear, popmidyearcnstmo) %>%
  pivot_wider(names_from = sex,
              values_from = c(popmidyear, popmidyearcnstmo)) %>%
  mutate(
    popmidyear = popmidyear_Male + popmidyear_Female,
    popmidyearcnstmo = popmidyearcnstmo_Male + popmidyearcnstmo_Female
  ) %>%
  group_by(year, region) %>%
  summarise(
    medium_65p = sum(popmidyear[age_start>=65]),
    cnstmo_65p = sum(popmidyearcnstmo[age_start>=65]),
    medium_total = sum(popmidyear),
    cnstmo_total = sum(popmidyearcnstmo),
    medium_share65p = medium_65p/medium_total,
    cnstmo_share65p = cnstmo_65p/cnstmo_total
  ) %>%
  ungroup() %>%
  mutate(forecast = ifelse(year > 2022, TRUE, FALSE)) %>%
  filter(region == 'DE')

proj65p$fig <-
  proj65p$data %>%
  ggplot(aes(x = year, group = region)) +
  geom_point(
    aes(y = medium_share65p), size = 1,
    data = . %>% filter(!forecast)
  ) +
  geom_line(
    aes(y = medium_share65p),
    linetype = 1,
    data = . %>% filter(!forecast)
  ) +
  geom_line(
    aes(y = medium_share65p),
    linetype = 1,
    data = . %>% filter(forecast)
  ) +
  geom_line(
    aes(y = cnstmo_share65p),
    color = 'darkred',
    linetype = 1,
    data = . %>% filter(forecast)
  ) +
  geom_text(
    aes(x = year+0.2, y = medium_share65p,
        label = paste0(
          formatC(medium_share65p*100, digits = 1, format = 'f'),
          '%'
        )
    ),
    hjust = 0, vjust = 0, size = 3,
    data = . %>% filter(year == max(year))
  ) +
  geom_text(
    aes(x = year-5, y = medium_share65p-0.01,
        label = 'Steigende\nLebenserwartung'),
    hjust = 1, vjust = 1, size = 3, color = 'black',
    data = . %>% filter(year == max(year))
  ) +
  geom_text(
    aes(x = year+0.2, y = cnstmo_share65p,
        label = paste0(
          formatC(cnstmo_share65p*100, digits = 1, format = 'f'),
          '%'
        )
    ),
    hjust = 0, vjust = 1, size = 3, color = 'darkred',
    data = . %>% filter(year == max(year))
  ) +
  geom_text(
    aes(x = year-7.7, y = cnstmo_share65p-0.035,
        label = 'Stagnierende\nLebenserwartung'),
    hjust = 0, vjust = 1, size = 3, color = 'darkred',
    data = . %>% filter(year == max(year))
  ) +
  scale_y_continuous(breaks = seq(0,1,0.01), labels = scales::percent) +
  coord_cartesian(xlim = c(2000, 2034)) +
  MyGGplotTheme() +
  labs(y = 'Bevölkerungsanteil 65+', x = NULL)

proj65p$fig

# Export ----------------------------------------------------------

saveRDS(proj65p$data, paths$output$rds_proj65p)

ExportFigure(
  proj65p$fig, paths$output$fig, filename = '52-proj65p',
  width = 81.5, height = 60, dpi = 300, device = 'pdf'
)
