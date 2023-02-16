# Plot life expectancy

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(openxlsx)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  region = './cfg/region_metadata.csv',
  projectioninput = './out/30-projectioninput.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  fig = './out/',
  xlsx_e0 = './out/51-e0.xlsx'
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

# Function --------------------------------------------------------

CalculateLifeTable <-
  function (df, x, nx, Dx, Ex) {
    
    require(dplyr)
    
    df %>%
      transmute(
        x = {{x}},
        nx = {{nx}},
        mx = {{Dx}}/{{Ex}},
        px = exp(-mx*{{nx}}),
        qx = 1-px,
        lx = head(cumprod(c(1, px)), -1),
        dx = c(-diff(lx), tail(lx, 1)),
        Lx = ifelse(mx==0, lx*nx, dx/mx),
        Tx = rev(cumsum(rev(Lx))),
        ex = Tx/lx
      )
    
  }

# Load data -------------------------------------------------------

dat$proj <- readRDS(paths$input$projectioninput)

# Calculate and plot e0 -------------------------------------------

e0 <- list()

e0$data <-
  dat$proj %>%
  group_by(year, sex, region) %>%
  mutate(
    nx = age_width,
    mx = death/population_py,
    px = exp(-mx*nx),
    qx = 1-px,
    lx = head(cumprod(c(1, px)), -1),
    dx = c(-diff(lx), tail(lx, 1)),
    Lx = ifelse(mx==0, lx*nx, dx/mx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx/lx
  ) %>%
  left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) %>%
  filter(year %in% 2000:2022)

e0$fig <-
  e0$data %>%
  filter(age_start == 0, region == 'DE') %>%
  ggplot(aes(x = year, y = ex, group = sex)) +
  geom_point(
    data = . %>% filter(year >= 2021)
  ) +
  geom_point(aes(y = lifeexpectancy_eurostat),
             data = . %>% filter(year < 2021)) +
  scale_x_continuous(
    breaks = seq(2000, 2022, 1),
    labels = c('2000', rep('', 21), '2022'),
    limits = c(2000, 2022)
  ) +
  scale_y_continuous(breaks = 70:90) +
  MyGGplotTheme(grid = 'y', axis = 'x') +
  labs(
    y = 'Perioden-Lebenserwartung', x = NULL
  )

e0$fig

# Export ----------------------------------------------------------

ExportFigure(
  e0$fig, paths$output$fig, filename = '51-e0',
  width = 81.5, height = 60, dpi = 300, device = 'pdf'
)

write.xlsx(e0$data, file = paths$output$xlsx_e0,
           keepNA = TRUE, na.string = '.',
           firstRow = TRUE, firstCol = TRUE, overwrite = TRUE)
