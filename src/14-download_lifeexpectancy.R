# Download data on life-expectancy
#
# (1) Download life expectancy estimates by age, sex, and region from
#     Eurostat

# Init ------------------------------------------------------------

library(eurostat)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  eurostat_lifeexpectancy = 'demo_mlexpec'
)
paths$output <- list(
  lifeexpectancy = './dat/14-lifeexpectancy.rds'
)

# Download eurostat life expectancy -------------------------------

lifeexpectancy <-
  get_eurostat(paths$input$eurostat_lifeexpectancy, type = 'code')

# Export ----------------------------------------------------------

saveRDS(lifeexpectancy, file = paths$output$lifeexpectancy)
