# Download data on net-migration
#
# (1) Download netmigration projections by age, sex, and region from
#     Eurostat

# Init ------------------------------------------------------------

library(eurostat)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  eurostat_netmigration = 'proj_19nanmig'
)
paths$output <- list(
  netmigration = './dat/13-netmigration.rds'
)

# Download Eurostat Netmigration ----------------------------------

netmigration <- get_eurostat(paths$input$eurostat_netmigration, type = 'code')

# Export ----------------------------------------------------------

saveRDS(netmigration, file = paths$output$netmigration)
