# Project population under pandemic and non-pandemic scenarios

# Init ------------------------------------------------------------

library(yaml)
library(tidyverse)
library(StMoMo)

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
  projections = './out/40-projections.rds',
  fig = './out',
  leecarter_rds = './out/40-leecarter.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# global objects and functions
source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  region = filter(
    read_csv(paths$input$region),
    region_code_iso3166_2 %in% config$skeleton$regions
  )
  lee_carter_fitting_period = 2000:2019
  nsim = 250
  seed = 1987
})

# Functions -------------------------------------------------------

# array structure for population data
MakePopArray <- function (
    measure,
    age = config$skeleton$age$start:config$skeleton$age$end,
    year = config$skeleton$year$start:config$skeleton$year$end,
    sex = config$skeleton$sex,
    region = config$skeleton$regions,
    nsim = 0
) {
  
  if (isTRUE(nsim > 0)) {
    
    X <- array(
      measure,
      dim = c(
        length(age), length(year), length(sex),
        length(region), length(1:nsim)
      ),
      dimnames = list(
        age = age, year = year, sex = sex,
        region = region, nsim = 1:nsim
      )
    )
    
  } else {
    
    X <- array(
      measure,
      dim = c(length(age), length(year), length(sex), length(region)),
      dimnames = list(age = age, year = year, sex = sex, region = region)
    )
    
    return(X)
    
  }
  
  return(X)
  
}

# create stmomo object
Pop2StMoMo <- function (
    X,
    years, sex, label
) {
  age = 0:100
  
  Dxt_vec <- X[['death']]
  Dxt <- matrix(Dxt_vec, nrow = 101, ncol = length(years))
  colnames(Dxt) <- years
  rownames(Dxt) <- age
  
  Ext_vec <- X[['population_py']]
  Ext <- matrix(Ext_vec, nrow = 101, ncol = length(years))
  colnames(Ext) <- years
  rownames(Ext) <- age
  
  stmomodata <- structure(
    list(Dxt = Dxt, Ext = Ext, ages = age, years = years, 
         type = 'central', series = sex, label = label),
    class = "StMoMoData"
  )
  
  return(stmomodata)
}

# Lee-Carter Forecast
ForecastLeeCarter <- function (stmomo, h, nsim, seed) {

  constLC <- function(ax, bx, kt, b0x, gc, wxt, ages) {
    c1 <- mean(kt[1, ], na.rm = TRUE)
    c2 <- sum(bx[, 1], na.rm = TRUE)
    list(ax = ax + c1 * bx, bx = bx / c2, kt = c2 * (kt - c1))
  }
  LC_def <- StMoMo(link = 'log', staticAgeFun = TRUE,
                   periodAgeFun = 'NP', constFun = constLC)
  LC_fit <- fit(LC_def, data = stmomo)
  LC_sim <-
    simulate(LC_fit, nsim = nsim, h = h,
             jumpchoice = 'actual',
             seed = seed,
             kt.method = 'mrwd'#, kt.order = c(1,1,2)
             )[['rates']]
  
  return(list(fit = LC_fit, sim = LC_sim))
}

#' Leslie Population Projection
ProjectPopulation <- function (
    P0_xm, P0_xf, M_xtm, M_xtf, F_xtf, N_xtm, N_xtf, n = 1, srb = 1.04
) {
  
  nage = length(P0_xm)
  
  # first project the female population for n steps
  # population matrix, first column is jump off population
  Proj_xtf <- matrix(0, nrow = nage, ncol = n+1)
  Proj_xtf[,1] <- P0_xf
  for (i in 1:n+1) {
    # Leslie projection matrix
    A_tf <- matrix(0, nrow = nage, ncol = nage)
    diag(A_tf[-1,-nage]) <- head(exp(-M_xtf[,i-1]), -1)
    A_tf[1,] <- F_xtf[,i-1] * 1/(1+srb)
    
    # project population
    Proj_xtf[,i] <- A_tf%*%Proj_xtf[,i-1] + N_xtf[,i-1]
  }
  
  # now project the male population with male births derived from
  # projected female births via sex ratio
  Proj_xtm <- matrix(0, nrow = nage, ncol = n+1)
  Proj_xtm[,1] <- P0_xm
  for (i in 1:n+1) {
    A_tm <- matrix(0, nage, ncol = nage)
    diag(A_tm[-1,-nage]) <- head(exp(-M_xtm[,i-1]), -1)
    Proj_xtm[,i] <- A_tm%*%Proj_xtm[,i-1] + N_xtm[,i-1]
    Proj_xtm[1,i] <- Proj_xtf[1,i]*srb
  }
  
  dimnames(Proj_xtf) <-
    list(age = 0:(nrow(Proj_xtf)-1), year = 0:n)
  dimnames(Proj_xtm) <-
    list(age = 0:(nrow(Proj_xtm)-1), year = 0:n)
  
  projection <- list(
    female = Proj_xtf, male = Proj_xtm
  )
  
  return(projection)
  
}

# Load population data --------------------------------------------

projectioninput <- readRDS(paths$input$projectioninput)

# Forecast mortality ----------------------------------------------

mortality_forecast <- list()

mortality_forecast$matrix <-
  MakePopArray(NA, year = 2020:2030, nsim = cnst$nsim)

# stochastic forecast of mortality rates 2020-30
for (country in config$skeleton$regions) {
  for (sex in unlist(config$skeleton$sex)) {
    
    cat('Forecast:', country, sex, '\n')
 
    fitting_period <- cnst$lee_carter_fitting_period
    # no data available prior to 2001 for Croatia
    if (country == 'CZ') {fitting_period <- 2005:2019}
    if (country == 'DK') {fitting_period <- 2007:2019}
    if (country == 'IT') {fitting_period <- 2011:2019}
    
    # subset to single population by period and age
    projectioninput_sub <- projectioninput[
      projectioninput$region == country &
      projectioninput$year %in% fitting_period &
      projectioninput$sex == sex,]
    if (
      anyNA(c(projectioninput_sub$death,
              projectioninput_sub$population_py))|
      nrow(projectioninput_sub)==0
    ) { next }
    
    model_input <- Pop2StMoMo(
      projectioninput_sub,
      years = fitting_period,
      sex = sex, label = country
    )

    fit <- ForecastLeeCarter(model_input, h = 11, nsim = cnst$nsim,
                            seed = cnst$seed)

    mortality_forecast$matrix[,,sex,country,] <- fit[['sim']]
    
  }
}

# Prepare population matrices -------------------------------------

fertility <- MakePopArray(projectioninput$fertility)
migration <- MakePopArray(projectioninput$netmigration_eurostat)
jumpoff <- MakePopArray(projectioninput$popjan1st)
mortality_observed <- MakePopArray(projectioninput$death/
                                     projectioninput$population_py)

apply(fertility, c('region', 'year'), anyNA)
apply(migration, c('region', 'year'), anyNA)
apply(jumpoff, c('region', 'year'), anyNA)[,'2019']
apply(mortality_observed, c('region', 'year'), anyNA)
apply(mortality_forecast$matrix, c('region', 'year'), anyNA)

# Projection result matrix ----------------------------------------

projections <- list()

projections$horizont <- as.character(2020:2022)
projections$jumpoff <- '2020'

projections$expected <-
  array(
    dim = c(
      config$skeleton$age$end-config$skeleton$age$start+1,
      3, 3, length(config$skeleton$regions), cnst$nsim
    ),
    dimnames = list(
      age = config$skeleton$age$start:config$skeleton$age$end,
      year = projections$horizont,
      sex = c(config$skeleton$sex, 'Total'),
      region = config$skeleton$regions,
      nsim = 1:cnst$nsim
    )
  )
projections$observed <- projections$expected

# Project populations under pandemic and nonpandemic scena --------

for (region in config$skeleton$regions) {
  
  cat(region, '\n')
  
  # project 2019 population
  # under application of pandemic mortality rates
  observed <- ProjectPopulation(
    P0_xm = jumpoff[,projections$jumpoff,'Male',region],
    P0_xf = jumpoff[,projections$jumpoff,'Female',region],
    M_xtm = mortality_observed[,projections$horizont,'Male',region],
    M_xtf = mortality_observed[,projections$horizont,'Female',region],
    F_xtf = fertility[,projections$horizont,'Female',region],
    N_xtm = migration[,projections$horizont,'Male',region],
    N_xtf = migration[,projections$horizont,'Female',region],
    n = 3, srb = 1.04
  )
  projections$observed[,,'Female',region,] <- observed$female[,-1]
  projections$observed[,,'Male',region,] <- observed$male[,-1]
  
  # project 2019 population
  # under application of pre-pandemic trend mortality rates
  for (sim in 1:cnst$nsim) {
    
    expected <- ProjectPopulation(
      P0_xm = jumpoff[,projections$jumpoff,'Male',region],
      P0_xf = jumpoff[,projections$jumpoff,'Female',region],
      M_xtm = mortality_forecast$matrix[,projections$horizont,'Male',region,sim],
      M_xtf = mortality_forecast$matrix[,projections$horizont,'Female',region,sim],
      F_xtf = fertility[,projections$horizont,'Female',region],
      N_xtm = migration[,projections$horizont,'Male',region],
      N_xtf = migration[,projections$horizont,'Female',region],
      n = 3, srb = 1.04
    )
    projections$expected[,,'Female',region,sim] <-
      expected$female[,-1]
    projections$expected[,,'Male',region,sim] <-
      expected$male[,-1]
    
  }
}

# add female and male projections to total
projections$observed[,,'Total',,] <-
  projections$observed[,,'Male',,] + projections$observed[,,'Female',,]
projections$expected[,,'Total',,] <-
  projections$expected[,,'Male',,] + projections$expected[,,'Female',,]

# Convert projection array to df ----------------------------------

projections$all <- abind::abind(
  projections$observed,
  projections$expected,
  along = 6, use.dnns = TRUE
)
dimnames(projections$all)[[6]] <- c('observed', 'expected')
names(dimnames(projections$all))[[6]] <- 'scenario'

projections$df <-
  as.data.frame.table(projections$all, stringsAsFactors = FALSE) %>%
  pivot_wider(names_from = scenario, values_from = Freq) %>%
  mutate(across(c(age, year, nsim), ~as.integer(.x)))

# Validate Lee-Carter projection ----------------------------------

leecarter <- list()

# calculate life-expectancy for observed data
leecarter$data$observed <-
  as.data.frame.table(mortality_observed, responseName = 'mx') %>%
  mutate(year = as.integer(as.character(year))) %>% 
  group_by(year, sex, region) %>%
  mutate(
    nx = ifelse(age == 100, Inf, 1),
    px = exp(-mx*nx),
    qx = 1-px,
    lx = head(cumprod(c(1, px)), -1),
    dx = c(-diff(lx), tail(lx, 1)),
    Lx = ifelse(mx==0, lx*nx, dx/mx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx/lx
  ) %>%
  filter(year < 2020, year >= 2000, age == 0)

# calculate life-expectancy from forecasted data
leecarter$data$predicted <-
  as.data.frame.table(mortality_forecast$matrix, responseName = 'mx') %>%
  mutate(year = as.integer(as.character(year))) %>% 
  group_by(year, sex, region, nsim) %>%
  mutate(
    nx = ifelse(age == 100, Inf, 1),
    px = exp(-mx*nx),
    qx = 1-px,
    lx = head(cumprod(c(1, px)), -1),
    dx = c(-diff(lx), tail(lx, 1)),
    Lx = ifelse(mx==0, lx*nx, dx/mx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx/lx
  ) %>%
  filter(age == 0)

# plot life expectacy projections
leecarter$plot <-
  data.frame() %>%
  ggplot() +
  geom_line(
    aes(x = year, y = ex, color = sex),
    data = leecarter$data$observed
  ) +
  geom_line(
    aes(x = year, y = ex, group = interaction(nsim,sex), color = sex),
    alpha = 0.01, data = leecarter$data$predicted
  ) +
  scale_color_manual(values = unlist(config$figspec$colors$sex)) +
  facet_wrap(~region, scales = 'free_y') +
  MyGGplotTheme(panel_border = TRUE) +
  labs(y = 'Lebenserwartung', x = NULL,
       title = 'Poisson-Lee-Carter projections of life expectancy 2020-30')

leecarter$plot

# Export ----------------------------------------------------------

saveRDS(projections$df, file = paths$output$projections, compress = 'xz')

ExportFigure(
  leecarter$plot, path = paths$output$fig, scale = 1.5,
  filename = '40-leecarter', device = 'pdf'
)
saveRDS(leecarter$data, file = paths$output$leecarter_rds)
