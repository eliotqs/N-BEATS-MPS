library(data.table)
library(openxlsx)
library(magrittr)
library(Mcomp)
library(stringr)
library(DescTools)

########################## Actuals ##########################
## Load (raw) actuals for M3 and M4
## We collect actuals for both the test sets and training sets
## Training actuals can be used for MASE/RMSSE scaling later on

#############################################################
dataset_subset <- 'Monthly' #'Other' 
dataset_subset_indicator <- 'M' #'O'
FH <- 18#8#18
FL <- 6#2#6
FO <- 13#7#13
dataset_selection <- 'M3' #'M3'
#############################################################
# the M3 dataset is loaded via the "Mcomp" package 
if (dataset_selection == 'M3'){
  
  MS <- subset(M3, dataset_subset)
  # item_id M1385/N2786 --> one observation in test set is -1200 
  # in M3 data via raw source vs 1200 in M3 via R package
  
  MS_actuals_train <- data.frame()
  max_l <- 0
  max_n <- 0
  for (i in c(1:length(MS))) {
    if (length(MS[[i]]$x) > max_l) {max_l <- length(MS[[i]]$x)}
    if (MS[[i]]$n > max_n) {max_n <- MS[[i]]$n}
  }
  if (max_l != max_n) print('TS length error')
  
  MS_actuals_test <- data.frame()
  
  counter <- 1
  for (i in c(1:length(MS))) {
    ts <- MS[[i]]
    
    MS_actuals_train[i,1] <- MS_actuals_test[i,1] <- ts$st
    MS_actuals_train[i,2] <- MS_actuals_test[i,2] <- ts$sn
    MS_actuals_train[i,3:c(max_l+2)] <- c(ts$x, rep(NA, max_l-length(ts$x)))
    MS_actuals_test[i,3:c(FH+2)] <- ts$xx
    
    counter <- counter+1
  }
  
  MS_actuals_train <- data.table(MS_actuals_train)
  MS_actuals_test <- data.table(MS_actuals_test)
  
  setnames(MS_actuals_train, names(MS_actuals_train), c('item_id1', 'item_id2', 1:max_l))
  setnames(MS_actuals_test, names(MS_actuals_test), c('item_id1', 'item_id2', 1:FH))
  MS_actuals_train[, item_id := as.numeric(str_remove(item_id1, dataset_subset_indicator))]
  MS_actuals_test[, item_id := as.numeric(str_remove(item_id1, dataset_subset_indicator))]
  MS_actuals_train[, item_id1 := NULL]
  MS_actuals_train[, item_id2 := NULL]
  MS_actuals_test[, item_id1 := NULL]
  MS_actuals_test[, item_id2 := NULL]
  setcolorder(MS_actuals_train, c('item_id'))
  setorder(MS_actuals_train, item_id)
  setcolorder(MS_actuals_test, c('item_id'))
  setorder(MS_actuals_test, item_id)
  
  rm(MS, ts)
  
# the M4 datasets can be found online, we included links to the Monthly dataset:
  # TRAINSET:   'https://raw.githubusercontent.com/Mcompetitions/M4-methods/master/Dataset/Train/Monthly-train.csv'
  # TESTSET:    'https://raw.githubusercontent.com/Mcompetitions/M4-methods/master/Dataset/Test/Monthly-test.csv'
} else if (dataset_selection == 'M4') {
  
  MS_actuals_train <- read.csv("datasets/M4/Monthly-train.csv")
  MS_actuals_test <- read.csv("datasets/M4/Monthly-test.csv")
  
  max_l <- ncol(MS_actuals_train)-1
  
  setDT(MS_actuals_test)
  setDT(MS_actuals_train)
  setnames(MS_actuals_train, names(MS_actuals_train), c('item_id', 1:max_l))
  setnames(MS_actuals_test, names(MS_actuals_test), c('item_id', 1:FH))
  MS_actuals_train[, item_id := as.numeric(str_remove(item_id, dataset_subset_indicator))]
  MS_actuals_test[, item_id := as.numeric(str_remove(item_id, dataset_subset_indicator))]
  
}

MS_actuals_all <- merge.data.table(MS_actuals_train, MS_actuals_test, by = 'item_id')
setnames(MS_actuals_all, names(MS_actuals_all), c('item_id', 1:c(ncol(MS_actuals_train)-1+FH)))
MS_actuals_all <- melt.data.table(MS_actuals_all, id.vars = 'item_id')
MS_actuals_all <- MS_actuals_all[!is.na(value)]
setorder(MS_actuals_all, variable)
MS_actuals_all[, variable := c(1:.N) , by = list(item_id)]
MS_actuals_all[, variable := as.numeric(variable)]
MS_actuals_all[, diff_sq := (value - shift(value))^2, by = item_id]

MS_actuals_test[, fc_origin := 1]
MS_actuals_test <- melt.data.table(MS_actuals_test,
                                   id.vars = c('item_id', 'fc_origin'),
                                   variable.name = 'forecast_horizon',
                                   value.name = 'value')
MS_actuals_test[, forecast_horizon := as.numeric(forecast_horizon)]

MS_A <- data.table()
for (origin in c(1:FO)) {
  MS_A_fc_origin <- MS_actuals_test[forecast_horizon %in% c(origin:(origin+(FL-1)))]
  MS_A_fc_origin[, fc_origin := origin]
  MS_A_fc_origin[, forecast_horizon := (forecast_horizon - origin + 1)]
  
  n_obs_remove <- FH - (origin-1)
  MS_actuals_all_subset <- data.table(MS_actuals_all)
  MS_actuals_all_subset[, max_obs := (max(variable) - n_obs_remove), by = list(item_id)]
  MS_actuals_all_subset <- MS_actuals_all_subset[variable <= max_obs]
  
  MS_dfs <- MS_actuals_all_subset[, list(discount_factor = mean(diff_sq, na.rm = T)), by = item_id]
  MS_A_fc_origin <- merge.data.table(MS_A_fc_origin, MS_dfs, by = 'item_id')
  
  # MS_dfs <- MS_actuals_all_subset[, list(discount_factor = mean(sqrt(diff_sq), na.rm = T)), by = item_id]
  # MS_A_fc_origin <- merge.data.table(MS_A_fc_origin, MS_dfs, by = 'item_id')
  
  MS_A <- rbind(MS_A, MS_A_fc_origin)
}
setorder(MS_A, item_id, fc_origin, forecast_horizon)

########################## Evaluation forecasts DT ##########################
n_methods <- 7
n_items <- unique(MS_A$item_id)
MS_F <- matrix(NA, nrow = length(n_items), ncol = 1 + n_methods) %>% data.table()
setnames(MS_F, c('ID',
                 'ETS','ARIMA','THETA',
                 'NBEATS', 'NBEATSS', 
                 'NBEATSMPSC', 'NBEATSMPSDEC'))
MS_F[, ID := unique(MS_A$item_id)]

MS_SMAPE <- data.table(MS_F)
MS_SMAPC <- data.table(MS_F)
MS_SMAPC0 <- data.table(MS_F)
MS_SMAPC1 <- data.table(MS_F)
MS_SMAPC2 <- data.table(MS_F)
MS_SMAPC3 <- data.table(MS_F)
MS_SMAPC4 <- data.table(MS_F)
MS_RMSSE <- data.table(MS_F)
MS_RMSSC <- data.table(MS_F)
MS_RMSSC0 <- data.table(MS_F)
MS_RMSSC1 <- data.table(MS_F)
MS_RMSSC2 <- data.table(MS_F)
MS_RMSSC3 <- data.table(MS_F)
MS_RMSSC4 <- data.table(MS_F)

########################## Evaluation metrics and functions ##########################
SMAPE <- function(actuals, forecasts){
  200 * mean(abs(forecasts - actuals)/(abs(actuals) + abs(forecasts)))
}

SMAPC <- function(forecasts, forecasts_previous){
  200 * mean(abs(forecasts - forecasts_previous)/(abs(forecasts) + abs(forecasts_previous)), na.rm = T)
}

RMSSE <- function(actuals, forecasts, discount_factor){
  sqrt(mean((actuals - forecasts)^2/discount_factor))
}

RMSSC <- function(forecasts, forecasts_previous, discount_factor){
  sqrt(mean((forecasts - forecasts_previous)^2/discount_factor, na.rm = T))
}

MS_METHOD_PREP <- function(MS_METHOD){
  MS_METHOD <- melt.data.table(MS_METHOD,
                               id.vars = c('item_id', 'fc_origin'),
                               variable.name = 'forecast_horizon',
                               value.name = 'forecast')
  MS_METHOD[, forecast_horizon := as.numeric(forecast_horizon)]
  setorder(MS_METHOD, item_id, fc_origin, forecast_horizon)
  MS_METHOD[forecast < 0, forecast := 0]
  
  # MS_METHOD[, forecast_previous := shift(forecast, (FL-1))]
  MS_METHOD[, forecast_previous_gap0 := shift(forecast, 1*(FL-1))]
  MS_METHOD[, forecast_previous_gap1 := shift(forecast, 2*(FL-1))]
  MS_METHOD[, forecast_previous_gap2 := shift(forecast, 3*(FL-1))]
  MS_METHOD[, forecast_previous_gap3 := shift(forecast, 4*(FL-1))]
  MS_METHOD[, forecast_previous_gap4 := shift(forecast, 5*(FL-1))]
  
  # MS_METHOD[fc_origin == 1, forecast_previous := NA]
  MS_METHOD[fc_origin == 1, forecast_previous_gap0 := NA]
  MS_METHOD[fc_origin == 1, forecast_previous_gap1 := NA]
  MS_METHOD[fc_origin == 1, forecast_previous_gap2 := NA]
  MS_METHOD[fc_origin == 1, forecast_previous_gap3 := NA]
  MS_METHOD[fc_origin == 1, forecast_previous_gap4 := NA]
  MS_METHOD[fc_origin == 2, forecast_previous_gap1 := NA]
  MS_METHOD[fc_origin == 2, forecast_previous_gap2 := NA]
  MS_METHOD[fc_origin == 2, forecast_previous_gap3 := NA]
  MS_METHOD[fc_origin == 2, forecast_previous_gap4 := NA]
  MS_METHOD[fc_origin == 3, forecast_previous_gap2 := NA]
  MS_METHOD[fc_origin == 3, forecast_previous_gap3 := NA]
  MS_METHOD[fc_origin == 3, forecast_previous_gap4 := NA]
  MS_METHOD[fc_origin == 4, forecast_previous_gap3 := NA]
  MS_METHOD[fc_origin == 4, forecast_previous_gap4 := NA]
  MS_METHOD[fc_origin == 5, forecast_previous_gap4 := NA]
  
  # MS_METHOD[forecast_horizon == FL, forecast_previous := NA]
  MS_METHOD[forecast_horizon == FL, forecast_previous_gap0 := NA]
  MS_METHOD[forecast_horizon == FL, forecast_previous_gap1 := NA]
  MS_METHOD[forecast_horizon == FL, forecast_previous_gap2 := NA]
  MS_METHOD[forecast_horizon == FL, forecast_previous_gap3 := NA]
  MS_METHOD[forecast_horizon == FL, forecast_previous_gap4 := NA]
  MS_METHOD[forecast_horizon == FL-1, forecast_previous_gap1 := NA]
  MS_METHOD[forecast_horizon == FL-1, forecast_previous_gap2 := NA]
  MS_METHOD[forecast_horizon == FL-1, forecast_previous_gap3 := NA]
  MS_METHOD[forecast_horizon == FL-1, forecast_previous_gap4 := NA]
  MS_METHOD[forecast_horizon == FL-2, forecast_previous_gap2 := NA]
  MS_METHOD[forecast_horizon == FL-2, forecast_previous_gap3 := NA]
  MS_METHOD[forecast_horizon == FL-2, forecast_previous_gap4 := NA]
  MS_METHOD[forecast_horizon == FL-3, forecast_previous_gap3 := NA]
  MS_METHOD[forecast_horizon == FL-3, forecast_previous_gap4 := NA]
  MS_METHOD[forecast_horizon == FL-4, forecast_previous_gap4 := NA]
  
  MS_METHOD[, actual := MS_A$value]
  MS_METHOD[, discount_factor := MS_A$discount_factor]
  return(MS_METHOD)
}

MS_METHOD_RESULTS <- function(MS_METHOD_PREP, METHOD){
  MS_METHOD_ITEM_ORIGIN <- MS_METHOD_PREP[, 
                                          list(SMAPE = SMAPE(actual, forecast),
                                               # SMAPC = SMAPC(forecast, forecast_previous),
                                               SMAPC0 = SMAPC(forecast, forecast_previous_gap0),
                                               SMAPC1 = SMAPC(forecast, forecast_previous_gap1),
                                               SMAPC2 = SMAPC(forecast, forecast_previous_gap2),
                                               SMAPC3 = SMAPC(forecast, forecast_previous_gap3),
                                               SMAPC4 = SMAPC(forecast, forecast_previous_gap4),
                                               RMSSE = RMSSE(actual, forecast, unique(discount_factor)),
                                               # RMSSC = RMSSC(forecast, forecast_previous, unique(discount_factor))
                                               RMSSC0 = RMSSC(forecast, forecast_previous_gap0, unique(discount_factor)),
                                               RMSSC1 = RMSSC(forecast, forecast_previous_gap1, unique(discount_factor)),
                                               RMSSC2 = RMSSC(forecast, forecast_previous_gap2, unique(discount_factor)),
                                               RMSSC3 = RMSSC(forecast, forecast_previous_gap3, unique(discount_factor)),
                                               RMSSC4 = RMSSC(forecast, forecast_previous_gap4, unique(discount_factor))
                                          ),
                                          by = list(fc_origin, item_id)]
  MS_METHOD_ITEM <- MS_METHOD_ITEM_ORIGIN[,
                                          list(SMAPE = mean(SMAPE),
                                               # SMAPC = mean(SMAPC, na.rm = T),
                                               SMAPC0 = mean(SMAPC0, na.rm = T),
                                               SMAPC1 = mean(SMAPC1, na.rm = T),
                                               SMAPC2 = mean(SMAPC2, na.rm = T),
                                               SMAPC3 = mean(SMAPC3, na.rm = T),
                                               SMAPC4 = mean(SMAPC4, na.rm = T),
                                               RMSSE = mean(RMSSE),
                                               # RMSSC = mean(RMSSC, na.rm = T)
                                               RMSSC0 = mean(RMSSC0, na.rm = T),
                                               RMSSC1 = mean(RMSSC1, na.rm = T),
                                               RMSSC2 = mean(RMSSC2, na.rm = T),
                                               RMSSC3 = mean(RMSSC3, na.rm = T),
                                               RMSSC4 = mean(RMSSC4, na.rm = T)
                                          ),
                                          by = list(item_id)]
  MS_METHOD_ITEM[, SMAPC := mean(c(SMAPC0, SMAPC1, SMAPC2, SMAPC3, SMAPC4)), by = list(item_id)]
  MS_METHOD_ITEM[, RMSSC := mean(c(RMSSC0, RMSSC1, RMSSC2, RMSSC3, RMSSC4)), by = list(item_id)]
  MS_SMAPE[, (METHOD) := MS_METHOD_ITEM$SMAPE]
  MS_SMAPC[, (METHOD) := MS_METHOD_ITEM$SMAPC]
  MS_SMAPC0[, (METHOD) := MS_METHOD_ITEM$SMAPC0]
  MS_SMAPC1[, (METHOD) := MS_METHOD_ITEM$SMAPC1]
  MS_SMAPC2[, (METHOD) := MS_METHOD_ITEM$SMAPC2]
  MS_SMAPC3[, (METHOD) := MS_METHOD_ITEM$SMAPC3]
  MS_SMAPC4[, (METHOD) := MS_METHOD_ITEM$SMAPC4]
  MS_RMSSE[, (METHOD) := MS_METHOD_ITEM$RMSSE]
  MS_RMSSC[, (METHOD) := MS_METHOD_ITEM$RMSSC]
  MS_RMSSC0[, (METHOD) := MS_METHOD_ITEM$RMSSC0]
  MS_RMSSC1[, (METHOD) := MS_METHOD_ITEM$RMSSC1]
  MS_RMSSC2[, (METHOD) := MS_METHOD_ITEM$RMSSC2]
  MS_RMSSC3[, (METHOD) := MS_METHOD_ITEM$RMSSC3]
  MS_RMSSC4[, (METHOD) := MS_METHOD_ITEM$RMSSC4]
}

########################## Forecast methods ##########################

# ETS ##### (change the location of the files)
if (dataset_selection=='M4'){
  MS_ETS <- read.csv('m4m_Jente/M4M_ETS.csv')
} else {
  MS_ETS <- read.csv('m3m_Jente/M3M_ETS_probabilistic.csv')
}

setDT(MS_ETS)

MS_ETS[, item_id := as.integer(as.factor(item_id))]
MS_ETS <- MS_ETS[type == 'mean_forecast']
MS_ETS[, type := NULL]
MS_ETS <- MS_METHOD_PREP(MS_ETS)
MS_METHOD_RESULTS(MS_ETS, 'ETS')

rm(MS_ETS)



# ARIMA ##### (change the location of the files)
if (dataset_selection=='M4'){
  MS_ARIMA <- read.csv('m4m_Jente/M4M_ARIMA.csv')
} else {
  MS_ARIMA <- read.csv('m3m_Jente/M3M_ARIMA_probabilistic.csv')
}
setDT(MS_ARIMA)
MS_ARIMA[, item_id := as.integer(as.factor(item_id))]
MS_ARIMA <- MS_ARIMA[type == 'mean_forecast']
MS_ARIMA[, type := NULL]
MS_ARIMA <- MS_METHOD_PREP(MS_ARIMA)
MS_METHOD_RESULTS(MS_ARIMA, 'ARIMA')

rm(MS_ARIMA)

# THETA ##### (change the location of the files)
if (dataset_selection=='M4'){
  MS_THETA <- read.csv('m4m_Jente/M4M_THETA.csv')
} else {
  MS_THETA <- read.csv('m3m_Jente/M3M_THETA_probabilistic.csv')
}
setDT(MS_THETA)
MS_THETA[, item_id := as.integer(as.factor(item_id))]
MS_THETA <- MS_THETA[type == 'mean_forecast']
MS_THETA[, type := NULL]
MS_THETA <- MS_METHOD_PREP(MS_THETA)
MS_METHOD_RESULTS(MS_THETA, 'THETA')

rm(MS_THETA)

# NBEATS ##### (change the location of the files)
if (dataset_selection == 'M3'){
  datafolder <- paste0('RESULTATEN/M3_NBEATS/')
} else if (dataset_selection == 'M4')  {
  datafolder <- paste0('RESULTATEN/M4_NBEATS/')}
MS_NBEATS_files <- list.files(datafolder)
MS_NBEATS_files <- paste0(datafolder, MS_NBEATS_files)
MS_NBEATS <- lapply(MS_NBEATS_files, fread) %>% rbindlist()
MS_NBEATS <- MS_NBEATS[type == 'forecast']
MS_NBEATS[, type := NULL]
MS_NBEATS <- MS_NBEATS[, lapply(.SD, median) , by = list(item_id, fc_origin), .SDcols = as.character(c(1:FL))]
MS_NBEATS <- MS_METHOD_PREP(MS_NBEATS)
MS_METHOD_RESULTS(MS_NBEATS, 'NBEATS')

rm(MS_NBEATS, MS_NBEATS_files)

# NBEATS-S ##### (change the location of the files)
if (dataset_selection == 'M3'){
  datafolder <- paste0('RESULTATEN/M3_NBEATS_S/')
} else if (dataset_selection == 'M4')  {
  datafolder <- paste0('RESULTATEN/M4_NBEATS_S/')}
MS_NBEATSS_files <- list.files(datafolder)
MS_NBEATSS_files <- paste0(datafolder, MS_NBEATSS_files)
MS_NBEATSS <- lapply(MS_NBEATSS_files, fread) %>% rbindlist()
MS_NBEATSS <- MS_NBEATSS[type == 'forecast']
MS_NBEATSS[, type := NULL]
MS_NBEATSS <- MS_NBEATSS[, lapply(.SD, median) , by = list(item_id, fc_origin), .SDcols = as.character(c(1:FL))]
MS_NBEATSS <- MS_METHOD_PREP(MS_NBEATSS)
MS_METHOD_RESULTS(MS_NBEATSS, 'NBEATSS')

rm(MS_NBEATSS, MS_NBEATSS_files)

# NBEATS-MPS-CONSTANT ##### (change the location of the files)
if (dataset_selection == 'M3'){
  datafolder <- paste0('RESULTATEN/M3_NBEATS_MPS_CONSTANT/')
} else if (dataset_selection == 'M4')  {
  datafolder <- paste0('RESULTATEN/M4_NBEATS_MPS_CONSTANT/')}
MS_NBEATSMPSC_files <- list.files(datafolder)
MS_NBEATSMPSC_files <- paste0(datafolder, MS_NBEATSMPSC_files)
MS_NBEATSMPSC <- lapply(MS_NBEATSMPSC_files, fread) %>% rbindlist()
setDT(MS_NBEATSMPSC)
MS_NBEATSMPSC <- MS_NBEATSMPSC[type == 'forecast']
MS_NBEATSMPSC[, type := NULL]
MS_NBEATSMPSC <- MS_NBEATSMPSC[, lapply(.SD, median) , by = list(item_id, fc_origin), .SDcols = as.character(c(1:FL))]
MS_NBEATSMPSC <- MS_METHOD_PREP(MS_NBEATSMPSC)
MS_METHOD_RESULTS(MS_NBEATSMPSC, 'NBEATSMPSC')

rm(MS_NBEATSMPSC, MS_NBEATSMPSC_files)

# NBEATS-MPS-D ##### (change the location of the files)
if (dataset_selection == 'M3'){
  datafolder <- paste0('RESULTATEN/M3_NBEATS_MPS_DEC/')
} else if (dataset_selection == 'M4')  {
  datafolder <- paste0('RESULTATEN/M4_NBEATS_MPS_DEC/')}

MS_NBEATSMPSDEC_files <- list.files(datafolder)
MS_NBEATSMPSDEC_files <- paste0(datafolder, MS_NBEATSMPSDEC_files)
MS_NBEATSMPSDEC <- lapply(MS_NBEATSMPSDEC_files, fread) %>% rbindlist()
setDT(MS_NBEATSMPSDEC)
MS_NBEATSMPSDEC <- MS_NBEATSMPSDEC[type == 'forecast']
MS_NBEATSMPSDEC[, type := NULL]
MS_NBEATSMPSDEC <- MS_NBEATSMPSDEC[, lapply(.SD, median) , by = list(item_id, fc_origin), .SDcols = as.character(c(1:FL))]
MS_NBEATSMPSDEC <- MS_METHOD_PREP(MS_NBEATSMPSDEC)
MS_METHOD_RESULTS(MS_NBEATSMPSDEC, 'NBEATSMPSDEC')

rm(MS_NBEATSMPSDEC, MS_NBEATSMPSDEC_files)

########################## Basic comparison measures #######################

MS_SMAPC0 %>% colMeans() %>% round(2)
MS_SMAPC1 %>% colMeans() %>% round(2)
MS_SMAPC2 %>% colMeans() %>% round(2)
MS_SMAPC3 %>% colMeans() %>% round(2)
MS_SMAPC4 %>% colMeans() %>% round(2)

MS_SMAPC %>% colMeans() %>% round(2)
MS_SMAPE %>% colMeans() %>% round(2)
# MS_RMSSE %>% colMeans() %>% round(3)
# MS_RMSSC %>% colMeans() %>% round(3)

########################## Statistical comparison ##########################
library(tsutils)

setnames(MS_SMAPE, 
         names(MS_SMAPE),
         c('ID',
           'ETS','ARIMA','THETA',
           'N-BEATS', 'N-BEATS-S', 
           'N-BEATS-MPS-C', 'NBEATS-MPS-D'))
setnames(MS_SMAPC, 
         names(MS_SMAPC),
         c('ID',
           'ETS','ARIMA','THETA',
           'N-BEATS', 'N-BEATS-S', 
           'N-BEATS-MPS-C', 'N-BEATS-MPS-D'))
setnames(MS_SMAPC0, 
         names(MS_SMAPC0),
         c('ID',
           'ETS','ARIMA','THETA',
           'N-BEATS', 'N-BEATS-S', 
           'N-BEATS-MPS-C', 'N-BEATS-MPS-D'))

MCB_SMAPE <- nemenyi(as.matrix(MS_SMAPE[,2:8]), plottype = 'vmcb')
MCB_SMAPC <- nemenyi(as.matrix(MS_SMAPC[,2:8]), plottype = 'vmcb')
MCB_SMAPC0 <- nemenyi(as.matrix(MS_SMAPC0[,2:8]), plottype = 'vmcb')
# nemenyi(as.matrix(MS_RMSSE[,2:8]), plottype = 'vmcb')
# nemenyi(as.matrix(MS_RMSSC[,2:8]), plottype = 'vmcb')


# Foresight ##### (change the location of the files)
png(file = paste0('/Users/jentevanbelle/desktop/',
                  dataset_selection,
                  dataset_subset_indicator,
                  '_MCB_SMAPE.png'),
    units = "in", width = 5, height = 4, res = 300)
nemenyi(as.matrix(MS_SMAPE[,c(2:8)]), plottype = 'vmcb')
dev.off()
png(file = paste0('/Users/jentevanbelle/desktop/',
                  dataset_selection,
                  dataset_subset_indicator,
                  '_MCB_SMAPC.png'),
    units = "in", width = 5, height = 4, res = 300)
nemenyi(as.matrix(MS_SMAPC[,c(2:8)]), plottype = 'vmcb')
dev.off()
