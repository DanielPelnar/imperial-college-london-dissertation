# Master's thesis***
####### REGRESSIONS ###########
rm(list=ls()) # removes old working directory

### Libraries:
# install.packages("quantmod")
# install.packages("MuMIn")
# install.packages('openxlsx') 
# library(readxl)
# library(quantmod)
# library(zoo)
# library(tseries)
# library(MumIn)
# library(xlsx)
# library(openxlsx)
# library(sandwich)
# library(lmtest)

# Set up working directory
setwd("YOUR_PATH") # set up your working directory

#****************************************************************************************************#
# Importing data (Each import is one contract. There are 12 contracts: 6 for BTC and 6 for ETH):

C1_BTC <- read_excel("1_BTC_CONTRACT_BTCUSDT_210326.xlsx",
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C2_BTC <- read_excel("2_BTC_CONTRACT_BTCUSDT_210625.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C3_BTC <- read_excel("3_BTC_CONTRACT_BTCUSDT_210924.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C4_BTC <- read_excel("4_BTC_CONTRACT_BTCUSDT_211231.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C5_BTC <- read_excel("5_BTC_CONTRACT_BTCUSDT_220325.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C6_BTC <- read_excel("6_BTC_CONTRACT_BTCUSDT_220624.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))


C1_ETH <- read_excel("1_ETH_CONTRACT_ETHUSDT_210326.xlsx",
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C2_ETH <- read_excel("2_ETH_CONTRACT_ETHUSDT_210625.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C3_ETH <- read_excel("3_ETH_CONTRACT_ETHUSDT_210924.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C4_ETH <- read_excel("4_ETH_CONTRACT_ETHUSDT_211231.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C5_ETH <- read_excel("5_ETH_CONTRACT_ETHUSDT_220325.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))

C6_ETH <- read_excel("6_ETH_CONTRACT_ETHUSDT_220624.xlsx", 
                     col_types = c("numeric", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric" 
                     ))



#
# =========================================================================================
#                                           FORMATING STARTS
#                                           *************
# =========================================================================================
#


#****************************************************************************************************#
# BITCOIN CONTRACTS

### CONTRACT: C1_BTC ###
# rates as proportion (changing from %):
C1_BTC$rate_4w_no_nan <- C1_BTC$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C1_BTC$rate_3m_no_nan <- C1_BTC$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C1_BTC$obs, n=1)
C1_BTC$time_to_maturity <- maturity_day - C1_BTC$obs
C1_BTC$time_to_maturity_proportion <- C1_BTC$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C1_BTC$theoretical_futures_price <- C1_BTC$spot_price_index * exp(C1_BTC$rate_4w_no_nan * C1_BTC$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C1_BTC$mispricing_term_1 <- C1_BTC$theoretical_futures_price - C1_BTC$future_close                                    # just range 
C1_BTC$mispricing_term_2 <- abs(C1_BTC$theoretical_futures_price - C1_BTC$future_close)                               # absolute value of range
C1_BTC$mispricing_term_3 <- (C1_BTC$theoretical_futures_price - C1_BTC$future_close) / C1_BTC$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C1_BTC$mispricing_term_4 <- abs((C1_BTC$theoretical_futures_price - C1_BTC$future_close) / C1_BTC$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C1_BTC$mispricing_term_5 <- C1_BTC$spot_price_index - C1_BTC$future_close                                             # just range -> Basis = s-f
C1_BTC$mispricing_term_6 <- abs(C1_BTC$spot_price_index - C1_BTC$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C1_BTC$mispricing_term_7 <- (C1_BTC$spot_price_index - C1_BTC$future_close) / C1_BTC$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C1_BTC$mispricing_term_8 <- abs((C1_BTC$spot_price_index - C1_BTC$future_close) / C1_BTC$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C1_BTC$spot_price_index_return <- c(Delt(C1_BTC$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C1_BTC$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C1_BTC$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C1_BTC$volatility_spot_price_index_return <- C1_BTC$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C1_BTC$futures_return <- c(Delt(C1_BTC$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C1_BTC$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C1_BTC$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C1_BTC$volatility_futures_return <- C1_BTC$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C1_BTC$lost_coins_proxy <- C1_BTC$outstanding_supply - C1_BTC$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C1_BTC$BTCUSDT_or_ETHUSDT_volume <- C1_BTC$BTCUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C1_BTC$future_volume <- C1_BTC$future_volume / 100
C1_BTC$BTCUSDT_or_ETHUSDT_volume <- C1_BTC$BTCUSDT_or_ETHUSDT_volume / 10000 

### CONTRACT: C2_BTC ###
# rates as proportion (changing from %):
C2_BTC$rate_4w_no_nan <- C2_BTC$rate_4w_no_nan / 100 
C2_BTC$rate_3m_no_nan <- C2_BTC$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C2_BTC$obs, n=1)
C2_BTC$time_to_maturity <- maturity_day - C2_BTC$obs
C2_BTC$time_to_maturity_proportion <- C2_BTC$time_to_maturity / 360 
# theoretical futures price:
C2_BTC$theoretical_futures_price <- C2_BTC$spot_price_index * exp(C2_BTC$rate_4w_no_nan * C2_BTC$time_to_maturity_proportion) 
# DEPENDENT VARIABLE: mispricing terms (all variations):
C2_BTC$mispricing_term_1 <- C2_BTC$theoretical_futures_price - C2_BTC$future_close                                    
C2_BTC$mispricing_term_2 <- abs(C2_BTC$theoretical_futures_price - C2_BTC$future_close)                               
C2_BTC$mispricing_term_3 <- (C2_BTC$theoretical_futures_price - C2_BTC$future_close) / C2_BTC$future_close * 100      
C2_BTC$mispricing_term_4 <- abs((C2_BTC$theoretical_futures_price - C2_BTC$future_close) / C2_BTC$future_close * 100) 
# BASIS:
C2_BTC$mispricing_term_5 <- C2_BTC$spot_price_index - C2_BTC$future_close                                             
C2_BTC$mispricing_term_6 <- abs(C2_BTC$spot_price_index - C2_BTC$future_close)                                        
C2_BTC$mispricing_term_7 <- (C2_BTC$spot_price_index - C2_BTC$future_close) / C2_BTC$future_close * 100               
C2_BTC$mispricing_term_8 <- abs((C2_BTC$spot_price_index - C2_BTC$future_close) / C2_BTC$future_close * 100)          
# return of spot price index (daily return):
C2_BTC$spot_price_index_return <- c(Delt(C2_BTC$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C2_BTC$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C2_BTC$spot_price_index_return), window, FUN = function(x) sd(x)))  
C2_BTC$volatility_spot_price_index_return <- C2_BTC$volatility_spot_price_index_return * sqrt(365) 
# return of futures:
C2_BTC$futures_return <- c(Delt(C2_BTC$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C2_BTC$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C2_BTC$futures_return), window, FUN = function(x) sd(x))) 
C2_BTC$volatility_futures_return <- C2_BTC$volatility_futures_return * sqrt(365) 
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C2_BTC$lost_coins_proxy <- C2_BTC$outstanding_supply - C2_BTC$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C2_BTC$BTCUSDT_or_ETHUSDT_volume <- C2_BTC$BTCUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C2_BTC$future_volume <- C2_BTC$future_volume / 100
C2_BTC$BTCUSDT_or_ETHUSDT_volume <- C2_BTC$BTCUSDT_or_ETHUSDT_volume / 10000 


### CONTRACT: C3_BTC ###
# rates as proportion (changing from %):
C3_BTC$rate_4w_no_nan <- C3_BTC$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C3_BTC$rate_3m_no_nan <- C3_BTC$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C3_BTC$obs, n=1)
C3_BTC$time_to_maturity <- maturity_day - C3_BTC$obs
C3_BTC$time_to_maturity_proportion <- C3_BTC$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C3_BTC$theoretical_futures_price <- C3_BTC$spot_price_index * exp(C3_BTC$rate_4w_no_nan * C3_BTC$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C3_BTC$mispricing_term_1 <- C3_BTC$theoretical_futures_price - C3_BTC$future_close                                    # just range 
C3_BTC$mispricing_term_2 <- abs(C3_BTC$theoretical_futures_price - C3_BTC$future_close)                               # absolute value of range
C3_BTC$mispricing_term_3 <- (C3_BTC$theoretical_futures_price - C3_BTC$future_close) / C3_BTC$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C3_BTC$mispricing_term_4 <- abs((C3_BTC$theoretical_futures_price - C3_BTC$future_close) / C3_BTC$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C3_BTC$mispricing_term_5 <- C3_BTC$spot_price_index - C3_BTC$future_close                                             # just range -> Basis = s-f
C3_BTC$mispricing_term_6 <- abs(C3_BTC$spot_price_index - C3_BTC$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C3_BTC$mispricing_term_7 <- (C3_BTC$spot_price_index - C3_BTC$future_close) / C3_BTC$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C3_BTC$mispricing_term_8 <- abs((C3_BTC$spot_price_index - C3_BTC$future_close) / C3_BTC$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C3_BTC$spot_price_index_return <- c(Delt(C3_BTC$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C3_BTC$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C3_BTC$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C3_BTC$volatility_spot_price_index_return <- C3_BTC$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C3_BTC$futures_return <- c(Delt(C3_BTC$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C3_BTC$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C3_BTC$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C3_BTC$volatility_futures_return <- C3_BTC$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C3_BTC$lost_coins_proxy <- C3_BTC$outstanding_supply - C3_BTC$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C3_BTC$BTCUSDT_or_ETHUSDT_volume <- C3_BTC$BTCUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C3_BTC$future_volume <- C3_BTC$future_volume / 100
C3_BTC$BTCUSDT_or_ETHUSDT_volume <- C3_BTC$BTCUSDT_or_ETHUSDT_volume / 10000 

### CONTRACT: C4_BTC ###
# rates as proportion (changing from %):
C4_BTC$rate_4w_no_nan <- C4_BTC$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C4_BTC$rate_3m_no_nan <- C4_BTC$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C4_BTC$obs, n=1)
C4_BTC$time_to_maturity <- maturity_day - C4_BTC$obs
C4_BTC$time_to_maturity_proportion <- C4_BTC$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C4_BTC$theoretical_futures_price <- C4_BTC$spot_price_index * exp(C4_BTC$rate_4w_no_nan * C4_BTC$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C4_BTC$mispricing_term_1 <- C4_BTC$theoretical_futures_price - C4_BTC$future_close                                    # just range 
C4_BTC$mispricing_term_2 <- abs(C4_BTC$theoretical_futures_price - C4_BTC$future_close)                               # absolute value of range
C4_BTC$mispricing_term_3 <- (C4_BTC$theoretical_futures_price - C4_BTC$future_close) / C4_BTC$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C4_BTC$mispricing_term_4 <- abs((C4_BTC$theoretical_futures_price - C4_BTC$future_close) / C4_BTC$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C4_BTC$mispricing_term_5 <- C4_BTC$spot_price_index - C4_BTC$future_close                                             # just range -> Basis = s-f
C4_BTC$mispricing_term_6 <- abs(C4_BTC$spot_price_index - C4_BTC$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C4_BTC$mispricing_term_7 <- (C4_BTC$spot_price_index - C4_BTC$future_close) / C4_BTC$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C4_BTC$mispricing_term_8 <- abs((C4_BTC$spot_price_index - C4_BTC$future_close) / C4_BTC$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C4_BTC$spot_price_index_return <- c(Delt(C4_BTC$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C4_BTC$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C4_BTC$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C4_BTC$volatility_spot_price_index_return <- C4_BTC$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C4_BTC$futures_return <- c(Delt(C4_BTC$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C4_BTC$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C4_BTC$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C4_BTC$volatility_futures_return <- C4_BTC$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C4_BTC$lost_coins_proxy <- C4_BTC$outstanding_supply - C4_BTC$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C4_BTC$BTCUSDT_or_ETHUSDT_volume <- C4_BTC$BTCUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C4_BTC$future_volume <- C4_BTC$future_volume / 100
C4_BTC$BTCUSDT_or_ETHUSDT_volume <- C4_BTC$BTCUSDT_or_ETHUSDT_volume / 10000 

### CONTRACT: C5_BTC ###
# rates as proportion (changing from %):
C5_BTC$rate_4w_no_nan <- C5_BTC$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C5_BTC$rate_3m_no_nan <- C5_BTC$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C5_BTC$obs, n=1)
C5_BTC$time_to_maturity <- maturity_day - C5_BTC$obs
C5_BTC$time_to_maturity_proportion <- C5_BTC$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C5_BTC$theoretical_futures_price <- C5_BTC$spot_price_index * exp(C5_BTC$rate_4w_no_nan * C5_BTC$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C5_BTC$mispricing_term_1 <- C5_BTC$theoretical_futures_price - C5_BTC$future_close                                    # just range 
C5_BTC$mispricing_term_2 <- abs(C5_BTC$theoretical_futures_price - C5_BTC$future_close)                               # absolute value of range
C5_BTC$mispricing_term_3 <- (C5_BTC$theoretical_futures_price - C5_BTC$future_close) / C5_BTC$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C5_BTC$mispricing_term_4 <- abs((C5_BTC$theoretical_futures_price - C5_BTC$future_close) / C5_BTC$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C5_BTC$mispricing_term_5 <- C5_BTC$spot_price_index - C5_BTC$future_close                                             # just range -> Basis = s-f
C5_BTC$mispricing_term_6 <- abs(C5_BTC$spot_price_index - C5_BTC$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C5_BTC$mispricing_term_7 <- (C5_BTC$spot_price_index - C5_BTC$future_close) / C5_BTC$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C5_BTC$mispricing_term_8 <- abs((C5_BTC$spot_price_index - C5_BTC$future_close) / C5_BTC$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C5_BTC$spot_price_index_return <- c(Delt(C5_BTC$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C5_BTC$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C5_BTC$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C5_BTC$volatility_spot_price_index_return <- C5_BTC$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C5_BTC$futures_return <- c(Delt(C5_BTC$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C5_BTC$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C5_BTC$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C5_BTC$volatility_futures_return <- C5_BTC$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C5_BTC$lost_coins_proxy <- C5_BTC$outstanding_supply - C5_BTC$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C5_BTC$BTCUSDT_or_ETHUSDT_volume <- C5_BTC$BTCUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C5_BTC$future_volume <- C5_BTC$future_volume / 100
C5_BTC$BTCUSDT_or_ETHUSDT_volume <- C5_BTC$BTCUSDT_or_ETHUSDT_volume / 10000 

### CONTRACT: C6_BTC ###
# rates as proportion (changing from %):
C6_BTC$rate_4w_no_nan <- C6_BTC$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C6_BTC$rate_3m_no_nan <- C6_BTC$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C6_BTC$obs, n=1)
C6_BTC$time_to_maturity <- maturity_day - C6_BTC$obs
C6_BTC$time_to_maturity_proportion <- C6_BTC$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C6_BTC$theoretical_futures_price <- C6_BTC$spot_price_index * exp(C6_BTC$rate_4w_no_nan * C6_BTC$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C6_BTC$mispricing_term_1 <- C6_BTC$theoretical_futures_price - C6_BTC$future_close                                    # just range 
C6_BTC$mispricing_term_2 <- abs(C6_BTC$theoretical_futures_price - C6_BTC$future_close)                               # absolute value of range
C6_BTC$mispricing_term_3 <- (C6_BTC$theoretical_futures_price - C6_BTC$future_close) / C6_BTC$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C6_BTC$mispricing_term_4 <- abs((C6_BTC$theoretical_futures_price - C6_BTC$future_close) / C6_BTC$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C6_BTC$mispricing_term_5 <- C6_BTC$spot_price_index - C6_BTC$future_close                                             # just range -> Basis = s-f
C6_BTC$mispricing_term_6 <- abs(C6_BTC$spot_price_index - C6_BTC$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C6_BTC$mispricing_term_7 <- (C6_BTC$spot_price_index - C6_BTC$future_close) / C6_BTC$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C6_BTC$mispricing_term_8 <- abs((C6_BTC$spot_price_index - C6_BTC$future_close) / C6_BTC$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C6_BTC$spot_price_index_return <- c(Delt(C6_BTC$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C6_BTC$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C6_BTC$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C6_BTC$volatility_spot_price_index_return <- C6_BTC$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C6_BTC$futures_return <- c(Delt(C6_BTC$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C6_BTC$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C6_BTC$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C6_BTC$volatility_futures_return <- C6_BTC$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C6_BTC$lost_coins_proxy <- C6_BTC$outstanding_supply - C6_BTC$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C6_BTC$BTCUSDT_or_ETHUSDT_volume <- C6_BTC$BTCUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C6_BTC$future_volume <- C6_BTC$future_volume / 100
C6_BTC$BTCUSDT_or_ETHUSDT_volume <- C6_BTC$BTCUSDT_or_ETHUSDT_volume / 10000 



# ETHEREUM CONTRACTS
### CONTRACT: C1_ETH ###
# rates as proportion (changing from %):
C1_ETH$rate_4w_no_nan <- C1_ETH$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C1_ETH$rate_3m_no_nan <- C1_ETH$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C1_ETH$obs, n=1)
C1_ETH$time_to_maturity <- maturity_day - C1_ETH$obs
C1_ETH$time_to_maturity_proportion <- C1_ETH$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C1_ETH$theoretical_futures_price <- C1_ETH$spot_price_index * exp(C1_ETH$rate_4w_no_nan * C1_ETH$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C1_ETH$mispricing_term_1 <- C1_ETH$theoretical_futures_price - C1_ETH$future_close                                    # just range 
C1_ETH$mispricing_term_2 <- abs(C1_ETH$theoretical_futures_price - C1_ETH$future_close)                               # absolute value of range
C1_ETH$mispricing_term_3 <- (C1_ETH$theoretical_futures_price - C1_ETH$future_close) / C1_ETH$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C1_ETH$mispricing_term_4 <- abs((C1_ETH$theoretical_futures_price - C1_ETH$future_close) / C1_ETH$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C1_ETH$mispricing_term_5 <- C1_ETH$spot_price_index - C1_ETH$future_close                                             # just range -> Basis = s-f
C1_ETH$mispricing_term_6 <- abs(C1_ETH$spot_price_index - C1_ETH$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C1_ETH$mispricing_term_7 <- (C1_ETH$spot_price_index - C1_ETH$future_close) / C1_ETH$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C1_ETH$mispricing_term_8 <- abs((C1_ETH$spot_price_index - C1_ETH$future_close) / C1_ETH$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C1_ETH$spot_price_index_return <- c(Delt(C1_ETH$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C1_ETH$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C1_ETH$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C1_ETH$volatility_spot_price_index_return <- C1_ETH$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C1_ETH$futures_return <- c(Delt(C1_ETH$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C1_ETH$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C1_ETH$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C1_ETH$volatility_futures_return <- C1_ETH$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C1_ETH$lost_coins_proxy <- C1_ETH$outstanding_supply - C1_ETH$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C1_ETH$BTCUSDT_or_ETHUSDT_volume <- C1_ETH$ETHUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C1_ETH$future_volume <- C1_ETH$future_volume / 10000 # future volume for ETH measured in terms of 10000 contracts
C1_ETH$BTCUSDT_or_ETHUSDT_volume <- C1_ETH$BTCUSDT_or_ETHUSDT_volume / 1000000 # volume in terms of 10^6 units

### CONTRACT: C2_ETH ###
# rates as proportion (changing from %):
C2_ETH$rate_4w_no_nan <- C2_ETH$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C2_ETH$rate_3m_no_nan <- C2_ETH$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C2_ETH$obs, n=1)
C2_ETH$time_to_maturity <- maturity_day - C2_ETH$obs
C2_ETH$time_to_maturity_proportion <- C2_ETH$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C2_ETH$theoretical_futures_price <- C2_ETH$spot_price_index * exp(C2_ETH$rate_4w_no_nan * C2_ETH$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C2_ETH$mispricing_term_1 <- C2_ETH$theoretical_futures_price - C2_ETH$future_close                                    # just range 
C2_ETH$mispricing_term_2 <- abs(C2_ETH$theoretical_futures_price - C2_ETH$future_close)                               # absolute value of range
C2_ETH$mispricing_term_3 <- (C2_ETH$theoretical_futures_price - C2_ETH$future_close) / C2_ETH$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C2_ETH$mispricing_term_4 <- abs((C2_ETH$theoretical_futures_price - C2_ETH$future_close) / C2_ETH$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C2_ETH$mispricing_term_5 <- C2_ETH$spot_price_index - C2_ETH$future_close                                             # just range -> Basis = s-f
C2_ETH$mispricing_term_6 <- abs(C2_ETH$spot_price_index - C2_ETH$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C2_ETH$mispricing_term_7 <- (C2_ETH$spot_price_index - C2_ETH$future_close) / C2_ETH$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C2_ETH$mispricing_term_8 <- abs((C2_ETH$spot_price_index - C2_ETH$future_close) / C2_ETH$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C2_ETH$spot_price_index_return <- c(Delt(C2_ETH$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C2_ETH$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C2_ETH$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C2_ETH$volatility_spot_price_index_return <- C2_ETH$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C2_ETH$futures_return <- c(Delt(C2_ETH$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C2_ETH$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C2_ETH$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C2_ETH$volatility_futures_return <- C2_ETH$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C2_ETH$lost_coins_proxy <- C2_ETH$outstanding_supply - C2_ETH$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C2_ETH$BTCUSDT_or_ETHUSDT_volume <- C2_ETH$ETHUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C2_ETH$future_volume <- C2_ETH$future_volume / 10000 # future volume for ETH measured in terms of 10000 contracts
C2_ETH$BTCUSDT_or_ETHUSDT_volume <- C2_ETH$BTCUSDT_or_ETHUSDT_volume / 1000000 # volume in terms of 10^6 units


### CONTRACT: C3_ETH ###
# rates as proportion (changing from %):
C3_ETH$rate_4w_no_nan <- C3_ETH$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C3_ETH$rate_3m_no_nan <- C3_ETH$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C3_ETH$obs, n=1)
C3_ETH$time_to_maturity <- maturity_day - C3_ETH$obs
C3_ETH$time_to_maturity_proportion <- C3_ETH$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C3_ETH$theoretical_futures_price <- C3_ETH$spot_price_index * exp(C3_ETH$rate_4w_no_nan * C3_ETH$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C3_ETH$mispricing_term_1 <- C3_ETH$theoretical_futures_price - C3_ETH$future_close                                    # just range 
C3_ETH$mispricing_term_2 <- abs(C3_ETH$theoretical_futures_price - C3_ETH$future_close)                               # absolute value of range
C3_ETH$mispricing_term_3 <- (C3_ETH$theoretical_futures_price - C3_ETH$future_close) / C3_ETH$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C3_ETH$mispricing_term_4 <- abs((C3_ETH$theoretical_futures_price - C3_ETH$future_close) / C3_ETH$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C3_ETH$mispricing_term_5 <- C3_ETH$spot_price_index - C3_ETH$future_close                                             # just range -> Basis = s-f
C3_ETH$mispricing_term_6 <- abs(C3_ETH$spot_price_index - C3_ETH$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C3_ETH$mispricing_term_7 <- (C3_ETH$spot_price_index - C3_ETH$future_close) / C3_ETH$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C3_ETH$mispricing_term_8 <- abs((C3_ETH$spot_price_index - C3_ETH$future_close) / C3_ETH$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C3_ETH$spot_price_index_return <- c(Delt(C3_ETH$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C3_ETH$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C3_ETH$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C3_ETH$volatility_spot_price_index_return <- C3_ETH$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C3_ETH$futures_return <- c(Delt(C3_ETH$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C3_ETH$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C3_ETH$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C3_ETH$volatility_futures_return <- C3_ETH$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C3_ETH$lost_coins_proxy <- C3_ETH$outstanding_supply - C3_ETH$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C3_ETH$BTCUSDT_or_ETHUSDT_volume <- C3_ETH$ETHUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C3_ETH$future_volume <- C3_ETH$future_volume / 10000 # future volume for ETH measured in terms of 10000 contracts
C3_ETH$BTCUSDT_or_ETHUSDT_volume <- C3_ETH$BTCUSDT_or_ETHUSDT_volume / 1000000 # volume in terms of 10^6 units

### CONTRACT: C4_ETH ###
# rates as proportion (changing from %):
C4_ETH$rate_4w_no_nan <- C4_ETH$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C4_ETH$rate_3m_no_nan <- C4_ETH$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C4_ETH$obs, n=1)
C4_ETH$time_to_maturity <- maturity_day - C4_ETH$obs
C4_ETH$time_to_maturity_proportion <- C4_ETH$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C4_ETH$theoretical_futures_price <- C4_ETH$spot_price_index * exp(C4_ETH$rate_4w_no_nan * C4_ETH$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C4_ETH$mispricing_term_1 <- C4_ETH$theoretical_futures_price - C4_ETH$future_close                                    # just range 
C4_ETH$mispricing_term_2 <- abs(C4_ETH$theoretical_futures_price - C4_ETH$future_close)                               # absolute value of range
C4_ETH$mispricing_term_3 <- (C4_ETH$theoretical_futures_price - C4_ETH$future_close) / C4_ETH$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C4_ETH$mispricing_term_4 <- abs((C4_ETH$theoretical_futures_price - C4_ETH$future_close) / C4_ETH$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C4_ETH$mispricing_term_5 <- C4_ETH$spot_price_index - C4_ETH$future_close                                             # just range -> Basis = s-f
C4_ETH$mispricing_term_6 <- abs(C4_ETH$spot_price_index - C4_ETH$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C4_ETH$mispricing_term_7 <- (C4_ETH$spot_price_index - C4_ETH$future_close) / C4_ETH$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C4_ETH$mispricing_term_8 <- abs((C4_ETH$spot_price_index - C4_ETH$future_close) / C4_ETH$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C4_ETH$spot_price_index_return <- c(Delt(C4_ETH$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C4_ETH$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C4_ETH$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C4_ETH$volatility_spot_price_index_return <- C4_ETH$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C4_ETH$futures_return <- c(Delt(C4_ETH$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C4_ETH$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C4_ETH$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C4_ETH$volatility_futures_return <- C4_ETH$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C4_ETH$lost_coins_proxy <- C4_ETH$outstanding_supply - C4_ETH$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C4_ETH$BTCUSDT_or_ETHUSDT_volume <- C4_ETH$ETHUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C4_ETH$future_volume <- C4_ETH$future_volume / 10000 # future volume for ETH measured in terms of 10000 contracts
C4_ETH$BTCUSDT_or_ETHUSDT_volume <- C4_ETH$BTCUSDT_or_ETHUSDT_volume / 1000000 # volume in terms of 10^6 units

### CONTRACT: C5_ETH ###
# rates as proportion (changing from %):
C5_ETH$rate_4w_no_nan <- C5_ETH$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C5_ETH$rate_3m_no_nan <- C5_ETH$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C5_ETH$obs, n=1)
C5_ETH$time_to_maturity <- maturity_day - C5_ETH$obs
C5_ETH$time_to_maturity_proportion <- C5_ETH$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C5_ETH$theoretical_futures_price <- C5_ETH$spot_price_index * exp(C5_ETH$rate_4w_no_nan * C5_ETH$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C5_ETH$mispricing_term_1 <- C5_ETH$theoretical_futures_price - C5_ETH$future_close                                    # just range 
C5_ETH$mispricing_term_2 <- abs(C5_ETH$theoretical_futures_price - C5_ETH$future_close)                               # absolute value of range
C5_ETH$mispricing_term_3 <- (C5_ETH$theoretical_futures_price - C5_ETH$future_close) / C5_ETH$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C5_ETH$mispricing_term_4 <- abs((C5_ETH$theoretical_futures_price - C5_ETH$future_close) / C5_ETH$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C5_ETH$mispricing_term_5 <- C5_ETH$spot_price_index - C5_ETH$future_close                                             # just range -> Basis = s-f
C5_ETH$mispricing_term_6 <- abs(C5_ETH$spot_price_index - C5_ETH$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C5_ETH$mispricing_term_7 <- (C5_ETH$spot_price_index - C5_ETH$future_close) / C5_ETH$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C5_ETH$mispricing_term_8 <- abs((C5_ETH$spot_price_index - C5_ETH$future_close) / C5_ETH$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C5_ETH$spot_price_index_return <- c(Delt(C5_ETH$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C5_ETH$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C5_ETH$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C5_ETH$volatility_spot_price_index_return <- C5_ETH$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C5_ETH$futures_return <- c(Delt(C5_ETH$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C5_ETH$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C5_ETH$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C5_ETH$volatility_futures_return <- C5_ETH$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C5_ETH$lost_coins_proxy <- C5_ETH$outstanding_supply - C5_ETH$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C5_ETH$BTCUSDT_or_ETHUSDT_volume <- C5_ETH$ETHUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C5_ETH$future_volume <- C5_ETH$future_volume / 10000 # future volume for ETH measured in terms of 10000 contracts
C5_ETH$BTCUSDT_or_ETHUSDT_volume <- C5_ETH$BTCUSDT_or_ETHUSDT_volume / 1000000 # volume in terms of 10^6 units

### CONTRACT: C6_ETH ###
# rates as proportion (changing from %):
C6_ETH$rate_4w_no_nan <- C6_ETH$rate_4w_no_nan / 100 # changing from %: needed for theoretical futures price calculation;
C6_ETH$rate_3m_no_nan <- C6_ETH$rate_3m_no_nan / 100
# time to maturity:
maturity_day <- tail(C6_ETH$obs, n=1)
C6_ETH$time_to_maturity <- maturity_day - C6_ETH$obs
C6_ETH$time_to_maturity_proportion <- C6_ETH$time_to_maturity / 360 # this is used for theoretical futures price calculation;
# theoretical futures price:
C6_ETH$theoretical_futures_price <- C6_ETH$spot_price_index * exp(C6_ETH$rate_4w_no_nan * C6_ETH$time_to_maturity_proportion) # price_index according to Binance
# DEPENDENT VARIABLE: mispricing terms (all variations):
C6_ETH$mispricing_term_1 <- C6_ETH$theoretical_futures_price - C6_ETH$future_close                                    # just range 
C6_ETH$mispricing_term_2 <- abs(C6_ETH$theoretical_futures_price - C6_ETH$future_close)                               # absolute value of range
C6_ETH$mispricing_term_3 <- (C6_ETH$theoretical_futures_price - C6_ETH$future_close) / C6_ETH$future_close * 100      # % difference between theoretical futures price and market-quoted futures price
C6_ETH$mispricing_term_4 <- abs((C6_ETH$theoretical_futures_price - C6_ETH$future_close) / C6_ETH$future_close * 100) # abs: % difference between theoretical futures price and market-quoted futures price
# BASIS:
C6_ETH$mispricing_term_5 <- C6_ETH$spot_price_index - C6_ETH$future_close                                             # just range -> Basis = s-f
C6_ETH$mispricing_term_6 <- abs(C6_ETH$spot_price_index - C6_ETH$future_close)                                        # absolute value of range -> |Basis| = |s-f|
C6_ETH$mispricing_term_7 <- (C6_ETH$spot_price_index - C6_ETH$future_close) / C6_ETH$future_close * 100               # % difference between spot and market-quoted futures price -> Basis per f
C6_ETH$mispricing_term_8 <- abs((C6_ETH$spot_price_index - C6_ETH$future_close) / C6_ETH$future_close * 100)          # abs: % difference between spot and market-quoted futures price -> Basis per f
# return of spot price index (daily return):
C6_ETH$spot_price_index_return <- c(Delt(C6_ETH$spot_price_index))
# volatility return of spot price index (computed as standard deviation of returns within 30 days interval):
window <- 30 
C6_ETH$volatility_spot_price_index_return <- c(rep(NA, window-1), rollapply(as.vector(C6_ETH$spot_price_index_return), window, FUN = function(x) sd(x))) # not annualized 
C6_ETH$volatility_spot_price_index_return <- C6_ETH$volatility_spot_price_index_return * sqrt(365) # annualized
# return of futures:
C6_ETH$futures_return <- c(Delt(C6_ETH$future_close))
#	volatility return of futures (computed as standard deviation of futures returns within 30 days interval):
window <- 30 
C6_ETH$volatility_futures_return <- c(rep(NA, window-1), rollapply(as.vector(C6_ETH$futures_return), window, FUN = function(x) sd(x))) # not annualized 
C6_ETH$volatility_futures_return <- C6_ETH$volatility_futures_return * sqrt(365) # annualized
# Proxy for lost coins = Outstanding_supply - Five_year_activity_supply:
C6_ETH$lost_coins_proxy <- C6_ETH$outstanding_supply - C6_ETH$five_year_active_supply
# BTCUSDT and ETHUSDT volume:
C6_ETH$BTCUSDT_or_ETHUSDT_volume <- C6_ETH$ETHUSDT_volume
# !!! For scalling (helps with interpreting regression results - not that many zeros):
C6_ETH$future_volume <- C6_ETH$future_volume / 10000 # future volume for ETH measured in terms of 10000 contracts
C6_ETH$BTCUSDT_or_ETHUSDT_volume <- C6_ETH$BTCUSDT_or_ETHUSDT_volume / 1000000 # volume in terms of 10^6 units






#
# =========================================================================================
#                                                   `TRIMMING STARTS
#                                                     *************
# =========================================================================================
#

#****************************************************************************************************#
### TRIMMING DATASETS 
###3 OPTIONS, only one can be uncommented:

# # A: NOT TRIMMED at all
# C1_BTC <- C1_BTC
# C2_BTC <- C2_BTC
# C3_BTC <- C3_BTC
# C4_BTC <- C4_BTC
# C5_BTC <- C5_BTC
# C6_BTC <- C6_BTC
# #
# C1_ETH <- C1_ETH
# C2_ETH <- C2_ETH
# C3_ETH <- C3_ETH
# C4_ETH <- C4_ETH
# C5_ETH <- C5_ETH
# C6_ETH <- C6_ETH


# B: TRIMMED (only end - 1 obs) THIS ONE IS USED IN THE THESIS
C1_BTC <- C1_BTC[1:51, ]
C2_BTC <- C2_BTC[1:101, ]
C3_BTC <- C3_BTC[1:98, ]
C4_BTC <- C4_BTC[1:100, ]
C5_BTC <- C5_BTC[1:91, ]
C6_BTC <- C6_BTC[1:94, ]
#
C1_ETH <- C1_ETH[1:50, ]
C2_ETH <- C2_ETH[1:101, ]
C3_ETH <- C3_ETH[1:98, ]
C4_ETH <- C4_ETH[1:100, ]
C5_ETH <- C5_ETH[1:91, ]
C6_ETH <- C6_ETH[1:94, ]



# # C: TRIMMED (BOTH start and end)
# C1_BTC <- C1_BTC[1:49, ]  # 49 observations
# C2_BTC <- C2_BTC[40:99, ] # 60 observations
# C3_BTC <- C3_BTC[37:96, ] # 60 observations
# C4_BTC <- C4_BTC[39:98, ] # 60 observations
# C5_BTC <- C5_BTC[30:89, ] # 60 observations
# C6_BTC <- C6_BTC[33:92, ] # 60 observations
# #
# C1_ETH <- C1_ETH[1:48, ]  # 48 observations
# C2_ETH <- C2_ETH[40:99, ] # 60 observations
# C3_ETH <- C3_ETH[37:96, ] # 60 observations
# C4_ETH <- C4_ETH[39:98, ] # 60 observations
# C5_ETH <- C5_ETH[30:89, ] # 60 observations
# C6_ETH <- C6_ETH[33:92, ] # 60 observations



### EXPORT
# library(writexl)
# sheets <- list("C1_BTC" = C1_BTC, "C2_BTC" = C2_BTC, "C3_BTC" = C3_BTC, "C4_BTC" = C4_BTC, "C5_BTC" = C5_BTC, "C6_BTC" = C6_BTC)
# write_xlsx(sheets, "BTC_exported_dataframes_FINAL.xlsx")
# 
# sheets <- list("C1_ETH" = C1_ETH, "C2_ETH" = C2_ETH, "C3_ETH" = C3_ETH, "C4_ETH" = C4_ETH, "C5_ETH" = C5_ETH, "C6_ETH" = C6_ETH)
# write_xlsx(sheets, "ETH_exported_dataframes_FINAL.xlsx")



#
# =========================================================================================
#                                           Regressions STARTS
#                                             *************
# =========================================================================================
#


#****************************************************************************************************#
# Newey and West:
# ?NeweyWest

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ BTC:
print("------------------------C1_BTC START-------------------")
attach(C1_BTC)
lm_C1_BTC <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C1_BTC)
summary(lm_C1_BTC)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C1_BTC, vcov.=NeweyWest(lm_C1_BTC, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE)) # HAC STANDARD ERRORS (and p-values)
detach(C1_BTC)
print("------------------------C1_BTC END---------------------")

print("------------------------C2_BTC START-------------------")
attach(C2_BTC)
lm_C2_BTC <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C2_BTC)
summary(lm_C2_BTC)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C2_BTC, vcov.=NeweyWest(lm_C2_BTC, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C2_BTC)
print("------------------------C2_BTC END---------------------")


print("------------------------C3_BTC START-------------------")
attach(C3_BTC)
lm_C3_BTC <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C3_BTC)
summary(lm_C3_BTC)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C3_BTC, vcov.=NeweyWest(lm_C3_BTC, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C3_BTC)
print("------------------------C3_BTC END---------------------")


print("------------------------C4_BTC START-------------------")
attach(C4_BTC)
lm_C4_BTC <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C4_BTC)
summary(lm_C4_BTC)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C4_BTC, vcov.=NeweyWest(lm_C4_BTC, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C4_BTC)
print("------------------------C4_BTC END---------------------")


print("------------------------C5_BTC START-------------------")
attach(C5_BTC)
lm_C5_BTC <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C5_BTC)
summary(lm_C5_BTC)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C5_BTC, vcov.=NeweyWest(lm_C5_BTC, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C5_BTC)
print("------------------------C5_BTC END---------------------")


print("------------------------C6_BTC START-------------------")
attach(C6_BTC)
lm_C6_BTC <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C6_BTC)
summary(lm_C6_BTC)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C6_BTC, vcov.=NeweyWest(lm_C6_BTC, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C6_BTC)
print("------------------------C6_BTC END---------------------")





### &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& ETH:
print("------------------------C1_ETH START-------------------")
attach(C1_ETH)
lm_C1_ETH <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C1_ETH)
summary(lm_C1_ETH)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C1_ETH, vcov.=NeweyWest(lm_C1_ETH, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C1_ETH)
print("------------------------C1_ETH END---------------------")


print("------------------------C2_ETH START-------------------")
attach(C2_ETH)
lm_C2_ETH <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C2_ETH)
summary(lm_C2_ETH)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C2_ETH, vcov.=NeweyWest(lm_C2_ETH, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C2_ETH)
print("------------------------C2_ETH END---------------------")


print("------------------------C3_ETH START-------------------")
attach(C3_ETH)
lm_C3_ETH <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C3_ETH)
summary(lm_C3_ETH)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C3_ETH, vcov.=NeweyWest(lm_C3_ETH, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C3_ETH)
print("------------------------C3_ETH END---------------------")


print("------------------------C4_ETH START-------------------")
attach(C4_ETH)
lm_C4_ETH <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C4_ETH)
summary(lm_C4_ETH)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C4_ETH, vcov.=NeweyWest(lm_C4_ETH, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C4_ETH)
print("------------------------C4_ETH END---------------------")


print("------------------------C5_ETH START-------------------")
attach(C5_ETH)
lm_C5_ETH <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C5_ETH)
summary(lm_C5_ETH)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C5_ETH, vcov.=NeweyWest(lm_C5_ETH, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C5_ETH)
print("------------------------C5_ETH END---------------------")


print("------------------------C6_ETH START-------------------")
attach(C6_ETH)
lm_C6_ETH <- lm(mispricing_term_1 ~
                  future_volume
                +
                  I(future_volume * future_volume)
                # +
                #   marketcap_dominance
                # +
                #   BTCUSDT_or_ETHUSDT_volume
                # +
                #   scam_fraud_breach
                ,
                data=C6_ETH)
summary(lm_C6_ETH)
print("# lmtest  and sandwich libraries, Newey and West:")
coeftest(lm_C6_ETH, vcov.=NeweyWest(lm_C6_ETH, lag=3, prewhite=FALSE, adjust=TRUE, verbose=TRUE))
detach(C6_ETH)
print("------------------------C6_ETH END---------------------")















