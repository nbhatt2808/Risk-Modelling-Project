setwd('C:/Users/bneer/OneDrive/Desktop/Harrisburg-Risk Modelling/Final Project')

library(QRM)
library(qrmdata)
library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(FRAPO)
library(timeSeries)
library(fBasics)
library(ghyp)
library(fExtremes)
library(ismev)
library(evir)
library(tseries)
library(forecast)
library(tidyverse)
library(copula)
library(ghyp)
library(fBasics)
options(scipen = 999)

# duration
from = "2008-01-01"
to = "2020-10-01"

get_date <- function(ticker, from, to){
  stock_xts <- as.xts(data.frame(getSymbols.yahoo(ticker, src = 'yahoo', 
                                                  return.class = "xts",
                                                  index.class = "Date",
                                                  from = from, to = to, 
                                                  auto.assign = F,
                                                  periodicity = "monthly")))
  stock_df <- as.data.frame(stock_xts)
  stock_df <- cbind(Date = index(stock_xts), data.frame(stock_df, row.names = NULL))
  stock_df$Date <- as.Date(stock_df$Date)
  date <- as.character(stock_df$Date)
  return(date)
}

data_prep <- function(ticker, from, to){
  stock_xts <- as.xts(data.frame(getSymbols.yahoo(ticker, src = 'yahoo', 
                                                  return.class = "xts",
                                                  index.class = "Date",
                                                  from = from, to = to, 
                                                  auto.assign = F,
                                                  periodicity = "monthly")))
  stock_df <- as.data.frame(stock_xts)
  stock_df <- cbind(Date = index(stock_xts), data.frame(stock_df, row.names = NULL))
  stock_df$Date <- as.Date(stock_df$Date)
  date <- as.character(stock_df$Date)
  stock_df <- stock_df[,c(1,7)]
  colnames(stock_df)[2] <- ticker
  stock_ts <- timeSeries(stock_df[,2], charvec = date)
  colnames(stock_ts) <- ticker
  return(stock_ts)
}

curr <- function(ticker, currency_name, from, to){
  currency_xts <- as.xts(data.frame(getSymbols.yahoo(ticker, src = 'yahoo', 
                                                     return.class = "xts",
                                                     index.class = "Date",
                                                     from = from, to = to, 
                                                     auto.assign = F,
                                                     periodicity = "monthly")))
  currency_df <- as.data.frame(currency_xts)
  currency_df <- cbind(Date = index(currency_xts), data.frame(currency_df, row.names = NULL))
  currency_df$Date <- as.Date(currency_df$Date)
  date <- as.character(currency_df$Date)
  currency_df <- currency_df[,c(1,7)]
  colnames(currency_df)[2] <- currency_name
  currency_df[,2] <- 1/currency_df[,2]
  currency_ts <- timeSeries(currency_df[,2], charvec = date)
  colnames(currency_ts) <- currency_name
  return(currency_ts)
}

comm <- function(ticker, commodity_name, from, to){
  commodity_xts <- as.xts(data.frame(getSymbols.yahoo(ticker, src = 'yahoo', 
                                                     return.class = "xts",
                                                     index.class = "Date",
                                                     from = from, to = to, 
                                                     auto.assign = F,
                                                     periodicity = "monthly")))
  commodity_df <- as.data.frame(commodity_xts)
  commodity_df <- cbind(Date = index(commodity_xts), data.frame(commodity_df, row.names = NULL))
  commodity_df$Date <- as.Date(commodity_df$Date)
  date <- as.character(commodity_df$Date)
  commodity_df <- commodity_df[,c(1,7)]
  colnames(commodity_df)[2] <- commodity_name
  commodity_ts <- timeSeries(commodity_df[,2], charvec = date)
  colnames(commodity_ts) <- commodity_name
  return(commodity_ts)
}

# stocks
aep <- data_prep("AEP", from, to)
duk <- data_prep("DUK", from, to)
d <- data_prep("D", from, to)
t <- data_prep("TMUS", from, to)
vz <- data_prep("VZ", from, to)
amt <- data_prep("AMT", from, to)
cci <- data_prep("CCI", from, to)

# currencies
from_curr = "2008-01-01"
to_curr = "2020-09-01"
yen <- curr("JPY=X", "Yen", from_curr, to_curr)
euro <- curr("EUR=X", "Euro", from_curr, to_curr)
pounds <- curr("GBP=X", "Pounds", from_curr, to_curr)

# Commodities
gold <- comm("GC=F", "Gold", from, to)
oil <- comm("CL=F", "Oil", from, to)
base_metals <- comm("DBB", "Metals", from, to)

# gathering all
assets <- cbind(aep, duk, d, t, vz, amt, cci, yen, euro, pounds, gold, oil,
                base_metals)
head(assets)

# computing the portfolio
# covariance
V <- cov(assets)
V

# correlation
r <- cor(assets)
r

# returns
R <- returnseries(assets, method = "discrete", trim = T)
head(R)

gmvw <- Weights(PGMV(R)) # Global Minimum Variance Portfolio
mdpw <- Weights(PMD(R)) # Most Diversified Portfolio

# combining results
w <- cbind(gmvw, mdpw)
w

# Portfolio
pret <- apply(w, 2, function(x) R %*% x / 100)
head(pret)

# timeseries objects

# GMVP
date <- as.character(get_date("AEP", from, to))
gmvp <- timeSeries(pret[,1], charvec = date[2:length(date)])
colnames(gmvp) <- "GMVP"
head(gmvp)

# MDP
mdp <- timeSeries(pret[,2], charvec = date[2:length(date)])
colnames(mdp) <- "MDP"
head(mdp)

# Expected Returns
apply(pret, 2, mean)

# Fitting the right distribution
aic_gmvp <- stepAIC.ghyp(gmvp)
aic_gmvp$fit.table
aic_gmvp$fit.table[aic_gmvp$fit.table$aic == min(aic_gmvp$fit.table$aic),]

aic_mdp <- stepAIC.ghyp(mdp)
aic_mdp$fit.table
aic_mdp$fit.table[aic_mdp$fit.table$aic == min(aic_mdp$fit.table$aic),]

# fitting the distribution
gvgfit_gmvp <- fit.VGuv(gmvp, symmetric = T, na.rm = T)
gvgfit_mdp <- fit.VGuv(mdp, symmetric = T, na.rm = T)

p <- c(0.01, 0.05, 0.1)
portvar_gmvp <- abs(qghyp(p, gvgfit_gmvp))
portvar_mdp <- abs(qghyp(p, gvgfit_mdp))

portes_gmvp <- abs(ESghyp(p, gvgfit_gmvp))
portes_gmvp

portes_mdp <- abs(ESghyp(p, gvgfit_mdp))
portes_mdp

# fitting the arima model
losses <- R*-1
head(R)
head(losses)

auto.arima(losses$AEP) # (0,0,0)
auto.arima(losses$DUK) # (2,0,1)
auto.arima(losses$D) # (1,0,0)
auto.arima(losses$TMUS) # (0,0,0)
auto.arima(losses$VZ) # (2,0,0)
auto.arima(losses$AMT) # (0,0,0)
auto.arima(losses$CCI) # (0,0,0)
auto.arima(losses$Yen) # (0,0,0)
auto.arima(losses$Euro) # (0,0,0)
auto.arima(losses$Pounds) # (0,0,0)
auto.arima(losses$Gold) # (0,0,1)
auto.arima(losses$Oil) # (2,0,1)
auto.arima(losses$Metals) # (0,0,0)

# fitting the models
arimaaep <- arima(losses$AEP, order = c(0,0,0), include.mean = F)
std_res_aep <- (arimaaep$residuals - mean(arimaaep$residuals))/sd(arimaaep$residuals)
acf(arimaaep$residuals)
qqnorm(std_res_aep)
qqline(std_res_aep)

arimaoil <- arima(losses$Oil, order = c(2,0,1), include.mean = F)
std_res_oil <- (arimaoil$residuals - mean(arimaoil$residuals))/sd(arimaoil$residuals)
acf(arimaoil$residuals)
qqnorm(std_res_oil, main = "QQ Plot Oil")
qqline(std_res_oil)

arimagold <- arima(losses$Gold, order = c(0,0,1), include.mean = F)
std_res_gold <- (arimagold$residuals - mean(arimagold$residuals))/sd(arimagold$residuals)
acf(arimagold$residuals)
qqnorm(std_res_gold, title = "QQ PLot Gold")
qqline(std_res_gold)

arimametal <- arima(losses$Metals, order = c(0,0,0), include.mean = F)
std_res_metal <- (arimametal$residuals - mean(arimametal$residuals))/sd(arimametal$residuals)
acf(arimametal$residuals)
qqnorm(std_res_metal, main = "QQ PLot Metals")
qqline(std_res_metal)

# remaining asset models
arimaduk <- arima(losses$DUK, order = c(2,0,1), include.mean = F)
arimad <- arima(losses$D, order = c(1,0,0), include.mean = F)
arimatmus <- arima(losses$TMUS, order = c(0,0,0), include.mean = F)
arimavz <- arima(losses$VZ, order = c(2,0,0), include.mean = F)
arimaamt <- arima(losses$AMT, order = c(0,0,0), include.mean = F)
arimacci <- arima(losses$CCI, order = c(0,0,0), include.mean = F)
arimayen <- arima(losses$Yen, order = c(0,0,0), include.mean = F)
arimaeuro <- arima(losses$Euro, order = c(0,0,0), include.mean = F)
arimapounds <- arima(losses$Pounds, order = c(0,0,0), include.mean = F)

# predicting losses for the next period
aep_forecast <- predict(arimaaep, n.ahead = 1)
d_forecast <- predict(arimad, n.ahead = 1)
duk_forecast <- predict(arimaduk, n.ahead = 1)
tmus_forecast <- predict(arimatmus, n.ahead = 1)
vz_forecast <- predict(arimavz, n.ahead = 1)
amt_forecast <- predict(arimaamt, n.ahead = 1)
cci_forecast <- predict(arimacci, n.ahead = 1)
yen_forecast <- predict(arimayen, n.ahead = 1)
euro_forecast <- predict(arimaeuro, n.ahead = 1)
pounds_forecast <- predict(arimapounds, n.ahead = 1)
oil_forecast <- predict(arimaoil, n.ahead = 1)
gold_forecast <- predict(arimagold, n.ahead = 1)
metal_forecast <- predict(arimametal, n.ahead = 1)

# accumulating the forecasts
preds <- c(AEP = aep_forecast$pred[1], D = d_forecast$pred[1],
           DUK = duk_forecast$pred[1], TMUS = tmus_forecast$pred[1],
           VZ = vz_forecast$pred[1], AMT = amt_forecast$pred[1],
           OIL = oil_forecast$pred[1], CCI = cci_forecast$pred[1],
           YEN = yen_forecast$pred[1], EURO = euro_forecast$pred[1],
           POUNDS = pounds_forecast$pred[1], GOLD = gold_forecast$pred[1],
           METAL = metal_forecast$pred[1])
preds

ESgarch <- function(y, p = 0.95){
  gfit <- garchFit(~garch(1,1), data = y, cond.dist = "std", trace = F)
  sigma <- predict(gfit, n.ahead = 1)[3]
  df <- coef(gfit)['shape']
  ES <- sigma*(dt(qt(p,df),df)/(1-p))*((df+(qt(p,df))^2)/(df-1))
  return(ES)
}

es<- c(AEP = ESgarch(losses$AEP)[1], D = ESgarch(losses$D)[1],
       DUK = ESgarch(losses$DUK)[1], TMUS = ESgarch(losses$TMUS)[1],
       VZ = ESgarch(losses$AEP)[1], AMT = ESgarch(losses$AMT)[1],
       CCI = ESgarch(losses$CCI)[1], YEN = ESgarch(losses$Yen)[1],
       EURO = ESgarch(losses$Euro)[1], POUNDS = ESgarch(losses$Pounds)[1],
       GOLD = ESgarch(losses$Gold)[1], OIL = ESgarch(losses$Oil)[1],
       METAL = ESgarch(losses$Metals)[1])
unlist(es)

# portfolio expected shortfall
es_pgmv <- ESgarch(na.omit(gmvp), p = 0.95) # PGMV
es_pgmv
es_mdp <- ESgarch(na.omit(mdp), p = 0.95) # MDP
es_mdp
unlist(c(PGMV = es_pgmv, MDP = es_mdp))

# Copula model
gfitgoy <- lapply(losses, garchFit, formula=~arma(0,0) + garch(1,1), 
                  cond.dist = "std", trace = F)
gprog <- unlist(lapply(gfitgoy, function(x) predict(x, n.ahead = 1)[3]))
gshape <- unlist(lapply(gfitgoy, function(x) x@fit$coef[5]))
gresid <- as.matrix(data.frame(lapply(gfitgoy,function(x) x@residuals / sqrt(x@h.t))))
head(gresid)

U <- sapply(1:13, function(y) pt(gresid[, y], df = gshape[y]))
head(U)

cop <- fit.tcopula(Udata = U, method = "Kendall")
cop$P

set.seed(1)
rcop <- rcopula.t(100000, df = cop$nu, Sigma = cop$P)

qcop <- sapply(1:13, function(x) qstd(rcop[, x], nu = gshape[x]))
head(qcop)

ht.mat <- matrix(gprog, nrow = 100000, ncol = ncol(losses), byrow = TRUE)
head(ht.mat)

Weights(pgmv_weights2)
pfall <- (qcop * ht.mat) %*% gmvw
head(pfall)
tail(pfall)

pfall.es95 <- median(tail(sort(pfall), 5000))
pfall.es95
ESgarch(na.omit(gmvp), p = 0.95)
