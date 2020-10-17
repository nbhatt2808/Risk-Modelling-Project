# setting the working directory
setwd('C:/Users/bneer/OneDrive/Desktop/Harrisburg-Risk Modelling/Final Project')

# getting the required packages
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
options(scipen = 999)

# getting the data for four asset classes: abbots, bonds, commodities, currencies

# abbots: Comparing multiple industries

# Healthcare companies

# 1. abbot Laboratories (Health Care Equipment)
abbot <- as.xts(data.frame(getSymbols('ABT', src = 'yahoo', 
                                      from = '2008-01-01', to = '2020-10-06', 
                                      auto.assign = F)))
abbot_df <- as.data.frame(abbot)
abbot_df <- cbind(Date = index(abbot), data.frame(abbot_df, row.names = NULL))
head(abbot_df)
tail(abbot_df)
plot.xts(abbot$ABT.Adjusted)

# Market_cap
abbot_cap <- read.csv('Pharma companies/ABT_data.csv')
abbot_cap$Period <- as.Date(abbot_cap$Period, format = "%Y-%m-%d %H:%M:%S")
head(abbot_cap,2)
abbot_cap <- abbot_cap %>%
              map_df(rev)
colnames(abbot_cap)[1] <- "Date" 
abbot_cap[,2] <- abbot_cap[,2]/1000

head(abbot_cap,2)
abbot_df$Date <- as.Date(abbot_df$Date)
abbot <- inner_join(abbot_df, abbot_cap, by = "Date")
abbot <- abbot[,c(1,7,8)]
head(abbot,2)

#--------------------------------------------------------------------------------
from = "2008-01-01"
to = "2020-10-06"

get_data <- function(ticker, path, from, to, df_xts = F){
  stock_xts <- as.xts(data.frame(getSymbols(ticker, src = 'yahoo', 
                                            from = from, to = to, 
                                            auto.assign = F)))
  stock_df <- as.data.frame(stock_xts)
  stock_df <- cbind(Date = index(stock_xts), data.frame(stock_df, row.names = NULL))
  stock_df$Date <- as.Date(stock_df$Date)
  stock_cap <- read.csv(paste(path, '/', ticker,"_data.csv", sep = ""))
  stock_cap$Period <- as.Date(stock_cap$Period)
  stock_cap <- stock_cap %>%
    map_df(rev)
  colnames(stock_cap)[1] <- "Date" 
  #stock_cap[,2] <- stock_cap[,2]/1000
  stock <- inner_join(stock_df, stock_cap, by = "Date")
  stock <- stock[,c(1,7,8)]
  stock_ts <- timeSeries(stock[,2:ncol(stock)], charvec = as.character(stock$Date))
  
  if(df_xts){
    return(stock_xts)
  }else{
    return(stock_ts)
  }
}

pharma_path <- 'Pharma companies'

# Pharma Industries
# 1) Abbott Laboratories
abbot_ts <- get_data("ABT", path = pharma_path, from = from, to = to)
head(abbot_ts,2)

# 2) Johnson & Johnson
jnj_ts <- get_data("JNJ", path = pharma_path, from = from, to = to)
head(jnj_ts, 2)

# 3) Pfizer
pfizer_ts <- get_data("PFE", path = pharma_path, from = from, to = to)
head(pfizer_ts, 2)

# 4) Merck & Co
merck_ts <- get_data("MRK", path = pharma_path, from = from, to = to)
head(merck_ts, 2)

# 5) Gilead Sciences
gilead_ts <- get_data("GILD", path = pharma_path, from = from, to = to)
head(gilead_ts, 2)

# 6) GlaxoSmithKline
gsk_ts <- get_data("GSK", path = pharma_path, from = from, to = to)
head(gsk_ts, 2)

# 7) Amgen
amgen_ts <- get_data("AMGN", path = pharma_path, from = from, to = to)
head(amgen_ts, 2)

# 8) Eli Lily
lily_ts <- get_data("LLY", path = pharma_path, from = from, to = to)
head(lily_ts, 2)

# 9) Bristol-Myers Squibb
myers_ts <- get_data("BMY", path = pharma_path, from = from, to = to)
head(myers_ts, 2)

# 10) Biogen
biogen_ts <- get_data("BIIB", path = pharma_path, from = from, to = to)
head(biogen_ts, 2)

# Gathering market caps
date = as.character(abbot_df$Date)
pharma_market <- data.frame(abbot_cap = abbot_ts$Abbott.Laboratories.Market.Cap,
                            jnj_cap = jnj_ts$Johnson...Johnson.Market.Cap,
                            pfizer_cap = pfizer_ts$Pfizer.Inc.Market.Cap,
                            merck_cap = merck_ts$Merck...Co.Inc.Market.Cap,
                            gilead_cap = gilead_ts$Gilead.Sciences.Inc.Market.Cap,
                            gsk_cap = gsk_ts$GlaxoSmithKline.PLC.Market.Cap,
                            amgen_cap = amgen_ts$Amgen.Inc.Market.Cap, 
                            lily_cap = lily_ts$Eli.Lilly.and.Co.Market.Cap,
                            myers_cap = myers_ts$Bristol.Myers.Squibb.Co.Market.Cap,
                            biogen_cap = biogen_ts$Biogen.Inc.Market.Cap)
dim(pharma_market)
head(pharma_market)

pharma_market$total <- rowSums(pharma_market)
head(pharma_market)

# lets keep the divisor by using the first day in such a way that we get a value 
# of 1000. 
divisor <- pharma_market_ts$total[1]/1000
divisor

pharma_market$PHAR <- round(pharma_market$total/divisor, 2)
head(pharma_market)

pharma_market_ts <- timeSeries(pharma_market, charvec = date)
head(pharma_market_ts)

# writing a function to compute the industry indices for any industry
get_index <- function(industry_abbrev, vector_ticker, path, from, to){
  stock_xts <- as.xts(data.frame(getSymbols(vector_ticker[1], src = 'yahoo', 
                                            from = from, to = to, 
                                            auto.assign = F)))
  stock_df <- as.data.frame(stock_xts)
  stock_df <- cbind(Date = index(stock_xts), data.frame(stock_df, row.names = NULL))
  stock_df$Date <- as.Date(stock_df$Date) 
  date <- as.character(stock_df$Date)
  ind_cap <- data.frame(matrix(0, nrow = length(date), 
                               ncol = length(vector_ticker)))
  for(i in 1:length(vector_ticker)){
        ind_market <- get_data(vector_ticker[i], path = path, from = from, to = to)
        ind_cap[,i] <- ind_market[,2]
  }
  names(ind_cap) <- paste0(vector_ticker, "_cap")
  ind_cap$total <- rowSums(ind_cap)
  divisor <- ind_cap$total[1]/1000
  ind_cap[,industry_abbrev] <- round(ind_cap$total/divisor, 2)
  return(ind_cap)
}

#-------------------------------------------------------------------------------
# Phara Industry
pharma_path <- 'Pharma companies'
pharma_index <- get_index("PHAR", c("ABT", "JNJ", "PFE", "MRK", 
                                    "GILD", "GSK", "AMGN", "LLY", 
                                    "BMY", "BIIB"), pharma_path, from, to)
head(pharma_index,3)
colnames(pharma_index)

# perfectly consistent

#-----------------------------------------------------------------------------
# Electric Utility industry
# Companies: 
# 1) American Electric Power (AEP)
# 2) Consolidated Edison (ED)
# 3) Dominion Energy Inc (D)
# 4) Duke Energy (DUK)
# 5) Edison International (EIX)
# 6) Entergy Corp (ETR)
# 7) Southern Company (SO)
# 8) FirstEnergy Corp (FE)
# 9) PPL Corp (PPL)
# 10) Public Service Enterprise Group (PEG)
utility_path <- "Utility companies"
utility_index <- get_index("UTIL", c("AEP", "ED", "D", "DUK",
                                     "EIX", "ETR", "SO",
                                     "FE", "PPL", "PEG"), utility_path, from, to)

head(utility_index,3)
colnames(utility_index)

#-------------------------------------------------------------------------------
# Movies & Entertainment industry
# Companies: 
# 1) Live Nation ENtertainment (LYV)
# 2) Netflix (NFLX)
# 3) The Walt Disney Company (DIS)
movies_path <- "Movies companies"
movies_index <- get_index("ENMT", c("LYV", "NFLX", "DIS"), 
                           movies_path, from, to)

head(movies_index,3)
colnames(movies_index)

#--------------------------------------------------------------------------------
# Telecom industry
# Companies: 
# 1) AT&t (T)
# 2) T-Mobile US Inc. (TMUS)
# 3) verizon (VZ)
# 4) Comcast Corp (CMCSA)
telecom_path <- "Telecom companies"
telecom_index <- get_index("TELE", c("T", "TMUS", "VZ", "CMCSA"), 
                           telecom_path, from, to)

head(telecom_index,3)
colnames(telecom_index)

#-------------------------------------------------------------------------------
# Investment Banking & Brokerage
# Companies: 
# 1) Charles Scwab (SCHW)
# 2) Goldman Sachs (GS)
# 3) Morgan Stanley (MS)
# 4) Raymond James Financial (RJF)
banking_path <- "Finance companies"
banking_index <- get_index("BANK", c("SCHW", "GS", "MS", "RJF"), 
                           banking_path, from, to)

head(banking_index,3)
colnames(banking_index)

#-------------------------------------------------------------------------------
# Tech industry with the tech giants
# Companies: 
# 1) Apple (AAPL)
# 2) Microsoft (MSFT)
# 3) Amazon (AMZN)
# 4) Google (GOOGL)
# 5) Intel (INTC)
# 6) IBM Corporation (IBM)
# 7) Oracle (ORCL)
# 8) Adobe (ADBE)
tech_path <- "Tech companies"
tech_index <- get_index("TECH", c("AAPL", "MSFT", "AMZN", "GOOGL",
                                  "INTC", "IBM", "ORCL", "ADBE"), 
                        tech_path, from, to)

head(tech_index,3)
colnames(tech_index)

#-------------------------------------------------------------------------------
# Real Estate industry mainly REITS Specialized Reits
# Companies:  
# 1) SBA Communications (SBAC)
# 2) American Tower Corp (AMT) 
# 3) Extra Space Storage (EXR) 
# 4) Crown Castle International Corp (CCI) 
# 5) Digital Reality Trust Inc (DLR) 
# 6) Equinix Inc (EQIX) 
# 7) Public Storage (PSA)
real_path <- "Real estate companies"
real_index <- get_index("REAL", c("SBAC", "AMT", "EXR", "CCI",
                                  "DLR", "EQIX", "PSA"), 
                        real_path, from, to)

head(real_index,3)
colnames(real_index)

#-------------------------------------------------------------------------------
# getting the SP500 market index
sp500_xts <- as.xts(data.frame(getSymbols("^GSPC", src = 'yahoo', 
                                      from = from, to = to, 
                                      auto.assign = F)))

sp500_df <- as.data.frame(sp500_xts)
sp500_df <- cbind(Date = index(sp500_xts), data.frame(sp500_df, row.names = NULL))
sp500_df$Date <- as.Date(sp500_df$Date)
head(sp500_df,2)

divisor_sp500 <- sp500_df$GSPC.Adjusted[1]/1000
sp500_df$SPI <- round(sp500_df$GSPC.Adjusted/divisor_sp500, 2)
head(sp500_df, 3)

#-------------------------------------------------------------------------------
# industry data
industry_df <- data.frame(SPI = sp500_df$SPI, PHAR = pharma_index$PHAR,
                          UTIL = utility_index$UTIL, ENMT = movies_index$ENMT,
                          TELE = telecom_index$TELE, BANK = banking_index$BANK,
                          TECH = tech_index$TECH, REAL = real_index$REAL)

industry_ts <- timeSeries(industry_df, charvec = date)
head(SPISECTOR,3)
head(industry_ts, 3)

industry_df2 <- data.frame(Date = date, 
                           SPI = sp500_df$SPI, PHAR = pharma_index$PHAR,
                           UTIL = utility_index$UTIL, ENMT = movies_index$ENMT,
                           TELE = telecom_index$TELE, BANK = banking_index$BANK,
                           TECH = tech_index$TECH, REAL = real_index$REAL)

#write.csv(industry_df2, file = "industry_data.csv", row.names = F)
