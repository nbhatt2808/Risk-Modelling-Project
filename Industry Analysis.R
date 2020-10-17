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
options(scipen = 999)

industry_df <- read.csv('industry_data.csv')
head(industry_df)

date <- as.character(industry_df$Date)
industry_ts <- timeSeries(industry_df[,-1], charvec = date)
head(industry_ts)

indexes <- interpNA(industry_ts[,-1], method = "linear")
head(indexes)

returns <- returnseries(indexes, method = "discrete", trim = T)
head(returns)

covar <- cov(returns)
covar

gmvw <- Weights(PGMV(returns)) # Global Minimum Variance Portfolio
mdpw <- Weights(PMD(returns)) # Most Diversified Portfolio
mtdw <- Weights(PMTD(returns)) # Minimum Tail Dependent Portfolio
ercw <- Weights(PERC(covar)) # Equal risk contributed portfolio

# combining results
w <- cbind(gmvw, mdpw, mtdw, ercw)
w

# Marginal Risk Contributions
mar_risk_cont <- apply(w, 2, mrc, Sigma = covar)
rownames(mar_risk_cont) <- colnames(indexes)
colnames(mar_risk_cont) <- c("GMV", "MDP", "MTD", "ERC")
mar_risk_cont

# plot of allocations
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
dotchart(gmvw, xlim = c(0, 60), main = "Global Minimum Variance \n Allocation", pch = 19)
dotchart(mtdw, xlim = c(0, 60), main = "Minimum Tail Dependent \n Allocation", pch = 19)
dotchart(mdpw, xlim = c(0, 60), main = "Maximum Diversified Portfolio \n Allocation", pch = 19)
dotchart(ercw, xlim = c(0, 60), main = "Equal Risk Contribution \n Allocation", pch = 19)

# comparison of allocations
oldpar <- par(no.readonly = TRUE)
dotchart(mdpw - gmvw, xlim = c(-20, 20), main = "MDP vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
dotchart(mtdw - gmvw, xlim = c(-20, 20), main = "MTD vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
dotchart(ercw - gmvw, xlim = c(-20, 20), main = "ERC vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")

# comparing allocations between portfolios
sector <- factor(rep(rownames(mar_risk_cont), 4),
                 levels = sort(rownames(mar_risk_cont)))
port <- factor(rep(colnames(mar_risk_cont), each = 7),
               levels = colnames(mar_risk_cont))
mrcdf <- data.frame(MRC = c(mar_risk_cont), port, sector)
mrcdf

dotchart(mdpw - gmvw, xlim = c(-40, 40), main = "MDP vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
dotchart(mtdw - gmvw, xlim = c(-40, 40), main = "MTD vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
dotchart(ercw - gmvw, xlim = c(-40, 40), main = "ERC vs. GMV",
         pch = 19)
abline(v = 0, col = "grey")
par(oldpar)

dotplot(sector ~ MRC | port, groups = port, data = mrcdf,
        xlab = "Percentages",
        main = "MR Contributions by Sector per Portfolio",
        col = "black", pch = 19)
dotplot(port ~ MRC | sector, groups = sector, data = mrcdf,
        xlab = "Percentages",
        main = "MR Contributions by Portfolio per Sector",
        col = "black", pch = 19)

# converting returns to decimals
returns_dec <- returns / 100
head(returns_dec)

# computing portfolio returns for each portfolio
pret <- apply(w, 2, function(x) returns_dec %*% x / 100)
head(pret)

# standard deviation
std_dev <- apply(pret, 2, sd) * 100
std_dev

# Expected Shortfall at 95% level
es95 <- apply(pret, 2, function(x)
  abs(ES(R = x, method = "modified") * 100))
es95

# Diversification Ratio
dr <- apply(w, 2, dr, Sigma = covar)

# Concentration Ratio
cr <- apply(w, 2, cr, Sigma = covar)
cr

# Expected Returns
exret <- apply(pret, 2, function(x) mean(x)*100)

# combining the results
res <- rbind(std_dev, es95, dr, cr, exret)
res

# extracting all other columns than the date columns
industry_sub <- industry_df[,2:ncol(industry_df)]
head(industry_sub)

# converting to time series
date_ind <- as.character(industry_df$Date)
industry_sub_ts <- timeSeries(industry_sub, charvec = date_ind)
head(industry_sub_ts)

# creating data for markets and assets
train_rows <- round(0.80 * nrow(industry_sub_ts), 0)
test_rows <- round(0.20 * nrow(industry_sub_ts), 0)
train_rows + test_rows
rm <-  returnseries(industry_sub_ts[1:train_rows,1], trim = T)
head(rm)
ra <- returnseries(industry_sub_ts[1:train_rows,2:ncol(industry_sub_ts)], trim = T)
head(ra)

# beta - co-movement with the market
beta <- apply(ra, 2, function(x) cov(x, rm) / var(rm))
beta

# tau - kendall rank correlation
tau <- apply(ra, 2, function(x) cor(x, rm, method = "kendall"))
tau

# copula parameter estimates
theta <- copClayton@iTau(tau)
theta

# lambda- estimates of interdependence between each stock and the SP500 at the
# lower tail
lambda <- copClayton@lambdaL(theta)
lambda

# selecting the betas below median betas
idxBeta <- beta < median(beta)
idxBeta[idxBeta]
beta[idxBeta]

# inverse log weighted scaled portfolio weights
wBeta <- -1 * log(abs(beta[idxBeta]))
wBeta <- wBeta / sum(wBeta) * 100
wBeta

# selecting the lambdas below median lambda
idxTD <- lambda < median(lambda)
idxTD[idxTD]
beta[idxTD]

# inverse log weighted scaled portfolio weights
wTD <- -1 * log(lambda[idxTD])
wTD <- wTD / sum(wTD) * 100
wTD

# testing portfolio with these weights on out of sample data
rmo <-  returnseries(industry_sub_ts[train_rows:nrow(industry_sub_ts),1],
                     method = "discrete", percentage = F) + 1
head(rmo)
rao <- returnseries(industry_sub_ts[train_rows:nrow(industry_sub_ts),
                                    2:ncol(industry_sub_ts)], 
                    method = "discrete", percentage = F) + 1
head(rao)
nrow(rm) + nrow(rmo)

# set the first observation to 1 for rmo
rmo[1] <- 100
head(rmo)

# computing the cumulative performance of the index
rmequity <- cumprod(rmo)
rmequity

# picking columns from rao that were chosen by the low beta portfolio
lbequity <- rao[, idxBeta]
head(lbequity)

# assigning the beta vector as the first row of lbequity
lbequity[1, ] <- wBeta
head(lbequity)

# cumulative performance of the low beta portfolio in the out of sample 
# period
lbequity <- rowSums(apply(lbequity, 2, cumprod))
head(lbequity)

# picking columns from rao that were chosen by the low tail dependence portfolio
tdequity <- rao[, idxTD]
head(tdequity)

# assigning the beta vector as the first row of tdequity
tdequity[1, ] <- wTD
head(tdequity)

# cumulative performance of the low tail dependent portfolio in the out of sample 
# period
tdequity <- rowSums(apply(tdequity, 2, cumprod))
head(tdequity)

# combining market performance, low beta portfolio performance and
# low tail dependent performance
y <- cbind(rmequity, lbequity, tdequity)
summary(y)

# time series plots for out of sample periods
par(mfrow = c(1, 1))
plot(rmequity, type = "l", ylim = range(y), ylab = "SP500",
     xlab = "Out-of-Sample Periods")
lines(lbequity, lty = 2, col = "red")
lines(tdequity, lty = 3, col = "blue")
legend("topleft",
       legend = c("SP500", "Low Beta", "Lower Tail Dep."),
       lty = 1:3, 
       cex = 0.70, col = c("black", "red", "blue"))

# creating a barplot of relative performance of SP500, LOw Beta Portfolio
# and Low Tail Dependency Portfolio
relout <- rbind((lbequity / rmequity - 1) * 100,
                (tdequity / rmequity - 1) * 100)
head(relout)
relout <- relout[, -1]

plot(relout[1,], ylab = '', type = "l", lty = 2, col = "red", 
     main = "Relative Performance to SP500")
lines(relout[2,], lty = 2, col = "blue")
abline(h = 0)
legend("topleft",
       legend = c("Low Beta", "Lower Tail Dep"),
       lty = 1:3, 
       cex = 0.70, col = c("red", "blue"))

