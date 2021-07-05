setwd("D:/")
library(quantmod) 
library(fBasics)
library(sn)
library(PerformanceAnalytics)
library(car)
library(tseries)
library(forecast)
library(fGarch)

##The relations among GDP USA, Canada, and UK

getSymbols("CLVMNACSCAB1GQUK",src="FRED", from="1980-01-01",to="2011-06-30")
gdpuk <- CLVMNACSCAB1GQUK
gdpuk1 <- gdpuk[c(22:147),]
dim(gdpuk1)
tail(gdpuk1)

getSymbols("NAEXKP01CAQ189S",src="FRED", from="1980-01-01",to="2011-06-30")
#Data Adjustment
gdpca <- (NAEXKP01CAQ189S[,1]/1000000)
gdpca1 <- gdpca[c(78:203),]
dim(gdpca1)
tail(gdpca1)

getSymbols("GDPC1",src="FRED", from="1980-01-01",to="2011-06-30")
gdpusa <- (GDPC1[,1]*1000)
gdpusa1 <- gdpusa[c(134:259),]
dim(gdpusa1)
tail(gdpusa1)

GDP=cbind(as.numeric(gdpuk1), as.numeric(gdpca1),as.numeric(gdpusa1))
dim(GDP)
colnames(GDP) <- c("UK","CA","USA")

require(MTS)
#Question 3
#QUestion 3.1 
GDPA <- GDP
GDPA = log(GDP,base=exp(10))
zt=diffM(GDPA)*100
colnames(zt) <- c("UK","CA","USA")
MTSplot(zt)


VARorder(zt)
#BIC suggests that we should use lag-1, AIC suggests lag-2
m1 = VAR(zt,p=1)
MTSdiag(m1)
varfit=VAR(zt,p=1)
summary(varfit)
m2 = refVAR(m1,thres = 1.645)
#ModelChecking
MTSdiag(m2)


#Question 3.2

#Impulse response function of the fitted model
detach("package:MTS")
require(vars)
VARselect(zt,lag.max=15)
#SC suggests 1-lag model the same as BIC
m3=VAR(zt,p=1)
summary(m3)
impls=irf(m3)
plot(impls)
#Interpretation


#Question 3.3
#Forecast error variance decomposition
fevd(m3,n.ahead=1)
fevd(m3,n.ahead=6)
#Interpretation

#Question 3.4
#Co-integration vector
fit1 <- lm(gdpuk1 ~ gdpusa1 + gdpca1)
summary(fit1)

fit2 <- lm(gdpusa1 ~ gdpuk1 + gdpca1)
summary(fit2)

fit3 <- lm(gdpca1 ~ gdpusa1 + gdpuk1)
summary(fit3)
#Question 3.5
#ECM-VAR model (aka. VECM)

test1 <- ca.jo(zt, type='trace',ecdet='const', K = 2)
summary(test1)
test2 <- ca.jo(zt, type='eigen',ecdet='const', K = 2)
summary(test2)
require("urca")
#model1 <- VECM(zt, 2, r = 1, estim = ('2OLS'))
#(model1)

#diagnostic test
#Need to transform VECM to VAR
m1var <- vec2var(test1, r=1)

#Serial Correlation

serial1 <- serial.test(m1var, lags.pt =5, type= "PT.asymptotic")
serial1

#Arch effects

arch1 <- arch.test(m1var, lags.multi =15, multivariate.only=TRUE)
arch1
# No arch effects H0 is not rejected
norm1 <- normality.test(m1var,multivariate.only=TRUE)
norm1
# H0 is rejected, Not normal dist.

# Impulse function

m3irf <- irf(m1var)
plot(m3irf)

#Variance decomposition
fevdm1var <- fevd(m1var, n.ahead=10)
plot(fevdm1var)
