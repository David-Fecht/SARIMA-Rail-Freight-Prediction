#SARIMA-Rail-Freight-Prediction

rm(list = ls( ))

library(astsa)

dat <- read.table("FRED_Rail_Freight_Carloads.txt", header=TRUE)
freight <- ts(dat$RAILFRTCARLOADS, frequency=12, start=2000)

x <- as.numeric(freight)

# Plot of the data
dev.new(width=8, height=6)
tsplot(freight, ylab="Carloads", xlab="Year", type="l", main="Rail Freight Carloads")

# ACF and PACF of x_t
dev.new(width=8, height=6)
acf2(x, max.lag=72, main="Estimated ACF & PACF of Rail Freight Carload data")

# Plot of (1-B^12)*x_t 
dev.new(width=8, height=6)
tsplot(diff(freight, lag=12, differences=1), ylab=expression((1-B^12)*x[t]), xlab="Year", type="o", main=expression(paste("Plot of ", (1-B^12)*x[t])))

# Plot of (1-B)(1-B^12)*x_t 
dev.new(width=8, height=6)
tsplot(diff(diff(freight, lag=12, differences=1)), ylab=expression((1-B)(1-B^12)*x[t]), xlab="Year", type="l", main=expression(paste("Plot of ", (1-B)(1-B^12)*x[t])))

# ACF and PACF of (1-B)(1-B^12)*x_t
dev.new(width=8, height=6)
acf2(diff(diff(x, lag=12, differences=1)), max.lag=72, main=expression(paste("Est. ACF & PACF for ", (1-B)(1-B^12)*x[t], " data")))


# Fit models and examine diagnostics
source("examine.mod.R")

#model: p = 0, q = 0, P = 0, Q = 1
dev.new(width=8, height=6)
mod.fit.010.011 <- sarima(freight, p=0, d=1, q=0, P=0, D=1, Q=1, S=12)
mod.fit.010.011
examine.mod(mod.fit.010.011, 0,1,0, 0,1,1, 12, lag.max=36)
#decent model/decent diagnostics

#model: p = 0, q = 1, P = 0, Q = 1
dev.new(width=8, height=6)
mod.fit.011.011 <- sarima(freight, p=0, d=1, q=1, P=0, D=1, Q=1, S=12)
mod.fit.011.011
examine.mod(mod.fit.011.011, 0,1,1, 0,1,1, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 1, q = 0, P = 0, Q = 1
dev.new(width=8, height=6)
mod.fit.110.011 <- sarima(freight, p=1, d=1, q=0, P=0, D=1, Q=1, S=12)
mod.fit.110.011
examine.mod(mod.fit.110.011, 1,1,0, 0,1,1, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 1, q = 1, P = 0, Q = 1
dev.new(width=8, height=6)
mod.fit.111.011 <- sarima(freight, p=1, d=1, q=1, P=0, D=1, Q=1, S=12)
mod.fit.111.011
examine.mod(mod.fit.111.011, 1,1,1, 0,1,1, 12, lag.max=36)
#decent model/skewed normality

#model: p = 1, q = 2, P = 0, Q = 1
dev.new(width=8, height=6)
mod.fit.112.011 <- sarima(freight, p=1, d=1, q=2, P=0, D=1, Q=1, S=12)
mod.fit.112.011
examine.mod(mod.fit.112.011, 1,1,2, 0,1,1, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 1, q = 3, P = 0, Q = 1
dev.new(width=8, height=6)
mod.fit.113.011 <- sarima(freight, p=1, d=1, q=3, P=0, D=1, Q=1, S=12)
mod.fit.113.011
examine.mod(mod.fit.113.011, 1,1,3, 0,1,1, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 2, q = 1, P = 0, Q = 1
dev.new(width=8, height=6)
mod.fit.211.011 <- sarima(freight, p=2, d=1, q=1, P=0, D=1, Q=1, S=12)
mod.fit.211.011
examine.mod(mod.fit.211.011, 2,1,1, 0,1,1, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 3, q = 1, P = 0, Q = 1
dev.new(width=8, height=6)
mod.fit.311.011 <- sarima(freight, p=3, d=1, q=1, P=0, D=1, Q=1, S=12)
mod.fit.311.011
examine.mod(mod.fit.311.011, 3,1,1, 0,1,1, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 4, q = 1, P = 0, Q = 1
dev.new(width=8, height=6)
mod.fit.411.011 <- sarima(freight, p=4, d=1, q=1, P=0, D=1, Q=1, S=12)
mod.fit.411.011
examine.mod(mod.fit.411.011, 4,1,1, 0,1,1, 12, lag.max=36)
#decent model/decent diagnostics

#model: p = 1, q = 0, P = 1, Q = 0
dev.new(width=8, height=6)
mod.fit.110.110 <- sarima(freight, p=1, d=1, q=0, P=1, D=1, Q=0, S=12)
mod.fit.110.110
examine.mod(mod.fit.110.110, 1,1,0, 1,1,0, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 0, q = 1, P = 1, Q = 0
dev.new(width=8, height=6)
mod.fit.011.110 <- sarima(freight, p=0, d=1, q=1, P=1, D=1, Q=0, S=12)
mod.fit.011.110
examine.mod(mod.fit.110.110, 0,1,1, 1,1,0, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 1, q = 1, P = 1, Q = 1
dev.new(width=8, height=6)
mod.fit.111.111 <- sarima(freight, p=1, d=1, q=1, P=1, D=1, Q=1, S=12)
mod.fit.111.111
examine.mod(mod.fit.111.111, 1,1,1, 1,1,1, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 1, q = 1, P = 1, Q = 0
dev.new(width=8, height=6)
mod.fit.111.110 <- sarima(freight, p=1, d=1, q=1, P=1, D=1, Q=0, S=12)
mod.fit.111.110
examine.mod(mod.fit.111.110, 1,1,1, 1,1,0, 12, lag.max=36)
#weak model/poor parameter estimates

#model: p = 1, q = 1, P = 2, Q = 0
dev.new(width=8, height=6)
mod.fit.111.210 <- sarima(freight, p=1, d=1, q=1, P=2, D=1, Q=0, S=12)
mod.fit.111.210
examine.mod(mod.fit.111.210, 1,1,1, 2,1,0, 12, lag.max=36)
#decent model/decent diagnostics

#model: p = 1, q = 1, P = 3, Q = 0
dev.new(width=8, height=6)
mod.fit.111.310 <- sarima(freight, p=1, d=1, q=1, P=3, D=1, Q=0, S=12)
mod.fit.111.310
examine.mod(mod.fit.111.310, 1,1,1, 3,1,0, 12, lag.max=36)
#decent model/decent diagnostics

#model: p = 1, q = 1, P = 4, Q = 0
dev.new(width=8, height=6)
mod.fit.111.410 <- sarima(freight, p=1, d=1, q=1, P=4, D=1, Q=0, S=12)
mod.fit.111.410
examine.mod(mod.fit.111.410, 1,1,1, 4,1,0, 12, lag.max=36)
#weak model/poor diagnostics

#model: p = 0, q = 1, P = 3, Q = 0
dev.new(width=8, height=6)
mod.fit.011.310 <- sarima(freight, p=0, d=1, q=1, P=3, D=1, Q=0, S=12)
mod.fit.011.310
examine.mod(mod.fit.011.310, 0,1,1, 3,1,0, 12, lag.max=36)
#weak model/poor diagnostics

#model: p = 1, q = 1, P = 3, Q = 1
dev.new(width=8, height=6)
mod.fit.111.311 <- sarima(freight, p=1, d=1, q=1, P=3, D=1, Q=1, S=12)
mod.fit.111.311
examine.mod(mod.fit.111.311, 1,1,1, 3,1,1, 12, lag.max=36)
#decent model/decent diagnostics

#model: p = 1, q = 1, P = 4, Q = 1
dev.new(width=8, height=6)
mod.fit.111.411 <- sarima(freight, p=1, d=1, q=1, P=4, D=1, Q=1, S=12)
mod.fit.111.411
examine.mod(mod.fit.111.411, 1,1,1, 4,1,1, 12, lag.max=36)
#weak model/poor diagnostics


data.frame(mod.name=c("ARIMA(0,1,0)x(0,1,1)_12","ARIMA(1,1,1)x(0,1,1)_12","ARIMA(4,1,1)x(0,1,1)_12","ARIMA(1,1,1)x(2,1,0)_12","ARIMA(1,1,1)x(3,1,0)_12","ARIMA(1,1,1)x(3,1,1)_12"), AIC=c(mod.fit.010.011$AIC, mod.fit.111.011$AIC, mod.fit.411.011$AIC, mod.fit.111.210$AIC, mod.fit.111.310$AIC, mod.fit.111.311$AIC), AICc=c(mod.fit.010.011$AICc, mod.fit.111.011$AICc, mod.fit.411.011$AICc,  mod.fit.111.210$AICc, mod.fit.111.310$AICc, mod.fit.111.311$AICc), BIC=c(mod.fit.010.011$BIC, mod.fit.111.011$BIC, mod.fit.411.011$BIC, mod.fit.111.210$BIC, mod.fit.111.310$BIC, mod.fit.111.311$BIC))

# Forecasts 12 time periods into the future
dev.new(width=8, height=6)
fore.mod <- sarima.for(freight, n.ahead=12, p=1, d=1, q=1, P=0, D=1, Q=1, S=12, plot.all=FALSE, main ="ARIMA(1,1,1)x(0,1,1)_12" )
fore.mod

pred.mod <- freight - ts(mod.fit.111.011$fit$residuals, frequency=12, start=2000)

dev.new(width=8, height=6)
tsplot(freight, ylab="freight", xlab="Time", type="o", main="Rail Freight Carload 2000-2022")
lines(pred.mod, col="red", type="o", pch=17) 
legend("topright", legend=c("Observed", "Forecast"), lty=c("solid", "solid"), col=c("black", "red"), pch=c(1, 17), bty="n")
