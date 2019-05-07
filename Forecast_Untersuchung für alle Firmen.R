#MSFT------------------------------------
adf.test(MSFT$B.Data$BPS_E)
kpss.test(MSFT$B.Data$BPS_E)

adf.test(MSFT$P.Data$P)
kpss.test(MSFT$P.Data$P)

adf.test(MSFT$Ratio.PB$PB)
kpss.test(MSFT$Ratio.PB$PB)

dat <- window(MSFT$Ratio.PB$PB, start = c(1990), end = c(2000, 1))
model <- auto.arima(dat)
plot(forecast(model))

#AAPL----------------------------------
adf.test(AAPL$B.Data$BPS_E)
kpss.test(AAPL$B.Data$BPS_E)

adf.test(AAPL$P.Data$P)
kpss.test(AAPL$P.Data$P)

adf.test(AAPL$Ratio.PB$PB)
kpss.test(AAPL$Ratio.PB$PB)

dat <- window(ORCL$Ratio.PB$PB, start = c(1990, 2), end = c(2008))
model <- auto.arima(dat)
plot(forecast(model))

#ORCL----------------------------------
adf.test(ORCL$B.Data$BPS_E)
kpss.test(ORCL$B.Data$BPS_E)

adf.test(ORCL$P.Data$P)
kpss.test(ORCL$P.Data$P)

adf.test(ORCL$Ratio.PB$PB)
kpss.test(ORCL$Ratio.PB$PB)

#IBM---------------------------------
adf.test(IBM$B.Data$BPS_E)
kpss.test(IBM$B.Data$BPS_E)

adf.test(IBM$P.Data$P)
kpss.test(IBM$P.Data$P)

adf.test(IBM$Ratio.PB$PB)
kpss.test(IBM$Ratio.PB$PB)
