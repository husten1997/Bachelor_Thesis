#MSFT------------------------------------
adf.test(MSFT$B.Data$BPS_E)
kpss.test(MSFT$B.Data$BPS_E)

adf.test(MSFT$P.Data$P)
kpss.test(MSFT$P.Data$P)

adf.test(MSFT$Ratio.PB$PB)
kpss.test(MSFT$Ratio.PB$PB)


#PB ARIMA
dat <- window(MSFT$Ratio.PB$PB, start = c(1990), end = c(2018, 3))
model <- auto.arima(dat)
mean((MSFT$Ratio.PB$PB - model$fitted)^2, na.rm = TRUE)

#PB ETS
model <- ets(dat, model = "MAN", damped = FALSE)
mean((MSFT$Ratio.PB$PB - model$fitted)^2, na.rm = TRUE)

#PB mit P und B forecast
P.a <-0.999
P.b <- 0.051
B.a <- 0.584
B.b <- 0.157
l <- length(MSFT$Ratio.PB$PB)

PB.forecast <- rep(NA, l)
for(i in c(4:(l-5))){
  P.model <- ets(window(MSFT$P.Data$P, end = c(1990 + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
  B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
  PB.forecast[i+1] <- (forecast(P.model)$mean / forecast(B.model)$mean)[1]
  #e[i] <- (MSFT$Ratio.PB$PB[i+1] - PB.forecast[1])
}
e <- (MSFT$Ratio.PB$PB - PB.forecast)
mean(e^2, na.rm = TRUE)

#AAPL----------------------------------
adf.test(AAPL$B.Data$BPS_E)
kpss.test(AAPL$B.Data$BPS_E)

adf.test(AAPL$P.Data$P)
kpss.test(AAPL$P.Data$P)

adf.test(AAPL$Ratio.PB$PB)
kpss.test(AAPL$Ratio.PB$PB)

#PB ARIMA
dat <- window(AAPL$Ratio.PB$PB, start = c(1990), end = c(2018, 3))
model <- auto.arima(dat)
mean((AAPL$Ratio.PB$PB - model$fitted)^2, na.rm = TRUE)

#PB ETS
model <- ets(dat, model = "MAN", damped = FALSE)
mean((AAPL$Ratio.PB$PB - model$fitted)^2, na.rm = TRUE)

#PB mit P und B forecast
P.a <-0.905
P.b <- 0.091
B.a <- 0.261 #glaube hier ist das Problem, dass B.a kleiner ist als B.b
B.b <- 0.386
l <- length(AAPL$Ratio.PB$PB)

PB.forecast <- rep(NA, l)
for(i in c(16:(l-5))){
  P.model <- ets(window(AAPL$P.Data$P, end = c(1990 + (i-1)*0.25)), model = "MAN", alpha = P.a, beta = P.b, damped = FALSE)
  B.model <- ets(window(AAPL$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), model = "MAN", alpha = B.a, beta = B.b, damped = FALSE)
  PB.forecast[i+1] <- (forecast(P.model)$mean / forecast(B.model)$mean)[1]
  #e[i] <- (AAPL$Ratio.PB$PB[i+1] - PB.forecast[1])
}
e <- (AAPL$Ratio.PB$PB - PB.forecast)
mean(e^2, na.rm = TRUE)

#ORCL----------------------------------
adf.test(ORCL$B.Data$BPS_E)
kpss.test(ORCL$B.Data$BPS_E)

adf.test(ORCL$P.Data$P)
kpss.test(ORCL$P.Data$P)

adf.test(ORCL$Ratio.PB$PB)
kpss.test(ORCL$Ratio.PB$PB)

#PB ARIMA
dat <- window(ORCL$Ratio.PB$PB, start = c(1990), end = c(2018, 3))
model <- auto.arima(dat)
mean((ORCL$Ratio.PB$PB - model$fitted)^2, na.rm = TRUE)

#PB ETS
model <- ets(dat, model = "MAN", damped = FALSE)
mean((ORCL$Ratio.PB$PB - model$fitted)^2, na.rm = TRUE)

#PB mit P und B forecast
P.a <-0.9
P.b <- 0.01
B.a <- 0.5
B.b <- 0.31
l <- length(ORCL$Ratio.PB$PB)

PB.forecast <- rep(NA, l)
for(i in c(4:(l-5))){
  P.model <- ets(window(ORCL$P.Data$P, end = c(1990 + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
  B.model <- ets(window(ORCL$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
  PB.forecast[i+1] <- (forecast(P.model)$mean / forecast(B.model)$mean)[1]
  #e[i] <- (ORCL$Ratio.PB$PB[i+1] - PB.forecast[1])
}
e <- (ORCL$Ratio.PB$PB - PB.forecast)
mean(e^2, na.rm = TRUE)

#IBM---------------------------------
adf.test(IBM$B.Data$BPS_E)
kpss.test(IBM$B.Data$BPS_E)

adf.test(IBM$P.Data$P)
kpss.test(IBM$P.Data$P)

adf.test(IBM$Ratio.PB$PB)
kpss.test(IBM$Ratio.PB$PB)

#PB ARIMA
dat <- window(IBM$Ratio.PB$PB, start = c(1990), end = c(2018, 3))
model <- auto.arima(dat)
mean((IBM$Ratio.PB$PB - model$fitted)^2, na.rm = TRUE)

#PB ETS
model <- ets(dat, model = "MAN", damped = FALSE)
mean((IBM$Ratio.PB$PB - model$fitted)^2, na.rm = TRUE)

#PB mit P und B forecast
P.a <-0.999
P.b <- 0.051
B.a <- 0.584
B.b <- 0.157
l <- length(IBM$Ratio.PB$PB)

PB.forecast <- rep(NA, l)
for(i in c(4:(l-5))){
  P.model <- ets(window(IBM$P.Data$P, end = c(1990 + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
  B.model <- ets(window(IBM$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
  PB.forecast[i+1] <- (forecast(P.model)$mean / forecast(B.model)$mean)[1]
  #e[i] <- (IBM$Ratio.PB$PB[i+1] - PB.forecast[1])
}
e <- (IBM$Ratio.PB$PB - PB.forecast)
mean(e^2, na.rm = TRUE)
