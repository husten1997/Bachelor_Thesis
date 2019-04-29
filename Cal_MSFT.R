



#Dichotomes Merkmal für die Blase-----------------------
MSFT$Ratio.PB$redAlert <- 0
MSFT$Ratio.PB$redAlert[c(29:41)] <- 1

MSFT$Ratio.PB$redAlert <- ts(MSFT$Ratio.PB$redAlert, start = c(1990, 1), frequency = 4)

plot(MSFT$Ratio.PB$PB)
points(MSFT$Ratio.PB$PB, col = as.character(factor(MSFT$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

plot(MSFT$P.Data$P)
points(MSFT$P.Data$P, col = as.character(factor(MSFT$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

#WIP mit I(PB)--------------------------------------
MSFT$Ratio.PB$d.PB.MA <- ts(c(rep(NA, 7), rollmean(MSFT$Ratio.PB$d.PB, k = 8)), start = c(1990, 1), frequency = 4) 

plot(MSFT$Ratio.PB$d.PB)
lines(MSFT$Ratio.PB$d2.PB, col = c("orange"))
lines(MSFT$Ratio.PB$d4.PB, col = c("red"))
#lines(ts(c(rep(NA, 7), rollmean(MSFT$Ratio.PB$d.PB, k = 8)), start = c(1990, 1), frequency = 4), col = c("orange"))
#lines(ts(c(rep(NA, 11), rollmean(MSFT$Ratio.PB$d.PB, k = 12)), start = c(1990, 1), frequency = 4), col = c("red"))
#lines(ts(c(rep(NA, 15), rollmean(MSFT$Ratio.PB$d.PB, k = 16)), start = c(1990, 1), frequency = 4), col = c("green"))
abline(h = 0)
plot.dens(MSFT$Ratio.PB$d.PB, title = c("Density"), plot.norm = TRUE, plot.lines = FALSE)

#PB mit sd schätzer über die Zeit
par(mfrow = c(2, 1))
plot(window(MSFT$P.Data$P, end = c(2001, 1)))
plot(window(MSFT$Ratio.PB$PB, end = c(2001, 1)))
lines(window(ts(MSFT$Ratio.PB$PB[1] + sqrt(9.144297* c(1:length(MSFT$Ratio.PB$PB))), start = c(1990, 1)), col = c("orange")), end = c(2001, 1))
lines(window(ts(MSFT$Ratio.PB$PB[1] + sqrt(9.144297 * 2 *  c(1:length(MSFT$Ratio.PB$PB))), start = c(1990, 1)), col = c("red")), end = c(2001, 1))
lines(window(ts(MSFT$Ratio.PB$PB[1] - sqrt(9.144297* c(1:length(MSFT$Ratio.PB$PB))), start = c(1990, 1)), col = c("orange")), end = c(2001, 1))
lines(window(ts(MSFT$Ratio.PB$PB[1] - sqrt(9.144297 * 2 *  c(1:length(MSFT$Ratio.PB$PB))), start = c(1990, 1)), col = c("red")), end = c(2001, 1))
par(mfrow = c(1, 1))


#PB Forecast

(P.model <- ets(window(MSFT$P.Data$P, end = c(1999, 2)), opt.crit = "mae"))
P.model <- ets(window(MSFT$P.Data$P, end = c(1999, 2)), model = "MAN", alpha = 0.999, beta = 0.2508)
plot(forecast(P.model))

(B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(1999, 2)), opt.crit = "mae"))
B.model <- ets(window(MSFT$B.Data$BPS_E, end =  c(1999, 2)), model = "MAN", alpha = 0.7465, beta = 0.7012)
plot(forecast(B.model))


PB.forecast <- ts((forecast(P.model)$mean / forecast(B.model)$mean), start = c(2000, 2), frequency = 4)
plot(MSFT$Ratio.PB$PB)
lines(PB.forecast, col = c("red"))



P.a <- 0.785
P.b <- 0.2945
B.a <- 0.3893
B.b <- 0.3893
mod <- "MAN"
MSFT$Ratio.PB$d.PB.forecast <- NA
MSFT$P.Data$P.forecast <- NA
MSFT$B.Data$B.forecast <- NA
for(i in c(4:115)){
  P.model <- ets(window(MSFT$P.Data$P, end = c(1990 + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
  B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
  PB.forecast <- (forecast(P.model)$mean / forecast(B.model)$mean)
  MSFT$Ratio.PB$d.PB.forecast[i] <- PB.forecast[5] - PB.forecast[1]
  MSFT$P.Data$P.forecast[i+1] <- forecast(P.model)$mean[1]
  MSFT$B.Data$B.forecast[i+1] <- forecast(B.model)$mean[1]
}

MSFT$P.Data$P.forecast <- ts(MSFT$P.Data$P.forecast, start = c(1990, 1), frequency = 4)
MSFT$B.Data$B.forecast <- ts(MSFT$B.Data$B.forecast, start = c(1990, 1), frequency = 4)
MSFT$Ratio.PB$PB.forecast <- MSFT$P.Data$P.forecast / MSFT$B.Data$B.forecast

plot(MSFT$P.Data$P)
lines(MSFT$P.Data$P.forecast, col = c("orange"))
#lines(ts(MSFT$P.Data$P - MSFT$P.Data$P.forecast, start = c(1990, 1), frequency = 4), col = c("red"))
lines(c(NA, test[,1]), col = c("green"))

plot(MSFT$Ratio.PB$PB)
lines(MSFT$Ratio.PB$PB.forecast, col = c("orange"))

result <- data.table(matrix(NA, nrow = 116, ncol = 7))
colnames(result) <- c("time", "Intercept", "trend1", "trend2", "trend3", "trend4", "trend5")
result$time <- seq(from = 1990, length.out =  116, by = 0.25)

result.fitted <- data.table(matrix(NA, nrow = 116, ncol = 4))
colnames(result.fitted) <- c("trend2", "trend3", "trend4", "trend5")
result.fitted$trend1 <- MSFT$Ratio.PB$PB
result.fitted$trend2 <- MSFT$Ratio.PB$PB
result.fitted$trend3 <- MSFT$Ratio.PB$PB
result.fitted$trend4 <- MSFT$Ratio.PB$PB
result.fitted$trend5 <- MSFT$Ratio.PB$PB
result.fitted$trend6 <- MSFT$Ratio.PB$PB

for(i in c(4:111)){
  data <- data.table(P = window(MSFT$P.Data$P, end = c(1990 + (i+4)*0.25)))
  data$B <- window(MSFT$B.Data$BPS_E, end = c(1990 + (i+4)*0.25))
  P.model <- ets(window(MSFT$P.Data$P, end = c(1990 + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
  B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
  PB.forecast <- (forecast(P.model)$mean / forecast(B.model)$mean)
  data$PB <- window(MSFT$Ratio.PB$PB, end = c(1990 + (i+4)*0.25))
  data$PB[c((i+1):(i+5))] <- PB.forecast[c(1:5)]
  data$PB <- ts(data$PB, start = c(1990, 1), frequency = 4)
  
  Poly.model1 <- tslm(PB ~ I(trend), data = data)
  Poly.model2 <- tslm(PB ~ I(trend) + I(trend^2), data = data)
  Poly.model3 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3), data = data)
  Poly.model4 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4), data = data)
  Poly.model5 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5), data = data)
  Poly.model6 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5)+ I(trend^6), data = data)
  
  result$Intercept[i] <- summary(Poly.model3)$coefficients[1,4]
  result$trend1[i] <- summary(Poly.model1)$coefficients[2,4]
  result$trend2[i] <- summary(Poly.model2)$coefficients[3,4]
  result$trend3[i] <- summary(Poly.model3)$coefficients[4,4]
  result$trend4[i] <- summary(Poly.model4)$coefficients[5,4]
  result$trend5[i] <- summary(Poly.model5)$coefficients[6,4]
  result$trend6[i] <- summary(Poly.model6)$coefficients[7,4]
  if(i < 50){
    plot(MSFT$Ratio.PB$PB, main = c(1990 + (i-1)*0.25))
    lines(ts(Poly.model4$fitted.values, start = c(1990, 1), frequency = 4), col = c("red"))
  }
  
  result.fitted$trend2[i] <- if(result$trend2[i] < 0.1) result.fitted$trend2[i] else NA
  result.fitted$trend3[i] <- if(result$trend3[i] < 0.1) result.fitted$trend3[i] else NA
  result.fitted$trend4[i] <- if(result$trend4[i] < 0.1) result.fitted$trend4[i] else NA
  result.fitted$trend5[i] <- if(result$trend5[i] < 0.1) result.fitted$trend5[i] else NA
  result.fitted$trend6[i] <- if(result$trend6[i] < 0.1) result.fitted$trend6[i] else NA
  
  
}

result$Intercept <- ts(result$Intercept, start = c(1990, 1), frequency = 4)
result$trend1 <- ts(result$trend1, start = c(1990, 1), frequency = 4)
result$trend2 <- ts(result$trend2, start = c(1990, 1), frequency = 4)
result$trend3 <- ts(result$trend3, start = c(1990, 1), frequency = 4)
result$trend4 <- ts(result$trend4, start = c(1990, 1), frequency = 4)
result$trend5 <- ts(result$trend5, start = c(1990, 1), frequency = 4)
result$trend6 <- ts(result$trend6, start = c(1990, 1), frequency = 4)

plot(result$Intercept, ylim = c(0, 0.2))
lines(result$trend1, col = c("red"))
lines(result$trend2, col = c("green"))
lines(result$trend3, col = c("orange"))
#lines(result$trend4, col = c("yellow"))
#lines(result$trend5, col = c("blue"))
abline(v = c(1990:2020), col = c("grey"))

x11()
par(mfrow = c(3, 2))
plot(MSFT$Ratio.PB$PB, col = c("grey"), ylim = c(0, 50), main = c("Trend 1"))
lines(result.fitted$trend1, col = c("red"))
plot(MSFT$Ratio.PB$PB, col = c("grey"), ylim = c(0, 50), main = c("Trend 2"))
lines(result.fitted$trend2, col = c("red"))
plot(MSFT$Ratio.PB$PB, col = c("grey"), ylim = c(0, 50), main = c("Trend 3"))
lines(result.fitted$trend3, col = c("red"))
plot(MSFT$Ratio.PB$PB, col = c("grey"), ylim = c(0, 50), main = c("Trend 4"))
lines(result.fitted$trend4, col = c("red"))
plot(MSFT$Ratio.PB$PB, col = c("grey"), ylim = c(0, 50), main = c("Trend 5"))
lines(result.fitted$trend5, col = c("red"))
plot(MSFT$Ratio.PB$PB, col = c("grey"), ylim = c(0, 50), main = c("Trend 6"))
lines(result.fitted$trend6, col = c("red"))
par(mfrow = c(1, 1))


View(result)
MSFT$Ratio.PB$nd.PB <- 0
MSFT$Ratio.PB$nd.PB[MSFT$Ratio.PB$d.PB.forecast > 0 & !is.na(MSFT$Ratio.PB$d.PB.forecast)] <- as.factor(1)

plot(as.numeric(MSFT$Ratio.PB$PB), col = as.character(factor(MSFT$Ratio.PB$nd.PB, labels = c("red", "green"))), type = c("p"))
lines(as.numeric(MSFT$Ratio.PB$PB))

plot(as.numeric(MSFT$P.Data$P), col = as.character(factor(MSFT$Ratio.PB$nd.PB, labels = c("red", "green"))), type = c("p"))
lines(as.numeric(MSFT$P.Data$P))

fore <- function(x, h){
  model <- ets(x, model = "MAN", alpha = 0.785, beta = 0.2945)
  forecast(model, h = h)
}
test <- tsCV(MSFT$P.Data$P, fore, h = 2)
plot(test)

MSFT$Ratio.PB$MA.PB <- ts(c(rep(NA, 15), rollmean(MSFT$Ratio.PB$PB, k = 16)), start = c(1990, 1), frequency = 4)
MSFT$Ratio.PB$GapP.B.MA <- (MSFT$Ratio.PB$PB - MSFT$Ratio.PB$MA.PB)/MSFT$Ratio.PB$MA.PB
plot(MSFT$Ratio.PB$PB, ylim = c(-5, 50))
lines(MSFT$Ratio.PB$MA.PB, col = c("red"))

lines((MSFT$Ratio.PB$PB - MSFT$Ratio.PB$MA.PB)*10/MSFT$Ratio.PB$MA.PB, col = c("blue"))
abline(h = 0)

plot.dens(MSFT$Ratio.PB$GapP.B.MA, title = c("bla"), plot.norm = TRUE, plot.lines = TRUE)

