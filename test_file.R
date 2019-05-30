start <- 1990
envi <- MSFT
mod = "MAN"
P.a <- 0.999
P.b <- 0.051
B.a <- 0.584
B.b <- 0.157
wind <- 16
sta <- 1990
sig <- 0.4

l <- length(envi$Ratio.PB$PB)
result <- data.table(matrix(NA, nrow = l, ncol = 8))
colnames(result) <- c("time", "Intercept", "trend1", "trend2", "trend3", "trend4", "trend5", "trend6")
result$time <- seq(to = 2018.75, length.out =  l, by = 0.25)

result$fit_trend1 <- envi$Ratio.PB$PB
result$fit_trend2 <- envi$Ratio.PB$PB
result$fit_trend3 <- envi$Ratio.PB$PB
result$fit_trend4 <- envi$Ratio.PB$PB
result$fit_trend5 <- envi$Ratio.PB$PB
result$fit_trend6 <- envi$Ratio.PB$PB

result$Intercept <- NA
result$trend1 <- NA
result$trend2 <- NA
result$trend3 <- NA
result$trend4 <- NA
result$trend5 <- NA
result$trend6 <- NA

result$est.Intercept <- NA
result$est.trend1 <- NA
result$est.trend2 <- NA
result$est.trend3 <- NA
result$est.trend4 <- NA
result$est.trend5 <- NA
result$est.trend6 <- NA

envi$Ratio.PB$sig_trend1 <- 0
envi$Ratio.PB$sig_trend2 <- 0
envi$Ratio.PB$sig_trend3 <- 0
envi$Ratio.PB$sig_trend4 <- 0
envi$Ratio.PB$sig_trend5 <- 0

result$p_val <- NA

result$BP <- NA

par(mfrow = c(1, 2))

for(i in c(wind:(l-5))){
  #del# data <- data.table(P = window(envi$P.Data$P, end = c(sta + (i+4)*0.25)))
  #del# data$B <- window(envi$B.Data$BPS_E, end = c(sta + (i+4)*0.25))
  cur.period <- c(sta + i*0.25)
  P.model <- ets(window(envi$P.Data$P, end = c(sta + (i)*0.25)), model = mod, alpha = P.a, beta = P.b)
  B.model <- ets(window(envi$B.Data$BPS_E, end = c(sta + (i)*0.25)), model = mod, alpha = B.a, beta = B.b)
  PB.forecast <- (forecast(P.model)$mean / forecast(B.model)$mean)
  data_PB <- data.table(PB = window(envi$Ratio.PB$PB, start = c(sta + (i-wind + 1)*0.25), end = c(sta + (i+4)*0.25)))
  data_PB$PB[c((length(data_PB$PB)-3):(length(data_PB$PB)))] <- PB.forecast[c(1:4)]
  data_PB$PB <- ts(data_PB$PB, start = c(sta + (i-wind + 1)*0.25), frequency = 4)
  
  Poly.model1 <- tslm(PB ~ I(trend), data = data_PB)
  Poly.model2 <- tslm(PB ~ I(trend) + I(trend^2), data = data_PB)
  Poly.model3 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3), data = data_PB)
  Poly.model4 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4), data = data_PB)
  Poly.model5 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5), data = data_PB)
  Poly.model6 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5)+ I(trend^6), data = data_PB)
  
  s <- sd(data_PB$PB)
  TS <- (result$est.trend1[i] - sig) / (s / sqrt(wind + 4))
  p_val <- pt(TS, summary(Poly.model1)$df[2], lower = TRUE)
  
  result$Intercept[i] <- summary(Poly.model3)$coefficients[1,4]
  result$trend1[i] <- summary(Poly.model1)$coefficients[2,4]
  #result$trend1[i] <- p_val
  result$trend2[i] <- summary(Poly.model2)$coefficients[3,4]
  result$trend3[i] <- summary(Poly.model3)$coefficients[4,4]
  result$trend4[i] <- summary(Poly.model4)$coefficients[5,4]
  result$trend5[i] <- summary(Poly.model5)$coefficients[6,4]
  result$trend6[i] <- summary(Poly.model6)$coefficients[7,4]
  
  result$est.Intercept[i] <- summary(Poly.model3)$coefficients[1,1]
  result$est.trend1[i] <- summary(Poly.model1)$coefficients[2,1]
  result$est.trend2[i] <- summary(Poly.model2)$coefficients[3,1]
  result$est.trend3[i] <- summary(Poly.model3)$coefficients[4,1]
  result$est.trend4[i] <- summary(Poly.model4)$coefficients[5,1]
  result$est.trend5[i] <- summary(Poly.model5)$coefficients[6,1]
  result$est.trend6[i] <- summary(Poly.model6)$coefficients[7,1]
  
  r <- sig
  R <- c(0, 1)
  linearHypothesis(model = Poly.model1, hypothesis.matrix = R, rhs = r)
  trend.sig <- linearHypothesis(model = Poly.model1, hypothesis.matrix = R, rhs = r)$'Pr(>F)'[2]
  
  pt(summary(Poly.model1)$coefficients[2,3], summary(Poly.model1)$df[2], lower=FALSE)
  
  sd.b <- summary(Poly.model1)$coefficients[2,1]/summary(Poly.model1)$coefficients[2,3]
  TS <- (result$est.trend1[i] - sig) / sd.b
  #p_val <- pt(TS, summary(Poly.model1)$df[2], lower = FALSE)
  #coeftest()
  result$p_val[i] <- pt(TS, summary(Poly.model1)$df[2], lower = FALSE)
  
  
  #acf(Poly.model1$residuals)
  #pacf(Poly.model1$residuals)
  
  result$fit_trend1[i] <- if(result$trend1[i] < 0.1 & result$p_val[i] < 0.05) result$fit_trend1[i] else NA
  envi$Ratio.PB$sig_trend1[i]<- if(result$trend1[i] < 0.1 & result$p_val[i] < 0.05) 1 else 0
  #result$fit_trend1[i] <- if(result$trend1[i] < 0.1 & Poly.model1$coefficients[2] > 0.4) result$fit_trend1[i] else NA
  #envi$Ratio.PB$sig_trend1[i]<- if(result$trend1[i] < 0.1 & Poly.model1$coefficients[2] > 0.4) 1 else 0
  result$fit_trend2[i] <- if(result$trend2[i] < 0.1 & Poly.model2$coefficients[3] > 0) result$fit_trend2[i] else NA
  envi$Ratio.PB$sig_trend2[i]<- if(result$trend2[i] < 0.1 & Poly.model2$coefficients[3] > 0) 1 else 0
  result$fit_trend3[i] <- if(result$trend3[i] < 0.1 & Poly.model3$coefficients[4] > 0) result$fit_trend3[i] else NA
  envi$Ratio.PB$sig_trend3[i]<- if(result$trend3[i] < 0.1 & Poly.model3$coefficients[4] > 0) 1 else 0
  result$fit_trend4[i] <- if(result$trend4[i] < 0.1 & Poly.model4$coefficients[5] > 0) result$fit_trend4[i] else NA
  envi$Ratio.PB$sig_trend4[i]<- if(result$trend4[i] < 0.1 & Poly.model4$coefficients[5] > 0) 1 else 0
  result$fit_trend5[i] <- if(result$trend5[i] < 0.1 & Poly.model5$coefficients[6] > 0) result$fit_trend5[i] else NA
  envi$Ratio.PB$sig_trend5[i]<- if(result$trend5[i] < 0.1 & Poly.model5$coefficients[6] > 0) 1 else 0
  result$fit_trend6[i] <- if(result$trend6[i] < 0.1 & Poly.model6$coefficients[7] > 0) result$fit_trend6[i] else NA
}


par(mfrow = c(1, 1))

result$Intercept <- ts(result$Intercept, start = sta, frequency = 4)
result$trend1 <- ts(result$trend1, start = sta, frequency = 4)
result$trend2 <- ts(result$trend2, start = sta, frequency = 4)
result$trend3 <- ts(result$trend3, start = sta, frequency = 4)
result$trend4 <- ts(result$trend4, start = sta, frequency = 4)
result$trend5 <- ts(result$trend5, start = sta, frequency = 4)
result$trend6 <- ts(result$trend6, start = sta, frequency = 4)

result$fit_trend1 <- ts(result$fit_trend1, start = sta, frequency = 4)
result$fit_trend2 <- ts(result$fit_trend2, start = sta, frequency = 4)
result$fit_trend3 <- ts(result$fit_trend3, start = sta, frequency = 4)
result$fit_trend4 <- ts(result$fit_trend4, start = sta, frequency = 4)
result$fit_trend5 <- ts(result$fit_trend5, start = sta, frequency = 4)
result$fit_trend6 <- ts(result$fit_trend6, start = sta, frequency = 4)

#plot--------------------------------------------------------------------------
x11()
par(mfrow = c(3, 2), lwd = 3)
plot(envi$Ratio.PB$PB, col = c("grey"), ylim = range(envi$Ratio.PB$PB), main = c("Trend 1"))
lines(result$fit_trend1, col = c("red"))
plot(envi$Ratio.PB$PB, col = c("grey"), ylim = range(envi$Ratio.PB$PB), main = c("Trend 2"))
lines(result$fit_trend2, col = c("red"))
plot(envi$Ratio.PB$PB, col = c("grey"), ylim = range(envi$Ratio.PB$PB), main = c("Trend 3"))
lines(result$fit_trend3, col = c("red"))
plot(envi$Ratio.PB$PB, col = c("grey"), ylim = range(envi$Ratio.PB$PB), main = c("Trend 4"))
lines(result$fit_trend4, col = c("red"))
plot(envi$Ratio.PB$PB, col = c("grey"), ylim = range(envi$Ratio.PB$PB), main = c("Trend 5"))
lines(result$fit_trend5, col = c("red"))
plot(envi$Ratio.PB$PB, col = c("grey"), ylim = range(envi$Ratio.PB$PB), main = c("Trend 6"))
lines(result$fit_trend6, col = c("red"))
par(mfrow = c(1, 1))

#

print(prop.table(ftable(envi$Ratio.PB$sig_trend1, envi$Ratio.PB$redAlert)))
print(sum(prop.table(ftable(envi$Ratio.PB$sig_trend1, envi$Ratio.PB$redAlert))[c(1,4)]))

View(result)