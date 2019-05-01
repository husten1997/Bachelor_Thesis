start <- 1990.25
envi <- ORCL
mod = "MAN"
P.a <- 0.9
P.b <- 0.01
B.a <- 0.5
B.b <- 0.21

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

envi$Ratio.PB$sig_trend3 <- 0
envi$Ratio.PB$sig_trend4 <- 0
envi$Ratio.PB$sig_trend5 <- 0

for(i in c(4:(l-5))){
  data <- data.table(P = window(envi$P.Data$P, end = c(start + (i+4)*0.25)))
  data$B <- window(envi$B.Data$BPS_E, end = c(start + (i+4)*0.25))
  P.model <- ets(window(envi$P.Data$P, end = c(start + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
  B.model <- ets(window(envi$B.Data$BPS_E, end = c(start + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
  PB.forecast <- (forecast(P.model)$mean / forecast(B.model)$mean)
  data$PB <- window(envi$Ratio.PB$PB, end = c(start + (i+4)*0.25))
  data$PB[c((i+1):(i+5))] <- PB.forecast[c(1:5)]
  data$PB <- ts(data$PB, start = c(start, 1), frequency = 4)
  
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
  
  result$fit_trend1[i] <- if(result$trend1[i] < 0.1) result$fit_trend1[i] else NA
  result$fit_trend2[i] <- if(result$trend2[i] < 0.1) result$fit_trend2[i] else NA
  result$fit_trend3[i] <- if(result$trend3[i] < 0.1) result$fit_trend3[i] else NA
  envi$Ratio.PB$sig_trend3[i]<- if(result$trend3[i] < 0.1) 1 else 0
  result$fit_trend4[i] <- if(result$trend4[i] < 0.1) result$fit_trend4[i] else NA
  envi$Ratio.PB$sig_trend4[i]<- if(result$trend4[i] < 0.1) 1 else 0
  result$fit_trend5[i] <- if(result$trend5[i] < 0.1) result$fit_trend5[i] else NA
  envi$Ratio.PB$sig_trend5[i]<- if(result$trend5[i] < 0.1) 1 else 0
  result$fit_trend6[i] <- if(result$trend6[i] < 0.1) result$fit_trend6[i] else NA
  
}

result$Intercept <- ts(result$Intercept, start = start, frequency = 4)
result$trend1 <- ts(result$trend1, start = start, frequency = 4)
result$trend2 <- ts(result$trend2, start = start, frequency = 4)
result$trend3 <- ts(result$trend3, start = start, frequency = 4)
result$trend4 <- ts(result$trend4, start = start, frequency = 4)
result$trend5 <- ts(result$trend5, start = start, frequency = 4)
result$trend6 <- ts(result$trend6, start = start, frequency = 4)

result$fit_trend1 <- ts(result$fit_trend1, start = start, frequency = 4)
result$fit_trend2 <- ts(result$fit_trend2, start = start, frequency = 4)
result$fit_trend3 <- ts(result$fit_trend3, start = start, frequency = 4)
result$fit_trend4 <- ts(result$fit_trend4, start = start, frequency = 4)
result$fit_trend5 <- ts(result$fit_trend5, start = start, frequency = 4)
result$fit_trend6 <- ts(result$fit_trend6, start = start, frequency = 4)