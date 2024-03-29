plot.dens <- function(dataset, a = 2, title, color = c("black", "black", "black"), plot.norm = FALSE, plot.lines = TRUE){
  m <- mean(dataset, na.rm = TRUE)
  s <- sd(dataset, na.rm = TRUE)
  pp <- plot(density(dataset, na.rm = TRUE), main = title, col = color[1])
  if(plot.lines){
    pp <- abline(v = m + a*s, col = color[2])
    pp <- abline(v = m - a*s, col = color[3])
  }
  if(plot.norm){
    pp <- lines(density(rnorm(n = 1000000, mean = m, sd = s)), col = c("grey"))
  }
  invisible(pp)
}

plot.ext <- function(dataset, a = 2, title = NULL, color = c("black", "green", "orange")){
  m <- mean(dataset, na.rm = TRUE)
  s <- sd(dataset, na.rm = TRUE)
  ind_up <- which(dataset > (m + a * s))
  ind_down <- which(dataset < (m - a * s))
  plot_data <- data.table(a = dataset)
  plot_data$b <- dataset
  plot_data$c <- dataset
  plot_data$b[-ind_up] <- NA
  plot_data$c[-ind_down] <- NA
  
  
  plot(plot_data$a, col = color[1], type = c("l"), main = title)
  points(plot_data$b, col = color[2])
  points(plot_data$c, col = color[3])
}


import <- function(envi, P_start_date, BV_start_date, count_of_days){
  envi$BV$EPS_ttm <- rep(0)
  for(i in c((nrow(envi$BV)-3):1)){
    envi$BV$EPS_ttm[i] <- sum(envi$BV$NI[c((i+3):i)]) / envi$BV$Shares[i] 
  }
  
  #calculation of changes
  envi$BV$d.BPS_E <- c((ts(envi$BV$BPS_E) - lag(ts(envi$BV$BPS_E)))/lag(ts(envi$BV$BPS_E)),0)
  envi$BV$d.EPS_ttm <- c((ts(envi$BV$EPS_ttm) - lag(ts(envi$BV$EPS_ttm)))/lag(ts(envi$BV$EPS_ttm)),0)
  
  
  #generating and filling of the main xts Object (Data Frame)
  envi$Data <- data.frame(matrix(0, nrow = count_of_days, ncol = 13))
  names(envi$Data) <- c("Date", "PPS", "BV_E", "BV_T", "Shares", "BPS_E", "BPS_D", "BPS_T", "NI", "EPS", "EPS_ttm", "d.BPS_E", "d.EPS_ttm")
  envi$Data[,1] <- seq(as.Date(P_start_date),length=count_of_days ,by="days")
  
  
  #mapping bv and mv on the smae timeline
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$MV$Date) ,2] <- as.numeric(rev(envi$MV$Close))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,3] <- as.numeric(rev(envi$BV$BV_E))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,4] <- as.numeric(rev(envi$BV$BV_T))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,5] <- as.numeric(rev(envi$BV$Shares))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,6] <- as.numeric(rev(envi$BV$BPS_E))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,7] <- as.numeric(rev(envi$BV$BPS_D))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,8] <- as.numeric(rev(envi$BV$BPS_T))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,9] <- as.numeric(rev(envi$BV$NI))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,10] <- as.numeric(rev(envi$BV$EPS))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,11] <- as.numeric(rev(envi$BV$EPS_ttm))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,12] <- as.numeric(rev(envi$BV$d.BPS_E))
  envi$Data[as.character.Date(envi$Data[,1]) %in% as.character.Date(envi$BV$Date) ,13] <- as.numeric(rev(envi$BV$d.EPS_ttm))
  
  #canceling out artefacts from the calculation of the changes
  envi$Data$d.BPS_E[is.nan(envi$Data$d.BPS_E)] <- 0
  envi$Data$d.BPS_E[is.infinite(envi$Data$d.BPS_E)] <- 0
  
  envi$Data$d.EPS_ttm[is.nan(envi$Data$d.EPS_ttm)] <- 0
  envi$Data$d.EPS_ttm[is.infinite(envi$Data$d.EPS_ttm)] <- 0
  
  #forward values
  for(i in c(2:count_of_days)){
    if(envi$Data[i, 2] == 0) envi$Data[i, 2] <- envi$Data[i-1, 2]
    if(envi$Data[i, 9] == 0) envi$Data[i, 9] <- envi$Data[i-1, 9]
    if(envi$Data[i, 10] == 0) envi$Data[i, 10] <- envi$Data[i-1, 10]
    if(envi$Data[i, 11] == 0) envi$Data[i, 11] <- envi$Data[i-1, 11]
    if(envi$Data[i, 12] == 0) envi$Data[i, 12] <- envi$Data[i-1, 12]
    if(envi$Data[i, 13] == 0) envi$Data[i, 13] <- envi$Data[i-1, 13]
  }
  
  
  #Price------------------------------------------------------------------------
  envi$P_xts <- xts(envi$Data$PPS, order.by = envi$Data$Date)
  colnames(envi$P_xts) <- c("P")
  #envi$P_xts$MA <- xts(ma(envi$P_xts$P, 181), order.by = envi$Data$Date)
  #envi$P_xts$MA2 <- xts(ma(envi$P_xts$P, 913), order.by = envi$Data$Date)
  
  envi$P_xts$MA <- xts(c(rep(NA, 89), rollmeanr(envi$P_xts$P, 90)), order.by = envi$Data$Date)
  #envi$P_xts$MA <- xts(ma(envi$P_xts$P, 90), order.by = envi$Data$Date)
  envi$P_xts$d.P <- (envi$P_xts$P - lag(envi$P_xts$P, 1))/lag(envi$P_xts$P, 1)
  envi$P_xts$d.P[is.nan(envi$P_xts$d.P)] <- NA
  envi$P_xts$d.P[is.infinite(envi$P_xts$d.P)] <- NA
  envi$P_xts$d.P_MA <- (envi$P_xts$MA - lag(envi$P_xts$MA, 1))/lag(envi$P_xts$MA, 1)
  envi$P_xts$d.P_MA[is.nan(envi$P_xts$d.P_MA)] <- NA
  envi$P_xts$d.P_MA[is.infinite(envi$P_xts$d.P_MA)] <- NA
  
  #BV---------------------------------------------------------------------------
  envi$BV_xts <- xts(envi$Data$BPS_E, order.by = envi$Data$Date)
  colnames(envi$BV_xts) <- c("BPS_E")
  envi$BV_xts$BPS_E[envi$BV_xts$BPS_E == 0] <- NA
  envi$BV_xts$BPS_E[1] <- 0
  envi$BV_xts$BPS_E[BV_start_date] <- 0
  envi$BV_xts$BPS_E <- xts(na.interp(envi$BV_xts$BPS_E), order.by = envi$Data$Date)
  envi$BV_xts$BPS_E[BV_start_date] <- NA
  
  envi$BV_xts$d.BPS_E <- xts(envi$Data$d.BPS_E, order.by = envi$Data$Date)
  envi$BV_xts$d.EPS_ttm <- xts(envi$Data$d.EPS_ttm, order.by = envi$Data$Date)
  #envi$BV_xts$d.BPS_E <- (envi$BV_xts$BPS_E - lag(envi$BV_xts$BPS_E, 1))/lag(envi$BV_xts$BPS_E, 1)
  #envi$BV_xts$d.BPS_E[is.nan(envi$BV_xts$d.BPS_E)] <- NA
  #envi$BV_xts$d.BPS_E[is.na(envi$BV_xts$d.BPS_E)] <- 0
  #envi$BV_xts$d.BPS_E[is.infinite(envi$BV_xts$d.BPS_E)] <- NA
  
  envi$BV_xts$BPS_D <- xts(envi$Data$BPS_D, order.by = envi$Data$Date)
  envi$BV_xts$BPS_D[envi$BV_xts$BPS_D == 0] <- NA
  envi$BV_xts$BPS_D[1] <- 0
  envi$BV_xts$BPS_D[BV_start_date] <- 0
  envi$BV_xts$BPS_D <- xts(na.interp(envi$BV_xts$BPS_D), order.by = envi$Data$Date)
  
  
  #NI---------------------------------------------------------------------------
  envi$EPS_xts <- xts(envi$Data$EPS, order.by = envi$Data$Date)
  colnames(envi$EPS_xts) <- c("EPS")
  envi$EPS_xts$EPS[envi$EPS_xts$EPS <= 0] <- NA
  
  envi$EPS_xts$EPS_ttm <- xts(envi$Data$EPS_ttm, order.by = envi$Data$Date)
  envi$EPS_xts$EPS_ttm[envi$EPS_xts$EPS_ttm <= 0] <- NA
  
  #PB---------------------------------------------------------------------------
  #View(BV_xts)
  envi$Ratios.PB <- xts(envi$P_xts$MA / envi$BV_xts$BPS_E, order.by = envi$Data$Date)
  colnames(envi$Ratios.PB) <- c("PB")
  envi$Ratios.PB$PB[envi$Ratios.PB$PB == Inf] <- 0
  
  envi$Ratios.PB$MA <- xts(c(rep(NA, 539), rollmeanr(envi$Ratios.PB$PB, 540)), order.by = envi$Data$Date)
  #envi$Ratios.PB$MA2 <- xts(c(rep(NA, 99), rollmeanr(envi$Ratios.PB$PB, 100)), order.by = envi$Data$Date)
  
  #envi$Ratios.PB$MA <- xts(ma(envi$Ratios.PB$PB, 540), order.by = envi$Data$Date)
  #envi$Ratios.PB$MA <- xts(c(rep(NA, 539), rollmeanr(envi$Ratios.PB$P, 540)), order.by = envi$Data$Date)
  envi$Ratios.PB$MA2 <- xts(ma(envi$Ratios.PB$PB, 360), order.by = envi$Data$Date)
  envi$Ratios.PB$NivCleaned <- envi$Ratios.PB$PB - envi$Ratios.PB$MA
  envi$Ratios.PB$d.PB <- (envi$Ratios.PB$PB - lag(envi$Ratios.PB$PB, 1))/lag(envi$Ratios.PB$PB, 1)
  envi$Ratios.PB$d.PB[is.nan(envi$Ratios.PB$d.PB)] <- NA
  envi$Ratios.PB$d.PB[is.infinite(envi$Ratios.PB$d.PB)] <- NA
  
  #PE--------------------------------------------------------------------------
  envi$Ratios.PE <- xts((envi$P_xts$MA + envi$BV_xts$BPS_D)/envi$EPS_xts$EPS_ttm, order.by = envi$Data$Date)
  colnames(envi$Ratios.PE) <- c("PE")
  envi$Ratios.PE$PE[envi$Ratios.PE$PE == Inf] <- 0
  
  #envi$Ratios.PE$MA <- xts(c(rep(NA, 199), rollmeanr(envi$Ratios.PE$PE, 200)), order.by = envi$Data$Date)
  #envi$Ratios.PE$MA2 <- xts(c(rep(NA, 99), rollmeanr(envi$Ratios.PE$PE, 100)), order.by = envi$Data$Date)
  envi$Ratios.PE$MA <- xts(ma(envi$Ratios.PE$PE, 540), order.by = envi$Data$Date)
  envi$Ratios.PE$MA2 <- xts(ma(envi$Ratios.PE$PE, 360), order.by = envi$Data$Date)
  envi$Ratios.PE$NivCleaned <- envi$Ratios.PE$PE - envi$Ratios.PE$MA
  
  envi$Ratios.PE$d.PE <- (envi$Ratios.PE$PE - lag(envi$Ratios.PE$PE, 1))/lag(envi$Ratios.PE$PE, 1)
  envi$Ratios.PE$d.PE[is.nan(envi$Ratios.PE$d.PE)] <- NA
  envi$Ratios.PE$d.PE[is.infinite(envi$Ratios.PE$d.PE)] <- NA
}

p.overview <- function(envi){
  par(mfrow = c(2,2))
  plot(envi$P_xts, col = c("black", "red", "green"))
  plot(envi$BV_xts$BPS_E)
  plot(envi$Ratios.PB)
  plot(envi$Ratios.PE)
  
  plot(envi$P_xts$d.P)
  plot.dens(envi$P_xts$d.P_MA, a = 2, title = c("Density of change in Price"))
  
  plot.ext(envi$P_xts$d.P_MA, a = 2, title = c("Etrems of change in Prices"))
  par(mfrow = c(1,1))
}

get.extrm_ind <- function(dataset, a = 2){
  m <- mean(dataset, na.rm = TRUE)
  s <- sd(dataset, na.rm = TRUE)
  ind_up <- which(dataset > (m + a * s))
  ind_down <- which(dataset < (m - a * s))
  return(list(ind_up, ind_down))
}

import <- function(envi, start_d, end_d, start_p){
  #Price Data-------------------------------------------------------------------------------------
  envi$P.Data_pre <- data.table(aggregate(ts(as.numeric(tapply(as.numeric(rev(envi$MV$Close)), as.yearmon(rev(envi$MV$Date)), mean)), start = start_p, frequency = 12), nfrequency = 4))
  colnames(envi$P.Data_pre) <- c("P")
  
  envi$P.Data_pre$d.P <- ts(c(NA, (envi$P.Data_pre$P - lag(envi$P.Data_pre$P, -1))/lag(envi$P.Data_pre$P, -1)), end = end_d, frequency = 4)
  envi$P.Data_pre$d.P[is.nan(envi$P.Data_pre$d.P)] <- NA
  envi$P.Data_pre$d.P[is.infinite(envi$P.Data_pre$d.P)] <- NA
  
  envi$P.Data <- data.table(window(envi$P.Data_pre$P, start = start_d))
  colnames(envi$P.Data) <- c("P")
  envi$P.Data$d.P <- window(envi$P.Data_pre$d.P, start = start_d)
  envi$P.Data$nd.P <- 0
  envi$P.Data$nd.P[envi$P.Data$d.P > 0] <- as.factor(1)
  envi$P.Data$sma.P <- ma(envi$P.Data$P, 8)
  envi$P.Data$bma.P <- c(rep(NA, 7), rollmean(envi$P.Data$P, k = 8))
  
  #Fundamental Data-------------------------------------------------------------------------------
  #I should write them down into the B.Data table => more convenient
  envi$BV$EPS_ttm <- rep(0)
  for(i in c((nrow(envi$BV)-3):1)){
    envi$BV$EPS_ttm[i] <- sum(envi$BV$NI[c((i+3):i)]) / envi$BV$Shares[i] 
  }
  envi$BV$d.BPS_E <- c((ts(envi$BV$BPS_E) - lag(ts(envi$BV$BPS_E)))/lag(ts(envi$BV$BPS_E)),0)
  envi$BV$d.EPS_ttm <- c((ts(envi$BV$EPS_ttm) - lag(ts(envi$BV$EPS_ttm)))/lag(ts(envi$BV$EPS_ttm)),0)
  
  envi$B.Data <- data.table(ts(envi$BV$BV_E, start = start_d, frequency = 4))
  colnames(envi$B.Data) <- c("BV_E")
  #envi$B.Data$BV_D <- ts(rev(envi$BV$BV_D), start = c(1990, 1), frequency = 4)
  envi$B.Data$BV_T <- ts(rev(envi$BV$BV_T), start = start_d, frequency = 4)
  envi$B.Data$BPS_E <- ts(rev(envi$BV$BPS_E), start = start_d, frequency = 4)
  envi$B.Data$BPS_D <- ts(envi$BV$BPS_D, start = start_d, frequency = 4)
  envi$B.Data$BPS_T <- ts(rev(envi$BV$BPS_T), start = start_d, frequency = 4)
  envi$B.Data$d.BPS_E <- ts(rev(envi$BV$d.BPS_E), start = start_d, frequency = 4)
  
  #PB-----------------------------------------------------------------------------------------
  envi$Ratio.PB <- data.table(ts(envi$P.Data$P / envi$B.Data$BPS_E, start = start_d, frequency = 4))
  colnames(envi$Ratio.PB) <- c("PB")
  
  envi$Ratio.PB$d.PB <- ts(c(NA, envi$Ratio.PB$PB - lag(envi$Ratio.PB$PB, -1)), start = start_d, frequency = 4)
  envi$Ratio.PB$d.PB[is.nan(envi$Ratio.PB$d.PB)] <- NA
  envi$Ratio.PB$d.PB[is.infinite(envi$Ratio.PB$d.PB)] <- NA
  
  envi$Ratio.PB$d2.PB <- ts(c(rep(NA, 2), envi$Ratio.PB$PB - lag(envi$Ratio.PB$PB, -2)), start = start_d, frequency = 4)
  envi$Ratio.PB$d2.PB[is.nan(envi$Ratio.PB$d.PB)] <- NA
  envi$Ratio.PB$d2.PB[is.infinite(envi$Ratio.PB$d.PB)] <- NA
  
  envi$Ratio.PB$d4.PB <- ts(c(rep(NA, 4), envi$Ratio.PB$PB - lag(envi$Ratio.PB$PB, -4)), start = start_d, frequency = 4)
  envi$Ratio.PB$d4.PB[is.nan(envi$Ratio.PB$d.PB)] <- NA
  envi$Ratio.PB$d4.PB[is.infinite(envi$Ratio.PB$d.PB)] <- NA
  
  envi$Ratio.PB$d.PB.MA <- ts(c(rep(NA, 7), rollmean(envi$Ratio.PB$d.PB, k = 8)), start = start_d, frequency = 4)
  envi$Ratio.PB$sma.PB <- ma(envi$Ratio.PB$PB, 16)
  envi$Ratio.PB$bma.PB <- c(rep(NA, 15), rollmean(envi$Ratio.PB$PB, k = 16))
}

fite.model <- function(envi, mod = "MAN", P.a = 0.9, P.b = 0.11, B.a = 0.9, B.b = 0.11, sta = 1990, wind = 8, sig = 0.4){
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
  

  
  for(i in c(wind:(l-5))){
    data <- data.table(P = window(envi$P.Data$P, end = c(sta + (i+4)*0.25)))
    data$B <- window(envi$B.Data$BPS_E, end = c(sta + (i+4)*0.25))
    P.model <- ets(window(envi$P.Data$P, end = c(sta + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
    B.model <- ets(window(envi$B.Data$BPS_E, end = c(sta + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
    PB.forecast <- (forecast(P.model)$mean / forecast(B.model)$mean)
    data_PB <- data.table(PB = window(envi$Ratio.PB$PB, start = c(sta + (i-wind)*0.25) ,end = c(sta + (i+4)*0.25)))
    data_PB$PB[c((length(data_PB$PB)-4):(length(data_PB$PB)))] <- PB.forecast[c(1:5)]
    data_PB$PB <- ts(data_PB$PB, start = c(sta + (i-wind)*0.25), frequency = 4)
    
    Poly.model1 <- tslm(PB ~ I(trend), data = data_PB)
    Poly.model2 <- tslm(PB ~ I(trend) + I(trend^2), data = data_PB)
    Poly.model3 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3), data = data_PB)
    Poly.model4 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4), data = data_PB)
    Poly.model5 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5), data = data_PB)
    Poly.model6 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5)+ I(trend^6), data = data_PB)
    
    result$Intercept[i] <- summary(Poly.model3)$coefficients[1,4]
    result$trend1[i] <- summary(Poly.model1)$coefficients[2,4]
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
    
    pt(summary(Poly.model1)$coefficients[2,3], summary(Poly.model1)$df[2], lower=TRUE)
    
    s <- sd(data_PB$PB)
    TS <- (result$est.trend1[i] - sig) / (s * sqrt(wind + 4))
    p-val <- pt(TS, summary(Poly.model1)$df[2], lower = TRUE)
    #coeftest()
    
    
    result$fit_trend1[i] <- if(result$trend1[i] < 0.1 & trend.sig < 0.05) result$fit_trend1[i] else NA
    envi$Ratio.PB$sig_trend1[i]<- if(result$trend1[i] < 0.1 & trend.sig < 0.05) 1 else 0
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
  
  return(result)
}

fite.model.ARIMA <- function(envi, mod = "MAN", P.a = 0.9, P.b = 0.11, B.a = 0.9, B.b = 0.11, sta = 1990, wind = 8){
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
  
  envi$Ratio.PB$sig_trend1 <- 0
  envi$Ratio.PB$sig_trend2 <- 0
  envi$Ratio.PB$sig_trend3 <- 0
  envi$Ratio.PB$sig_trend4 <- 0
  envi$Ratio.PB$sig_trend5 <- 0
  
  for(i in c(wind:(l-5))){
    data <- data.table(P = window(envi$P.Data$P, end = c(sta + (i+4)*0.25)))
    data$B <- window(envi$B.Data$BPS_E, end = c(sta + (i+4)*0.25))
    P.model <- auto.arima(window(envi$P.Data$P, end = c(sta + (i-1)*0.25)), d = 0)
    B.model <- auto.arima(window(envi$B.Data$BPS_E, end = c(sta + (i-1)*0.25)), d = 0)
    PB.forecast <- (forecast(P.model)$mean / forecast(B.model)$mean)
    data_PB <- data.table(PB = window(envi$Ratio.PB$PB, start = c(sta + (i-wind)*0.25) ,end = c(sta + (i+4)*0.25)))
    data_PB$PB[c((length(data_PB$PB)-4):(length(data_PB$PB)))] <- PB.forecast[c(1:5)]
    data_PB$PB <- ts(data_PB$PB, start = c(sta + (i-8)*0.25), frequency = 4)
    
    Poly.model1 <- tslm(PB ~ I(trend), data = data_PB)
    Poly.model2 <- tslm(PB ~ I(trend) + I(trend^2), data = data_PB)
    Poly.model3 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3), data = data_PB)
    Poly.model4 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4), data = data_PB)
    Poly.model5 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5), data = data_PB)
    Poly.model6 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5)+ I(trend^6), data = data_PB)
    
    result$Intercept[i] <- summary(Poly.model3)$coefficients[1,4]
    result$trend1[i] <- summary(Poly.model1)$coefficients[2,4]
    result$trend2[i] <- summary(Poly.model2)$coefficients[3,4]
    result$trend3[i] <- summary(Poly.model3)$coefficients[4,4]
    result$trend4[i] <- summary(Poly.model4)$coefficients[5,4]
    result$trend5[i] <- summary(Poly.model5)$coefficients[6,4]
    result$trend6[i] <- summary(Poly.model6)$coefficients[7,4]
    
    r <- 0.4
    R <- c(0, 1)
    linearHypothesis(model = Poly.model1, hypothesis.matrix = R, rhs = r)
    #coeftest()
    
    result$fit_trend1[i] <- if(result$trend1[i] < 0.1 & Poly.model1$coefficients[2] > 0.4) result$fit_trend1[i] else NA
    envi$Ratio.PB$sig_trend1[i]<- if(result$trend1[i] < 0.1 & Poly.model1$coefficients[2] > 0.4) 1 else 0
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
  
  return(result)
}

fite.model.PB.ARIMA <- function(envi, mod = "MAN", P.a = 0.9, P.b = 0.11, B.a = 0.9, B.b = 0.11, sta = 1990, wind = 8){
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
  
  envi$Ratio.PB$sig_trend1 <- 0
  envi$Ratio.PB$sig_trend2 <- 0
  envi$Ratio.PB$sig_trend3 <- 0
  envi$Ratio.PB$sig_trend4 <- 0
  envi$Ratio.PB$sig_trend5 <- 0
  
  for(i in c(wind:(l-5))){
    data <- data.table(P = window(envi$P.Data$P, end = c(sta + (i+4)*0.25)))
    data$B <- window(envi$B.Data$BPS_E, end = c(sta + (i+4)*0.25))
    PB.forecast <- forecast(auto.arima(window(envi$Ratio.PB$PB, end = c(sta + (i-1)*0.25))))$mean
    data_PB <- data.table(PB = window(envi$Ratio.PB$PB, start = c(sta + (i-wind)*0.25) ,end = c(sta + (i+4)*0.25)))
    data_PB$PB[c((length(data_PB$PB)-4):(length(data_PB$PB)))] <- PB.forecast[c(1:5)]
    data_PB$PB <- ts(data_PB$PB, start = c(sta + (i-8)*0.25), frequency = 4)
    
    Poly.model1 <- tslm(PB ~ I(trend), data = data_PB)
    Poly.model2 <- tslm(PB ~ I(trend) + I(trend^2), data = data_PB)
    Poly.model3 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3), data = data_PB)
    Poly.model4 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4), data = data_PB)
    Poly.model5 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5), data = data_PB)
    Poly.model6 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5)+ I(trend^6), data = data_PB)
    
    result$Intercept[i] <- summary(Poly.model3)$coefficients[1,4]
    result$trend1[i] <- summary(Poly.model1)$coefficients[2,4]
    result$trend2[i] <- summary(Poly.model2)$coefficients[3,4]
    result$trend3[i] <- summary(Poly.model3)$coefficients[4,4]
    result$trend4[i] <- summary(Poly.model4)$coefficients[5,4]
    result$trend5[i] <- summary(Poly.model5)$coefficients[6,4]
    result$trend6[i] <- summary(Poly.model6)$coefficients[7,4]
    
    r <- 0.4
    R <- c(0, 1)
    linearHypothesis(model = Poly.model1, hypothesis.matrix = R, rhs = r)
    #coeftest()
    
    result$fit_trend1[i] <- if(result$trend1[i] < 0.1 & Poly.model1$coefficients[2] > 0.4) result$fit_trend1[i] else NA
    envi$Ratio.PB$sig_trend1[i]<- if(result$trend1[i] < 0.1 & Poly.model1$coefficients[2] > 0.4) 1 else 0
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
  
  return(result)
}

fite.model.PB.ETS <- function(envi, mod = "MAN", P.a = 0.9, P.b = 0.11, B.a = 0.9, B.b = 0.11, sta = 1990, wind = 8){
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
  
  envi$Ratio.PB$sig_trend1 <- 0
  envi$Ratio.PB$sig_trend2 <- 0
  envi$Ratio.PB$sig_trend3 <- 0
  envi$Ratio.PB$sig_trend4 <- 0
  envi$Ratio.PB$sig_trend5 <- 0
  
  for(i in c(wind:(l-5))){
    data <- data.table(P = window(envi$P.Data$P, end = c(sta + (i+4)*0.25)))
    data$B <- window(envi$B.Data$BPS_E, end = c(sta + (i+4)*0.25))
    PB.forecast <- forecast(ets(window(envi$Ratio.PB$PB, end = c(sta + (i-1)*0.25))))$mean
    data_PB <- data.table(PB = window(envi$Ratio.PB$PB, start = c(sta + (i-wind)*0.25) ,end = c(sta + (i+4)*0.25)))
    data_PB$PB[c((length(data_PB$PB)-4):(length(data_PB$PB)))] <- PB.forecast[c(1:5)]
    data_PB$PB <- ts(data_PB$PB, start = c(sta + (i-8)*0.25), frequency = 4)
    
    Poly.model1 <- tslm(PB ~ I(trend), data = data_PB)
    Poly.model2 <- tslm(PB ~ I(trend) + I(trend^2), data = data_PB)
    Poly.model3 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3), data = data_PB)
    Poly.model4 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4), data = data_PB)
    Poly.model5 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5), data = data_PB)
    Poly.model6 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5)+ I(trend^6), data = data_PB)
    
    result$Intercept[i] <- summary(Poly.model3)$coefficients[1,4]
    result$trend1[i] <- summary(Poly.model1)$coefficients[2,4]
    result$trend2[i] <- summary(Poly.model2)$coefficients[3,4]
    result$trend3[i] <- summary(Poly.model3)$coefficients[4,4]
    result$trend4[i] <- summary(Poly.model4)$coefficients[5,4]
    result$trend5[i] <- summary(Poly.model5)$coefficients[6,4]
    result$trend6[i] <- summary(Poly.model6)$coefficients[7,4]
    
    r <- 0.4
    R <- c(0, 1)
    linearHypothesis(model = Poly.model1, hypothesis.matrix = R, rhs = r)
    #coeftest()
    
    result$fit_trend1[i] <- if(result$trend1[i] < 0.1 & Poly.model1$coefficients[2] > 0.4) result$fit_trend1[i] else NA
    envi$Ratio.PB$sig_trend1[i]<- if(result$trend1[i] < 0.1 & Poly.model1$coefficients[2] > 0.4) 1 else 0
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
  
  return(result)
}

plot.result <- function(envi, result){
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
}

PB.CV <- function(envi, r.P.a = c(0.5, 0.99), r.P.b = c(0.01, 0.5), r.B.a = c(0.5, 0.99), r.B.b = c(0.01, 0.5), step = 0.1){
  envi$CV <- matrix(NA, nrow = 0, ncol = 5)
  l <- length(envi$Ratio.PB$PB)
  for(P.a in seq(from = r.P.a[1], to = r.P.a[2], by = step)){
    for(P.b in seq(from = r.P.b[1], to = r.P.b[2], by = step)){
      for(B.a in seq(from = r.B.a[1], to = r.B.a[2], by = step)){
        for(B.b in seq(from = r.B.b[1], to = r.B.b[2], by = step)){
          print(c(P.a, P.b, B.a, B.b))
          #system.time(
          {
            PB.forecast <- rep(NA, l)
            tryCatch({
              for(i in c(8:(l-2))){
                P.model <- ets(window(envi$P.Data$P, end = c(1990 + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
                B.model <- ets(window(envi$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
                PB.forecast[i+1] <- (forecast(P.model)$mean / forecast(B.model)$mean)[1]
                #e[i] <- (envi$Ratio.PB$PB[i+1] - PB.forecast[1])
              }
              e <- envi$Ratio.PB$PB - PB.forecast
            }, error = function(a){
              e <- NA
            })
            mse <- mean(e^2, na.rm = TRUE)
            envi$CV <- rbind(envi$CV, c(P.a, P.b, B.a, B.b, mse))
          }
          #)
          
          
        }
      }
    }
  }
}

con.table <- function(envi){
  print("------------------")
  print("Trend 1")
  print(prop.table(ftable(envi$Ratio.PB$sig_trend1, envi$Ratio.PB$redAlert)))
  print(sum(prop.table(ftable(envi$Ratio.PB$sig_trend1, envi$Ratio.PB$redAlert))[c(1,4)]))
  print("------------------")
  print("Trend 2")
  print(prop.table(ftable(envi$Ratio.PB$sig_trend2, envi$Ratio.PB$redAlert)))
  print(sum(prop.table(ftable(envi$Ratio.PB$sig_trend2, envi$Ratio.PB$redAlert))[c(1,4)]))
  print("------------------")
  print("Trend 3")
  print(prop.table(ftable(envi$Ratio.PB$sig_trend3, envi$Ratio.PB$redAlert)))
  print(sum(prop.table(ftable(envi$Ratio.PB$sig_trend3, envi$Ratio.PB$redAlert))[c(1,4)]))
  print("------------------")
  print("Trend 4")
  print(prop.table(ftable(envi$Ratio.PB$sig_trend4, envi$Ratio.PB$redAlert)))
  print(sum(prop.table(ftable(envi$Ratio.PB$sig_trend4, envi$Ratio.PB$redAlert))[c(1,4)]))
  print("------------------")
  print("Trend 5")
  print(prop.table(ftable(envi$Ratio.PB$sig_trend5, envi$Ratio.PB$redAlert)))
  print( sum(prop.table(ftable(envi$Ratio.PB$sig_trend5, envi$Ratio.PB$redAlert))[c(1,4)]))
  print("------------------")
}
