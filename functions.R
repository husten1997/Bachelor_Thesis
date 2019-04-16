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

plot.ext <- function(dataset, a = 2, title, color = c("black", "green", "orange")){
  m <- mean(dataset, na.rm = TRUE)
  s <- sd(dataset, na.rm = TRUE)
  ind_up <- index(dataset[dataset > (m + a * s)])
  ind_down <- index(dataset[dataset < (m - a * s)])
  plot_data <- dataset
  colnames(plot_data) <- c("a")
  plot_data$b <- dataset
  plot_data$c <- dataset
  plot_data$a[index(plot_data) %in% ind_up] <- NA
  plot_data$a[index(plot_data) %in% ind_down] <- NA
  plot_data$b[!index(plot_data) %in% ind_up] <- NA
  plot_data$c[!index(plot_data) %in% ind_down] <- NA
  plot(plot_data, col = color, type = c("p"), main = title)
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
  ind_up <- index(dataset[dataset > (m + a * s)])
  ind_down <- index(dataset[dataset < (m - a * s)])
  #View(cbind(ind_up, ind_down))
  return(list(ind_up, ind_down))
}
