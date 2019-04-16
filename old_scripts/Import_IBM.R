



IBM_MV <- read_excel("Data_Eikon/American_Electronics/IBM.xlsx", 
                      sheet = "Prices", col_types = c("date", 
                                                      "numeric"))

IBM_BV <- read_excel("Data_Eikon/American_Electronics/IBM.xlsx", 
                      sheet = "Data", col_types = c("date", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", "numeric"))
IBM_BV$EPS_ttm <- rep(0)
for(i in c((nrow(IBM_BV)-3):1)){
  IBM_BV$EPS_ttm[i] <- sum(IBM_BV$NI[c((i+3):i)]) / IBM_BV$Shares[i] 
}

IBM_BV$d.BPS_E <- c((ts(IBM_BV$BPS_E) - lag(ts(IBM_BV$BPS_E)))/lag(ts(IBM_BV$BPS_E)),0)


IBM_BV$d.EPS_ttm <- c((ts(IBM_BV$EPS_ttm) - lag(ts(IBM_BV$EPS_ttm)))/lag(ts(IBM_BV$EPS_ttm)),0)


IBM_Dates <- data.frame(matrix(0, nrow = 14204, ncol = 13))
names(IBM_Dates) <- c("Date", "PPS", "BV_E", "BV_T", "Shares", "BPS_E", "BPS_D", "BPS_T", "NI", "EPS", "EPS_ttm", "d.BPS_E", "d.EPS_ttm")
IBM_Dates[,1] <- seq(as.Date("1980-03-17"),length=14204,by="days")


IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_MV$Date) ,2] <- as.numeric(rev(IBM_MV$IBM))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,3] <- as.numeric(rev(IBM_BV$BV_E))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,4] <- as.numeric(rev(IBM_BV$BV_T))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,5] <- as.numeric(rev(IBM_BV$Shares))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,6] <- as.numeric(rev(IBM_BV$BPS_E))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,7] <- as.numeric(rev(IBM_BV$BPS_D))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,8] <- as.numeric(rev(IBM_BV$BPS_T))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,9] <- as.numeric(rev(IBM_BV$NI))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,10] <- as.numeric(rev(IBM_BV$EPS))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,11] <- as.numeric(rev(IBM_BV$EPS_ttm))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,12] <- as.numeric(rev(IBM_BV$d.BPS_E))
IBM_Dates[as.character.Date(IBM_Dates[,1]) %in% as.character.Date(IBM_BV$Date) ,13] <- as.numeric(rev(IBM_BV$d.EPS_ttm))

IBM_Dates$d.BPS_E[is.nan(IBM_Dates$d.BPS_E)] <- 0
IBM_Dates$d.BPS_E[is.infinite(IBM_Dates$d.BPS_E)] <- 0

IBM_Dates$d.EPS_ttm[is.nan(IBM_Dates$d.EPS_ttm)] <- 0
IBM_Dates$d.EPS_ttm[is.infinite(IBM_Dates$d.EPS_ttm)] <- 0

i <- 1
for(i in c(2:nrow(IBM_Dates))){
  if(IBM_Dates[i, 2] == 0) IBM_Dates[i, 2] <- IBM_Dates[i-1, 2]
  if(IBM_Dates[i, 9] == 0) IBM_Dates[i, 9] <- IBM_Dates[i-1, 9]
  if(IBM_Dates[i, 10] == 0) IBM_Dates[i, 10] <- IBM_Dates[i-1, 10]
  if(IBM_Dates[i, 11] == 0) IBM_Dates[i, 11] <- IBM_Dates[i-1, 11]
  if(IBM_Dates[i, 12] == 0) IBM_Dates[i, 12] <- IBM_Dates[i-1, 12]
  if(IBM_Dates[i, 13] == 0) IBM_Dates[i, 13] <- IBM_Dates[i-1, 13]
}


#Price------------------------------------------------------------------------
IBM_P_xts <- xts(IBM_Dates$PPS, order.by = IBM_Dates$Date)
colnames(IBM_P_xts) <- c("P")
IBM_P_xts$MA <- xts(ma(IBM_P_xts$P, 181), order.by = IBM_Dates$Date)
IBM_P_xts$MA2 <- xts(ma(IBM_P_xts$P, 913), order.by = IBM_Dates$Date)
IBM_P_xts$d.P <- (IBM_P_xts$P - lag(IBM_P_xts$P, 1))/lag(IBM_P_xts$P, 1)
IBM_P_xts$d.P[is.nan(IBM_P_xts$d.P)] <- NA
IBM_P_xts$d.P[is.infinite(IBM_P_xts$d.P)] <- NA

#BV---------------------------------------------------------------------------
IBM_BV_xts <- xts(IBM_Dates$BPS_E, order.by = IBM_Dates$Date)
colnames(IBM_BV_xts) <- c("BPS_E")
IBM_BV_xts$BPS_E[IBM_BV_xts$BPS_E == 0] <- NA
IBM_BV_xts$BPS_E[1] <- 0
IBM_BV_xts$BPS_E["/1989-12-30"] <- 0
IBM_BV_xts$BPS_E <- xts(na.interp(IBM_BV_xts$BPS_E), order.by = IBM_Dates$Date)

IBM_BV_xts$d.BPS_E <- xts(IBM_Dates$d.BPS_E, order.by = IBM_Dates$Date)
IBM_BV_xts$d.EPS_ttm <- xts(IBM_Dates$d.EPS_ttm, order.by = IBM_Dates$Date)
#IBM_BV_xts$d.BPS_E <- (IBM_BV_xts$BPS_E - lag(IBM_BV_xts$BPS_E, 1))/lag(IBM_BV_xts$BPS_E, 1)
#IBM_BV_xts$d.BPS_E[is.nan(IBM_BV_xts$d.BPS_E)] <- NA
#IBM_BV_xts$d.BPS_E[is.na(IBM_BV_xts$d.BPS_E)] <- 0
#IBM_BV_xts$d.BPS_E[is.infinite(IBM_BV_xts$d.BPS_E)] <- NA

IBM_BV_xts$BPS_D <- xts(IBM_Dates$BPS_D, order.by = IBM_Dates$Date)
IBM_BV_xts$BPS_D[IBM_BV_xts$BPS_D == 0] <- NA
IBM_BV_xts$BPS_D[1] <- 0
IBM_BV_xts$BPS_D["/1990-03-29"] <- 0
IBM_BV_xts$BPS_D <- xts(na.interp(IBM_BV_xts$BPS_D), order.by = IBM_Dates$Date)


#NI---------------------------------------------------------------------------
IBM_EPS_xts <- xts(IBM_Dates$EPS, order.by = IBM_Dates$Date)
colnames(IBM_EPS_xts) <- c("EPS")
IBM_EPS_xts$EPS[IBM_EPS_xts$EPS <= 0] <- NA

IBM_EPS_xts$EPS_ttm <- xts(IBM_Dates$EPS_ttm, order.by = IBM_Dates$Date)
IBM_EPS_xts$EPS_ttm[IBM_EPS_xts$EPS_ttm <= 0.001] <- NA

#PB---------------------------------------------------------------------------
IBM_Ratios.PB <- xts(IBM_P_xts$P / IBM_BV_xts$BPS_E, order.by = IBM_Dates$Date)
colnames(IBM_Ratios.PB) <- c("PB")
IBM_Ratios.PB$PB[IBM_Ratios.PB$PB == Inf] <- 0

IBM_Ratios.PB$MA <- xts(c(rep(NA, 199), rollmeanr(IBM_Ratios.PB$PB, 200)), order.by = IBM_Dates$Date)
IBM_Ratios.PB$MA2 <- xts(c(rep(NA, 99), rollmeanr(IBM_Ratios.PB$PB, 100)), order.by = IBM_Dates$Date)

IBM_Ratios.PB$d.PB <- (IBM_Ratios.PB$PB - lag(IBM_Ratios.PB$PB, 1))/lag(IBM_Ratios.PB$PB, 1)
IBM_Ratios.PB$d.PB[is.nan(IBM_Ratios.PB$d.PB)] <- NA
IBM_Ratios.PB$d.PB[is.infinite(IBM_Ratios.PB$d.PB)] <- NA

#PE--------------------------------------------------------------------------
IBM_Ratios.PE <- xts((IBM_P_xts$P + IBM_BV_xts$BPS_D)/IBM_EPS_xts$EPS_ttm, order.by = IBM_Dates$Date)
colnames(IBM_Ratios.PE) <- c("PE")
IBM_Ratios.PE$PE[IBM_Ratios.PE$PE == Inf] <- 0

IBM_Ratios.PE$MA <- xts(c(rep(NA, 199), rollmeanr(IBM_Ratios.PE$PE, 200)), order.by = IBM_Dates$Date)
IBM_Ratios.PE$MA2 <- xts(c(rep(NA, 99), rollmeanr(IBM_Ratios.PE$PE, 100)), order.by = IBM_Dates$Date)

IBM_Ratios.PE$d.PE <- (IBM_Ratios.PE$PE - lag(IBM_Ratios.PE$PE, 1))/lag(IBM_Ratios.PE$PE, 1)
IBM_Ratios.PE$d.PE[is.nan(IBM_Ratios.PE$d.PE)] <- NA
IBM_Ratios.PE$d.PE[is.infinite(IBM_Ratios.PE$d.PE)] <- NA

#Plot------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(IBM_P_xts, col = c("black", "red", "green"))
plot(IBM_BV_xts$BPS_E)
plot(IBM_Ratios.PB)
plot(IBM_Ratios.PE)


plot(IBM_BV_xts$d.BPS_E)
plot(density(IBM_BV$d.BPS_E))
abline(v = mean(IBM_BV_xts$d.BPS_E) + 2*sd(IBM_BV$d.BPS_E))
abline(v = mean(IBM_BV_xts$d.BPS_E) - 2*sd(IBM_BV$d.BPS_E))

plot(IBM_P_xts$d.P)
m <- mean(IBM_P_xts$d.P[!is.na(IBM_P_xts$d.P)])
s <- sd(IBM_P_xts$d.P[!is.na(IBM_P_xts$d.P)])
a <- 3
plot(density(IBM_P_xts$d.P[!is.na(IBM_P_xts$d.P)]))
abline(v = m + a*s)
abline(v = m - a*s)


IBM_P_ind <- index(IBM_P_xts$d.P[IBM_P_xts$d.P > (m + a*s)
                                   |IBM_P_xts$d.P < (m - a*s)])
plot_data <- xts(IBM_P_xts$d.P, order.by = IBM_Dates$Date)
colnames(plot_data) <- c("a")
plot_data$b <- xts(IBM_P_xts$d.P, order.by = IBM_Dates$Date)
plot_data$b[!(index(plot_data$b) %in% IBM_P_ind)] <- NA
plot_data$a[index(plot_data$b) %in% IBM_P_ind] <- NA
plot(plot_data, col = c("black", "green"), type = c("p"))