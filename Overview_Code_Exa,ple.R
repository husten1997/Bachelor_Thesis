P_start_date <- "1986-03-13"
BV_start_date <- "/1990-03-29"
count_of_days <- 12017

MSFT$BV$EPS_ttm <- rep(0)
for(i in c((nrow(MSFT$BV)-3):1)){
  MSFT$BV$EPS_ttm[i] <- sum(MSFT$BV$NI[c((i+3):i)]) / MSFT$BV$Shares[i] 
}

#calculation of changes
MSFT$BV$d.BPS_E <- c((ts(MSFT$BV$BPS_E) - lag(ts(MSFT$BV$BPS_E)))/lag(ts(MSFT$BV$BPS_E)),0)
MSFT$BV$d.EPS_ttm <- c((ts(MSFT$BV$EPS_ttm) - lag(ts(MSFT$BV$EPS_ttm)))/lag(ts(MSFT$BV$EPS_ttm)),0)


#generating and filling of the main xts Object (Data Frame)
MSFT$Data <- data.frame(matrix(0, nrow = count_of_days, ncol = 13))
names(MSFT$Data) <- c("Date", "PPS", "BV_E", "BV_T", "Shares", "BPS_E", "BPS_D", "BPS_T", "NI", "EPS", "EPS_ttm", "d.BPS_E", "d.EPS_ttm")
MSFT$Data[,1] <- seq(as.Date(P_start_date),length=count_of_days ,by="days")



MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$MV$Date) ,2] <- as.numeric(rev(MSFT$MV$Close))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,3] <- as.numeric(rev(MSFT$BV$BV_E))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,4] <- as.numeric(rev(MSFT$BV$BV_T))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,5] <- as.numeric(rev(MSFT$BV$Shares))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,6] <- as.numeric(rev(MSFT$BV$BPS_E))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,7] <- as.numeric(rev(MSFT$BV$BPS_D))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,8] <- as.numeric(rev(MSFT$BV$BPS_T))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,9] <- as.numeric(rev(MSFT$BV$NI))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,10] <- as.numeric(rev(MSFT$BV$EPS))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,11] <- as.numeric(rev(MSFT$BV$EPS_ttm))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,12] <- as.numeric(rev(MSFT$BV$d.BPS_E))
MSFT$Data[as.character.Date(MSFT$Data[,1]) %in% as.character.Date(MSFT$BV$Date) ,13] <- as.numeric(rev(MSFT$BV$d.EPS_ttm))


MSFT$Data$d.BPS_E[is.nan(MSFT$Data$d.BPS_E)] <- 0
MSFT$Data$d.BPS_E[is.infinite(MSFT$Data$d.BPS_E)] <- 0

MSFT$Data$d.EPS_ttm[is.nan(MSFT$Data$d.EPS_ttm)] <- 0
MSFT$Data$d.EPS_ttm[is.infinite(MSFT$Data$d.EPS_ttm)] <- 0

i <- 1
for(i in c(2:count_of_days)){
  if(MSFT$Data[i, 2] == 0) MSFT$Data[i, 2] <- MSFT$Data[i-1, 2]
  if(MSFT$Data[i, 9] == 0) MSFT$Data[i, 9] <- MSFT$Data[i-1, 9]
  if(MSFT$Data[i, 10] == 0) MSFT$Data[i, 10] <- MSFT$Data[i-1, 10]
  if(MSFT$Data[i, 11] == 0) MSFT$Data[i, 11] <- MSFT$Data[i-1, 11]
  if(MSFT$Data[i, 12] == 0) MSFT$Data[i, 12] <- MSFT$Data[i-1, 12]
  if(MSFT$Data[i, 13] == 0) MSFT$Data[i, 13] <- MSFT$Data[i-1, 13]
}


#Price------------------------------------------------------------------------
MSFT$P_xts <- xts(MSFT$Data$PPS, order.by = MSFT$Data$Date)
colnames(MSFT$P_xts) <- c("P")
#MSFT$P_xts$MA <- xts(ma(MSFT$P_xts$P, 181), order.by = MSFT$Data$Date)
#MSFT$P_xts$MA2 <- xts(ma(MSFT$P_xts$P, 913), order.by = MSFT$Data$Date)

MSFT$P_xts$MA <- xts(c(rep(NA, 89), rollmeanr(MSFT$P_xts$P, 90)), order.by = MSFT$Data$Date)
#MSFT$P_xts$MA <- xts(ma(MSFT$P_xts$P, 90), order.by = MSFT$Data$Date)
MSFT$P_xts$d.P <- (MSFT$P_xts$P - lag(MSFT$P_xts$P, 1))/lag(MSFT$P_xts$P, 1)
MSFT$P_xts$d.P[is.nan(MSFT$P_xts$d.P)] <- NA
MSFT$P_xts$d.P[is.infinite(MSFT$P_xts$d.P)] <- NA
MSFT$P_xts$d.P_MA <- (MSFT$P_xts$MA - lag(MSFT$P_xts$MA, 1))/lag(MSFT$P_xts$MA, 1)
MSFT$P_xts$d.P_MA[is.nan(MSFT$P_xts$d.P_MA)] <- NA
MSFT$P_xts$d.P_MA[is.infinite(MSFT$P_xts$d.P_MA)] <- NA

#BV---------------------------------------------------------------------------
MSFT$BV_xts <- xts(MSFT$Data$BPS_E, order.by = MSFT$Data$Date)
colnames(MSFT$BV_xts) <- c("BPS_E")
MSFT$BV_xts$BPS_E[MSFT$BV_xts$BPS_E == 0] <- NA
MSFT$BV_xts$BPS_E[1] <- 0
MSFT$BV_xts$BPS_E[BV_start_date] <- 0
MSFT$BV_xts$BPS_E <- xts(na.interp(MSFT$BV_xts$BPS_E), order.by = MSFT$Data$Date)
MSFT$BV_xts$BPS_E[BV_start_date] <- NA

MSFT$BV_xts$d.BPS_E <- xts(MSFT$Data$d.BPS_E, order.by = MSFT$Data$Date)
MSFT$BV_xts$d.EPS_ttm <- xts(MSFT$Data$d.EPS_ttm, order.by = MSFT$Data$Date)
#MSFT$BV_xts$d.BPS_E <- (MSFT$BV_xts$BPS_E - lag(MSFT$BV_xts$BPS_E, 1))/lag(MSFT$BV_xts$BPS_E, 1)
#MSFT$BV_xts$d.BPS_E[is.nan(MSFT$BV_xts$d.BPS_E)] <- NA
#MSFT$BV_xts$d.BPS_E[is.na(MSFT$BV_xts$d.BPS_E)] <- 0
#MSFT$BV_xts$d.BPS_E[is.infinite(MSFT$BV_xts$d.BPS_E)] <- NA

MSFT$BV_xts$BPS_D <- xts(MSFT$Data$BPS_D, order.by = MSFT$Data$Date)
MSFT$BV_xts$BPS_D[MSFT$BV_xts$BPS_D == 0] <- NA
MSFT$BV_xts$BPS_D[1] <- 0
MSFT$BV_xts$BPS_D[BV_start_date] <- 0
MSFT$BV_xts$BPS_D <- xts(na.interp(MSFT$BV_xts$BPS_D), order.by = MSFT$Data$Date)


#NI---------------------------------------------------------------------------
MSFT$EPS_xts <- xts(MSFT$Data$EPS, order.by = MSFT$Data$Date)
colnames(MSFT$EPS_xts) <- c("EPS")
MSFT$EPS_xts$EPS[MSFT$EPS_xts$EPS <= 0] <- NA

MSFT$EPS_xts$EPS_ttm <- xts(MSFT$Data$EPS_ttm, order.by = MSFT$Data$Date)
MSFT$EPS_xts$EPS_ttm[MSFT$EPS_xts$EPS_ttm <= 0] <- NA

#PB---------------------------------------------------------------------------
#View(BV_xts)
MSFT$Ratios.PB <- xts(MSFT$P_xts$MA / MSFT$BV_xts$BPS_E, order.by = MSFT$Data$Date)
colnames(MSFT$Ratios.PB) <- c("PB")
MSFT$Ratios.PB$PB[MSFT$Ratios.PB$PB == Inf] <- 0

#MSFT$Ratios.PB$MA <- xts(c(rep(NA, 199), rollmeanr(MSFT$Ratios.PB$PB, 200)), order.by = MSFT$Data$Date)
#MSFT$Ratios.PB$MA2 <- xts(c(rep(NA, 99), rollmeanr(MSFT$Ratios.PB$PB, 100)), order.by = MSFT$Data$Date)

MSFT$Ratios.PB$MA <- xts(ma(MSFT$Ratios.PB$PB, 540), order.by = MSFT$Data$Date)
#MSFT$Ratios.PB$MA <- xts(c(rep(NA, 539), rollmeanr(MSFT$Ratios.PB$P, 540)), order.by = MSFT$Data$Date)
MSFT$Ratios.PB$MA2 <- xts(ma(MSFT$Ratios.PB$PB, 360), order.by = MSFT$Data$Date)
MSFT$Ratios.PB$NivCleanded <- MSFT$Ratios.PB$PB - MSFT$Ratios.PB$MA
MSFT$Ratios.PB$d.PB <- (MSFT$Ratios.PB$PB - lag(MSFT$Ratios.PB$PB, 1))/lag(MSFT$Ratios.PB$PB, 1)
MSFT$Ratios.PB$d.PB[is.nan(MSFT$Ratios.PB$d.PB)] <- NA
MSFT$Ratios.PB$d.PB[is.infinite(MSFT$Ratios.PB$d.PB)] <- NA

#PE--------------------------------------------------------------------------
MSFT$Ratios.PE <- xts((MSFT$P_xts$MA + MSFT$BV_xts$BPS_D)/MSFT$EPS_xts$EPS_ttm, order.by = MSFT$Data$Date)
colnames(MSFT$Ratios.PE) <- c("PE")
MSFT$Ratios.PE$PE[MSFT$Ratios.PE$PE == Inf] <- 0

#MSFT$Ratios.PE$MA <- xts(c(rep(NA, 199), rollmeanr(MSFT$Ratios.PE$PE, 200)), order.by = MSFT$Data$Date)
#MSFT$Ratios.PE$MA2 <- xts(c(rep(NA, 99), rollmeanr(MSFT$Ratios.PE$PE, 100)), order.by = MSFT$Data$Date)
MSFT$Ratios.PE$MA <- xts(ma(MSFT$Ratios.PE$PE, 540), order.by = MSFT$Data$Date)
MSFT$Ratios.PE$MA2 <- xts(ma(MSFT$Ratios.PE$PE, 360), order.by = MSFT$Data$Date)


MSFT$Ratios.PE$d.PE <- (MSFT$Ratios.PE$PE - lag(MSFT$Ratios.PE$PE, 1))/lag(MSFT$Ratios.PE$PE, 1)
MSFT$Ratios.PE$d.PE[is.nan(MSFT$Ratios.PE$d.PE)] <- NA
MSFT$Ratios.PE$d.PE[is.infinite(MSFT$Ratios.PE$d.PE)] <- NA

par(mfrow = c(2,2))
plot(MSFT$P_xts, col = c("black", "red", "green"), main = c("Price"))
plot(MSFT$BV_xts$BPS_E, main = c("Equity Book Value"))
plot(MSFT$Ratios.PB, main = c("PB Ratio"))
plot(MSFT$Ratios.PE, main = c("PE Ratio"))

plot(MSFT$P_xts$d.P, main = c("Change of Price"))
plot.dens(dataset = MSFT$P_xts$d.P, a = 2, title = c("Density of Change in Price"))

plot.ext(MSFT$P_xts$d.P, a = 2, title = c("Extrems of change in Prices"))
par(mfrow = c(1,1))