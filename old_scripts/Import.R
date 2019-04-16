MSFT_MV <- read_excel("Data_Eikon/American_Electronics/Microsoft.xlsx", 
                        sheet = "Prices", col_types = c("date", 
                                                        "numeric"))

MSFT_BV <- read_excel("Data_Eikon/American_Electronics/Microsoft.xlsx", 
                        sheet = "Tabelle1", col_types = c("date", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric", "numeric"))
#Calculation EPS ttm
MSFT_BV$EPS_ttm <- rep(0)
for(i in c((nrow(MSFT_BV)-3):1)){
  MSFT_BV$EPS_ttm[i] <- sum(MSFT_BV$NI[c((i+3):i)]) / MSFT_BV$Shares[i] 
}

#calculation of changes
MSFT_BV$d.BPS_E <- c((ts(MSFT_BV$BPS_E) - lag(ts(MSFT_BV$BPS_E)))/lag(ts(MSFT_BV$BPS_E)),0)
MSFT_BV$d.EPS_ttm <- c((ts(MSFT_BV$EPS_ttm) - lag(ts(MSFT_BV$EPS_ttm)))/lag(ts(MSFT_BV$EPS_ttm)),0)


#generating and filling of the main xts Object (Data Frame)
MSFT_Dates <- data.frame(matrix(0, nrow = 12017, ncol = 13))
names(MSFT_Dates) <- c("Date", "PPS", "BV_E", "BV_T", "Shares", "BPS_E", "BPS_D", "BPS_T", "NI", "EPS", "EPS_ttm", "d.BPS_E", "d.EPS_ttm")
MSFT_Dates[,1] <- seq(as.Date("1986-03-13"),length=12017,by="days")


MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_MV$Date) ,2] <- as.numeric(MSFT_MV$MSFT.O_Close)
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,3] <- as.numeric(rev(MSFT_BV$BV_E))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,4] <- as.numeric(rev(MSFT_BV$BV_T))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,5] <- as.numeric(rev(MSFT_BV$Shares))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,6] <- as.numeric(rev(MSFT_BV$BPS_E))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,7] <- as.numeric(rev(MSFT_BV$BPS_D))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,8] <- as.numeric(rev(MSFT_BV$BPS_T))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,9] <- as.numeric(rev(MSFT_BV$NI))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,10] <- as.numeric(rev(MSFT_BV$EPS))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,11] <- as.numeric(rev(MSFT_BV$EPS_ttm))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,12] <- as.numeric(rev(MSFT_BV$d.BPS_E))
MSFT_Dates[as.character.Date(MSFT_Dates[,1]) %in% as.character.Date(MSFT_BV$Date) ,13] <- as.numeric(rev(MSFT_BV$d.EPS_ttm))


MSFT_Dates$d.BPS_E[is.nan(MSFT_Dates$d.BPS_E)] <- 0
MSFT_Dates$d.BPS_E[is.infinite(MSFT_Dates$d.BPS_E)] <- 0

MSFT_Dates$d.EPS_ttm[is.nan(MSFT_Dates$d.EPS_ttm)] <- 0
MSFT_Dates$d.EPS_ttm[is.infinite(MSFT_Dates$d.EPS_ttm)] <- 0

i <- 1
for(i in c(2:12017)){
  if(MSFT_Dates[i, 2] == 0) MSFT_Dates[i, 2] <- MSFT_Dates[i-1, 2]
  if(MSFT_Dates[i, 9] == 0) MSFT_Dates[i, 9] <- MSFT_Dates[i-1, 9]
  if(MSFT_Dates[i, 10] == 0) MSFT_Dates[i, 10] <- MSFT_Dates[i-1, 10]
  if(MSFT_Dates[i, 11] == 0) MSFT_Dates[i, 11] <- MSFT_Dates[i-1, 11]
  if(MSFT_Dates[i, 12] == 0) MSFT_Dates[i, 12] <- MSFT_Dates[i-1, 12]
  if(MSFT_Dates[i, 13] == 0) MSFT_Dates[i, 13] <- MSFT_Dates[i-1, 13]
}


#Price------------------------------------------------------------------------
MSFT_P_xts <- xts(MSFT_Dates$PPS, order.by = MSFT_Dates$Date)
colnames(MSFT_P_xts) <- c("P")
#MSFT_P_xts$MA <- xts(ma(MSFT_P_xts$P, 181), order.by = MSFT_Dates$Date)
#MSFT_P_xts$MA2 <- xts(ma(MSFT_P_xts$P, 913), order.by = MSFT_Dates$Date)

MSFT_P_xts$MA <- xts(c(rep(NA, 89), rollmeanr(MSFT_P_xts$P, 90)), order.by = MSFT_Dates$Date)
#MSFT_P_xts$MA <- xts(ma(MSFT_P_xts$P, 90), order.by = MSFT_Dates$Date)
MSFT_P_xts$d.P <- (MSFT_P_xts$P - lag(MSFT_P_xts$P, 1))/lag(MSFT_P_xts$P, 1)
MSFT_P_xts$d.P[is.nan(MSFT_P_xts$d.P)] <- NA
MSFT_P_xts$d.P[is.infinite(MSFT_P_xts$d.P)] <- NA
MSFT_P_xts$d.P_MA <- (MSFT_P_xts$MA - lag(MSFT_P_xts$MA, 1))/lag(MSFT_P_xts$MA, 1)
MSFT_P_xts$d.P_MA[is.nan(MSFT_P_xts$d.P_MA)] <- NA
MSFT_P_xts$d.P_MA[is.infinite(MSFT_P_xts$d.P_MA)] <- NA

#BV---------------------------------------------------------------------------
MSFT_BV_xts <- xts(MSFT_Dates$BPS_E, order.by = MSFT_Dates$Date)
colnames(MSFT_BV_xts) <- c("BPS_E")
MSFT_BV_xts$BPS_E[MSFT_BV_xts$BPS_E == 0] <- NA
MSFT_BV_xts$BPS_E[1] <- 0
MSFT_BV_xts$BPS_E["/1990-03-29"] <- 0
MSFT_BV_xts$BPS_E <- xts(na.interp(MSFT_BV_xts$BPS_E), order.by = MSFT_Dates$Date)
MSFT_BV_xts$BPS_E["/1990-03-29"] <- NA

MSFT_BV_xts$d.BPS_E <- xts(MSFT_Dates$d.BPS_E, order.by = MSFT_Dates$Date)
MSFT_BV_xts$d.EPS_ttm <- xts(MSFT_Dates$d.EPS_ttm, order.by = MSFT_Dates$Date)
#MSFT_BV_xts$d.BPS_E <- (MSFT_BV_xts$BPS_E - lag(MSFT_BV_xts$BPS_E, 1))/lag(MSFT_BV_xts$BPS_E, 1)
#MSFT_BV_xts$d.BPS_E[is.nan(MSFT_BV_xts$d.BPS_E)] <- NA
#MSFT_BV_xts$d.BPS_E[is.na(MSFT_BV_xts$d.BPS_E)] <- 0
#MSFT_BV_xts$d.BPS_E[is.infinite(MSFT_BV_xts$d.BPS_E)] <- NA

MSFT_BV_xts$BPS_D <- xts(MSFT_Dates$BPS_D, order.by = MSFT_Dates$Date)
MSFT_BV_xts$BPS_D[MSFT_BV_xts$BPS_D == 0] <- NA
MSFT_BV_xts$BPS_D[1] <- 0
MSFT_BV_xts$BPS_D["/1990-03-29"] <- 0
MSFT_BV_xts$BPS_D <- xts(na.interp(MSFT_BV_xts$BPS_D), order.by = MSFT_Dates$Date)


#NI---------------------------------------------------------------------------
MSFT_EPS_xts <- xts(MSFT_Dates$EPS, order.by = MSFT_Dates$Date)
colnames(MSFT_EPS_xts) <- c("EPS")
MSFT_EPS_xts$EPS[MSFT_EPS_xts$EPS <= 0] <- NA

MSFT_EPS_xts$EPS_ttm <- xts(MSFT_Dates$EPS_ttm, order.by = MSFT_Dates$Date)
MSFT_EPS_xts$EPS_ttm[MSFT_EPS_xts$EPS_ttm <= 0] <- NA

#PB---------------------------------------------------------------------------
#View(BV_xts)
MSFT_Ratios.PB <- xts(MSFT_P_xts$MA / MSFT_BV_xts$BPS_E, order.by = MSFT_Dates$Date)
colnames(MSFT_Ratios.PB) <- c("PB")
MSFT_Ratios.PB$PB[MSFT_Ratios.PB$PB == Inf] <- 0

#MSFT_Ratios.PB$MA <- xts(c(rep(NA, 199), rollmeanr(MSFT_Ratios.PB$PB, 200)), order.by = MSFT_Dates$Date)
#MSFT_Ratios.PB$MA2 <- xts(c(rep(NA, 99), rollmeanr(MSFT_Ratios.PB$PB, 100)), order.by = MSFT_Dates$Date)

MSFT_Ratios.PB$MA <- xts(ma(MSFT_Ratios.PB$PB, 540), order.by = MSFT_Dates$Date)
#MSFT_Ratios.PB$MA <- xts(c(rep(NA, 539), rollmeanr(MSFT_Ratios.PB$P, 540)), order.by = MSFT_Dates$Date)
MSFT_Ratios.PB$MA2 <- xts(ma(MSFT_Ratios.PB$PB, 360), order.by = MSFT_Dates$Date)
MSFT_Ratios.PB$NivCleanded <- MSFT_Ratios.PB$PB - MSFT_Ratios.PB$MA
MSFT_Ratios.PB$d.PB <- (MSFT_Ratios.PB$PB - lag(MSFT_Ratios.PB$PB, 1))/lag(MSFT_Ratios.PB$PB, 1)
MSFT_Ratios.PB$d.PB[is.nan(MSFT_Ratios.PB$d.PB)] <- NA
MSFT_Ratios.PB$d.PB[is.infinite(MSFT_Ratios.PB$d.PB)] <- NA

#PE--------------------------------------------------------------------------
MSFT_Ratios.PE <- xts((MSFT_P_xts$MA + MSFT_BV_xts$BPS_D)/MSFT_EPS_xts$EPS_ttm, order.by = MSFT_Dates$Date)
colnames(MSFT_Ratios.PE) <- c("PE")
MSFT_Ratios.PE$PE[MSFT_Ratios.PE$PE == Inf] <- 0

#MSFT_Ratios.PE$MA <- xts(c(rep(NA, 199), rollmeanr(MSFT_Ratios.PE$PE, 200)), order.by = MSFT_Dates$Date)
#MSFT_Ratios.PE$MA2 <- xts(c(rep(NA, 99), rollmeanr(MSFT_Ratios.PE$PE, 100)), order.by = MSFT_Dates$Date)
MSFT_Ratios.PE$MA <- xts(ma(MSFT_Ratios.PE$PE, 540), order.by = MSFT_Dates$Date)
MSFT_Ratios.PE$MA2 <- xts(ma(MSFT_Ratios.PE$PE, 360), order.by = MSFT_Dates$Date)


MSFT_Ratios.PE$d.PE <- (MSFT_Ratios.PE$PE - lag(MSFT_Ratios.PE$PE, 1))/lag(MSFT_Ratios.PE$PE, 1)
MSFT_Ratios.PE$d.PE[is.nan(MSFT_Ratios.PE$d.PE)] <- NA
MSFT_Ratios.PE$d.PE[is.infinite(MSFT_Ratios.PE$d.PE)] <- NA

#Plot------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(MSFT_P_xts, col = c("black", "red", "green"), main = c("Price"))
plot(MSFT_BV_xts$BPS_E, main = c("Equity Book Value"))
plot(MSFT_Ratios.PB, main = c("PB Ratio"))
plot(MSFT_Ratios.PE, main = c("PE Ratio"))


plot(MSFT_BV_xts$d.BPS_E, main = c("Change of Book Value per Share"))
plot.dens(MSFT_BV$d.BPS_E, a = 2, title = c("Density change of Book Value"))

plot(MSFT_P_xts$d.P, main = c("Change of Price"))
plot.dens(dataset = MSFT_P_xts$d.P, a = 2, title = c("Density of Change in Price"))


#plot(c(1:100), col = rep(c("black", "green"), 50))

#hist(MSFT_BV$d.BPS_E, freq = FALSE)
