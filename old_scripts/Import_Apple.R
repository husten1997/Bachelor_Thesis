

AAPL_MV <- read_excel("Data_Eikon/American_Electronics/Apple.xlsx", 
                     sheet = "Prices", col_types = c("date", 
                                                     "numeric"))

AAPL_BV <- read_excel("Data_Eikon/American_Electronics/Apple.xlsx", 
                     sheet = "Data", col_types = c("date", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric"))
AAPL_BV$EPS_ttm <- rep(0)
for(i in c((nrow(AAPL_BV)-3):1)){
  AAPL_BV$EPS_ttm[i] <- sum(AAPL_BV$NI[c((i+3):i)]) / AAPL_BV$Shares[i] 
}

AAPL_BV$d.BPS_E <- c((ts(AAPL_BV$BPS_E) - lag(ts(AAPL_BV$BPS_E)))/lag(ts(AAPL_BV$BPS_E)),0)


AAPL_BV$d.EPS_ttm <- c((ts(AAPL_BV$EPS_ttm) - lag(ts(AAPL_BV$EPS_ttm)))/lag(ts(AAPL_BV$EPS_ttm)),0)


AAPL_Dates <- data.frame(matrix(0, nrow = 13934, ncol = 13))
names(AAPL_Dates) <- c("Date", "PPS", "BV_E", "BV_T", "Shares", "BPS_E", "BPS_D", "BPS_T", "NI", "EPS", "EPS_ttm", "d.BPS_E", "d.EPS_ttm")
AAPL_Dates[,1] <- seq(as.Date("1980-12-12"),length=13934,by="days")


AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_MV$Date) ,2] <- as.numeric(rev(AAPL_MV$AAPL))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,3] <- as.numeric(rev(AAPL_BV$BV_E))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,4] <- as.numeric(rev(AAPL_BV$BV_T))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,5] <- as.numeric(rev(AAPL_BV$Shares))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,6] <- as.numeric(rev(AAPL_BV$BPS_E))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,7] <- as.numeric(rev(AAPL_BV$BPS_D))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,8] <- as.numeric(rev(AAPL_BV$BPS_T))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,9] <- as.numeric(rev(AAPL_BV$NI))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,10] <- as.numeric(rev(AAPL_BV$EPS))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,11] <- as.numeric(rev(AAPL_BV$EPS_ttm))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,12] <- as.numeric(rev(AAPL_BV$d.BPS_E))
AAPL_Dates[as.character.Date(AAPL_Dates[,1]) %in% as.character.Date(AAPL_BV$Date) ,13] <- as.numeric(rev(AAPL_BV$d.EPS_ttm))

AAPL_Dates$d.BPS_E[is.nan(AAPL_Dates$d.BPS_E)] <- 0
AAPL_Dates$d.BPS_E[is.infinite(AAPL_Dates$d.BPS_E)] <- 0

AAPL_Dates$d.EPS_ttm[is.nan(AAPL_Dates$d.EPS_ttm)] <- 0
AAPL_Dates$d.EPS_ttm[is.infinite(AAPL_Dates$d.EPS_ttm)] <- 0

i <- 1
for(i in c(2:nrow(AAPL_Dates))){
  if(AAPL_Dates[i, 2] == 0) AAPL_Dates[i, 2] <- AAPL_Dates[i-1, 2]
  if(AAPL_Dates[i, 9] == 0) AAPL_Dates[i, 9] <- AAPL_Dates[i-1, 9]
  if(AAPL_Dates[i, 10] == 0) AAPL_Dates[i, 10] <- AAPL_Dates[i-1, 10]
  if(AAPL_Dates[i, 11] == 0) AAPL_Dates[i, 11] <- AAPL_Dates[i-1, 11]
  if(AAPL_Dates[i, 12] == 0) AAPL_Dates[i, 12] <- AAPL_Dates[i-1, 12]
  if(AAPL_Dates[i, 13] == 0) AAPL_Dates[i, 13] <- AAPL_Dates[i-1, 13]
}


#Price------------------------------------------------------------------------
AAPL_P_xts <- xts(AAPL_Dates$PPS, order.by = AAPL_Dates$Date)
colnames(AAPL_P_xts) <- c("P")
AAPL_P_xts$MA <- xts(ma(AAPL_P_xts$P, 181), order.by = AAPL_Dates$Date)
AAPL_P_xts$MA2 <- xts(ma(AAPL_P_xts$P, 913), order.by = AAPL_Dates$Date)
AAPL_P_xts$d.P <- (AAPL_P_xts$P - lag(AAPL_P_xts$P, 1))/lag(AAPL_P_xts$P, 1)
AAPL_P_xts$d.P[is.nan(AAPL_P_xts$d.P)] <- NA
AAPL_P_xts$d.P[is.infinite(AAPL_P_xts$d.P)] <- NA

#BV---------------------------------------------------------------------------
AAPL_BV_xts <- xts(AAPL_Dates$BPS_E, order.by = AAPL_Dates$Date)
colnames(AAPL_BV_xts) <- c("BPS_E")
AAPL_BV_xts$BPS_E[AAPL_BV_xts$BPS_E == 0] <- NA
AAPL_BV_xts$BPS_E[1] <- 0
AAPL_BV_xts$BPS_E["/1989-09-28"] <- 0
AAPL_BV_xts$BPS_E <- xts(na.interp(AAPL_BV_xts$BPS_E), order.by = AAPL_Dates$Date)

AAPL_BV_xts$d.BPS_E <- xts(AAPL_Dates$d.BPS_E, order.by = AAPL_Dates$Date)
AAPL_BV_xts$d.EPS_ttm <- xts(AAPL_Dates$d.EPS_ttm, order.by = AAPL_Dates$Date)
#AAPL_BV_xts$d.BPS_E <- (AAPL_BV_xts$BPS_E - lag(AAPL_BV_xts$BPS_E, 1))/lag(AAPL_BV_xts$BPS_E, 1)
#AAPL_BV_xts$d.BPS_E[is.nan(AAPL_BV_xts$d.BPS_E)] <- NA
#AAPL_BV_xts$d.BPS_E[is.na(AAPL_BV_xts$d.BPS_E)] <- 0
#AAPL_BV_xts$d.BPS_E[is.infinite(AAPL_BV_xts$d.BPS_E)] <- NA

AAPL_BV_xts$BPS_D <- xts(AAPL_Dates$BPS_D, order.by = AAPL_Dates$Date)
AAPL_BV_xts$BPS_D[AAPL_BV_xts$BPS_D == 0] <- NA
AAPL_BV_xts$BPS_D[1] <- 0
AAPL_BV_xts$BPS_D["/1989-09-28"] <- 0
AAPL_BV_xts$BPS_D <- xts(na.interp(AAPL_BV_xts$BPS_D), order.by = AAPL_Dates$Date)


#NI---------------------------------------------------------------------------
AAPL_EPS_xts <- xts(AAPL_Dates$EPS, order.by = AAPL_Dates$Date)
colnames(AAPL_EPS_xts) <- c("EPS")
AAPL_EPS_xts$EPS[AAPL_EPS_xts$EPS <= 0] <- NA

AAPL_EPS_xts$EPS_ttm <- xts(AAPL_Dates$EPS_ttm, order.by = AAPL_Dates$Date)
AAPL_EPS_xts$EPS_ttm[AAPL_EPS_xts$EPS_ttm <= 0.001] <- NA

#PB---------------------------------------------------------------------------
AAPL_Ratios.PB <- xts(AAPL_P_xts$P / AAPL_BV_xts$BPS_E, order.by = AAPL_Dates$Date)
colnames(AAPL_Ratios.PB) <- c("PB")
AAPL_Ratios.PB$PB[AAPL_Ratios.PB$PB == Inf] <- 0

AAPL_Ratios.PB$MA <- xts(c(rep(NA, 199), rollmeanr(AAPL_Ratios.PB$PB, 200)), order.by = AAPL_Dates$Date)
AAPL_Ratios.PB$MA2 <- xts(c(rep(NA, 99), rollmeanr(AAPL_Ratios.PB$PB, 100)), order.by = AAPL_Dates$Date)

AAPL_Ratios.PB$d.PB <- (AAPL_Ratios.PB$PB - lag(AAPL_Ratios.PB$PB, 1))/lag(AAPL_Ratios.PB$PB, 1)
AAPL_Ratios.PB$d.PB[is.nan(AAPL_Ratios.PB$d.PB)] <- NA
AAPL_Ratios.PB$d.PB[is.infinite(AAPL_Ratios.PB$d.PB)] <- NA

#PE--------------------------------------------------------------------------
AAPL_Ratios.PE <- xts((AAPL_P_xts$P + AAPL_BV_xts$BPS_D)/AAPL_EPS_xts$EPS_ttm, order.by = AAPL_Dates$Date)
colnames(AAPL_Ratios.PE) <- c("PE")
AAPL_Ratios.PE$PE[AAPL_Ratios.PE$PE == Inf] <- 0

AAPL_Ratios.PE$MA <- xts(c(rep(NA, 199), rollmeanr(AAPL_Ratios.PE$PE, 200)), order.by = AAPL_Dates$Date)
AAPL_Ratios.PE$MA2 <- xts(c(rep(NA, 99), rollmeanr(AAPL_Ratios.PE$PE, 100)), order.by = AAPL_Dates$Date)

AAPL_Ratios.PE$d.PE <- (AAPL_Ratios.PE$PE - lag(AAPL_Ratios.PE$PE, 1))/lag(AAPL_Ratios.PE$PE, 1)
AAPL_Ratios.PE$d.PE[is.nan(AAPL_Ratios.PE$d.PE)] <- NA
AAPL_Ratios.PE$d.PE[is.infinite(AAPL_Ratios.PE$d.PE)] <- NA

#Plot------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(AAPL_P_xts, col = c("black", "red", "green"))
plot(AAPL_BV_xts$BPS_E)
plot(AAPL_Ratios.PB)
plot(AAPL_Ratios.PE)


plot(AAPL_BV_xts$d.BPS_E)
plot(density(AAPL_BV$d.BPS_E))
abline(v = mean(AAPL_BV_xts$d.BPS_E) + 2*sd(AAPL_BV$d.BPS_E))
abline(v = mean(AAPL_BV_xts$d.BPS_E) - 2*sd(AAPL_BV$d.BPS_E))

plot(AAPL_P_xts$d.P)
m <- mean(AAPL_P_xts$d.P[!is.na(AAPL_P_xts$d.P)])
s <- sd(AAPL_P_xts$d.P[!is.na(AAPL_P_xts$d.P)])
a <- 3
plot(density(AAPL_P_xts$d.P[!is.na(AAPL_P_xts$d.P)]))
abline(v = m + a*s)
abline(v = m - a*s)


AAPL_P_ind <- index(AAPL_P_xts$d.P[AAPL_P_xts$d.P > (m + a*s)
                                 |AAPL_P_xts$d.P < (m - a*s)])
plot_data <- xts(AAPL_P_xts$d.P, order.by = AAPL_Dates$Date)
colnames(plot_data) <- c("a")
plot_data$b <- xts(AAPL_P_xts$d.P, order.by = AAPL_Dates$Date)
plot_data$b[!(index(plot_data$b) %in% AAPL_P_ind)] <- NA
plot_data$a[index(plot_data$b) %in% AAPL_P_ind] <- NA
plot(plot_data, col = c("black", "green"), type = c("p"))