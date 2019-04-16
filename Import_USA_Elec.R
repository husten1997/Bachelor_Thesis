

ORCL_MV <- read_excel("Data_Eikon/American_Electronics/Oracle.xlsx", 
                     sheet = "Prices", col_types = c("date", 
                                                     "numeric"))

ORCL_BV <- read_excel("Data_Eikon/American_Electronics/Oracle.xlsx", 
                     sheet = "Data", col_types = c("date", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "numeric", "numeric"))
ORCL_BV$EPS_ttm <- rep(0)
for(i in c((nrow(ORCL_BV)-3):1)){
  ORCL_BV$EPS_ttm[i] <- sum(ORCL_BV$NI[c((i+3):i)]) / ORCL_BV$Shares[i] 
}

ORCL_BV$d.BPS_E <- c((ts(ORCL_BV$BPS_E) - lag(ts(ORCL_BV$BPS_E)))/lag(ts(ORCL_BV$BPS_E)),0)


ORCL_BV$d.EPS_ttm <- c((ts(ORCL_BV$EPS_ttm) - lag(ts(ORCL_BV$EPS_ttm)))/lag(ts(ORCL_BV$EPS_ttm)),0)


ORCL_Dates <- data.frame(matrix(0, nrow = 12018, ncol = 13))
names(ORCL_Dates) <- c("Date", "PPS", "BV_E", "BV_T", "Shares", "BPS_E", "BPS_D", "BPS_T", "NI", "EPS", "EPS_ttm", "d.BPS_E", "d.EPS_ttm")
ORCL_Dates[,1] <- seq(as.Date("1986-03-12"),length=12018,by="days")


ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_MV$Date) ,2] <- as.numeric(rev(ORCL_MV$ORCL))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,3] <- as.numeric(rev(ORCL_BV$BV_E))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,4] <- as.numeric(rev(ORCL_BV$BV_T))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,5] <- as.numeric(rev(ORCL_BV$Shares))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,6] <- as.numeric(rev(ORCL_BV$BPS_E))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,7] <- as.numeric(rev(ORCL_BV$BPS_D))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,8] <- as.numeric(rev(ORCL_BV$BPS_T))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,9] <- as.numeric(rev(ORCL_BV$NI))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,10] <- as.numeric(rev(ORCL_BV$EPS))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,11] <- as.numeric(rev(ORCL_BV$EPS_ttm))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,12] <- as.numeric(rev(ORCL_BV$d.BPS_E))
ORCL_Dates[as.character.Date(ORCL_Dates[,1]) %in% as.character.Date(ORCL_BV$Date) ,13] <- as.numeric(rev(ORCL_BV$d.EPS_ttm))

ORCL_Dates$d.BPS_E[is.nan(ORCL_Dates$d.BPS_E)] <- 0
ORCL_Dates$d.BPS_E[is.infinite(ORCL_Dates$d.BPS_E)] <- 0

ORCL_Dates$d.EPS_ttm[is.nan(ORCL_Dates$d.EPS_ttm)] <- 0
ORCL_Dates$d.EPS_ttm[is.infinite(ORCL_Dates$d.EPS_ttm)] <- 0

i <- 1
for(i in c(2:12017)){
  if(ORCL_Dates[i, 2] == 0) ORCL_Dates[i, 2] <- ORCL_Dates[i-1, 2]
  if(ORCL_Dates[i, 9] == 0) ORCL_Dates[i, 9] <- ORCL_Dates[i-1, 9]
  if(ORCL_Dates[i, 10] == 0) ORCL_Dates[i, 10] <- ORCL_Dates[i-1, 10]
  if(ORCL_Dates[i, 11] == 0) ORCL_Dates[i, 11] <- ORCL_Dates[i-1, 11]
  if(ORCL_Dates[i, 12] == 0) ORCL_Dates[i, 12] <- ORCL_Dates[i-1, 12]
  if(ORCL_Dates[i, 13] == 0) ORCL_Dates[i, 13] <- ORCL_Dates[i-1, 13]
}


#Price------------------------------------------------------------------------
ORCL_P_xts <- xts(ORCL_Dates$PPS, order.by = ORCL_Dates$Date)
colnames(ORCL_P_xts) <- c("P")
ORCL_P_xts$MA <- xts(ma(ORCL_P_xts$P, 181), order.by = ORCL_Dates$Date)
ORCL_P_xts$MA2 <- xts(ma(ORCL_P_xts$P, 913), order.by = ORCL_Dates$Date)
ORCL_P_xts$d.P <- (ORCL_P_xts$P - lag(ORCL_P_xts$P, 1))/lag(ORCL_P_xts$P, 1)
ORCL_P_xts$d.P[is.nan(ORCL_P_xts$d.P)] <- NA
ORCL_P_xts$d.P[is.infinite(ORCL_P_xts$d.P)] <- NA

#BV---------------------------------------------------------------------------
ORCL_BV_xts <- xts(ORCL_Dates$BPS_E, order.by = ORCL_Dates$Date)
colnames(ORCL_BV_xts) <- c("BPS_E")
ORCL_BV_xts$BPS_E[ORCL_BV_xts$BPS_E == 0] <- NA
ORCL_BV_xts$BPS_E[1] <- 0
ORCL_BV_xts$BPS_E["/1990-05-30"] <- 0
ORCL_BV_xts$BPS_E <- xts(na.interp(ORCL_BV_xts$BPS_E), order.by = ORCL_Dates$Date)

ORCL_BV_xts$d.BPS_E <- xts(ORCL_Dates$d.BPS_E, order.by = ORCL_Dates$Date)
ORCL_BV_xts$d.EPS_ttm <- xts(ORCL_Dates$d.EPS_ttm, order.by = ORCL_Dates$Date)
#ORCL_BV_xts$d.BPS_E <- (ORCL_BV_xts$BPS_E - lag(ORCL_BV_xts$BPS_E, 1))/lag(ORCL_BV_xts$BPS_E, 1)
#ORCL_BV_xts$d.BPS_E[is.nan(ORCL_BV_xts$d.BPS_E)] <- NA
#ORCL_BV_xts$d.BPS_E[is.na(ORCL_BV_xts$d.BPS_E)] <- 0
#ORCL_BV_xts$d.BPS_E[is.infinite(ORCL_BV_xts$d.BPS_E)] <- NA

ORCL_BV_xts$BPS_D <- xts(ORCL_Dates$BPS_D, order.by = ORCL_Dates$Date)
ORCL_BV_xts$BPS_D[ORCL_BV_xts$BPS_D == 0] <- NA
ORCL_BV_xts$BPS_D[1] <- 0
ORCL_BV_xts$BPS_D["/1990-03-29"] <- 0
ORCL_BV_xts$BPS_D <- xts(na.interp(ORCL_BV_xts$BPS_D), order.by = ORCL_Dates$Date)


#NI---------------------------------------------------------------------------
ORCL_EPS_xts <- xts(ORCL_Dates$EPS, order.by = ORCL_Dates$Date)
colnames(ORCL_EPS_xts) <- c("EPS")
ORCL_EPS_xts$EPS[ORCL_EPS_xts$EPS <= 0] <- NA

ORCL_EPS_xts$EPS_ttm <- xts(ORCL_Dates$EPS_ttm, order.by = ORCL_Dates$Date)
ORCL_EPS_xts$EPS_ttm[ORCL_EPS_xts$EPS_ttm <= 0.001] <- NA

#PB---------------------------------------------------------------------------
ORCL_Ratios.PB <- xts(ORCL_P_xts$P / ORCL_BV_xts$BPS_E, order.by = ORCL_Dates$Date)
colnames(ORCL_Ratios.PB) <- c("PB")
ORCL_Ratios.PB$PB[ORCL_Ratios.PB$PB == Inf] <- 0

ORCL_Ratios.PB$MA <- xts(c(rep(NA, 199), rollmeanr(ORCL_Ratios.PB$PB, 200)), order.by = ORCL_Dates$Date)
ORCL_Ratios.PB$MA2 <- xts(c(rep(NA, 99), rollmeanr(ORCL_Ratios.PB$PB, 100)), order.by = ORCL_Dates$Date)

ORCL_Ratios.PB$d.PB <- (ORCL_Ratios.PB$PB - lag(ORCL_Ratios.PB$PB, 1))/lag(ORCL_Ratios.PB$PB, 1)
ORCL_Ratios.PB$d.PB[is.nan(ORCL_Ratios.PB$d.PB)] <- NA
ORCL_Ratios.PB$d.PB[is.infinite(ORCL_Ratios.PB$d.PB)] <- NA

#PE--------------------------------------------------------------------------
ORCL_Ratios.PE <- xts((ORCL_P_xts$P + ORCL_BV_xts$BPS_D)/ORCL_EPS_xts$EPS_ttm, order.by = ORCL_Dates$Date)
colnames(ORCL_Ratios.PE) <- c("PE")
ORCL_Ratios.PE$PE[ORCL_Ratios.PE$PE == Inf] <- 0

ORCL_Ratios.PE$MA <- xts(c(rep(NA, 199), rollmeanr(ORCL_Ratios.PE$PE, 200)), order.by = ORCL_Dates$Date)
ORCL_Ratios.PE$MA2 <- xts(c(rep(NA, 99), rollmeanr(ORCL_Ratios.PE$PE, 100)), order.by = ORCL_Dates$Date)

ORCL_Ratios.PE$d.PE <- (ORCL_Ratios.PE$PE - lag(ORCL_Ratios.PE$PE, 1))/lag(ORCL_Ratios.PE$PE, 1)
ORCL_Ratios.PE$d.PE[is.nan(ORCL_Ratios.PE$d.PE)] <- NA
ORCL_Ratios.PE$d.PE[is.infinite(ORCL_Ratios.PE$d.PE)] <- NA

#Plot------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(ORCL_P_xts, col = c("black", "red", "green"))
plot(ORCL_BV_xts$BPS_E)
plot(ORCL_Ratios.PB)
plot(ORCL_Ratios.PE)


plot(ORCL_BV_xts$d.BPS_E)
plot(density(ORCL_BV$d.BPS_E))
abline(v = mean(ORCL_BV_xts$d.BPS_E) + 2*sd(ORCL_BV$d.BPS_E))
abline(v = mean(ORCL_BV_xts$d.BPS_E) - 2*sd(ORCL_BV$d.BPS_E))

plot(ORCL_P_xts$d.P)
m <- mean(ORCL_P_xts$d.P[!is.na(ORCL_P_xts$d.P)])
s <- sd(ORCL_P_xts$d.P[!is.na(ORCL_P_xts$d.P)])
a <- 3
plot(density(ORCL_P_xts$d.P[!is.na(ORCL_P_xts$d.P)]))
abline(v = m + a*s)
abline(v = m - a*s)


ORCL_P_ind <- index(ORCL_P_xts$d.P[ORCL_P_xts$d.P > (m + a*s)
                                 |ORCL_P_xts$d.P < (m - a*s)])
plot_data <- xts(ORCL_P_xts$d.P, order.by = ORCL_Dates$Date)
colnames(plot_data) <- c("a")
plot_data$b <- xts(ORCL_P_xts$d.P, order.by = ORCL_Dates$Date)
plot_data$b[!(index(plot_data$b) %in% ORCL_P_ind)] <- NA
plot_data$a[index(plot_data$b) %in% ORCL_P_ind] <- NA
plot(plot_data, col = c("black", "green"), type = c("p"))