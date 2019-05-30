P_start_date = "1986-03-01"
count_of_days = 396
BV_start_date = "/1990-03-29"

MSFT <- new.env()
MSFT$BV <- read_excel("Data_Eikon/American_Electronics/Microsoft.xlsx", 
                      sheet = "Tabelle1", col_types = c("date", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric", "numeric"))
MSFT$MV <- read_excel("Data_Eikon/American_Electronics/Microsoft.xlsx", 
                      sheet = "Prices", col_types = c("date", 
                                                      "numeric"))





#Price Data-------------------------------------------------------------------------------------
MSFT$P.Data_pre <- data.table(aggregate(ts(as.numeric(tapply(as.numeric(rev(MSFT$MV$Close)), as.yearmon(rev(MSFT$MV$Date)), mean)), start = c(1987, 1), frequency = 12), nfrequency = 4))
colnames(MSFT$P.Data_pre) <- c("P")

MSFT$P.Data_pre$P

MSFT$P.Data_pre$d.P <- ts(c(NA, (MSFT$P.Data_pre$P - lag(MSFT$P.Data_pre$P, -1))/lag(MSFT$P.Data_pre$P, -1)), start = c(1987, 1), frequency = 4)
MSFT$P.Data_pre$d.P[is.nan(MSFT$P.Data_pre$d.P)] <- NA
MSFT$P.Data_pre$d.P[is.infinite(MSFT$P.Data_pre$d.P)] <- NA

MSFT$P.Data <- data.table(window(MSFT$P.Data_pre$P, start = c(1990, 1)))
colnames(MSFT$P.Data) <- c("P")
MSFT$P.Data$d.P <- window(MSFT$P.Data_pre$d.P, start = c(1990, 1))
MSFT$P.Data$nd.P <- 0
MSFT$P.Data$nd.P[MSFT$P.Data$d.P > 0] <- as.factor(1)

adf.test(MSFT$P.Data$P)
(P.model <- ets(window(MSFT$P.Data$P, end = c(2000, 1)), opt.crit = "amse", nmse = 1))
(P.model <- auto.arima(window(MSFT$P.Data$P, end = c(2000, 2)), stationary = TRUE))
plot(forecast(P.model))
#Fundamental Data-------------------------------------------------------------------------------
#I should write them down into the B.Data table => more convenient
MSFT$BV$EPS_ttm <- rep(0)
for(i in c((nrow(MSFT$BV)-3):1)){
  MSFT$BV$EPS_ttm[i] <- sum(MSFT$BV$NI[c((i+3):i)]) / MSFT$BV$Shares[i] 
}
MSFT$BV$d.BPS_E <- c((ts(MSFT$BV$BPS_E) - lag(ts(MSFT$BV$BPS_E)))/lag(ts(MSFT$BV$BPS_E)),0)
MSFT$BV$d.EPS_ttm <- c((ts(MSFT$BV$EPS_ttm) - lag(ts(MSFT$BV$EPS_ttm)))/lag(ts(MSFT$BV$EPS_ttm)),0)

MSFT$B.Data <- data.table(ts(MSFT$BV$BV_E, start = c(1990, 1), frequency = 4))
colnames(MSFT$B.Data) <- c("BV_E")
#MSFT$B.Data$BV_D <- ts(rev(MSFT$BV$BV_D), start = c(1990, 1), frequency = 4)
MSFT$B.Data$BV_T <- ts(rev(MSFT$BV$BV_T), start = c(1990, 1), frequency = 4)
MSFT$B.Data$BPS_E <- ts(rev(MSFT$BV$BPS_E), start = c(1990, 1), frequency = 4)
MSFT$B.Data$BPS_D <- ts(MSFT$BV$BPS_D, start = c(1990, 1), frequency = 4)
MSFT$B.Data$BPS_T <- ts(rev(MSFT$BV$BPS_T), start = c(1990, 1), frequency = 4)
MSFT$B.Data$d.BPS_E <- ts(rev(MSFT$BV$d.BPS_E), start = c(1990, 1), frequency = 4)

adf.test(MSFT$B.Data$BPS_E)
(B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(2000, 1), opt.crit = "mse")))
(B.model <- auto.arima(window(MSFT$B.Data$BPS_E, end = c(2000, 1))))
plot(forecast(B.model))
#PB-----------------------------------------------------------------------------------------
MSFT$Ratio.PB <- data.table(ts(MSFT$P.Data$P / MSFT$B.Data$BPS_E, start = c(1990, 1), frequency = 4))
colnames(MSFT$Ratio.PB) <- c("PB")
plot(MSFT$Ratio.PB$PB)

adf.test((MSFT$Ratio.PB$PB-MSFT$Ratio.PB$PB[1]))

MSFT$Ratio.PB$d.PB <- ts(c(NA, MSFT$Ratio.PB$PB - lag(MSFT$Ratio.PB$PB, -1)), start = c(1990, 1), frequency = 4)
MSFT$Ratio.PB$d.PB[is.nan(MSFT$Ratio.PB$d.PB)] <- NA
MSFT$Ratio.PB$d.PB[is.infinite(MSFT$Ratio.PB$d.PB)] <- NA

MSFT$Ratio.PB$d2.PB <- ts(c(rep(NA, 2), MSFT$Ratio.PB$PB - lag(MSFT$Ratio.PB$PB, -2)), start = c(1990, 1), frequency = 4)
MSFT$Ratio.PB$d2.PB[is.nan(MSFT$Ratio.PB$d.PB)] <- NA
MSFT$Ratio.PB$d2.PB[is.infinite(MSFT$Ratio.PB$d.PB)] <- NA

MSFT$Ratio.PB$d4.PB <- ts(c(rep(NA, 4), MSFT$Ratio.PB$PB - lag(MSFT$Ratio.PB$PB, -4)), start = c(1990, 1), frequency = 4)
MSFT$Ratio.PB$d4.PB[is.nan(MSFT$Ratio.PB$d.PB)] <- NA
MSFT$Ratio.PB$d4.PB[is.infinite(MSFT$Ratio.PB$d.PB)] <- NA


?factor
