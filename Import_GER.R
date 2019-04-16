library(readxl)
library(xts)
library(forecast)

VW_MV <- read_excel("Data_Eikon/German_Automotive/VW.xlsx", 
                    col_types = c("date", "numeric"))

VW_BV <- read_excel("Data_Eikon/German_Automotive/VW.xlsx", 
                    sheet = "Data", col_types = c("date", 
                                                  "numeric", "numeric", "numeric"))

VW_Dates <- data.frame(matrix(0, nrow = 9621, ncol = 6))
names(VW_Dates) <- c("Date", "PPS", "BV", "Shares", "BPS", "PB_Ratio")
VW_Dates[,1] <- seq(as.Date("1992-11-04"),length=9621,by="days")


VW_Dates[as.character.Date(VW_Dates[,1]) %in% as.character.Date(VW_MV$Date) ,2] <- as.numeric(rev(VW_MV$`Close(VOWG_p.DE)`))
VW_Dates[as.character.Date(VW_Dates[,1]) %in% as.character.Date(VW_BV$Date) ,3] <- as.numeric(rev(VW_BV$BV))
VW_Dates[as.character.Date(VW_Dates[,1]) %in% as.character.Date(VW_BV$Date) ,4] <- as.numeric(rev(VW_BV$Shares))
VW_Dates[as.character.Date(VW_Dates[,1]) %in% as.character.Date(VW_BV$Date) ,5] <- as.numeric(rev(VW_BV$BPS))

for(i in c(2:9621)){
  if(VW_Dates[i, 2] == 0) VW_Dates[i, 2] <- VW_Dates[i-1, 2]
  if(VW_Dates[i, 4] == 0) VW_Dates[i, 4] <- VW_Dates[i-1, 4]
  if(VW_Dates[i, 5] == 0) VW_Dates[i, 5] <- VW_Dates[i-1, 5]
}

#Price--------------------------------------------------------------------------------------
VW_P_xts <- xts(VW_Dates$PPS, order.by = VW_Dates$Date)
colnames(VW_P_xts) <- c("P")
VW_P_xts$MA <- xts(ma(VW_P_xts$P, 181), order.by = VW_Dates$Date)
VW_P_xts$MA2 <- xts(ma(VW_P_xts$P, 913), order.by = VW_Dates$Date)

#BV-----------------------------------------------------------------------------------------
VW_BV_xts <- xts(VW_Dates$BV, order.by = VW_Dates$Date)
colnames(VW_BV_xts) <- c("BV")
VW_BV_xts$BV_Forw <- VW_BV_xts$BV
for(i in c(2:9621)){
  if(VW_BV_xts$BV[i] == 0) VW_BV_xts$BV_Forw[i] <- VW_BV_xts$BV_Forw[i-1] 
}
VW_BV_xts$BV[VW_BV_xts$BV == 0] <- NA
VW_BV_xts$BV_Int[1] <- 0
VW_BV_xts$BV_Int <- xts(na.interp(VW_BV_xts$BV), order.by = VW_Dates$Date)

#PB-----------------------------------------------------------------------------------------
VW_PB_Forw_xts <- xts(VW_P_xts$P / VW_BV_xts$BV_Forw, order.by = VW_Dates$Date)
colnames(VW_PB_Forw_xts) <- c("PB_Ratio")
VW_PB_Forw_xts$PB_Ratio[VW_PB_Forw_xts$PB_Ratio == Inf] <- 0
VW_PB_Forw_xts$MA <- xts(c(rep(NA, 199), rollmeanr(VW_PB_Forw_xts$PB_Ratio, 200)), order.by = VW_Dates$Date)
VW_PB_Forw_xts$MA2 <- xts(c(rep(NA, 99), rollmeanr(VW_PB_Forw_xts$PB_Ratio, 100)), order.by = VW_Dates$Date)

#Plot---------------------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(VW_P_xts, col = c("black", "red", "green"), main = "Market Price")
plot(VW_BV_xts$BV_Forw, main = "Book Value")
plot(VW_PB_Forw_xts, main = "P/B Ratio")
