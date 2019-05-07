#Start Script

rm(list = ls())
library(readxl)
library(xts)
library(forecast)
#library(aTSA)
library(tseries)
library(ggplot2)
#library(xlsx)
library(data.table)
library(car)

source("functions.R")
source("Import.R")



write.csv(file = "Data_Eikon/Quaterly/MSFT_P_Data.csv", MSFT$P.Data$P)
write.csv(file = "Data_Eikon/Quaterly/MSFT_B_Data.csv", MSFT$B.Data$BPS_E)
write.csv(file = "Data_Eikon/Quaterly/MSFT_PB_Data.csv", MSFT$Ratio.PB$PB)

write.csv(file = "Data_Eikon/Quaterly/AAPL_P_Data.csv", AAPL$P.Data$P)
write.csv(file = "Data_Eikon/Quaterly/AAPL_B_Data.csv", AAPL$B.Data$BPS_E)
write.csv(file = "Data_Eikon/Quaterly/AAPL_PB_Data.csv", AAPL$Ratio.PB$PB)

write.csv(file = "Data_Eikon/Quaterly/ORCL_P_Data.csv", ORCL$P.Data$P)
write.csv(file = "Data_Eikon/Quaterly/ORCL_B_Data.csv", ORCL$B.Data$BPS_E)
write.csv(file = "Data_Eikon/Quaterly/ORCL_PB_Data.csv", ORCL$Ratio.PB$PB)

write.csv(file = "Data_Eikon/Quaterly/IBM_P_Data.csv", IBM$P.Data$P)
write.csv(file = "Data_Eikon/Quaterly/IBM_B_Data.csv", IBM$B.Data$BPS_E)
write.csv(file = "Data_Eikon/Quaterly/IBM_PB_Data.csv", IBM$Ratio.PB$PB)
