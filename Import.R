MSFT <- new.env()
MSFT$BV <- read_excel("Data_Eikon/American_Electronics/Microsoft.xlsx", 
                      sheet = "Tabelle1", col_types = c("date", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric", "numeric"))
MSFT$MV <- read_excel("Data_Eikon/American_Electronics/Microsoft.xlsx", 
                      sheet = "Prices", col_types = c("date", 
                                                      "numeric"))

import(MSFT, start_d = c(1990, 1), end_d = c(2018, 4), c(1987, 1))
 
#AAPL---------------------------------------------------------------------------------------------
AAPL <- new.env()
AAPL$MV <- read_excel("Data_Eikon/American_Electronics/Apple.xlsx", 
                      sheet = "Prices", col_types = c("date", 
                                                      "numeric"))

AAPL$BV <- read_excel("Data_Eikon/American_Electronics/Apple.xlsx", 
                      sheet = "Data", col_types = c("date", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", "numeric"))
P_st_da <- "1980-12-12"
BV_st_da <- "/1989-09-28"
day_c <- 13934
import(AAPL, start_d = c(1990, 1), end_d = c(2018, 4), start_p = c(1981, 1))
#p.overview(AAPL)

#IBM---------------------------------------------------------------------------------------------------

IBM <- new.env()

IBM$MV <- read_excel("Data_Eikon/American_Electronics/IBM.xlsx", 
                     sheet = "Prices", col_types = c("date", 
                                                     "numeric"))

IBM$BV <- read_excel("Data_Eikon/American_Electronics/IBM.xlsx", 
                     sheet = "Data", col_types = c("date", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric"))

P_st_da <- "1980-03-17"
BV_st_da <- "/1989-12-30"
day_c <- 14204
import(IBM, start_d = c(1990, 1), end_d = c(2018, 4), start_p = c(1981, 1))

#ORCL---------------------------------------------------------------------------------------------------

ORCL <- new.env()

ORCL$MV <- read_excel("Data_Eikon/American_Electronics/Oracle.xlsx", 
                      sheet = "Prices", col_types = c("date", 
                                                      "numeric"))

ORCL$BV <- read_excel("Data_Eikon/American_Electronics/Oracle.xlsx", 
                      sheet = "Data", col_types = c("date", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", "numeric"))

P_st_da <- "1986-03-12"
BV_st_da <- "/1990-05-30"
day_c <- 12018
import(ORCL, start_d = c(1990, 2), end_d = c(2018, 4), start_p = c(1987, 1))




#Red Alert----------------------------------------------------------------------------------------------
#MSFT Red Alert
MSFT$Ratio.PB$redAlert <- 0
MSFT$Ratio.PB$redAlert[c(29:41)] <- 1
MSFT$Ratio.PB$redAlert[c(64:75)] <- 1

MSFT$Ratio.PB$redAlert <- ts(MSFT$Ratio.PB$redAlert, start = c(1990, 1), frequency = 4)

#AAPL Red Alert
AAPL$Ratio.PB$redAlert <- 0
AAPL$Ratio.PB$redAlert[c(34:43)] <- 1
AAPL$Ratio.PB$redAlert[c(59:75)] <- 1

AAPL$Ratio.PB$redAlert <- ts(AAPL$Ratio.PB$redAlert, start = c(1990, 1), frequency = 4)

#ORCL Red Alert
ORCL$Ratio.PB$redAlert <- 0
ORCL$Ratio.PB$redAlert[c(38:45)] <- 1

ORCL$Ratio.PB$redAlert <- ts(ORCL$Ratio.PB$redAlert, start = c(1990, 1), frequency = 4)

#IBM Red Alert
IBM$Ratio.PB$redAlert <- 0
IBM$Ratio.PB$redAlert[c(33:50)] <- 1
IBM$Ratio.PB$redAlert[c(68:73)] <- 1
IBM$Ratio.PB$redAlert[c(77:81)] <- 1
IBM$Ratio.PB$redAlert[c(87:105)] <- 1

IBM$Ratio.PB$redAlert <- ts(IBM$Ratio.PB$redAlert, start = c(1990, 1), frequency = 4)