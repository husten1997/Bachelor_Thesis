MSFT <- new.env()
MSFT$BV <- read_excel("Data_Eikon/American_Electronics/Microsoft.xlsx", 
                      sheet = "Tabelle1", col_types = c("date", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric", "numeric"))
MSFT$MV <- read_excel("Data_Eikon/American_Electronics/Microsoft.xlsx", 
                      sheet = "Prices", col_types = c("date", 
                                                      "numeric"))

import(MSFT, "1986-03-13", "/1990-03-29", 12017)
 
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
import(AAPL, P_start_date = P_st_da, BV_start_date = BV_st_da, count_of_days = day_c)
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
import(IBM, P_start_date = P_st_da, BV_start_date = BV_st_da, count_of_days = day_c)

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
import(ORCL, P_start_date = P_st_da, BV_start_date = BV_st_da, count_of_days = day_c)




