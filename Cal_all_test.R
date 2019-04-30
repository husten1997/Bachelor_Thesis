



MSFT.res <- fite.model(MSFT)
plot.result(MSFT, MSFT.res)

ftable(MSFT$Ratio.PB$sig_trend3, MSFT$Ratio.PB$redAlert)
ftable(MSFT$Ratio.PB$sig_trend4, MSFT$Ratio.PB$redAlert)
ftable(MSFT$Ratio.PB$sig_trend5, MSFT$Ratio.PB$redAlert)

AAPL.res <- fite.model(AAPL)
plot.result(AAPL, AAPL.res)

ftable(AAPL$Ratio.PB$sig_trend3, AAPL$Ratio.PB$redAlert)
ftable(AAPL$Ratio.PB$sig_trend4, AAPL$Ratio.PB$redAlert)
ftable(AAPL$Ratio.PB$sig_trend5, AAPL$Ratio.PB$redAlert)

ORCL.res <- fite.model(ORCL)
plot.result(ORCL, ORCL.res)

IBM.res <- fite.model(IBM)
plot.result(IBM, IBM.res)