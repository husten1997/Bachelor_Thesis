
#PB Forecasterror

#MSFT
PB.CV(MSFT)
MSFT$CV[order(MSFT$CV[,5])[1:5], ]

PB.CV(MSFT, r.P.a = c(0.8, 0.95), r.P.b = c(0.05, 0.15), r.B.a = c(0.8, 0.95), r.B.b = c(0.05, 0.15), step = 0.01)
MSFT$CV[order(MSFT$CV[,5])[1:5], ]

#AAPL
PB.CV(AAPL)
AAPL$CV[order(AAPL$CV[,5])[1:5], ]

PB.CV(MSFT, r.P.a = c(0.8, 0.95), r.P.b = c(0.05, 0.15), r.B.a = c(0.8, 0.95), r.B.b = c(0.025, 0.35), step = 0.01)
MSFT$CV[order(MSFT$CV[,5])[1:5], ]

#ORCL
PB.CV(ORCL)
ORCL$CV[order(ORCL$CV[,5])[1:5], ]

#IBM
PB.CV(IBM)
IBM$CV[order(IBM$CV[,5])[1:5], ]


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