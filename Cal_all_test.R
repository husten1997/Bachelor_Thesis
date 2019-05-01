#DONT RUN!!!!!!--------------------------------------------------------
#PB Forecasterror

#MSFT
PB.CV(MSFT)
MSFT$CV[order(MSFT$CV[,5])[1:5], ]

PB.CV(MSFT, r.P.a = c(0.88, 0.95), r.P.b = c(0.1, 0.15), r.B.a = c(0.85, 0.95), r.B.b = c(0.1, 0.15), step = 0.01)
MSFT$CV[order(MSFT$CV[,5])[1:5], ]

#AAPL
PB.CV(AAPL)
AAPL$CV[order(AAPL$CV[,5])[1:5], ]

PB.CV(MSFT, r.P.a = c(0.85, 0.95), r.P.b = c(0.1, 0.15), r.B.a = c(0.85, 0.95), r.B.b = c(0.27, 0.35), step = 0.01)
MSFT$CV[order(MSFT$CV[,5])[1:5], ]

#ORCL
PB.CV(ORCL)
ORCL$CV[order(ORCL$CV[,5])[1:5], ]

#IBM
PB.CV(IBM)
IBM$CV[order(IBM$CV[,5])[1:5], ]


#Save to Run----------------------------------------------------------------------
#MSFT Red Alert
MSFT$Ratio.PB$redAlert <- 0
MSFT$Ratio.PB$redAlert[c(29:41)] <- 1
MSFT$Ratio.PB$redAlert[c(64:75)] <- 1

MSFT$Ratio.PB$redAlert <- ts(MSFT$Ratio.PB$redAlert, start = c(1990, 1), frequency = 4)

plot(MSFT$Ratio.PB$PB, type = c("l"))
points(MSFT$Ratio.PB$PB, col = as.character(factor(MSFT$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

plot(MSFT$P.Data$P)
points(MSFT$P.Data$P, col = as.character(factor(MSFT$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

#AAPL Red Alert
AAPL$Ratio.PB$redAlert <- 0
AAPL$Ratio.PB$redAlert[c(34:43)] <- 1
AAPL$Ratio.PB$redAlert[c(59:75)] <- 1

AAPL$Ratio.PB$redAlert <- ts(AAPL$Ratio.PB$redAlert, start = c(1990, 1), frequency = 4)

plot(AAPL$Ratio.PB$PB, type = c("l"))
points(AAPL$Ratio.PB$PB, col = as.character(factor(AAPL$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

plot(AAPL$P.Data$P)
points(AAPL$P.Data$P, col = as.character(factor(AAPL$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

#ORCL Red Alert
ORCL$Ratio.PB$redAlert <- 0
ORCL$Ratio.PB$redAlert[c(38:45)] <- 1

ORCL$Ratio.PB$redAlert <- ts(ORCL$Ratio.PB$redAlert, start = c(1990, 1), frequency = 4)

plot(ORCL$Ratio.PB$PB, type = c("l"))
points(ORCL$Ratio.PB$PB, col = as.character(factor(ORCL$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

plot(ORCL$P.Data$P)
points(ORCL$P.Data$P, col = as.character(factor(ORCL$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

#IBM Red Alert
IBM$Ratio.PB$redAlert <- 0
IBM$Ratio.PB$redAlert[c(33:50)] <- 1
IBM$Ratio.PB$redAlert[c(68:73)] <- 1
IBM$Ratio.PB$redAlert[c(77:81)] <- 1
IBM$Ratio.PB$redAlert[c(87:105)] <- 1

IBM$Ratio.PB$redAlert <- ts(IBM$Ratio.PB$redAlert, start = c(1990, 1), frequency = 4)

plot(IBM$Ratio.PB$PB, type = c("l"))
points(IBM$Ratio.PB$PB, col = as.character(factor(IBM$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

plot(IBM$P.Data$P)
points(IBM$P.Data$P, col = as.character(factor(IBM$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

#MSFT Cal
MSFT.res <- fite.model(MSFT)
plot.result(MSFT, MSFT.res)

con.table(MSFT)


#AAPL Cal
AAPL.res <- fite.model(AAPL, P.a = 0.9, P.b = 0.11, B.a = 0.9, B.b = 0.31)
plot.result(AAPL, AAPL.res)

con.table(AAPL)


#ORCL cal
ORCL.res <- fite.model(ORCL, P.a = 0.9, P.b = 0.01, B.a = 0.5, B.b = 0.21, sta = 1990.25)
plot.result(ORCL, ORCL.res)

con.table(ORCL)


#IBM Cal
IBM.res <- fite.model(IBM, P.a = 0.9, P.b = 0.01, B.a = 0.9, B.b = 0.01)
plot.result(IBM, IBM.res)

con.table(IBM)

