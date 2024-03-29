#Plot MSFT------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(MSFT$P_xts, col = c("black", "red", "green"), main = c("Price"))
plot(MSFT$BV_xts$BPS_E, main = c("Equity Book Value"))
plot(MSFT$Ratios.PB, main = c("PB Ratio"))
plot(MSFT$Ratios.PE, main = c("PE Ratio"))


plot(MSFT$BV_xts$d.BPS_E, main = c("Change of Book Value per Share"))
plot.dens(MSFT$BV$d.BPS_E, a = 2, title = c("Density change of Book Value"), plot.norm = TRUE)

plot(MSFT$P_xts$d.P, main = c("Change of Price"))
plot.dens(dataset = MSFT$P_xts$d.P, a = 2, title = c("Density of Change in Price"))
par(mfrow = c(1,1))

plot.ext(MSFT$P.Data$d.P, a = 2, title = c("Test"))

#ggplot(data = MSFT$P_xts$P, aes(x=Index, y=P)) + geom_line()

#Plot AAPL------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(AAPL$P_xts, col = c("black", "red", "green"), main = c("AAPL: Price"))
plot(AAPL$BV_xts$BPS_E, main = c("AAPL: Equity Book Value"))
plot(AAPL$Ratios.PB, main = c("AAPL: PB Ratio"))
plot(AAPL$Ratios.PE, main = c("AAPL: PE Ratio"))


plot(AAPL$P_xts$d.P, main = c("AAPL: Change of Price"))
plot(AAPL$P_xts$d.P_MA, main = c("AAPL: Change of MA of Price"))
plot.dens(AAPL$P_xts$d.P_MA, a = 2, title = c("AAPL: Density of change in Price"), plot.norm = TRUE)

plot.ext(AAPL$P_xts$d.P_MA, a = 2, title = c("AAPL: Extrems of change in Prices"))
par(mfrow = c(1,1))

#Plot IBM------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(IBM$P_xts, col = c("black", "red", "green"), main = c("IBM: Price"))
plot(IBM$BV_xts$BPS_E, main = c("IBM: Equity Book Value"))
plot(IBM$Ratios.PB, main = c("IBM: PB Ratio"))
plot(IBM$Ratios.PE, main = c("IBM: PE Ratio"))


plot(IBM$P_xts$d.P, main = c("IBM: Change of Price"))
plot(IBM$P_xts$d.P_MA, main = c("IBM: Change of MA of Price"))
plot.dens(IBM$P_xts$d.P_MA, a = 2, title = c("IBM: Density of change in Price"), plot.norm = TRUE)

plot.ext(IBM$P_xts$d.P_MA, a = 2, title = c("IBM: Extrems of change in Prices"))
par(mfrow = c(1,1))

#Plot ORCL------------------------------------------------------------------------

par(mfrow = c(2,2))
plot(ORCL$P_xts, col = c("black", "red", "green"), main = c("ORCL: Price"))
plot(ORCL$BV_xts$BPS_E, main = c("ORCL: Equity Book Value"))
plot(ORCL$Ratios.PB, main = c("ORCL: PB Ratio"))
plot(ORCL$Ratios.PE, main = c("ORCL: PE Ratio"))


plot(ORCL$P_xts$d.P, main = c("ORCL: Change of Price"))
plot(ORCL$P_xts$d.P_MA, main = c("ORCL: Change of MA of Price"))
plot.dens(ORCL$P_xts$d.P_MA, a = 2, title = c("ORCL: Density of change in Price"), plot.norm = TRUE)

plot.ext(ORCL$P_xts$d.P_MA, a = 2, title = c("ORCL: Extrems of change in Prices"))
par(mfrow = c(1,1))


#Red Alert
#MSFT Red Alert
plot(MSFT$Ratio.PB$PB, type = c("l"))
points(MSFT$Ratio.PB$PB, col = as.character(factor(MSFT$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

plot(MSFT$P.Data$P)
points(MSFT$P.Data$P, col = as.character(factor(MSFT$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

#AAPL Red Alert
plot(AAPL$Ratio.PB$PB, type = c("l"))
points(AAPL$Ratio.PB$PB, col = as.character(factor(AAPL$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

plot(AAPL$P.Data$P)
points(AAPL$P.Data$P, col = as.character(factor(AAPL$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

#ORCL Red Alert
plot(ORCL$Ratio.PB$PB, type = c("l"))
points(ORCL$Ratio.PB$PB, col = as.character(factor(ORCL$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

plot(ORCL$P.Data$P)
points(ORCL$P.Data$P, col = as.character(factor(ORCL$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

#IBM Red Alert
plot(IBM$Ratio.PB$PB, type = c("l"))
points(IBM$Ratio.PB$PB, col = as.character(factor(IBM$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))

plot(IBM$P.Data$P)
points(IBM$P.Data$P, col = as.character(factor(IBM$Ratio.PB$redAlert, labels = c("green", "red"))))
abline(v = seq(from = 1990, length.out = 116, by = .25), col = c("grey"))
