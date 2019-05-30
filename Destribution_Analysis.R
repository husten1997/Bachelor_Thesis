
plot(density(MSFT$Ratio.PB$d.PB, na.rm = TRUE))
lines(density(AAPL$Ratio.PB$d.PB, na.rm = TRUE))
lines(density(ORCL$Ratio.PB$d.PB, na.rm = TRUE))
lines(density(IBM$Ratio.PB$d.PB, na.rm = TRUE))



deri <- data.table(matrix(NA, nrow = 4, ncol = 2))
colnames(deri) <- c("mean", "sd")
row.names(deri) <- c("MSFT", "AAPL", "ORCL", "IBM")
deri$sd <- c(
  sd(MSFT$Ratio.PB$d.PB, na.rm = TRUE),
  sd(AAPL$Ratio.PB$d.PB, na.rm = TRUE),
  sd(ORCL$Ratio.PB$d.PB, na.rm = TRUE),
  sd(IBM$Ratio.PB$d.PB, na.rm = TRUE)
)
deri$mean <- c(
  mean(MSFT$Ratio.PB$d.PB, na.rm = TRUE),
  mean(AAPL$Ratio.PB$d.PB, na.rm = TRUE),
  mean(ORCL$Ratio.PB$d.PB, na.rm = TRUE),
  mean(IBM$Ratio.PB$d.PB, na.rm = TRUE)
)

deri

deri <- data.table(matrix(NA, nrow = 4, ncol = 2))
colnames(deri) <- c("mean", "sd")
row.names(deri) <- c("MSFT", "AAPL", "ORCL", "IBM")
deri$sd <- c(
  sd(MSFT$Ratio.PB$PB, na.rm = TRUE),
  sd(AAPL$Ratio.PB$PB, na.rm = TRUE),
  sd(ORCL$Ratio.PB$PB, na.rm = TRUE),
  sd(IBM$Ratio.PB$PB, na.rm = TRUE)
)
deri$mean <- c(
  mean(MSFT$Ratio.PB$PB, na.rm = TRUE),
  mean(AAPL$Ratio.PB$PB, na.rm = TRUE),
  mean(ORCL$Ratio.PB$PB, na.rm = TRUE),
  mean(IBM$Ratio.PB$PB, na.rm = TRUE)
)

deri

plot(density(MSFT$Ratio.PB$PB, na.rm = TRUE))
lines(density(AAPL$Ratio.PB$PB, na.rm = TRUE))
lines(density(ORCL$Ratio.PB$PB, na.rm = TRUE))
lines(density(IBM$Ratio.PB$PB, na.rm = TRUE))


acf(MSFT$Ratio.PB$PB)
pacf(MSFT$Ratio.PB$PB)

kpss.test(MSFT$Ratio.PB$PB)
adf.test(MSFT$Ratio.PB$PB)

acf(MSFT$P.Data$P)
pacf(MSFT$P.Data$P)

#P Tests
table.out <- data.table(MSFT = rep(NA, 2), AAPL = rep(NA, 2), ORCL = rep(NA, 2), IBM = rep(NA, 2))
row.names(table.out) <- c("KPSS-Test", "ADF-Test")
table.out$MSFT[1] <- kpss.test(MSFT$P.Data$P)$p.value
table.out$MSFT[2] <- adf.test(MSFT$P.Data$P)$p.value
table.out$AAPL[1] <- kpss.test(AAPL$P.Data$P)$p.value
table.out$AAPL[2] <- adf.test(AAPL$P.Data$P)$p.value
table.out$ORCL[1] <- kpss.test(ORCL$P.Data$P)$p.value
table.out$ORCL[2] <- adf.test(ORCL$P.Data$P)$p.value
table.out$IBM[1] <- kpss.test(IBM$P.Data$P)$p.value
table.out$IBM[2] <- adf.test(IBM$P.Data$P)$p.value

write.csv(file = "outputfiles/P_Tests.csv", x = table.out)

#B Tests
table.out <- data.table(MSFT = rep(NA, 2), AAPL = rep(NA, 2), ORCL = rep(NA, 2), IBM = rep(NA, 2))
row.names(table.out) <- c("KPSS-Test", "ADF-Test")
table.out$MSFT[1] <- kpss.test(MSFT$B.Data$BPS_E)$p.value
table.out$MSFT[2] <- adf.test(MSFT$B.Data$BPS_E)$p.value
table.out$AAPL[1] <- kpss.test(AAPL$B.Data$BPS_E)$p.value
table.out$AAPL[2] <- adf.test(AAPL$B.Data$BPS_E)$p.value
table.out$ORCL[1] <- kpss.test(ORCL$B.Data$BPS_E)$p.value
table.out$ORCL[2] <- adf.test(ORCL$B.Data$BPS_E)$p.value
table.out$IBM[1] <- kpss.test(IBM$B.Data$BPS_E)$p.value
table.out$IBM[2] <- adf.test(IBM$B.Data$BPS_E)$p.value

write.csv(file = "outputfiles/B_Tests.csv", x = table.out)

#PB Tests
table.out <- data.table(MSFT = rep(NA, 2), AAPL = rep(NA, 2), ORCL = rep(NA, 2), IBM = rep(NA, 2))
row.names(table.out) <- c("KPSS-Test", "ADF-Test")
table.out$MSFT[1] <- kpss.test(MSFT$Ratio.PB$PB)$p.value
table.out$MSFT[2] <- adf.test(MSFT$Ratio.PB$PB)$p.value
table.out$AAPL[1] <- kpss.test(AAPL$Ratio.PB$PB)$p.value
table.out$AAPL[2] <- adf.test(AAPL$Ratio.PB$PB)$p.value
table.out$ORCL[1] <- kpss.test(ORCL$Ratio.PB$PB)$p.value
table.out$ORCL[2] <- adf.test(ORCL$Ratio.PB$PB)$p.value
table.out$IBM[1] <- kpss.test(IBM$Ratio.PB$PB)$p.value
table.out$IBM[2] <- adf.test(IBM$Ratio.PB$PB)$p.value

write.csv(file = "outputfiles/PB_Tests.csv", x = table.out)
