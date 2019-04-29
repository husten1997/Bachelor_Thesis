
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
