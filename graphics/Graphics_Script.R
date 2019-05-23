#MSFT PB SD Analysis

png(filename = "graphics/MSFT_PB_sdPB.png", width = 720, height = 480)
plot(MSFT$Ratio.PB$PB, main = c("MSFT PB with SD(PB)"))
abline(h = mean(MSFT$Ratio.PB$PB, na.rm = TRUE))
s <- mean(MSFT$Ratio.PB$PB, na.rm = TRUE) + (2 * sd(MSFT$Ratio.PB$PB, na.rm = TRUE))
abline(h = s, col = c("orange"))

s <- mean(MSFT$Ratio.PB$PB, na.rm = TRUE) - (2 * sd(MSFT$Ratio.PB$PB, na.rm = TRUE))
abline(h = s, col = c("orange"))

s <- mean(MSFT$Ratio.PB$PB, na.rm = TRUE) + (3 * sd(MSFT$Ratio.PB$PB, na.rm = TRUE))
abline(h = s, col = c("red"))

s <- mean(MSFT$Ratio.PB$PB, na.rm = TRUE) - (3 * sd(MSFT$Ratio.PB$PB, na.rm = TRUE))
abline(h = s, col = c("orange"))

legend(x = "topleft", c("PB", "2 * SD(PB)", "3 * SD(PB)"), col = c("black", "orange", "red"), lty = c(1, 1, 1))
dev.off()


#MSFT PB SD Analysis
png(filename = "graphics/MSFT_PB_sde.png", width = 720, height = 480)
plot(MSFT$Ratio.PB$PB, main = c("MSFT PB with SD(e)"))
s <- MSFT$Ratio.PB$PB[1] + 2.851944 * sqrt(c(1:length(MSFT$Ratio.PB$PB)))
time <- ts(s, start = c(1990, 1))
lines(time, col = c("orange"))

s <- MSFT$Ratio.PB$PB[1] + 2.851944 * sqrt(2 *  c(1:length(MSFT$Ratio.PB$PB)))
time <- ts(s, start = c(1990, 1))
lines(time, col = c("red"))

s <- MSFT$Ratio.PB$PB[1] - 2.851944 * sqrt(c(1:length(MSFT$Ratio.PB$PB)))
time <- ts(s, start = c(1990, 1))
lines(time, col = c("orange"))

s <- MSFT$Ratio.PB$PB[1] - 2.851944 * sqrt(2 *  c(1:length(MSFT$Ratio.PB$PB)))
time <- ts(s, start = c(1990, 1))
lines(time, col = c("red"))
legend(x = "topleft", c("PB", "2 * SD(e)", "3 * SD(e)"), col = c("black", "orange", "red"), lty = c(1, 1, 1))
dev.off()

#MSFT MA in I(PB)
png(filename = "graphics/MSFT_PB_MA.png", width = 720, height = 480)
par(lwd = 1)
plot(MSFT$Ratio.PB$d.PB, main = c("MSFT PB with MA"))

ma <- c(rep(NA, 7), rollmean(MSFT$Ratio.PB$d.PB, k = 8))
ma <- ts(ma, start = c(1990, 1), frequency = 4)
lines(ma, col = c("orange"))
abline(h = 0)
par(lwd = 1)
dev.off()

#MSFT show why polymodel doesnt work
start <- 1990
envi <- MSFT
mod = "MAN"
P.a <- 0.999
P.b <- 0.051
B.a <- 0.584
B.b <- 0.157

l <- length(envi$Ratio.PB$PB)
result <- data.table(matrix(NA, nrow = l, ncol = 8))
colnames(result) <- c("time", "Intercept", "trend1", "trend2", "trend3", "trend4", "trend5", "trend6")
result$time <- seq(to = 2018.75, length.out =  l, by = 0.25)

result$fit_trend1 <- envi$Ratio.PB$PB
result$fit_trend2 <- envi$Ratio.PB$PB
result$fit_trend3 <- envi$Ratio.PB$PB
result$fit_trend4 <- envi$Ratio.PB$PB
result$fit_trend5 <- envi$Ratio.PB$PB
result$fit_trend6 <- envi$Ratio.PB$PB

envi$Ratio.PB$sig_trend3 <- 0
envi$Ratio.PB$sig_trend4 <- 0
envi$Ratio.PB$sig_trend5 <- 0
i <- 69
data <- data.table(P = window(envi$P.Data$P, end = c(start + (i+4)*0.25)))
data$B <- window(envi$B.Data$BPS_E, end = c(start + (i+4)*0.25))
P.model <- ets(window(envi$P.Data$P, end = c(start + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
B.model <- ets(window(envi$B.Data$BPS_E, end = c(start + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
PB.forecast <- (forecast(P.model)$mean / forecast(B.model)$mean)
data.PB <- data.table(PB = window(envi$Ratio.PB$PB, end = c(start + (i+4)*0.25), start = c(start + (i-8)*0.25)))
data.PB$PB[c((length(data.PB$PB)-4):(length(data.PB$PB)))] <- PB.forecast[c(1:5)]
data.PB$PB <- ts(data.PB$PB, end = c(start + (i+4)*0.25), frequency = 4)

Poly.model1 <- tslm(PB ~ I(trend), data = data.PB)
Poly.model2 <- tslm(PB ~ I(trend) + I(trend^2), data = data.PB)
Poly.model3 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3), data = data.PB)
Poly.model4 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4), data = data.PB)
Poly.model5 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5), data = data.PB)
Poly.model6 <- tslm(PB ~ I(trend) + I(trend^2) + I(trend^3) + I(trend^4) + I(trend^5)+ I(trend^6), data = data.PB)

plot(MSFT$Ratio.PB$PB, col = c("grey"), main = i)
lines(data.PB$PB, col = c("black"))

lines(Poly.model3$fitted.values, col = c("red"))

#AR comparission
png(filename = "graphics/AR_Comp.png", width = 720, height = 480)
set.seed(12345)
par(mfrow = c(2, 1))
ar1 <- arima.sim(n = 300, list(ar = c(0.001)))
plot(ar1, main = c("beta = 0.001"), ylab = c())
ar2 <- arima.sim(n = 300, list(ar = c(0.999)))
plot(ar2, main = c("beta = 0.999"), ylab = c())
par(mfrow = c(1, 1))
dev.off()

#Prices of all Companies
png(filename = "graphics/ALL_P.png", width = 720, height = 480)
par(mfrow = c(2, 2))
plot(MSFT$P.Data$P, xlim = c(1990, 2018), main = c("Microsoft"), ylab = c("Preis"))
plot(AAPL$P.Data$P, xlim = c(1990, 2018), main = c("Apple"), ylab = c("Preis"))
plot(ORCL$P.Data$P, xlim = c(1990, 2018), main = c("Oracle"), ylab = c("Preis"))
plot(IBM$P.Data$P, xlim = c(1990, 2018), main = c("IBM"), ylab = c("Preis"))
par(mfrow = c(1, 1))
dev.off()

#Price Data of MSFT
png(filename = "graphics/MSFT_P.png", width = 720, height = 480)
plot(MSFT$P.Data$P, xlim = c(1990, 2018), main = c("Microsoft"), ylab = c("Preis"))
dev.off()

#ACF and PACF of P of MSFT
png(filename = "graphics/MSFT_P_ACF_PACF.png", width = 480, height = 480)
par(mfrow = c(2, 1))
acf(MSFT$P.Data$P, main = c("ACF: Preis Microsoft"))
pacf(MSFT$P.Data$P, main = c("PACF: Preis Microsoft"))
par(mfrow = c(1, 1))
dev.off()

#ACF and PACF of B of MSFT
png(filename = "graphics/MSFT_B_ACF_PACF.png", width = 480, height = 480)
par(mfrow = c(2, 1))
acf(MSFT$B.Data$BPS_E, main = c("ACF: Buchwert Microsoft"))
pacf(MSFT$B.Data$BPS_E, main = c("PACF: Buchwert Microsoft"))
par(mfrow = c(1, 1))
dev.off()


#BPS_E Data of MSFT
png(filename = "graphics/MSFT_B.png", width = 720, height = 480)
plot(MSFT$B.Data$BPS_E, xlim = c(1990, 2018), main = c("Microsoft"), ylab = c("Buchwert des Eigenkapitals"))
dev.off()

#RW Simulation


#ggplot--------------------------------------------------------------------------------
#Price history MSFT
ggplot(MSFT$P.Data, aes(x = index(P), y = P)) +
  geom_line() +
  scale_x_continuous(minor_breaks = c(1990:2020), labels = c(seq(from = 1990, to = 2019, by = 5), 2019), breaks = c(seq(from = 1990, to = 2019, by = 5), 2019)) +
  labs(title = c("Microsoft Preis Daten"), x = c("Zeit"), y = c("Preis")) +
  theme_minimal()
ggsave("graphics/MSFT_P.png", width = 20, height = 10, units = "cm", dpi = 300)

#BPS_E Data of MSFT
ggplot(MSFT$B.Data, aes(x = index(BPS_E), y = BPS_E)) +
  geom_line() +
  scale_x_continuous(minor_breaks = c(1990:2020), labels = c(seq(from = 1990, to = 2019, by = 5), 2019), breaks = c(seq(from = 1990, to = 2019, by = 5), 2019)) +
  labs(title = c("Microsoft Buchwert des Eigenkapitals pro Aktie"), x = c("Zeit"), y = c("BPS")) +
  theme_minimal()
ggsave("graphics/MSFT_B.png", width = 20, height = 10, units = "cm", dpi = 300)

#PB Data of MSFT
ggplot(MSFT$Ratio.PB, aes(x = index(PB), y = PB)) +
  geom_line() +
  scale_x_continuous(minor_breaks = c(1990:2020), labels = c(seq(from = 1990, to = 2019, by = 5), 2019), breaks = c(seq(from = 1990, to = 2019, by = 5), 2019)) +
  labs(title = c("Microsoft Price - Book Ratio"), x = c("Zeit"), y = c("P/B")) +
  theme_minimal()
ggsave("graphics/MSFT_PB.png", width = 20, height = 10, units = "cm", dpi = 300)

#PB Data of MSFT
ggplot(MSFT$Ratio.PB, aes(x = index(PB), y = PB)) +
  geom_line(aes(x = index(PB), y = PB, color = redAlert), show.legend = FALSE) +
  scale_x_continuous(minor_breaks = c(1990:2020), labels = c(seq(from = 1990, to = 2019, by = 5), 2019), breaks = c(seq(from = 1990, to = 2019, by = 5), 2019)) +
  labs(title = c("Microsoft Price - Book Ratio"), x = c("Zeit"), y = c("P/B")) +
  scale_color_gradient(low = "black", high = "red") +
  theme_minimal()
ggsave("graphics/MSFT_PB_marked.png", width = 20, height = 10, units = "cm", dpi = 300)

#MSFT MA in I(PB)
ggplot(MSFT$Ratio.PB, aes(x = index(PB), y = d.PB)) +
  geom_line(aes(x = index(PB), y = d.PB), col = c("black")) +
  geom_line(aes(x = index(PB), y = ma), col = c("orange"), na.rm = TRUE) +
  geom_hline(yintercept = 0, col = c("darkgrey")) +
  scale_x_continuous(minor_breaks = c(1990:2020), labels = c(seq(from = 1990, to = 2019, by = 5), 2019), breaks = c(seq(from = 1990, to = 2019, by = 5), 2019)) +
  labs(title = c("Microsoft I(PB) mit einem MA"), x = c("Zeit"), y = c("P/B")) +
  theme_minimal()
ggsave("graphics/MSFT_PB_MA.png", width = 20, height = 10, units = "cm", dpi = 300)

#MSFT PB SD Analysis
plot(MSFT$Ratio.PB$PB, main = c("MSFT PB with SD(e)"))
s <- MSFT$Ratio.PB$PB[1] + 2.851944 * sqrt(c(1:length(MSFT$Ratio.PB$PB)))
time <- ts(s, start = c(1990, 1))
lines(time, col = c("orange"))

s <- MSFT$Ratio.PB$PB[1] + 2.851944 * sqrt(2 *  c(1:length(MSFT$Ratio.PB$PB)))
time <- ts(s, start = c(1990, 1))
lines(time, col = c("red"))

s <- MSFT$Ratio.PB$PB[1] - 2.851944 * sqrt(c(1:length(MSFT$Ratio.PB$PB)))
time <- ts(s, start = c(1990, 1))
lines(time, col = c("orange"))

s <- MSFT$Ratio.PB$PB[1] - 2.851944 * sqrt(2 *  c(1:length(MSFT$Ratio.PB$PB)))
time <- ts(s, start = c(1990, 1))
lines(time, col = c("red"))
legend(x = "topleft", c("PB", "2 * SD(e)", "3 * SD(e)"), col = c("black", "orange", "red"), lty = c(1, 1, 1))

ggpl <- ggplot(MSFT$Ratio.PB, aes(x = index(PB), y = PB)) +
  geom_line(aes(x = index(PB), y = PB, color = "PB")) +
  scale_x_continuous(minor_breaks = c(1990:2020), labels = c(seq(from = 1990, to = 2019, by = 5), 2019), breaks = c(seq(from = 1990, to = 2019, by = 5), 2019)) +
  labs(title = c("Microsoft PB mit verschiedenen Varianzen"), x = c("Zeit"), y = c("P/B")) +
  scale_color_manual(breaks = c("PB", "2 * SD(e)", "3 * SD(e)"), values = c("orange", "red", "black"), name = "Legende") +
  #scale_fill_continuous(name = "Title", labels = c("PB", "2 * SD(e)", "3 * SD(e)")) +
  theme_minimal()

s <- MSFT$Ratio.PB$PB[1] + 2.851944 * sqrt(c(1:length(MSFT$Ratio.PB$PB)))
time <- ts(s, start = c(1990, 1))
ggpl <- ggpl +  geom_line(aes(x = index(PB), y = time, color = "2 * SD(e)"), na.rm = TRUE)

s <- MSFT$Ratio.PB$PB[1] + 2.851944 * sqrt(2 *  c(1:length(MSFT$Ratio.PB$PB)))
time2 <- ts(s, start = c(1990, 1))
ggpl <- ggpl +  geom_line(aes(x = index(PB), y = time2, color = "3 * SD(e)"), na.rm = TRUE)

s <- MSFT$Ratio.PB$PB[1] - 2.851944 * sqrt(c(1:length(MSFT$Ratio.PB$PB)))
time3 <- ts(s, start = c(1990, 1))
ggpl <- ggpl +  geom_line(aes(x = index(PB), y = time3, color = "2 * SD(e)"), na.rm = TRUE)

s <- MSFT$Ratio.PB$PB[1] - 2.851944 * sqrt(2 *  c(1:length(MSFT$Ratio.PB$PB)))
time4 <- ts(s, start = c(1990, 1))
ggpl <- ggpl +  geom_line(aes(x = index(PB), y = time4, color = "3 * SD(e)"), na.rm = TRUE)

ggpl
ggsave("graphics/MSFT_PB_sdPB.png", width = 20, height = 10, units = "cm", dpi = 300)

#AR comparison 
set.seed(12345)
plot.data <- data.table(ar1 = arima.sim(n = 300, list(ar = c(0.001))), ar2 = arima.sim(n = 300, list(ar = c(0.999))))
ggpl1 <- ggplot(plot.data, aes(x = index(ar1), y)) +
  geom_line(aes(x = index(ar1), y = ar1)) +
  #scale_x_continuous(minor_breaks = c(1990:2020), labels = c(seq(from = 1, to = 300, by = 5), 2019), breaks = c(seq(from = 1990, to = 2019, by = 5), 2019)) +
  labs(title = c("beta = 0.001"), x = NULL, y = c("AR")) +
  theme_minimal()
ggpl2 <- ggplot(plot.data, aes(x = index(ar2), y)) +
  geom_line(aes(x = index(ar2), y = ar2)) +
  #scale_x_continuous(minor_breaks = c(1990:2020), labels = c(seq(from = 1, to = 300, by = 5), 2019), breaks = c(seq(from = 1990, to = 2019, by = 5), 2019)) +
  labs(title = c("beta = 0.999"), x = c("Zeit"), y = c("AR")) +
  theme_minimal()
grid.arrange(ggpl1, ggpl2, nrow = 2)
ggsave("graphics/AR_Comp.png", width = 20, height = 10, units = "cm", dpi = 300) 
