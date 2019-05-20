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
par(mfrow = c(1, 2))
ar1 <- arima.sim(n = 300, list(ar = c(0.001)))
plot(ar1)
ar2 <- arima.sim(n = 300, list(ar = c(0.999)))
plot(ar2)
par(mfrow = c(1, 1))