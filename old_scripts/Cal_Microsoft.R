

plot(MSFT_P_xts, col = c("black", "red", "green"), main = "MSFT_Price")
par(mfrow = c(2,1))
plot(MSFT_P_xts$d.P_MA, main = "MSFT: Change of the MA of the Price")
plot(MSFT_P_xts$d.P, main = "MSFT: Change of Price")

par(mfrow = c(1,1))
m <- mean(MSFT_P_xts$d.P_MA[!is.na(MSFT_P_xts$d.P_MA)])
s <- sd(MSFT_P_xts$d.P_MA[!is.na(MSFT_P_xts$d.P_MA)])
a <- 2
plot(density(MSFT_P_xts$d.P_MA[!is.na(MSFT_P_xts$d.P_MA)]), main = "Density of Price changes")
abline(v = m + a*s)
abline(v = m - a*s)


MSFT_P_ind <- index(MSFT_P_xts$d.P_MA[MSFT_P_xts$d.P_MA > (m + a*s)
                                   |MSFT_P_xts$d.P_MA < (m - a*s)])
plot_data <- xts(MSFT_P_xts$d.P_MA, order.by = MSFT_Dates$Date)
colnames(plot_data) <- c("a")
plot_data$b <- xts(MSFT_P_xts$d.P_MA, order.by = MSFT_Dates$Date)
plot_data$b[!(index(plot_data$b) %in% MSFT_P_ind)] <- NA
plot_data$a[index(plot_data$b) %in% MSFT_P_ind] <- NA
plot(plot_data, col = c("black", "green"), type = c("p"), main = "MSFT: Change of the MA of the Price")

plot.ext(MSFT_P_xts$d.P_MA, 2, c("test"))

par(mfrow = c(2,2), pch = 19)
plot(MSFT_P_xts, col = c("black", "red", "green"), main = "MSFT_Price")
plot(MSFT_P_xts$d.P_MA, main = "MSFT: Change of the MA of the Price")
plot(density(MSFT_P_xts$d.P_MA[!is.na(MSFT_P_xts$d.P_MA)]), main = "Density of Price changes")
abline(v = m + a*s, col = c("green"))
abline(v = m - a*s, col = c("green"))
p <- plot(plot_data, col = c("black", "green"), type = c("p"), main = "MSFT: Change of the MA of the Price")
#p <- addLegend("topright", legend.names = c("within 2 * sd", "outside the 2 * sd interval"), col = c("black", "green"), lty = c(1,1), lwd = c(2, 1), on = 1)
p



par(mfrow = c(2,1), pch = 19)
PB.mean <- mean(as.numeric(MSFT_Ratios.PB$PB), na.rm = TRUE)
PB.sd <- sd(as.numeric(MSFT_Ratios.PB$PB), na.rm = TRUE)
PB.a <- 2
pp <- plot.xts(MSFT_Ratios.PB, main = c("PB"))
pp <- lines(xts(rep(mean(as.numeric(MSFT_Ratios.PB$PB), na.rm = TRUE), nrow(MSFT_Ratios.PB$PB)), order.by = MSFT_Dates$Date), on = 1, col = c("orange"))
pp <- lines(xts(rep(PB.mean + PB.a * PB.sd, nrow(MSFT_Ratios.PB$PB)), order.by = MSFT_Dates$Date), on = 1, col = c("orange"))
pp <- lines(xts(rep(PB.mean - PB.a * PB.sd, nrow(MSFT_Ratios.PB$PB)), order.by = MSFT_Dates$Date), on = 1, col = c("orange"))
pp <- addEventLines(xts(rep(NA, length(MSFT_P_ind)), order.by = MSFT_P_ind), col = c(rgb(0, 255, 0, max = 255, alpha = 50)))
#pp <- addLegend("topright", legend.names = c("PB", "MA of PB", "difference between PB and MA of PB"))
pp
plot.xts(plot_data, col = c("black", "green"), type = c("p"), main = "MSFT: Change of the MA of the Price")



#WIP---------------------------------------------------------------------------------------------------------------------------------------------
MSFT_NivClean_Ind <- index(MSFT_Ratios.PB$NivCleanded[!is.na(MSFT_Ratios.PB$NivCleanded)])

PB_cor <- data.frame(matrix(NA, nrow = 730, ncol = 4))
colnames(PB_cor) <- c("a", "b", "pVal_a", "pVal_b")

data.xts <- xts(MSFT_Ratios.PB$NivCleanded[MSFT_NivClean_Ind], order.by = MSFT_NivClean_Ind)
colnames(data.xts) <- c("PB")
data.xts$P <- MSFT_P_xts$d.P_MA[MSFT_NivClean_Ind]

for(i in c(1:730)){
  #data.xts$P <- lag(MSFT_P_xts$d.P_MA[MSFT_NivClean_Ind], i)
  data.xts$PB <- lag(MSFT_Ratios.PB$NivCleanded[MSFT_NivClean_Ind], i)
  #data.xts$P[is.na(data.xts$P)] <- 0
  model <- lm(data.xts$P ~ data.xts$PB)
  s <- summary(model)
  PB_cor$a[i] <- if(s$coefficients[1,4] < 0.05) s$coefficients[1,1] else NA
  PB_cor$b[i] <- if(s$coefficients[2,4] < 0.05) s$coefficients[2,1] else NA
  PB_cor$pVal_a[i] <- s$coefficients[1,4]
  PB_cor$pVal_b[i] <- s$coefficients[2,4]
  PB_cor$AIC[i] <- AIC(model)
  PB_cor$BIC[i] <- BIC(model)
}
View(PB_cor)

par(mfrow = c(1, 1))
plot(PB_cor$AIC, type = c("l"))
lines(PB_cor$BIC, col = c("orange"))
abline(v = 65)

data.xts$PB <- lag(MSFT_Ratios.PB$NivCleanded[MSFT_NivClean_Ind], 65)
model <- lm(data.xts$P ~ data.xts$PB, na.action = na.exclude)
summary(model)

plot(ts(MSFT_P_xts$d.P_MA[MSFT_NivClean_Ind]))
lines(c(rep(NA, 65), ts(model$fitted.values)), col = c("red"))

plot(density(as.numeric(model$residuals[!is.na(model$residuals)])))
#hist(as.numeric(model$residuals[!is.na(model$residuals)]))
res.m <- mean(as.numeric(model$residuals[!is.na(model$residuals)]))
res.sd <- sd(as.numeric(model$residuals[!is.na(model$residuals)]))
res.l <- length(as.numeric(model$residuals[!is.na(model$residuals)]))
lines(density(rnorm(mean = res.m, sd = res.sd, n = res.l)), col = c("orange"))

Acf(as.numeric(model$residuals[!is.na(model$residuals)]))
Pacf(as.numeric(model$residuals[!is.na(model$residuals)]))

(model.autoarima <- auto.arima(ts(model$residuals[!is.na(model$residuals)])))

plot(ts(model$residuals[!is.na(model$residuals)]))
lines(model.autoarima$fitted, col = c("red"))

(model.stationary <- auto.arima(ts(model$residuals[!is.na(model$residuals)]), d = 0))

plot(ts(model$residuals[!is.na(model$residuals)]))
lines(model.stationary$fitted, col = c("red"))

#ADL----------------------------------------------------------------------------------------

MSFT_NivClean_Ind <- index(MSFT_Ratios.PB$NivCleanded[!is.na(MSFT_Ratios.PB$NivCleanded)])

AIC_BIC <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(AIC_BIC) <- c("AIC", "BIC")

data.xts <- xts(MSFT_Ratios.PB$NivCleanded[MSFT_NivClean_Ind], order.by = MSFT_NivClean_Ind)
colnames(data.xts) <- c("PB")
data.xts$P <- MSFT_P_xts$d.P_MA[MSFT_NivClean_Ind]

#Test for the stationary requirement of P and PB
acf(data.xts$P)
pacf(data.xts$P)
acf(data.xts$PB)
pacf(data.xts$PB)
#one cann see that both seams to be non-stationary processes => making both stationary (with difference opperator and adf test)

adf.test(data.xts$P)
data.xts$IP <- data.xts$P-lag(data.xts$P, 1)
adf.test(data.xts$IP)
acf(data.xts$IP, na.action = na.exclude)
pacf(data.xts$IP, na.action = na.exclude)

adf.test(data.xts$PB)
data.xts$IPB <- data.xts$PB-lag(data.xts$PB, 1)
adf.test(data.xts$IPB)
acf(data.xts$IPB, na.action = na.exclude)
pacf(data.xts$IPB, na.action = na.exclude)


model <- lm(data.xts$IP ~ data.xts$IPB + lag(data.xts$IPB, 1) + lag(data.xts$IPB, 2) + lag(data.xts$IP, 1) + lag(data.xts$IP, 2))
AIC(model)
BIC(model)

plot.data <- data.xts$P
colnames(plot.data) <- c("P")
plot.data$fit <- model$fitted.values
plot(plot.data, type = c("l"), col = c("orange", "black"))
lines(model$fitted.values[!is.na(model$fitted.values)])





real <- as.numeric(model$residuals[!is.na(model$residuals)])
theo <- rnorm(mean = res.m, sd = res.sd, n = res.l)
chisq.test(real, theo)




#plot(density(as.numeric(MSFT_Ratios.PB$PB), na.rm = TRUE))
#mean(as.numeric(MSFT_Ratios.PB$PB), na.rm = TRUE)
