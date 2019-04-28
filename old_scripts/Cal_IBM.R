#WIP---------------------------------------------------------------------------------------------------------------------------------------------
IBM$NivClean_Ind <- index(IBM$Ratios.PB$NivCleanded[!is.na(IBM$Ratios.PB$NivCleanded)])

PB_cor <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(PB_cor) <- c("AIC", "BIC")

data.xts <- xts(IBM$Ratios.PB$NivCleanded[IBM$NivClean_Ind], order.by = IBM$NivClean_Ind)
colnames(data.xts) <- c("PB")
data.xts$P <- IBM$P_xts$d.P_MA[IBM$NivClean_Ind]

#find the best fitting model
for(i in c(1:730)){
  model <- lm(data.xts$P ~ lag(data.xts$PB, i))
  PB_cor$AIC[i] <- AIC(model)
  PB_cor$BIC[i] <- BIC(model)
}

(best_i <- order(PB_cor$AIC)[1])
(order(PB_cor$BIC)[1])

par(mfrow = c(1, 1))
plot(PB_cor$AIC, type = c("l"), main = c("AIC and BIC"))
lines(PB_cor$BIC, col = c("orange"))
abline(v = best_i)

#fitting of the best the model with the lowest AIC/BIC
data.xts$PB <- lag(IBM$Ratios.PB$NivCleanded[IBM$NivClean_Ind], 65)
model <- lm(data.xts$P ~ data.xts$PB, na.action = na.exclude)
summary(model)

AIC(model)
BIC(model)

plot(ts(IBM$P_xts$d.P_MA[IBM$NivClean_Ind]), main = c("Fitted Values"))
lines(c(rep(NA, 65), ts(model$fitted.values)), col = c("red"))

#residuals
plot.dens(as.numeric(model$residuals), title = c("Density of Residuals"), plot.norm = TRUE, plot.lines = FALSE)

par(mfrow = c(1, 2))
Acf(as.numeric(model$residuals[!is.na(model$residuals)]), main = c("ACF: Residuals"))
Pacf(as.numeric(model$residuals[!is.na(model$residuals)]), main = c("PACF: Residuals"))
par(mfrow = c(1, 1))

(model.autoarima <- auto.arima(ts(model$residuals[!is.na(model$residuals)])))

#plot(ts(model$residuals[!is.na(model$residuals)]), main = c("Residuals"))
#lines(model.autoarima$fitted, col = c("red"))

(model.stationary <- auto.arima(ts(model$residuals[!is.na(model$residuals)]), d = 0))

#plot(ts(model$residuals[!is.na(model$residuals)]), main = c("Residuals"))
#lines(model.stationary$fitted, col = c("red"))

#ADL----------------------------------------------------------------------------------------

IBM$NivClean_Ind <- index(IBM$Ratios.PB$NivCleaned[!is.na(IBM$Ratios.PB$NivCleaned)])

AIC_BIC <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(AIC_BIC) <- c("AIC", "BIC")

data.xts <- xts(IBM$Ratios.PB$NivCleaned[IBM$NivClean_Ind], order.by = IBM$NivClean_Ind)
colnames(data.xts) <- c("PB")
data.xts$P <- IBM$P_xts$d.P_MA[IBM$NivClean_Ind]

#Test for the stationary requirement of P and PB
par(mfrow = c(2, 2))
acf(data.xts$P, main = c("ACF: P"))
pacf(data.xts$P, main = c("PACF: P"))
acf(data.xts$PB, main = c("ACF: PB"))
pacf(data.xts$PB, main = c("PACF: PB"))

adf.test(data.xts$P)
kpss.test(data.xts$P)
data.xts$IP <- data.xts$P-lag(data.xts$P, 1)

adf.test(data.xts$IP[!is.na(data.xts$IP)])
kpss.test(data.xts$IP[!is.na(data.xts$IP)])
par(mfrow = c(1, 2))
acf(data.xts$IP, na.action = na.exclude, main = c("ACF: I(PB)"))
pacf(data.xts$IP, na.action = na.exclude, main = c("PACF: I(PB)"))
par(mfrow = c(1, 1))

adf.test(data.xts$PB)
kpss.test(data.xts$PB)
data.xts$IPB <- data.xts$PB-lag(data.xts$PB, 1)
adf.test(data.xts$IPB[!is.na(data.xts$IPB)])

#Fitting of the Model
model <- lm(data.xts$IP ~ data.xts$PB + lag(data.xts$PB, 1) + lag(data.xts$PB, 2) + lag(data.xts$IP, 1) + lag(data.xts$IP, 2))
summary(model)
AIC(model)
BIC(model)

par(mfrow = c(1, 1))
plot.data <- data.xts$IP
colnames(plot.data) <- c("P")
plot.data$fit <- c(rep(NA, 3), model$fitted.values)
#plot.data$fit <- model$fitted.values
#plot.data$res <- c(rep(NA, 3), abs(model$residuals))
plot(plot.data["1990/2000"], type = c("l"), col = c("black", "orange", "blue"), main = c("Fitted Values"))
plot(plot.data["2000/2010"], type = c("l"), col = c("black", "orange", "blue"), main = c("Fitted Values"))
plot(plot.data["2010/2019"], type = c("l"), col = c("black", "orange", "blue"), main = c("Fitted Values"))
#plot(c(rep(NA, 3), abs(model$residuals)), type = c("l"), col = c("blue"), main = c("Res"))

#Residuals
plot.dens(as.numeric(model$residuals), title = c("Density of Residuals"), plot.norm = TRUE, plot.lines = FALSE)
par(mfrow = c(1, 2))
acf(model$residuals, main = c("ACF: Residuen"))
pacf(model$residuals, main = c("PACF: Residuen"))
par(mfrow = c(1, 1))