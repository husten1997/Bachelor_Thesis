

plot(MSFT$P_xts, col = c("black", "red", "green"), main = "MSFT$Price")
par(mfrow = c(2,1))
plot(MSFT$P_xts$d.P_MA, main = "MSFT: Change of the MA of the Price")
plot(MSFT$P_xts$d.P, main = "MSFT: Change of Price")

par(mfrow = c(1,1))

plot.dens(MSFT$P_xts$d.P_MA, a = 2, title = c("Density of Price changes"))
plot.ext(MSFT$P_xts$d.P_MA, 2, c("MSFT: Change of the MA of the Price"))

par(mfrow = c(2,2), pch = 19)
plot(MSFT$P_xts, col = c("black", "red", "green"), main = "MSFT$Price")
plot(MSFT$P_xts$d.P_MA, main = "MSFT: Change of the MA of the Price")
plot.dens(MSFT$P_xts$d.P_MA, a = 2, title = c("Density of Price changes"))
p <- plot.ext(MSFT$P_xts$d.P_MA, 2, c("MSFT: Change of the MA of the Price"))
#p <- addLegend("topright", legend.names = c("within 2 * sd", "outside the 2 * sd interval"), col = c("black", "green"), lty = c(1,1), lwd = c(2, 1), on = 1)
p


par(mfrow = c(2,1), pch = 19)
extr_ind <- get.extrm_ind(MSFT$P_xts$d.P_MA)
pp <- plot.xts(MSFT$Ratios.PB, main = c("PB"))
pp <- addEventLines(xts(rep(NA, length(extr_ind[[1]])), order.by = extr_ind[[1]]), col = c(rgb(0, 255, 0, max = 255, alpha = 50)))
pp <- addEventLines(xts(rep(NA, length(extr_ind[[2]])), order.by = extr_ind[[2]]), col = c(rgb(255, 0, 0, max = 255, alpha = 50)))
pp
plot.ext(MSFT$P_xts$d.P_MA, a = 2, title = c("Extrems of change in Prices"))

#Simple PB Model---------------------------------------------------------------------------------------------------------------------------------------------
MSFT$NivClean_Ind <- index(MSFT$Ratios.PB$NivCleaned[!is.na(MSFT$Ratios.PB$NivCleaned)])

PB_cor <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(PB_cor) <- c("AIC", "BIC")

data.xts <- xts(MSFT$Ratios.PB$NivCleaned[MSFT$NivClean_Ind], order.by = MSFT$NivClean_Ind)
colnames(data.xts) <- c("PB")
data.xts$P <- MSFT$P_xts$d.P_MA[MSFT$NivClean_Ind]

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
data.xts$PB <- lag(MSFT$Ratios.PB$NivCleaned[MSFT$NivClean_Ind], 65)
model <- lm(data.xts$P ~ data.xts$PB, na.action = na.exclude)
summary(model)

AIC(model)
BIC(model)

plot(ts(MSFT$P_xts$d.P_MA[MSFT$NivClean_Ind]), main = c("Fitted Values"))
lines(c(rep(NA, 65), ts(model$fitted.values)), col = c("red"))

#Residuals
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

MSFT$NivClean_Ind <- index(MSFT$Ratios.PB$NivCleaned[!is.na(MSFT$Ratios.PB$NivCleaned)])

AIC_BIC <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(AIC_BIC) <- c("AIC", "BIC")

data.xts <- xts(MSFT$Ratios.PB$NivCleaned[MSFT$NivClean_Ind], order.by = MSFT$NivClean_Ind)
colnames(data.xts) <- c("PB")
data.xts$P <- MSFT$P_xts$d.P_MA[MSFT$NivClean_Ind]

#Test for the stationary requirement of P and PB
par(mfrow = c(2, 2))
acf(data.xts$P, main = c("ACF: P"))
pacf(data.xts$P, main = c("PACF: P"))
acf(data.xts$PB, main = c("ACF: PB"))
pacf(data.xts$PB, main = c("PACF: PB"))

adf.test(data.xts$P)
kpss.test(data.xts$P)
data.xts$IP <- data.xts$P-lag(data.xts$P, 1)
#data.xts$IP2 <- data.xts$IP-lag(data.xts$IP, 1)
#data.xts$IP3 <- data.xts$IP2-lag(data.xts$IP2, 1)

#(aut.arim <- auto.arima(ts(data.xts$IP)))
adf.test(data.xts$IP[!is.na(data.xts$IP)])
kpss.test(data.xts$IP[!is.na(data.xts$IP)])
par(mfrow = c(1, 2))
acf(data.xts$IP, na.action = na.exclude, main = c("ACF: I(PB)"))
pacf(data.xts$IP, na.action = na.exclude, main = c("PACF: I(PB)"))
par(mfrow = c(1, 1))

adf.test(data.xts$PB)
kpss.test(data.xts$PB)
data.xts$IPB <- data.xts$PB-lag(data.xts$PB, 1)
adf.test(data.xts$IPB)


#Fitting of the Model
#model <- lm(data.xts$P ~ data.xts$IPB + lag(data.xts$IPB, 1) + lag(data.xts$IPB, 2) + lag(data.xts$P, 1) + lag(data.xts$P, 2))
model <- lm(data.xts$P ~ data.xts$PB + lag(data.xts$PB, 1) + lag(data.xts$PB, 2) + lag(data.xts$P, 1) + lag(data.xts$P, 2))
summary(model)
AIC(model)
BIC(model)

par(mfrow = c(1, 1))
plot.data <- data.xts$P
colnames(plot.data) <- c("P")
#plot.data$fit <- c(rep(NA, 3), model$fitted.values)
plot.data$fit <- model$fitted.values
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
plot.data$res <- model$residuals
plot(plot.data$res)
plot(ts(plot.data$fit), ts(plot.data$res))
plot(ts(plot.data$fit), ts(plot.data$res^2))
bptest(model)
resettest(model)




#AR(2)-----------------------------------------------------------------------------------------------------
MSFT$NivClean_Ind <- index(MSFT$Ratios.PB$NivCleaned[!is.na(MSFT$Ratios.PB$NivCleaned)])

AIC_BIC <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(AIC_BIC) <- c("AIC", "BIC")

data.xts <- xts(MSFT$Ratios.PB$NivCleaned[MSFT$NivClean_Ind], order.by = MSFT$NivClean_Ind)
colnames(data.xts) <- c("PB")
data.xts$P <- MSFT$P_xts$d.P_MA[MSFT$NivClean_Ind]

model <- lm(data.xts$P ~ lag(data.xts$PB, 65) + lag(data.xts$PB, 66) + lag(data.xts$PB, 67))
summary(model)
AIC(model)
BIC(model)

plot.data <- data.xts$P
colnames(plot.data) <- c("P")
plot.data$fit <- model$fitted.values
plot(plot.data, type = c("l"), col = c("black", "orange"), main = c("Fitted Values"))

acf(model$residuals)
pacf(model$residuals)

#Simple PE Model--------------------------------------------------------------------------------------------

MSFT$NivClean_Ind <- index(MSFT$Ratios.PE$NivCleaned[!is.na(MSFT$Ratios.PE$NivCleaned)])

AIC_BIC <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(AIC_BIC) <- c("AIC", "BIC")

PE_cor <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(PE_cor) <- c("AIC", "BIC")

data.xts <- xts(MSFT$Ratios.PE$NivCleaned[MSFT$NivClean_Ind], order.by = MSFT$NivClean_Ind)
colnames(data.xts) <- c("PE")
data.xts$P <- MSFT$P_xts$d.P_MA[MSFT$NivClean_Ind]

#find the best fitting model
for(i in c(1:730)){
  model <- lm(data.xts$P ~ lag(data.xts$PE, i))
  PE_cor$AIC[i] <- AIC(model)
  PE_cor$BIC[i] <- BIC(model)
}

(best_i <- order(PE_cor$AIC)[1])
(order(PE_cor$BIC)[1])

par(mfrow = c(1, 1))
plot(PE_cor$AIC, type = c("l"), main = c("AIC and BIC"))
lines(PE_cor$BIC, col = c("orange"))
abline(v = best_i)

#fitting of the best the model with the lowest AIC/BIC
data.xts$PE <- lag(MSFT$Ratios.PE$NivCleaned[MSFT$NivClean_Ind], 52)
model <- lm(data.xts$P ~ data.xts$PE, na.action = na.exclude)
summary(model)

AIC(model)
BIC(model)
par(mfrow = c(1, 1))
plot(ts(MSFT$P_xts$d.P_MA[MSFT$NivClean_Ind]), main = c("Fitted Values"))
lines(c(rep(NA, 52), ts(model$fitted.values)), col = c("red"))

#residuals
plot.dens(as.numeric(model$residuals), title = c("Density of Residuals"), plot.norm = TRUE, plot.lines = FALSE)

par(mfrow = c(1, 2))
Acf(as.numeric(model$residuals[!is.na(model$residuals)]), main = c("ACF: Residuals"))
Pacf(as.numeric(model$residuals[!is.na(model$residuals)]), main = c("PACF: Residuals"))
par(mfrow = c(1, 1))

#ADL PE ----------------------------------------------------------------------------------------

MSFT$NivClean_Ind <- index(MSFT$Ratios.PE$NivCleaned[!is.na(MSFT$Ratios.PE$NivCleaned)])

AIC_BIC <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(AIC_BIC) <- c("AIC", "BIC")

data.xts <- xts(MSFT$Ratios.PE$NivCleaned[MSFT$NivClean_Ind], order.by = MSFT$NivClean_Ind)
colnames(data.xts) <- c("PE")
data.xts$P <- MSFT$P_xts$d.P_MA[MSFT$NivClean_Ind]

#Test for the stationary requirement of P and PE
par(mfrow = c(2, 2))
acf(data.xts$P, main = c("ACF: P"))
pacf(data.xts$P, main = c("PACF: P"))
acf(data.xts$PE, main = c("ACF: PE"))
pacf(data.xts$PE, main = c("PACF: PE"))

adf.test(data.xts$P)
kpss.test(data.xts$P)
data.xts$IP <- data.xts$P-lag(data.xts$P, 1)
#data.xts$IP2 <- data.xts$IP-lag(data.xts$IP, 1)
#data.xts$IP3 <- data.xts$IP2-lag(data.xts$IP2, 1)

#(aut.arim <- auto.arima(ts(data.xts$IP)))
adf.test(data.xts$IP[!is.na(data.xts$IP)])
kpss.test(data.xts$IP[!is.na(data.xts$IP)])
par(mfrow = c(1, 2))
acf(data.xts$IP, na.action = na.exclude, main = c("ACF: I(PE)"))
pacf(data.xts$IP, na.action = na.exclude, main = c("PACF: I(PE)"))
par(mfrow = c(1, 1))

adf.test(data.xts$PE)
kpss.test(data.xts$PE)
data.xts$IPE <- data.xts$PE-lag(data.xts$PE, 1)
adf.test(data.xts$IPE)


#Fitting of the Model
#model <- lm(data.xts$P ~ data.xts$IPE + lag(data.xts$IPE, 1) + lag(data.xts$IPE, 2) + lag(data.xts$P, 1) + lag(data.xts$P, 2))
model <- lm(data.xts$P ~ data.xts$PE + lag(data.xts$PE, 1) + lag(data.xts$PE, 2) + lag(data.xts$P, 1) + lag(data.xts$P, 2))
summary(model)
AIC(model)
BIC(model)

par(mfrow = c(1, 1))
plot.data <- data.xts$P
colnames(plot.data) <- c("P")
plot.data$fit <- model$fitted.values
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