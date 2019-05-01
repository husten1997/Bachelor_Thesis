MSFT$NivClean_Ind <- index(MSFT$Ratios.PB$NivCleanded[!is.na(MSFT$Ratios.PB$NivCleanded)])

AIC_BIC <- data.frame(matrix(NA, nrow = 730, ncol = 2))
colnames(AIC_BIC) <- c("AIC", "BIC")

data.xts <- xts(MSFT$Ratios.PB$NivCleanded[MSFT$NivClean_Ind], order.by = MSFT$NivClean_Ind)
colnames(data.xts) <- c("PB")
data.xts$P <- MSFT$P_xts$d.P_MA[MSFT$NivClean_Ind]

#Test for the stationary requirement of P and PB
par(mfrow = c(2, 2))
acf(data.xts$P, main = c("ACF: P"))
pacf(data.xts$P, main = c("PACF: P"))
acf(data.xts$PB, main = c("ACF: PB")
    
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
model <- lm(data.xts$IP ~ data.xts$PB + lag(data.xts$PB, 1) + lag(data.xts$PB, 2) + lag(data.xts$IP, 1) + lag(data.xts$IP, 2))
summary(model)
AIC(model)
BIC(model)

par(mfrow = c(1, 1))
plot.data <- data.xts$IP
colnames(plot.data) <- c("P")
plot.data$fit <- c(rep(NA, 3), model$fitted.values)
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



model <- lm(data.xts$P ~ data.xts$PB + lag(data.xts$PB, 1) + lag(data.xts$PB, 2) + lag(data.xts$P, 1) + lag(data.xts$P, 2))
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
