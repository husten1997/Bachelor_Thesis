#Simulation

#libs#################################################################################################################################################
library(forecast)
library(data.table)
library(zoo)



#Variablen###########################################################################################################################################

#I ist die anzahl der wiederholungen
I <- 10000
I2 <- 1000
#Fensterbreite der Trendregression
window.size <- 12
#e_mean ist der mittelwert der Verteilung aus der e gezogen wird, weicht er von 0 ab, soll ein Steigungsparameter simmuliert werden
e_mean <- 0
#ar.sim => wenn TRUE wird statt einem RW Porzess ein AR Prozess mit beta = 0.99 Simuliert
ar.sim <- FALSE
#plottet alles in ein Fenster oder nicht
plot.in.one.window <- TRUE


#set.seed(336585)



#data.out ist die Ergebnis Table, beta speichert dabei den Betaschätzer, TS die t-Teststatistik
data.out <- data.table(beta = rep(NA, I), TS = rep(NA, I), VAR = rep(NA, I), Q95 = rep(NA, I))


#Simulation#########################################################################################################################################
for(i in c(1:I)){
  e <- rnorm(window.size, mean = e_mean)
  pr <- ts(cumsum(e))
  if(ar.sim){
    pr <- arima.sim(model = list(ar = 0.99), n = 12)
  }
  model <- tslm(pr ~ trend)
  
  data.out$beta[i] <- model$coefficients[2]
  data.out$TS[i] <- summary(model)$coefficients[2, 3]
  data.out$VAR[i] <- var(data.out$TS, na.rm = TRUE)
  data.out$Q95[i] <- quantile(data.out$TS, na.rm = TRUE, probs = c(0.95))
}

if(plot.in.one.window) par(mfrow = c(2, 2))
##Verteilung der t-TS##############################################################################################################################

if(!plot.in.one.window) x11()
plot(density(data.out$TS), main = paste("Verteilung der t-Teststatistik, Mean: ", mean(data.out$TS)), xlab = paste("95%: ", quantile(data.out$TS, probs = c(0.95))))
#lines(density(rt(100000, 1)), col = c("blue"))
lines(density(rnorm(100000, mean = mean(data.out$TS))), col = c("red"))
abline(v = mean(data.out$TS), col = c("grey"))



#MA mit extending Winoow uzm festzustellen wie schnell die TS gegen ihren endgültigen Wert konvergiert#############################################

if(!plot.in.one.window) x11()
part_sum <- cumsum(data.out$TS)
extending_mean <- part_sum /index(data.out$TS)
plot(extending_mean, type = c("l"), main = c("Konvergenz TS"), ylab = c("mean TS"))
#abline(h = mean(data.out$TS), col = c("grey"))




#Verteilung des Betaschätzers######################################################################################################################

plot(density(data.out$beta), main = paste("Verteilung des Beta Schätzers, Mean: ", mean(data.out$beta)), xlab = paste("95%: ", quantile(data.out$beta, probs = c(0.95))))
#lines(density(rt(100000, df = 1)), col = c("blue"))
lines(density(rnorm(100000, mean = e_mean)), col = c("red"))
abline(v = mean(data.out$beta), col = c("grey"))




#MA mit extending Winoow uzm festzustellen wie schnell der Betaschätzer gegen seinen endgültigen Wert konvergiert#################################

if(!plot.in.one.window) x11()
part_sum <- cumsum(data.out$beta)
extending_mean <- part_sum /index(data.out$beta)
plot(extending_mean, type = c("l"), main = c("Konvergenz beta"), ylab = c("mean beta"))
abline(h = e_mean, col = c("grey"))

if(plot.in.one.window) par(mfrow = c(1, 1))

if(!plot.in.one.window) x11()
plot(data.out$VAR, type = c("l"), main = c("Konvergenz VAR(TS)"), ylab = c("mean beta"))
abline(h = var(data.out$TS), col = c("grey"))

if(!plot.in.one.window) x11()
plot(data.out$Q95, type = c("l"), main = c("Konvergenz Q95(TS)"), ylab = c("mean beta"))
abline(h = var(data.out$TS), col = c("grey"))



#Simulation#########################################################################################################################################


I <- 1000
I2 <- 1000

data.out2 <- data.table(TS.ar = rep(NA, I2), TS.rw = rep(NA, I2))
data.out <- data.table(beta = rep(NA, I), TS = rep(NA, I))

for(i2 in c(1:I2)){
  for(i in c(1:I)){
    e <- rnorm(window.size, mean = e_mean)
    pr <- ts(cumsum(e))
    if(ar.sim){
      pr <- arima.sim(model = list(ar = 0.99), n = 12)
    }
    model <- tslm(pr ~ trend)

    data.out$TS[i] <- summary(model)$coefficients[2, 3]
  }
  data.out2$TS.rw[i2] <- quantile(data.out$TS, probs = c(0.95))
}

for(i2 in c(1:I2)){
  for(i in c(1:I)){
    e <- rnorm(window.size, mean = e_mean)
    pr <- arima.sim(model = list(ar = 0.9), n = 12)
    model <- tslm(pr ~ trend)
    
    data.out$TS[i] <- summary(model)$coefficients[2, 3]
  }
  data.out2$TS.ar[i2] <- quantile(data.out$TS, probs = c(0.95))
}

par(mfrow = c(1, 2))
plot(density(data.out2$TS.ar), main = c("Verteilung des 95%Q der TS bei AR"), xlab = paste("Mean: ", mean(data.out2$TS.ar)))
abline(v = mean(data.out2$TS.ar))
plot(density(data.out2$TS.rw), main = c("Verteilung des 95%Q der TS bei RW"), xlab = paste("Mean: ", mean(data.out2$TS.rw)))
abline(v = mean(data.out2$TS.rw))
par(mfrow = c(1, 1))
