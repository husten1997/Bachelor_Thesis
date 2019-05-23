#Simulation
I <- 1000
data.out <- data.table(beta = rep(NA, I), TS = rep(NA, I))

set.seed(123456)

for(i in c(1:I)){
  e <- rnorm(12)
  #pr <- ts(cumsum(e))
  pr <- arima.sim(model = list(ar = 0.99), n = 2000)
  model <- tslm(pr ~ trend)
  
  data.out$beta[i] <- model$coefficients[2]
  data.out$TS[i] <- summary(model)$coefficients[2, 3]
}

(auto.arima(pr))
plot(density(data.out$TS))
plot(density(data.out$beta))
