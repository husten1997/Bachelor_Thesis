out.table <- data.table(matrix(NA, nrow = 116, ncol = 8))
colnames(out.table) <- c("PA", "PB", "Pl", "Pb", "BA", "BB", "Bl", "Bb")

PB.forec <- function(P.mod = "MAN", B.mod = "MAN", P.a, P.b, B.a, B.b){
  d.PB.forecast <- rep(NA, 116)
  P.forecast <- rep(NA, 116)
  B.forecast <- rep(NA, 116)
  for(i in c(12:115)){
    P.model <- ets(window(MSFT$P.Data$P, end = c(1990 + (i-1)*0.25)), model = "MAN", alpha = P.a, beta = P.b)
    B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), model = "MAN", alpha = B.a, beta = B.b)
    PB.forecast <- (forecast(P.model)$mean / forecast(B.model)$mean)
    d.PB.forecast[i] <- PB.forecast[5] - PB.forecast[1]
    P.forecast[i+1] <- forecast(P.model)$mean[1]
    B.forecast[i+1] <- forecast(B.model)$mean[1]
  }
  
  P.forecast <- ts(MSFT$P.Data$P.forecast, start = c(1990, 1), frequency = 4)
  B.forecast <- ts(MSFT$B.Data$B.forecast, start = c(1990, 1), frequency = 4)
  PB.forecast <- MSFT$P.Data$P.forecast / MSFT$B.Data$B.forecast
  e <- MSFT$Ratio.PB$PB - PB.forecast
  return(mean(e^2, na.rm = TRUE))
}
for(i in c(4:116)){
  P.model <- ets(window(MSFT$P.Data$P, end = c(1990 + (i-1)*0.25)), opt.crit = "mse", model = "MAN")
  B.model <- ets(window(MSFT$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), opt.crit = "mse", model = "MAN")
  out.table$PA[i] <- P.model$par[1]
  out.table$PB[i] <- P.model$par[2]
  out.table$Pl[i] <- P.model$par[3]
  out.table$Pb[i] <- P.model$par[4]
  out.table$Pmae[i] <- PB.forec(P.a = P.model$par[1], P.b = P.model$par[2], B.a = B.model$par[1], B.b = B.model$par[2])
  out.table$Pmodel[i] <- P.model$method
  
  out.table$BA[i] <- B.model$par[1]
  out.table$BB[i] <- B.model$par[2]
  out.table$Bl[i] <- B.model$par[3]
  out.table$Bb[i] <- B.model$par[4]
  out.table$Bmae[i] <- B.model$mse
  out.table$Bmodel[i] <- B.model$method
}

mean(out.table$PA, na.rm = TRUE)
mean(out.table$PB, na.rm = TRUE)
mean(out.table$Pl, na.rm = TRUE)
mean(out.table$Pb, na.rm = TRUE)

mean(out.table$BA, na.rm = TRUE)
mean(out.table$BB, na.rm = TRUE)
mean(out.table$Bl, na.rm = TRUE)
mean(out.table$Bb, na.rm = TRUE)

View(out.table)
plot(out.table$Bmae)