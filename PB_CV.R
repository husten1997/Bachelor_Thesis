

PB.CV <- function(envi, r.P.a = c(0.5, 0.99), r.P.b = c(0.01, 0.5), r.B.a = c(0.5, 0.99), r.B.b = c(0.01, 0.5), step = 0.1){
  envi$CV <- matrix(NA, nrow = 0, ncol = 5)
  l <- length(envi$Ratio.PB$PB)
  for(P.a in seq(from = r.P.a[1], to = r.P.a[2], by = step)){
    for(P.b in seq(from = r.P.b[1], to = r.P.b[2], by = step)){
      for(B.a in seq(from = r.B.a[1], to = r.B.a[2], by = step)){
        for(B.b in seq(from = r.B.b[1], to = r.B.b[2], by = step)){
          print(c(P.a, P.b, B.a, B.b))
          #system.time(
          {
            e <- rep(NA, l)
            tryCatch({
              for(i in c(4:(l-5))){
                P.model <- ets(window(envi$P.Data$P, end = c(1990 + (i-1)*0.25)), model = mod, alpha = P.a, beta = P.b)
                B.model <- ets(window(envi$B.Data$BPS_E, end = c(1990 + (i-1)*0.25)), model = mod, alpha = B.a, beta = B.b)
                PB.forecast <- (forecast(P.model)$mean / forecast(B.model)$mean)
                e[i] <- (envi$Ratio.PB$PB[i] - PB.forecast[1])
              }
            }, error = function(a){
              e <- NA
            })
            mse <- mean(e^2, na.rm = TRUE)
            envi$CV <- rbind(envi$CV, c(P.a, P.b, B.a, B.b, mse))
          }
          #)
          
          
        }
      }
    }
  }
}

