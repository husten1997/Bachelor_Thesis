L <- 1000

set.seed(123456)
e <- rnorm(L, 0, 2)
#e <- rep(1, L)
x <- ts(cumsum(e))
I <- c(NA, x - lag(x, -1))


plot(x, type = c("l"))
lines(sd(e))
#lines(ts(sqrt(c(1:L))))
#lines(ts(-sqrt(c(1:L))))

p <- rep(NA, L)
p[1] <- 1
p2 <- rep(NA, L)
for(i in c(2: L)){
  p[i] <- p[i - 1] * 2 * pnorm(-abs(I[i]), sd = 1)
  p2[i] <- 2*pnorm(-abs(e[i]), sd = 1)
  #p[i] <- dnorm(x[i], sd = i)
}

plot(p, type = c("l"))
#lines(p2, type = c("l"), col = c("orange"))

hist(rnorm(10000000, 0, 1), breaks = 8, probability = TRUE)


R <- 1000
L <- 1000

e <- rnorm(R)
x <- cimsum(e)
plot(x)
for()