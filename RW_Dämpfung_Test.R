

set.seed(123456)
e <- rnorm(10000, 0, 1)
x <- cumsum(e)

plot(x, type = c("l"))

#p <- 2* pnorm(-abs(e), mean = 0, sd = 1)
p <- 2 * dnorm(e)
wp_d <- e*p
wp_c <- e/2
wp_q <- (atan(5*e))*0.16

wx_d <- cumsum(wp_d)
wx_c <- cumsum(wp_c)
wx_q <- cumsum(wp_q)


#par(mfrow = c(2, 1))
plot(x, type = c("l"))
lines(wx_d, col = c("orange"))
lines(wx_c, col = c("green"))
lines(wx_q, col = c("red"))

#diff <- x - wx

#plot(diff, type = c("l"))
View(cbind(x, e, p, wp, wx))

par(mfrow = c(1,1))
plot(density(e), ylim = c(0, ceiling(max(density(wp)$y))))
lines(density(wp_d), col = c("orange"))
lines(density(wp_c), col = c("green"))
lines(density(wp_q), col = c("red"))
#Error Panaelty-----------------------------------------------------
e <- seq(-2, 2, by = .01)
p_d <- dnorm(e) * e
p_c <- e / 2
p_q <- (atan(5*e))*0.2

par(mfrow = c(1,1))
plot(e, p_c, type = c("l"))
lines(e, p_d, col = c("green"))
lines(e, p_q)

#test---------------------------------------------------------------
a <- rnorm(1000000, 0, 1)

b <- qnorm(pnorm(a), mean = 0, sd = 0.5)

c <- a*b

plot(density(a), ylim = c(0, ceiling(max(density(b)$y))))
lines(density(b), col = c("orange"))

a <- rnorm(1000000, 0, 1)

b <- pnorm(abs(a), mean = 0, sd = 1)
c <- a*b

plot(density(a), ylim = c(0, ceiling(max(density(c)$y))))
lines(density(c), col = c("orange"))
