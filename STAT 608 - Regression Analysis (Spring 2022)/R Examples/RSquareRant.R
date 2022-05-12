x <- c(0, 1, 2, 3, 16, 20)
y <- c(0.5, 4, 6, 7, 12, 22)

plot(x,y)

m1 <- lm(y ~ x)
m2 <- lm(log(y) ~ x)

abline(m1)

x <- seq(0, 20, by=0.01)
preds <- predict(m2, interval="confidence", newdata=data.frame(x))
lines(x, exp(preds[,1]), lty=2, col="red")



x <- c(0, 3, 8, 13, 16, 20)
y <- c(0.1, 0.4, 2, 10, 15, 16)
plot(x,y)

m1 <- lm(y ~ x)
m2 <- lm(log(y) ~ x)

abline(m1)

x <- seq(0, 20, by=0.01)
preds <- predict(m2, interval="confidence", newdata=data.frame(x))
lines(x, exp(preds[,1]), lty=2, col="red")

