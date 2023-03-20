x <- c(8, 5, 3, 2, 1)
y <- c(4, 5, 9, 12, 14)
plot(x,y, type = "b", main = "X VS Y")
abline(lm(y ~ x))

