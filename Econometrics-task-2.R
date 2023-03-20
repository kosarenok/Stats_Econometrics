library(readxl)
TR <- read_excel("Desktop/TeachingRatings.xls")
y <- TR$course_eval
x <- TR$beauty
# task A
plot(x, y, main = "Course_eval VS Beauty",
     xlab = "Professor's beauty", ylab = "Average course evaluation",
     pch = 19)
abline(lm(y~x))
# As we can see, there appears to be a weak positive relation
# task B

lmEval <- lm(course_eval~beauty, data = TR)
summary(lmEval)
# Estimate intercept = 3.99827; Slope intercept = 0.13300
# course_eval = 3.99827+ 0.13300 * beauty

# task C
3.99827+ 0.13300 * 0 # = 3.99827 Watson
sd(x) # 0.7886477
3.99827+ 0.13300 * 0.7886477 # = 4.10316 Stock

#task D
# A one standard deviation increase in beauty is expected to 
# increase course evaluation by 0.133 * 0.789 = 0.105, or 1/5 of a standard 
# deviation of course evaluations. The effect is small.
#task E
# R^2 = 0.03574, so it explains about 3.6 % of the sample data