library(readxl)
CD <- read_excel("Desktop/CollegeDistance.xls")
y <- CD$ed
x <- CD$dist
#task A
lmED <- lm(ed~dist, data = CD)
summary(lmED)
# estimated intercept = 13.95586; estimated slope = -0.07337
# Years of Education Completed = 13.95586 + (-0.07337) * distance
# It means that if colleges are built 10 miles closer to high schools,
# years of completed education increases by 0,073 years

#task B
13.95586 + (-0.07337) * 2 # = 13.80912 Bob lives in 20 miles away
13.95586 + (-0.07337) * 1 # = 13.88249 Bob lives in 10 miles away

#task C
# According to regression R^2 = 0.00745, so it explains only 0.7 % of the years

#task D
# 1.8 years