library(readxl)
qq <- read_excel("Downloads/cps12.xlsx")
lmAHE = lm(ahe~age, data = qq)     # task A
summary(lmAHE)

# The p-value for age is very small, so slope coefficient is statistically significant

t.test(x = cps12$age, y = cps12$ahe,
       conf.level = 0.90)
t.test(x = cps12$age, y = cps12$ahe,
       conf.level = 0.95)
t.test(x = cps12$age, y = cps12$ahe,
       conf.level = 0.99)