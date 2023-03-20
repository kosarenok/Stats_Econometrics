library(readxl)
Smoking <- read_excel("Smoking.xlsx")
#1
lm1<-lm(birthweight~smoker,data=Smoking)
summary(lm1)
confint(lm1)
#2
lm2<-lm(birthweight~smoker+alcohol+nprevist,data=Smoking)
summary(lm2)
confint(lm2)
#3
lm3<-lm(birthweight~smoker+alcohol+nprevist+unmarried,data=Smoking)
summary(lm3)
confint(lm3)
#A The estimated effect of smoking on birth weight equals to -252.23 grams in regression (1), equals to -217.58 grams in regression (2), equals to -175.377 grams in regression (3).
#B As we can see the 95% confidence intervals for the effect of smoking on birth weight is [-306.0736;-200.3831] in (1), is [-269.89226;-165.26789] in (2), is [-228.51092;-122.24290] in (3).
#C Yes, it seems so. Alcohol, Nprevist and Unmarried affect and determine the birthweight. According to the definition and regression performed in (1), 
# we can say that regression (1) suffers from omitted variable bias (because of the effect of Alcohol, Nprevist and Unmarried on Birthweight).
#D And regression(2) also suffers from the omitted variable bias (because of the effect of Unmarried on birthweight).
confint(lm3)
#E1 The 95% confidence interval of the coefficient Unmarried is [-238.1276,-136.1389].
#E2 The estimated coefficient is -187.133, which is within confidence interval. So it's statistically significant.
#E3 The magnitude is large, because the coefficient is large itself as well as the effect of it on the birthweight.
#E4 Unmarried women give birth to babies which on average weight 187.133 gram less than babies of married women. 
# I think that it happens so, because married women get more support from their husbands during pregnancy (both financial and psychological), 
# while unmarried women have more stress during pregnancy and can be not prepared for it.  I agree with the opinion of the family advocacy group.
lm4<-lm(birthweight~smoker+alcohol+nprevist+unmarried+educ,data=Smoking)
lm5<-lm(birthweight~smoker+alcohol+nprevist+tripre0+educ,data=Smoking)
lm6<-lm(birthweight~smoker+nprevist+unmarried+tripre0+age,data=Smoking)
library(stargazer)
library(AER)
rob_se <- list(sqrt(diag(vcovHC(lm1, type = "HC1"))),
               sqrt(diag(vcovHC(lm2, type = "HC1"))),
               sqrt(diag(vcovHC(lm3, type = "HC1"))),
               sqrt(diag(vcovHC(lm4, type = "HC1"))),
               sqrt(diag(vcovHC(lm5, type = "HC1"))),
               sqrt(diag(vcovHC(lm6, type = "HC1")))
               )
stargazer(lm1, lm2, lm3, lm4, lm5, lm6,
          digits = 3,
          header = F,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
          type="text",out="ch7question1.txt")
confint(lm6)
#F The regression lm_test6 is the best one, because it has the R2 and Adjusted R2 are the highest (0.091 and 0.090 respectively) and SER is the lowest (565.028). 
# The regression includes Smoker, Nprevist, Unmarried, Tripre0 and Age. The coefficient of Smoker can be noticed within boarders of all confidence intervals in B. 
# Reasonable confidence interval will be [-229.313364 -123.681751].
