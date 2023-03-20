library(readxl)
library(dplyr)
library(AER)
library(plm)
library(stats)
library(stargazer)
Task_Four <- read_excel("/Users/kosarenok/Downloads/Task_Four.xlsx")

#1.
avg_emp <- mean(Task_Four$employed)
t.test(Task_Four$employed)$"conf.int"

#2.
emp_lm <- lm(employed ~ age + I(age^2), data = Task_Four)
summary(emp_lm)
emp_lm
0.3074929 + 20 * 0.028272 - 0.0003266 * (20^2)
0.3074929 + 40 * 0.028272 - 0.0003266 * (40^2)
0.3074929 + 60 * 0.028272 - 0.0003266 * (60^2)
#3.
emp_probit <- glm(employed ~ age + I(age^2), 
                  family = binomial(link = "probit"), 
                  data = Task_Four)
coeftest(emp_probit, vcov. = vcovHC, type = "HC1")
(-1.2579285) + 20 * 0.121723 - 0.0014125 * (20^2)
(-1.2579285) + 40 * 0.121723 - 0.0014125 * (40^2)
(-1.2579285) + 60 * 0.121723 - 0.0014125 * (60^2) 
#4.
emp_logit <- glm(employed ~ age  +  I(age^2), 
                 family = binomial(link = "logit"), 
                 data = Task_Four)
coeftest(emp_logit, vcov. = vcovHC, type = "HC1")
(-2.4897541) + 20 * 0.225466 - 0.0026237 * (20^2) 
(-2.4897541) + 40 * 0.225466 - 0.0026237 * (40^2)
(-2.4897541) + 60 * 0.225466 - 0.0026237 * (60^2)
#6.
emp_lm_1 <- lm(employed ~ age + I(age^2) + female + race + government + private + self + married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac,data = Task_Four)
summary(emp_lm_1)
emp_lm_2 <- lm(employed ~ age + I(age^2) + female + race + private + self + government + married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac,data = Task_Four)
summary(emp_lm_2)
emp_lm_3 <- lm(employed ~ age + I(age^2) + female + race + government + private + self + married + union + ne_states + ce_states + we_states + so_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac,data = Task_Four)
summary(emp_lm_3)
emp_lm_4 <- lm(employed ~ age + I(age^2) + female + race + government + private + self + married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_bac + educ_adv,data = Task_Four)
summary(emp_lm_4)

emp_probit_1 <- glm(employed ~ age + I(age^2) + female + race + government + private + self + married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac, 
                    family = binomial(link = "probit"), 
                    data = Task_Four)
coeftest(emp_probit_1, vcov. = vcovHC, type = "HC1")
emp_logit_1 <- glm(employed ~ age + I(age^2) + female + race + government + private + self + married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac, 
                   family = binomial(link = "logit"), 
                   data = Task_Four)
coeftest(emp_logit_1, vcov. = vcovHC, type = "HC1")
rob_se <- list(sqrt(diag(vcovHC(emp_lm, type = "HC1"))),
               sqrt(diag(vcovHC(emp_lm_1, type = "HC1"))),
               sqrt(diag(vcovHC(emp_probit, type = "HC1"))),
               sqrt(diag(vcovHC(emp_probit_1, type = "HC1"))),
               sqrt(diag(vcovHC(emp_logit, type = "HC1"))),
               sqrt(diag(vcovHC(emp_logit_1, type = "HC1"))))
stargazer(emp_lm, 
          emp_lm_1, 
          emp_probit, 
          emp_probit_1,
          emp_logit,
          emp_logit_1, 
          digits = 5,
          type = "text",
          se = rob_se)

#7.
avg_unemp <- mean(Task_Four$unemployed)
t.test(Task_Four$unemployed)$"conf.int"
unemp_lm <- lm(unemployed ~ age  +  I(age^2),data = Task_Four)
summary(unemp_lm)
unemp_probit <- glm(unemployed ~ age  +  I(age^2), 
                    family = binomial(link = "probit"), 
                    data = Task_Four)
coeftest(unemp_probit, vcov. = vcovHC, type = "HC1")
unemp_logit <- glm(unemployed ~ age  +  I(age^2), 
                   family = binomial(link = "logit"), 
                   data = Task_Four)
coeftest(unemp_logit, vcov. = vcovHC, type = "HC1")
unemp_lm_1 <- lm(unemployed ~ age + I(age^2) + female + race + government + private + self + married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac,data = Task_Four)
summary(unemp_lm_1)
unemp_probit_1 <- glm(unemployed ~ age + I(age^2) + female + race + government + private + self + married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac, 
                      family = binomial(link = "probit"), 
                      data = Task_Four)
coeftest(unemp_probit_1, vcov. = vcovHC, type = "HC1")
unemp_logit_1 <- glm(unemployed ~ age + I(age^2) + female + race + government + private + self + married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac, 
                     family = binomial(link = "logit"), 
                     data = Task_Four)
coeftest(unemp_logit_1, vcov. = vcovHC, type = "HC1")
rob_se <- list(sqrt(diag(vcovHC(unemp_lm, type = "HC1"))),
               sqrt(diag(vcovHC(unemp_lm_1, type = "HC1"))),
               sqrt(diag(vcovHC(unemp_probit, type = "HC1"))),
               sqrt(diag(vcovHC(unemp_probit_1, type = "HC1"))),
               sqrt(diag(vcovHC(unemp_logit, type = "HC1"))),
               sqrt(diag(vcovHC(unemp_logit_1, type = "HC1"))))
stargazer(unemp_lm, 
          unemp_lm_1, 
          unemp_probit, 
          unemp_probit_1,
          unemp_logit,
          unemp_logit_1, 
          digits = 5,
          type = "text",
          se = rob_se)
#8.
emp_probit_8 <- glm(employed ~ age + I(age^2) + female + race + government + private + self + married + female * married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac, 
                    family = binomial(link = "probit"), 
                    data = Task_Four)
coeftest(emp_probit_8, vcov. = vcovHC, type = "HC1")
emp_logit_8 <- glm(employed ~ age + I(age^2) + female + race + government + private + self + married + female * married + union + ne_states + so_states + ce_states + we_states + educ_lths + educ_hs + educ_somecol + educ_aa + educ_adv + educ_bac, 
                   family = binomial(link = "logit"), 
                   data = Task_Four)
coeftest(emp_logit_8, vcov. = vcovHC, type = "HC1")

