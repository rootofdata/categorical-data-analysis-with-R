# Section 4.3 When the predictors are categorical
## Table 4.4: Sections 4.3.1 - 4.3.3
tab4.4 = scan(what=list(race="", azt="", yes=0, no=0)) # or you can read data into R using 'read.table()' function
White Yes 14 93 
White No 32 81  
Black Yes 11 52 
Black No 12 43

## Conditional Independence Test and Homogeneous Association Test Using the Logistic Regression Model
dfr = data.frame(tab4.4)
obj1.glm = glm(cbind(yes, no) ~ azt + race, data=dfr, family=binomial)
summary(obj1.glm)
coef(summary(obj1.glm)) # Wald test for H_0: azt has no association with AIDS symptoms
# [Q] How to perform the likelihood ratio test for H_0 : coefficient of azt = 0 ?
# [A] Compare the 'Residual deviance' values of two models with and without azt term.
obj2.glm = glm(cbind(yes, no) ~ race, data=dfr, family=binomial)
x = deviance(obj2.glm) - deviance(obj1.glm); 1 - pchisq(x, df=1) # approximately chi-squared dist with df = 1 if H_0 is true.
   # This is the conditional independence test.
   # The results are similar to Cochran-Mantel-Haenszel test.
obj3.glm = glm(cbind(yes, no) ~ azt + race + azt:race, data=dfr, family=binomial)
x = deviance(obj1.glm) - deviance(obj3.glm); 1 - pchisq(x, df=1) # The deviance for the saturated model is zero
   # This test is the homogeneous association test. 
   # The results are similar to Breslow-Day test.

## Cochran-Mantel-Haenszel Test and Breslow-Day Test
#partial.tab = xtabs(freq ~ ., data = cbind(expand.grid(azt=c("Yes","No"), symptoms=c("yes","no"), race=c("White","Black")), 
#                                            freq=c(14,32,93,81,11,12,52,43)))  # construct 2x2xK table
partial.tab = array(c(14,32,93,81,11,12,52,43), dim=c(2,2,2), 
              dimnames=list(azt=c("Yes","No"), symptoms=c("yes","no"), race=c("White","Black"))) # construct 2x2xK table
mantelhaen.test(partial.tab) # Cochran-Mantel-Haenszel Test
install.packages("DescTools") # Need this package for the Breslow-Day test
library(DescTools)
BreslowDayTest(partial.tab) # Breslow-Day Test
