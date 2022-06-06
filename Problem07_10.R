## Problem 7.10: loglinear model and logit model
c1 = c(1105,411111,4624,157342)
c2 = c(14,483,497,1008)
freq = c(c1, c2); n = sum(freq)
x = factor(c(1,1,2,2,1,1,2,2), levels=c(2,1), labels=c("none","seatbelt")) # Whether Seat Belt in Use
z = factor(c(1,2,1,2,1,2,1,2), levels=c(2,1), labels=c("No","Yes")) # Whether Ejected
y = factor(c(1,1,1,1,2,2,2,2), labels=c("Nonfatal","Fatal")) # Fatal Injury or not
## [a] Fit a loglinear model
fit.loglin = glm(freq ~ (x + y + z)^2, family=poisson)
1 - pchisq(fit.loglin$deviance, fit.loglin$df.residual) # p-value for GOF test is rather small. But be cautious of the huge sample size.
fit.indep = glm(freq ~ x + y + z, family=poisson)
1 - pchisq(fit.indep$deviance, fit.indep$df.residual) # p-value for GOF test
## [b] Fit an equvalent logit model
seatbelt = factor(c(1,1,2,2), levels=c(2,1), labels=c("notwear","wear"))
eject = factor(c(1,2,1,2), levels=c(2,1), labels=c("No","Yes")) # Fatal Injury or not
fit.logit = glm(cbind(c2, c1) ~ seatbelt + eject, family=binomial)
1 - pchisq(fit.logit$deviance, fit.logit$df.residual) # the same p-value
summary(fit.logit) # conditional odds of being killed decreases if you wear seatbelt, increases if you are ejected
## [c] Dissimilarity Index
(D = sum(abs(fit.loglin$y - fit.loglin$fitted.values))/(2*n)) # extremely small, so that the model is practically OK.
## Now we want to check the fitted values by the two models are equal
fit.loglin$fitted.values[5:8] # fitted counts of 'Fatal' by loglinear model
(c1 + c2)*fit.logit$fitted.values # fitted counts of 'Fatal' by logit model

