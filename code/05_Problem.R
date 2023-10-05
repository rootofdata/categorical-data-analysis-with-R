## Problem 5.9 (c)
logit.pi = function(x) -13.096 + 0.9625*x - 0.0160*x^2
curve(logit.pi(x), 0, 40)
## Problem 5.14
grouped = data.frame(x=0:2, yes=c(1,2,4), no=c(3,2,0))
ungrouped = data.frame(x=rep(0:2,c(4,4,4)), y=c(1,0,0,0,1,1,0,0,1,1,1,1))
M1.grouped = glm(cbind(yes,no) ~ x, data=grouped, family=binomial)  
M0.grouped = glm(cbind(yes,no) ~ 1, data=grouped, family=binomial)  
M1.ungrouped = glm(y ~ x, data=ungrouped, family=binomial)  
M0.ungrouped = glm(y ~ 1, data=ungrouped, family=binomial)  
## (a) The maximum log likelihood values 'depend' on the form of data entry 'in R'
c(logLik(M0.grouped), logLik(M0.ungrouped))  
c(logLik(M1.grouped), logLik(M1.ungrouped)) 
# but the difference does not depend on the form of data entry
logLik(M1.grouped) - logLik(M0.grouped)
logLik(M1.ungrouped) - logLik(M0.ungrouped) 
## (b) G^2(M) depend on the form of data entry. Why?
c(deviance(M0.grouped), deviance(M0.ungrouped))
c(deviance(M1.grouped), deviance(M1.ungrouped))
c(M0.grouped$df.residual, M0.ungrouped$df.residual)
c(M1.grouped$df.residual, M1.ungrouped$df.residual)
## (c) G^2(M_0 | M_1) do not depend on the form of data entry
deviance(M0.grouped) - deviance(M1.grouped) # = 2*(logLik(M1.grouped) - logLik(M0.grouped))
deviance(M0.ungrouped) - deviance(M1.ungrouped)
# What do you expect of the value of maximum likelihood of the 'saturated' model for ungrouped data?
(-2*logLik(M0.ungrouped) - deviance(M0.ungrouped))/2 # maximum likelihood = 1, so that maximum log likelihood = 0
# the value of maximum likelihood of the 'saturated' model for grouped data?
(-2*logLik(M0.grouped) - deviance(M0.grouped))/2 

