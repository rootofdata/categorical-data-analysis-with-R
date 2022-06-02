## Section 5.2 MODEL CHECKING

## Section 5.2.2: GOF and the Deviance
tab4.4 = scan(what=list(race="", azt="", yes=0, no=0)) # or you can read data into R using 'read.table()' function
White Yes 14 93 
White No 32 81  
Black Yes 11 52 
Black No 12 43

obj.M = glm(cbind(yes, no) ~ azt + race, data=tab4.4, family=binomial)
1 - pchisq(obj.M$deviance, df=obj.M$df.residual) # the overall GOF test for the working model
#obj.S = glm(cbind(yes, no) ~ azt + race + azt:race, data=tab4.4, family=binomial) # saturated model
#(obj.anova = anova(obj.M, obj.S))
#1 - pchisq(obj.anova$Deviance[2], df=obj.anova$Df[2]) # the overall GOF test for the working model

## Section 5.2.3: Hosmer-Lemeshow Test. Why not the overall GOF test?
crab = read.table("crab.dat") # The 'crab.dat' file should be in the working directory
names(crab) = c("color","spine","width","satell","weight")
crab$color = as.factor(crab$color); crab$spine = as.factor(crab$spine)
crab$y = as.numeric(crab$satell > 0)
obj = glm(y ~ width, crab, family=binomial)
y.fitted = predict(obj, newdata=crab, type="response") # estimated probabiities of Y_i = 1, i=1,...,n
library(ResourceSelection)
hoslem.test(crab$y, y.fitted, g=10) # large p-value indicating an adequate fit
# How about GOF of the model 'width + color'?
obj.color = glm(y ~ width + color, crab, family=binomial)
y.fitted = predict(obj.color, newdata=crab, type="response") 
hoslem.test(crab$y, y.fitted, g=10) # large p-value indicating an adequate fit

## Section 5.2.4 ~ 5.2.5: Residuals for Logit Models
dfr = read.table("Graduate_Florida.txt")
names(dfr) = c("dept","gender","yes","no")
obj = glm(cbind(yes,no) ~ dept, dfr, family=binomial)
summary(obj)
1 - pchisq(obj$deviance, obj$df.residual)
plot(rstandard(obj))
obj = glm(cbind(yes,no) ~ dept + gender, dfr, family=binomial)
summary(obj)
plot(rstandard(obj))

## Section 5.2.6 ~ 5.2.7: Influence Diagnosics and an Example. 
#  Construction of Table 5.6
tab5.6 = scan(what=list(bp=0, size=0, disease=0)) 
111.5 156 3
121.5 252 17
131.5 284 12
141.5 271 16
151.5 139 12
161.5 85 8
176.5 99 16
191.5 43 8

obj.bp = glm(cbind(disease, size - disease) ~ bp, data=tab5.6, family=binomial)
summary(obj.bp)
1 - pchisq(obj.bp$deviance, df=obj.bp$df.residual) # the overall GOF test for the working model. An adequate overall fit.
(inf.meas = influence.measures(obj.bp)) # compare with Table 5.6
stan.resid = rstandard(obj.bp) # standardized residuals
fitted.disease = tab5.6$size*predict(obj.bp, type="response") 
data.frame(BP=tab5.6$bp, n=tab5.6$size, Observed=tab5.6$disease, 
           Fitted=fitted.disease, SResidual=stan.resid, inf.meas$infmat[,c(2,3,5,6)]) # Table 5.6, some columns are different

