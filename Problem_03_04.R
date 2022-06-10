#4.5 with table4.10
#a
data4.5<-data.frame(TD=c(0,1,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,1),
                 Temp=c(66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58))
obj.glm=glm(TD~Temp,data=data4.5,family=binomial)
summary(obj.glm)
#b
logit<-function(x){
  exp(15.0429-0.2322*x)
}
result<-function(x){
  logit(x)/(1+logit(x))
}
result(31)

#c
15.0429/0.2322
EL50 = -coef(obj.glm)[1]/coef(obj.glm)[2]
EL50

#e
#(i) Wald test
coef(summary(obj.glm))
(-0.2322)

#(ii)likelihood-ratio test
obj.glm$null.deviance-obj.glm$deviance
1-pchisq(7.95196,1)

#4.12
#(d)
#wald test
(2.4044/0.6006)^2
1-pchisq(16.02665,1)
#likelihood-ratio test
20.35-5.01
1-pchisq(15.34,1)

#4.13
1-pchisq(7,1)
9*19/160

#Problem4.16
data = data.frame(EI = rep(c(1, 0), each = 8),
                      SN = rep(rep(c(1,0), each = 4),times =2),
                      TF = rep(rep(c(1, 0), each = 2), times = 4),
                      JP = rep(c(1, 0), times = 8),
                      YES = c(10, 8, 5, 7, 3, 2, 4, 15, 17, 
                              3, 6, 4, 1, 5, 1,6),
                      NO = c(67, 34, 101, 72, 20, 16, 27, 65, 123, 
                             49, 132, 102, 12, 30, 30 ,73))
data
#(a)
obj.glm = glm(cbind(YES, NO) ~ ., data= data, family = binomial)
summary(obj.glm)

#Problem 4.24
data4.24 = data.frame(D = c(45, 15, 40, 83, 90, 25, 35, 65, 95, 35, 75, 45, 50, 75, 30, 25, 20, 
                            60, 70, 30, 60, 61, 65, 15, 20, 45, 15, 25, 15, 30, 40, 15, 135, 20, 40),
                      T = c(0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1),
                      Y = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0))
data4.24

#(a) Fit the model for main effect
obj.glm = glm(Y ~ T + D, data = data4.24, family = binomial)
summary(obj.glm)

#(b)
exp(0.06868)

#(c)
obj2.glm = glm(Y ~ T + D + T*D, data =data4.24, family = binomial)
summary(obj2.glm)

#(d)
deviance(obj.glm)
deviance(obj2.glm)
x = deviance(obj.glm) - deviance(obj2.glm)
df = obj.glm$df.residual - obj2.glm$df.residual;df
1-pchisq(x, df)

1-pchisq(1.817,1)
#연습문제 5.7
data5.7 = data.frame(EI = rep(c(1, 0), each = 8),
                     SN = rep(rep(c(1,0), each = 4),times =2),
                     TF = rep(rep(c(1, 0), each = 2), times = 4),
                     JP = rep(c(1, 0), times = 8),
                     YES = c(13, 11, 16, 19, 6, 4, 6, 23, 32, 9, 34, 29, 4, 9, 4, 22),
                     NO = c(64, 31, 89, 60, 17, 14, 25, 57, 108, 43, 104, 76, 9, 26, 27, 57))
data5.7
mod1 = glm(cbind(YES, NO) ~1, data = data5.7, family = binomial)
summary(mod1)
#df :  # of settings of explanatory variables - number of parameters
#df : 16 - 1 = 15(Residual df)

mod2 = glm(cbind(YES, NO)~., data = data5.7, family = binomial)
summary(obj.glm)
#df :  # of settings of explanatory variables - number of parameters
#df : 16 - 5 = 11(Residual df)
mod3 = glm(cbind(YES, NO) ~ .^2, data = data5.7, family = binomial)
summary(mod11)
#df :  # of settings of explanatory variables - number of parameters
#df : 16 - 11 = 5(Residual df)

mod4 = glm(cbind(YES, NO) ~ .^3, data = data5.7, family = binomial)
summary(mod15)
#df :  # of settings of explanatory variables - number of parameters
#df : 16 - 15 = 1(Residual df)

#(b)
# AIC : -2*(max log likelihood - number of parameters) = 2*number of paramerers - 2*max log likelihood
AIC(mod1,mod2,mod3,mod4)
mod1$aic ; mod5$aic; mod11$aic; mod15$aic
#(c)
# c = 0.55 < 0.7 -> 만족스럽지 못한 모형이다.

#연습문제5.8
a번 : pihat > 0.5 => y = 1 / pihat <= 0.5 => y = 0
b번 : sensitivity : 94/111 / specificity : 25/62 



#연습문제 5.9
data5.9 = data.frame(LI = c(8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 32, 34, 38),
                     NR = c(0, 0, 0, 0, 0, 1, 2, 1, 0, 1, 1, 0, 1, 2),
                     NC = c(2, 2, 3, 3, 3, 1, 3, 2, 1, 1, 1, 1, 1, 3))
obj.glm = glm(cbind(NR, NC- NR) ~ LI, data = data5.9, family = binomial)
summary(obj.glm)
obj.glm2 = glm(cbind(NR, NC- NR) ~ .^2, data = data5.9, family = binomial)
summary(obj.glm2)
y.fitted = predict(obj.glm, newdata = data5.9, type = "response")
y.fitted*data5.9$NC

#(b)
#deviance(M_0) - deviance(M_1) = 15.7 - 11.8 = 3.9 / df = 12 - 11 = 1
1 - pchisq(3.9, 1) # H_0 : 기각 => 두 모형의 차이가 있다.
#(d)
#p-value > 0.05 => 모형의 적합도 O.K

#연습문제5.14
data14_1<- data.frame(X = rep(c(0, 1, 2), each = 4),
                      Y = c(1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1))
data14_1
data14_2 <- data.frame(X = c(0, 1, 2),
                       YES = c(1, 2, 4),
                       NO = c(3, 2, 0))
data14_2
obj.1 <- glm(Y ~ X, data= data14_1, family = binomial)
obj.12 <- glm(Y ~ X - X, data= data14_1, family = binomial)
summary(obj.1)
summary(obj.12)

obj.2 <- glm(cbind(YES, NO) ~ X, data = data14_2, family = binomial)
summary(obj.2)
obj2.2 <- glm(cbind(YES, NO) ~ X - X, data= data14_2, family = binomial)
summary(obj2.2)

#연습문제5.18
.


data5.18 <- data.frame(Study = c("Beijing", "Beijing", "Shanghai", "Shanghai", "Shenyang", "Shenyang", "Nanjing", "Nanjing", "Harbin", "Harbin", "Zhengzhou", "Zhengzhou", "Taiyuan", "Taiyuan", "Nanchang", "Nanchang"),
                       Smoking = c(rep(c("YES", "NO"), times= 8)),
                       L_Y = c(126, 35, 908, 497, 913, 336, 235, 58, 402, 121, 182, 72, 60, 11, 104, 21),
                       L_N = c(100, 61, 688, 807, 747, 598, 172, 121, 308, 215, 156, 98, 99, 43, 89, 36))
data5.18
obj.glm = glm(cbind(L_Y, L_N) ~ ., data = data5.18, family = binomial)
summary(obj.glm)
1 - pchisq(obj.glm$deviance, df=obj.glm$df.residual)
library(ResourceSelection)
stan.resid = rstandard(obj.glm)
stan.resid

#연습문제 5.22
x = c(10, 20, 30, 40, 60, 70, 80, 90)
y = c(rep(0,4), rep(1, 4))
fit = glm(y ~ x, family = binomial)
summary(fit)

x.d = c(x, 50, 50)
y.d = c(y, 1, 0)
fit = glm(y.d~x.d, family = binomial)
summary(fit)
x.d = c(x, 49.9, 50.1)
y.d = c(y, 1, 0)
fit = glm(y.d~x.d, family = binomial)
summary(fit)
#연습문제5.26
data5.26 <- data.frame(y = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1), 
                       x1 = c(-1.9, -0.1, 0.8, 0.9, -5.6, -2.4, -2.0, -0.6, -0.1, 0.4, 1.1, -1.5, 0.5, 0.8, 2.3, -5.3, -2.3, -1.7, -0.5, -0.1, 0.7),
                       x2 = c(-5.3, -5.2, -3.0, 3.4, -13.1, 1.8, -5.7, -2.4, -10.2, -17.2, -4.5, 3.9, 27.5, -1.6, 23.4, -19.8, -7.4, -3.9, -14.5, -9.9, -10.7),
                       x3 = c(-43, -32, -12, 1, -1, -9, -7, -7, -5, -9, -15, -15, 8, -2, 14, -33, 4, 13, -12, -11, -10))
data5.26
attach(data5.26)
jitter(x1)
length(jitter(x1))
plot(jitter(x1), data5.26$y, yaxp = c(0, 1, 1))
lines(lowess(x1, data5.26$y), col = 1)
detach(data5.26)
obj1.glm = glm(y ~ x1, data = data5.26, family = binomial)
summary(obj1.glm)
obj2.glm = glm(y ~ x2, data = data5.26, family = binomial)
summary(obj2.glm)
obj3.glm = glm(y ~ x3, data = data5.26, family = binomial)
summary(obj3.glm)

obj.glm = glm(y ~ ., data = data5.26, family = binomial)
summary(obj.glm)

#Problem 5.8
data5.9 = data.frame(LI = c(8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 32, 34, 38),
                     NR = c(0, 0, 0, 0, 0, 1, 2, 1, 0, 1, 1, 0, 1, 2),
                     NC = c(2, 2, 3, 3, 3, 1, 3, 2, 1, 1, 1, 1, 1, 3))
data5.9
obj.glm = glm(cbind(NR, NC-NR) ~ LI, data = data5.9, family = binomial)
summary(obj.glm)
y.fitted = predict(obj.glm, newdata = data5.9, type = "response")
y.fitted
y.fitted*data5.9$NC

#Problem5.14
data14_Ung <- data.frame(X = rep(c(0, 1, 2), each = 4),
                         Y = c(1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1))
data14_Ung
data14_G <- data.frame(X = c(0, 1, 2),
                       YES = c(1, 2, 4),
                       NO = c(3, 2, 0))
data14_G

obj.Ung <- glm(Y ~ X, data= data14_Ung, family = binomial)
obj2.Ung <- glm(Y ~ X - X, data= data14_Ung, family = binomial)
summary(obj.Ung)
summary(obj2.Ung)

obj.G <- glm(cbind(YES, NO) ~ X, data = data14_G, family = binomial)
summary(obj.G)
obj2.G <- glm(cbind(YES, NO) ~ X - X, data= data14_G, family = binomial)
summary(obj2.G)

#Problem5.18
data5.18 <- data.frame(Study = c("Beijing", "Beijing", "Shanghai", "Shanghai", "Shenyang", "Shenyang", "Nanjing", "Nanjing", "Harbin", "Harbin", "Zhengzhou", "Zhengzhou", "Taiyuan", "Taiyuan", "Nanchang", "Nanchang"),
                       Smoking = c(rep(c("YES", "NO"), times= 8)),
                       L_Y = c(126, 35, 908, 497, 913, 336, 235, 58, 402, 121, 182, 72, 60, 11, 104, 21),
                       L_N = c(100, 61, 688, 807, 747, 598, 172, 121, 308, 215, 156, 98, 99, 43, 89, 36))
data5.18
obj.glm = glm(cbind(L_Y, L_N) ~ Smoking + Study, data = data5.18, family = binomial)
summary(obj.glm)

#(b) GoF 검정
1 - pchisq(obj.glm$deviance, df=obj.glm$df.residual)

#(c) Residuals
library(ResourceSelection)
stan.resid = rstandard(obj.glm)
stan.resid

#Problem5.26
data5.26 <- data.frame(y = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1), 
                       x1 = c(-1.9, -0.1, 0.8, 0.9, -5.6, -2.4, -2.0, -0.6, -0.1, 0.4, 1.1, -1.5, 0.5, 0.8, 2.3, -5.3, -2.3, -1.7, -0.5, -0.1, 0.7),
                       x2 = c(-5.3, -5.2, -3.0, 3.4, -13.1, 1.8, -5.7, -2.4, -10.2, -17.2, -4.5, 3.9, 27.5, -1.6, 23.4, -19.8, -7.4, -3.9, -14.5, -9.9, -10.7),
                       x3 = c(-43, -32, -12, 1, -1, -9, -7, -7, -5, -9, -15, -15, 8, -2, 14, -33, 4, 13, -12, -11, -10))
data5.26

#(a)
obj1.glm = glm(y ~ x1, data = data5.26, family = binomial)
summary(obj1.glm)
obj2.glm = glm(y ~ x2, data = data5.26, family = binomial)
summary(obj2.glm)
obj3.glm = glm(y ~ x3, data = data5.26, family = binomial)
summary(obj3.glm)

#(b)
obj.glm = glm(y ~ ., data = data5.26, family = binomial)
summary(obj.glm)
attach(data5.26)
jitter(x1)
length(jitter(x1))
plot(jitter(x1), data5.26$y, yaxp = c(0, 1, 1))
lines(lowess(x1, data5.26$y), col = 1)
detach(data5.26)

## Problem 7.10: loglinear model and logit model
c1 = c(1105,411111,4624,157342)
c2 = c(14,483,497,1008)
freq = c(c1, c2); n = sum(freq)
x = factor(c(1,1,2,2,1,1,2,2), levels=c(2,1), labels=c("none","seatbelt")) # Whether Seat Belt in Use
z = factor(c(1,2,1,2,1,2,1,2), levels=c(2,1), labels=c("No","Yes")) # Whether Ejected
y = factor(c(1,1,1,1,2,2,2,2), labels=c("Nonfatal","Fatal")) # Fatal Injury or not

## [a] Fit a loglinear model
fit.loglin = glm(freq ~ (x + y + z)^2, family=poisson)
summary(fit.loglin)
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
