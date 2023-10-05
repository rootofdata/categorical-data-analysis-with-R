# We want to draw Fig. 4.2 and Fig. 4.3 in the same graph
setwd('C:/Users/dudtj/OneDrive - 숭실대학교 - Soongsil University/Desktop/대학자료/4-1/범주형 자료분석/code')
crab = read.table("crab.txt") # The 'crab.txt' file should be in the working directory
names(crab) = c("color","spine","width","satell","weight")
attach(crab)
y = as.numeric(satell > 0) #0 아님 1인 변수로 변환
plot(jitter(width), y, yaxp=c(0,1,1), xlab="Width", ylab="Presence of Satellites") #같은 값들을 jitter 함수로 변화를 줘서 다른 값이 찍히게끔 한다.
lines(lowess(width, y), col=4)
#obj.loess <- loess(y ~ width); y.loess = predict(obj.loess, crab); ord = order(width); lines(width[ord], y.loess[ord], col=4, lty=2)  # lines 안쓰고 하는 방법..
width.center = seq(22.75, 29.75)
sample.prop = c(0.36, 0.29, 0.61, 0.54, 0.68, 0.83, 0.83, 1)
points(width.center, sample.prop, pch=19, col=4)
obj.glm = glm(y ~ width, family=binomial)
summary(obj.glm)
crab
(EL50 = -coef(obj.glm)[1]/coef(obj.glm)[2]) # median effective level #등껍질의 넓이
pihat = predict(obj.glm, type="response") #적합값 #확률 -response  // a+bx 구할 때: link
ord = order(width)
lines(width[ord], pihat[ord], col=2)
# How to draw the 95% confidence band?
etahat = predict(obj.glm, type="link", se.fit=TRUE) #신뢰구간 구하기
eta.upper = etahat$fit + 2*etahat$se.fit 
eta.lower = etahat$fit - 2*etahat$se.fit 
pi.upper = 1/(1 + exp(-eta.upper)) 
pi.lower = 1/(1 + exp(-eta.lower))
#pihat = predict(obj.glm, type="response", se.fit=TRUE) #바로 신뢰구간 구하는방법.
#pi.upper = pihat$fit + 2*pihat$se.fit
#pi.lower = pihat$fit - 2*pihat$se.fit  #틀린방법은 아니나 확률을 정확히 구하려면 위 코드를 해야함
matlines(width[ord], cbind(pi.upper[ord], pi.lower[ord]), col=2, lty=2)
detach()
#################################################
#################### 실습2#######################
#################################################
# Section 4.3 When the predictors are categorical
## Table 4.4: Sections 4.3.1 - 4.3.3
tab4.4 = scan(what=list(race="", azt="", yes=0, no=0)) # or you can read data into R using 'read.table()' function
#White Yes 14 93 
#White No 32 81  
#Black Yes 11 52 
#Black No 12 43

## Conditional Independence Test and Homogeneous Association Test Using the Logistic Regression Model
dfr = data.frame(tab4.4)
obj1.glm = glm(cbind(yes, no) ~ azt + race, data=dfr, family=binomial) #성공횟수.실패횟수 ~설명변수들
summary(obj1.glm)
coef(summary(obj1.glm)) # Wald test for H_0: azt has no association with AIDS symptoms #azt가 y와 관련이 없다는 것을 기각.
# [Q] How to perform the likelihood ratio test for H_0 : coefficient of azt = 0 ?
# [A] Compare the 'Residual deviance' values of two models with and without azt term.
obj2.glm = glm(cbind(yes, no) ~ race, data=dfr, family=binomial)
x = deviance(obj2.glm) - deviance(obj1.glm); 1 - pchisq(x, df=1) # approximately chi-squared dist with df = 1 if H_0 is true.
# This is the conditional independence test. -> 조건부 독립
# The results are similar to Cochran-Mantel-Haenszel test.
obj3.glm = glm(cbind(yes, no) ~ azt + race + azt:race, data=dfr, family=binomial)
x = deviance(obj1.glm) - deviance(obj3.glm); 1 - pchisq(x, df=1) # The deviance for the saturated model is zero
# This test is the homogeneous association test. 
# The results are similar to Breslow-Day test.

## Cochran-Mantel-Haenszel Test and Breslow-Day Test
#partial.tab = xtabs(freq ~ ., data = cbind(expand.grid(azt=c("Yes","No"), symptoms=c("yes","no"), race=c("White","Black")), 
#                                            freq=c(14,32,93,81,11,12,52,43)))  # construct 2x2xK table
partial.tab = array(c(14,32,93,81,11,12,52,43), dim=c(2,2,2), 
                    dimnames=list(azt=c("Yes","No"), symptoms=c("yes","no"), race=c("White","Black"))) # construct 2x2xK table #앞 4개가 race=white일때.
mantelhaen.test(partial.tab) # Cochran-Mantel-Haenszel Test
#install.packages("DescTools") # Need this package for the Breslow-Day test
library(DescTools)
BreslowDayTest(partial.tab) # Breslow-Day Test 인종이 달라지더라도 y변수와의 관련성이 동질성인가? -> 0.2382로 귀무가설 기각하지 않음. 동질적이라고 봐도 무방.
#################################################
#################################################
## Section 4.4.3
crab = read.table("crab.txt") # The 'crab.txt' file should be in the working directory
names(crab) = c("color","spine","width","satell","weight")
attach(crab)
y = as.numeric(satell > 0)
class(color); table(color)
cc = factor(color, labels=c("1","2","3","4"))
class(cc); table(cc)
cc = relevel(cc, ref="4") # to print out Table 4.6 in the text
table(cc)
obj1 = glm(y ~ cc + width, family=binomial)
summary(obj1) # the same as Table 4.6
cs = ifelse(cc == "4", 0, 1)
class(cs); table(cs)
obj2 = glm(y ~ cs + width, family=binomial)
AIC(obj1, obj2) # cf. Section 5.1.5 for AIC
cs2 = as.numeric(cc) # assigning score of color as {1,2,3,4}
# cs2 = color - 1
obj3 = glm(y ~ cs2 + width, family=binomial)
AIC(obj1, obj2, obj3)

## Section 4.4.4 Allowing Interaction
obj4 = glm(y ~ cc + width + cc:width, family=binomial)
summary(obj4)
obj5 = glm(y ~ cs + width + cs:width, family=binomial)
summary(obj5)
(obj.anova = anova(obj2, obj5)) # compare two models without and with an interaction term
pchisq(obj.anova$Deviance[2], 1, lower.tail=FALSE) # The interaction term is 'not' significant
AIC(obj2, obj5) # AIC tells the same
detach(crab)

#################################################
#################05.04 실습######################
#################################################
setwd('C:/Users/dudtj/OneDrive - 숭실대학교 - Soongsil University/Desktop/대학자료/4-1/범주형 자료분석/code')
crab = read.table("crab.txt") # The 'crab.txt' file should be in the working directory
names(crab) = c("color","spine","width","satell","weight")
attach(crab)
y = as.numeric(satell > 0) #0 아님 1인 변수로 변환
class(color);table(color)
c=factor(color,labels=c('1','2','3','4'))
#c=factor(color,labels=c('ml','me','md','ma'))
class(c);table(c)
c= relevel(c,ref='4') #table4.6 #마지막 기저변수를 바꾸라. #4.11과 같이 3개의 
#c= relevel(c,ref='da') 
table(c) 
obj1 =glm(y~c+width,family = binomial)
summary(obj1)
cs=ifelse(c=='4',0,1)
#cs=ifelse(c=='da',0,1)
class(cs);table(cs)
obj2 = glm(y~cs+width,family = binomial)
AIC(obj1,obj2) #cf.section5.1.5

#실습
c=factor(color,labels=c('1','2','3','4'))
cs=as.numeric(c)
obj3=glm(y~cs2+width,family=binomial)
AIC(obj1,obj2,obj3)

#section 4.4.4 allowing interaction
obj4=glm(y~c+width+c:width,family=binomial)
obj5=glm(y~cs+width+cs:width,family=binomial)

#################################################
#################Problem 4.20####################
#################################################
dfr = scan(what=list(center="", trt="", success=0, failure=0)) # read Table 4.16 in a dataframe format
1 drug 11 25
1 control 10 27
2 drug 16 4
2 control 22 10
3 drug 14 5
3 control 7 12
4 drug 2 14
4 control 1 16
5 drug 6 11
5 control 0 12
6 drug 1 10
6 control 0 10
7 drug 1 4
7 control 1 8
8 drug 4 2
8 control 6 1
class(dfr) #list
# [a]
dfr = as.data.frame(dfr)
obj = glm(cbind(success,failure) ~ trt + center, data=dfr, family=binomial)
summary(obj)

# [b]
freq = c() # To make a 2x2x8 table
for(i in seq(1,16,by=2)) freq = c(freq, dfr$success[i:(i+1)], dfr$failure[i:(i+1)])
tab4.16 = array(freq, dim=c(2,2,8), dimnames=list(trt=c("drug","control"), response=c("success","failure"), center=paste(1:8)))
mantelhaen.test(tab4.16) #귀무가설을 기각할만한 증거가 있다. 약의 효과가 있다.
