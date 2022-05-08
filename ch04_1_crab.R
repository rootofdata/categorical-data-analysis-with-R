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


####05.04 실습#####
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
``