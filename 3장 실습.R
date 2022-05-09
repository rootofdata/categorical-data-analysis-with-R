crab= read.table(".txt")
names(crab)=c('color','spine','width','satell','weight')
attach(crab)

plot(width,jitter(satell))
lines(lowess(width,satell),col=2) #lowess : 평활곡선

obj.log = glm(satell~width,data=crab,family=poisson(link='log'))
summary(obj.log)

#결과값 : residual deviance 가 작으면 작을수록 모형 적합이 잘된다.
#                 aic가 작으면 작을수록 모형 설명 잘한다.
#설명변수의 수가 많으면 패널티를 준다.

start.values=coef(glm(staell~width,data=crab,family=gaussian))
obj.iden=glm(satell~width,data=crab,family=poisson(link='identity'),start=start.values)
summary(obj.iden)

#fig3.6
plot(width,jitter(satell))
vec=order(width)
lines(width[vec],obj.log$fitted.values[vec],col=4) #blue line
lines(width[vec],obj.iden$fitted.values[vec],col=3) #green line

##section 3.3.3 overdispersion
#section3.3.4 glm with negative binomial random component
library(MASS)
summary(glm.nb(satell~width,data=crab)) #negative binomial 적용 함수 glm.nb 
#overdispersion 문제 발생 
#aic 757.29임 근데 log에서는 927임 -> overdispersion 문제가 발생한다.
#summary(glm(satell~width,data=crab,family=poisson(link='log')))

#section3.3.5 count regression for rate data
tab3_4=read.table('table.txt')
names(tab3_4)=c('year','km','train','y')
attach(tab3_4)
nyears=year-1975

#eda
plot(nyears,y/km); lines(lowess(nyears,y/km),col=2) #smoothing lines in the boundary
plot(nyears,log(y/lm)); lines(lowess(nyears,log(y/km)),col=2)
#fit glms
pois.obj = glm(y~offset(log(km))+nyears,family=poisson)
#or, pois.obj =glm(y~nyears,data=tab3_4,family=poisson,offset=log(km)) 
summary(pois.obj)
nb.obj=glm.nb(y~offset(log(km))+nyears,data=tab3_4) #fit a negative binomial
summary(nb.obj)
AIC(pois.obj,nb.obj) #model selection : aic가 낮은 모델 선택
plot(nyears,log(y/km))
abline(coef(pois.obj),col=3) #poisson fit in green line
abline(coef(nb.obj),col=4) #negative binomial fit in blue line

rate=y/km;glm(rate~nyears,family=poisson) #this is not working
detach(tab3_4)
