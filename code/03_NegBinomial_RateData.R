#Problem 3.4 (a)
alcohol = rep(c(0, 0.5, 1.5, 4, 7), c(48+17066, 38+14464, 5+788, 1+176, 0 + 37))
malformation = rep(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),c(48, 17066, 38, 14464, 5, 788, 1, 176, 0, 37))
glm.obj1 = glm(malformation ~ alcohol, family = binomial(link = "identity"))
summary(glm.obj1)

#(b)
alcohol1 = rep(c(0, 1, 2, 3, 4), c(48+17066, 38+14464, 5+788, 1+176, 1 + 37))
malformation1 = rep(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),c(48, 17066, 38, 14464, 5, 788, 1, 176, 1, 37))
glm.obj2 = glm(malformation1 ~ alcohol1, family = binomial(link = "identity"))
summary(glm.obj2)


#(c)
alcohol2 = rep(c(0, 0.5, 1.5, 4, 7), c(48+17066, 38+14464, 5+788, 1+176, 1 + 37))
malformation2 = rep(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),c(48, 17066, 38, 14464, 5, 788, 1, 176, 1, 37))
glm.obj3 = glm(malformation2 ~ alcohol2, family = binomial(link = "logit"))
summary(glm.obj3)
glm.obj4 = glm(malformation2 ~ alcohol2, family = binomial(link = "probit"))
summary(glm.obj4)

#3.11
data_w = data.frame(rep(c("B", "A"), each = 10), c(9, 9, 8, 14, 8, 13, 11, 5, 7, 6, 8, 7, 6, 6, 3, 4, 7, 2, 3, 4))
names(data_w) = c("treatment", "imperfection")
obj.log = glm(imperfection ~ treatment, data = data_w, family = poisson(link = "log"))
summary(obj.log)

#3.12
data_w2 = data.frame(rep(c(rep(c(0,1), each = 5)), times =2))
names(data_w2) = "thickness"
data_w2
data_w3 = cbind(data_w, data_w2)
data_w3
obj.log2 = glm(imperfection ~ treatment + thickness, data = data_w3, family = poisson(link = "log"))
summary(obj.log2)
obj.log3 = glm(imperfection ~ treatment + thickness + treatment*thickness, data = data_w3, family = poisson(link = "log"))
summary(obj.log3)
AIC(obj.log2, obj.log3)

#3.20
#(a)
dfr = read.table("Table3_9.txt", header=T)
names(dfr) = c("age", "smoke", "personyears", "cdeaths") # not necessary
dfr.nonsmokers = subset(dfr, smoke=="Nonsmokers")
dfr.smokers = subset(dfr, smoke=="Smokers")
(rates.nonsmokers = with(dfr.nonsmokers, 1000*cdeaths/(personyears)))
(rates.smokers = with(dfr.smokers, 1000*cdeaths/(personyears)))
age.score = seq(40, 80, by=10)
plot(age.score, rates.nonsmokers/rates.smokers, type="b", xlab="Age", ylab="ratio of death rates" )
#(b)
obj1 = glm(cdeaths ~ age + smoke, family=poisson(), data=dfr, offset=log(personyears))
summary(obj1)
#(c)
age.variable = c(age.score, age.score) # age for nonsmokers and smokers
obj2 = glm(cdeaths ~ age + smoke + smoke:age.variable, family=poisson(), data=dfr, offset=log(personyears))
summary(obj2)
#(d)
LRT = deviance(obj1) - deviance(obj2) 
df.LRT = obj1$df.residual - obj2$df.residual 
1 - pchisq(LRT, df=df.LRT) 

