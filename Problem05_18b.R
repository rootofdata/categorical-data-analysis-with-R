# Problem 5.18 b. Pearson GOF test.
dfr <- scan(what=list(city="", smoking=0, Yes=0, No=0))
Beijing	1	126	100
Beijing	0	35	61	
Shanghai	1	908	688	
Shanghai	0	497	807	
Shenyang	1	913	747	
Shenyang	0	336	598	
Nanjing	1	235	172	
Nanjing	0	58 121	
Harbin	1	402	308
Harbin	0	121	215
Zhengzhou	1	182	156
Zhengzhou	0	72	98
Taiyuan	1	60	99
Taiyuan	0	11	43
Nanchang	1	104	89
Nanchang	0	21	36

obj = glm(cbind(Yes, No) ~ city + smoking, data=dfr, family=binomial)
observed = dfr$Yes
fitted = fitted(obj)*(dfr$Yes + dfr$No) # 'fitted(obj)' returns the fitted probability of Y=1
(X.sq = sum((observed - fitted)^2/fitted)) # the value of the statistic of Pearson GOF test
pchisq(X.sq, df=obj$df.residual, lower.tail=FALSE) # p-value of Pearson GOF test
