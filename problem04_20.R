# Problem 4.20
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

# [a]
dfr = as.data.frame(dfr)
obj = glm(cbind(success,failure) ~ trt + center, data=dfr, family=binomial)
summary(obj)

# [b]
freq = c() # To make a 2x2x8 table
for(i in seq(1,16,by=2)) freq = c(freq, dfr$success[i:(i+1)], dfr$failure[i:(i+1)])
tab4.16 = array(freq, dim=c(2,2,8), dimnames=list(trt=c("drug","control"), response=c("success","failure"), center=paste(1:8)))
mantelhaen.test(tab4.16)
