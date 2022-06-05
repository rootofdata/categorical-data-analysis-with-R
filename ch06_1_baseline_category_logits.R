## Section 6.1 LOGIT MODELS FOR NOMINAL RESPONSES
## Section 6.1.2 Example: Alligator Food Choice
dfr = scan(what=list(x=0, y="")) # Table 6.1
1.24 I 1.30 I 1.30 I 1.32 F 1.32 F 1.40 F 1.42 I 1.42 F
1.45 I 1.45 O 1.47 I 1.47 F 1.50 I 1.52 I 1.55 I 1.60 I
1.63 I 1.65 O 1.65 I 1.65 F 1.65 F 1.68 F 1.70 I 1.73 O
1.78 I 1.78 I 1.78 O 1.80 I 1.80 F 1.85 F 1.88 I 1.93 I
1.98 I 2.03 F 2.03 F 2.16 F 2.26 F 2.31 F 2.31 F 2.36 F
2.36 F 2.39 F 2.41 F 2.44 F 2.46 F 2.56 O 2.67 F 2.72 I
2.79 F 2.84 F 3.25 O 3.28 O 3.33 F 3.56 F 3.58 F 3.66 F
3.68 O 3.71 F 3.89 F

dfr = as.data.frame(dfr)
library(nnet)
dfr$y = factor(dfr$y, levels=c("O","F","I")) # The coeff. for the first class is set to zero in nnet
fit.M1 <- multinom(y ~ x, data=dfr) 
summary(fit.M1)
fit.M0 <- multinom(y ~ -x, data=dfr) # fit a model with only intercept
summary(fit.M0)
(dev = fit.M0$deviance - fit.M1$deviance)
(df = fit.M1$edf - fit.M0$edf) # difference of the number of parameters for the two model
1 - pchisq(dev, df) # Test of H_0: beta_1 = beta_2 = 0
predict(fit.M1, type="class")
prob.mat = predict(fit.M1, type="probs")
# To Draw Figure 6.1
plot(dfr$x, prob.mat[,1], type="l", xlim=range(dfr$x), ylim=c(0,1),
     xlab="Length of Alligator", ylab="Predicted Probability")
lines(dfr$x, prob.mat[,2])
lines(dfr$x, prob.mat[,3])

## simultaneous fitting vs separate fitting
#(dfr.small = subset(dfr, y=="F" | y=="O"))
#summary(multinom(y ~ x, data=dfr.small)) # NOT the same as the result of the simultaneous fitting 'fit.M1'

## Section 6.1.4 Example: Belief in Afterlife
mat = matrix(c(74,71,15,13,371,250,64,25,49,45,9,5), nrow=4) # The coeff. for the first class is set to zero in nnet
race = factor(c("W","W","B","B"), levels=c("B","W"))
gender = factor(c("F","M","F","M"), levels=c("M","F"))
fit.M <- multinom(mat ~ gender + race) 
summary(fit.M)
predict(fit.M, type="probs")[1,2] # the estimated probability of response 'Yes' on afterlife for white females
