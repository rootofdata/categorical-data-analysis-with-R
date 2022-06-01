## SECTION 6.2 Proportional Odds Model
# We need to transform the aggregated data in Table 6.7 into the raw data
freq = scan()
44 18 36 12 47 28 34 18 118 86 53 62 23 39 18 45 32 48 23 51

freq.mat = matrix(freq, nrow=4)
# proportional odds model을 적용할 때 grouped data를 
# 1. raw data로 만들어 모형을 적용하는 방법과
# 2. weights를 부여해 모형을 적용하는 방법 두 가지가 있다.
# 방법 1
gender = c(rep("female", sum(freq.mat[1:2,])), rep("male", sum(freq.mat[3:4,])))
party = rep(c("democrat","republic","democrat","republic"),rowSums(freq.mat))
party = factor(party, levels = c("republic","democrat")) # democrat = 1, republic = 0
ideology = numeric(length=0)
for (i in 1:4) ideology = c(ideology, rep(1:5, freq.mat[i,]))
ideology = factor(ideology, labels = c("very lib","slightly lib","moderate","slightly conserv","very conserv"))
dfr.raw = data.frame(ideology, gender, party)
library(MASS)
fit.raw = polr(ideology ~ party, data=dfr.raw) # proportional odds logistic regression
summary(fit.raw)

# 방법 2
#gender = c(rep("female", 10), rep("male", 10))
#party = rep(c("democrat","republic","democrat","republic"), c(5, 5, 5, 5))
#party = factor(party, levels = c("republic","democrat")) # democrat = 1, republic = 0
#ideology = c(rep(1:5, 4))
#ideology = factor(ideology, labels = c("very lib","slightly lib","moderate","slightly conserv","very conserv"))
#library(MASS)
#(freq.inrow = as.vector(t(freq.mat)))
#fit.weights = polr(ideology ~ party, weights = freq.inrow) # You can use the 'weights' argument
#summary(fit.weights)

beta = -coef(fit.raw) # sign of beta is reversed: See 'Details' in Help. 
a1 = fit.raw$zeta[1] # fit$zeta contains the intercepts
exp(beta) # odds ratio estimate of party effect
exp(a1 + beta*1)/(1 + exp(a1 + beta*1)) # estimate of P(Y = 1) for democratic
exp(a1 + beta*0)/(1 + exp(a1 + beta*0)) # estimate of P(Y = 1) for republican

## Section 6.2.3: Test of independence bet'n ideology and party affiliation
fit0 = polr(ideology ~ 1, data=dfr.raw) # model with only intercepts
summary(fit0)
1 - pchisq(fit0$deviance - fit.raw$deviance, df=1) # p-value for the likelihood ratio test

## Section 6.2.4: Checking Model Fit.
library(brant)
brant(fit.raw) # the Brant test = a score test of the proportional odds assumption

## Fit Non-Proportional (or Partial) Odds Model
library(ordinal)
fit.nonprop = clm(ideology ~ 1, nominal= ~ party, data=dfr.raw) 
summary(fit.nonprop) # Be cautious of the sign of the beta coefficients
#fit.prop = clm(ideology ~ party, data=dfr.raw) # the same result of the function 'polr'
#summary(fit.prop)
