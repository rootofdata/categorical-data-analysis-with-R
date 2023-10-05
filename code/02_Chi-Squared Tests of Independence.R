#install.packages("epiR")  
library(epiR)

#################################################################
# Section 2.1 ~ 2.3: Measures of Association
#################################################################
# Table 2.1: Belief in Afterlife
a.belief = array(c(509,398,116,104), dim=c(2,2)) # Table 2.1 as a matrix
t.belief = as.table(a.belief) # Table 2.1: Belief in Afterlife
epi.2by2(t.belief,method="cross.sectional")

# To give level names of Table 2.1
a.belief = array(c(509,398,116,104), dim=c(2,2), 
   dimnames=list(Gender=c("Females","Males"), Belief=c("Yes","No/Undecided")))
t.belief = as.table(a.belief) 

# Table 2.3: Aspirin Use and Myocardial Infarction
a.aspirin = array(c(189,104,10845,10933), dim=c(2,2))
t.aspirin = as.table(a.aspirin)  # Table 2.3
epi.2by2(t.aspirin,method="cohort.count")

#################################################################
# Section 2.4: Chi-Squared Tests of Independence
#################################################################
# cf. chisq.test for the standardized residuals: 'stdres' component
# cf. Problem 2.19

#################################################################
# Section 2.5: Testing Independence for Ordianl Data
#################################################################
tab.2.7 <- matrix(c(17066,14464,788,126,37,48,38,5,1,1), nrow=5)
library(vcdExtra)
CMHtest(tab.2.7, rscores=c(0,0.5,1.5,4,7))
CMHtest(tab.2.7, rscores=c(1,2,3,4,5)) # NOT SIGNIFICANT p-value! 
CMHtest(tab.2.7, rscores=c(0,0.5,1.5,4,6))
CMHtest(tab.2.7, rscores=c(0,0.5,1.5,4,8))

#################################################################
# Section 2.6: Exact Inference
#################################################################
# cf. 'fisher.test' function for the exact test
# cf. Problem 2.30 and 2.31

#################################################################
# Section 2.7: Marginal vs Conditional Independence
#################################################################
# To make Table 2.11
a2.11 = array(c(18,12,12,8,2,8,8,32), dim=c(2,2,2), 
  dimnames=list(Treatment=c("A","B"), Response=c("S","F"), Clinic=c("1","2")))
t2.11 = as.table(a2.11) 
library(epiR)
epi.2by2(t2.11[,,1]) # the conditional XY odds ratio for clinic 1
epi.2by2(t2.11[,,2]) # the conditional XY odds ratio for clinic 2
ftable(t2.11, row.vars=c(3,1), col.vars=2) # to display table 2.11 as in the textbook
ftable(t2.11, row.vars=1, col.vars=2) # marginal table
epi.2by2(ftable(t2.11, row.vars=1, col.vars=2)) # marginal odds ratio
epi.2by2(t2.11[,1,]) # the conditional XZ odds ratio for Y=1
epi.2by2(t2.11[,2,]) # the conditional XZ odds ratio for Y=2
epi.2by2(t2.11[1,,]) # the conditional YZ odds ratio for X=1
epi.2by2(t2.11[2,,]) # the conditional YZ odds ratio for X=2

