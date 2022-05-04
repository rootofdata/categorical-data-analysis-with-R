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