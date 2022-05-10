#범주형 자료분석 연습문제
#2.16
a.lungcancer = array(c(688, 21, 650, 59), dim = c(2, 2))
t.lungcancer = as.table(a.lungcancer)
install.packages("epiR")
library(epiR)
epi.2by2(t.lungcancer, method = "cross.sectional")
#2.33
a.death = array (c(19, 11, 132, 52), dim = c(2,2), dimnames = list(DefendantRace = c("white", "Black"), DeathPenalty = c("Yes", "No")))
a.death
t.death = as.table(a.death)
library(epiR)
epi.2by2(t.death, method = "cross.sectional")

a.death2 = array(c(0.5, 6.5, 9.5, 97.5), dim = c(2,2), dimnames = list(DefendantRace = c("white", "black"), DeathPenalty = c("Yes", "No")))
t.death2 = as.table(a.death2)
epi.2by2(t.death2)

a.death_marginal = array(c(19, 17, 141, 149), dim = c(2, 2), dimnames = list(DefendantRace = c("white", "black"), DeathPenalty = c("Yes", "No")))
t.death_marginal = as.table(a.death_marginal)
epi.2by2(t.death_marginal)



