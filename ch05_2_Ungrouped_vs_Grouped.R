## Section 5.2 Ungrouped Data vs Grouped Data
## The parameter estmates are the same, but G^2 and X^2 are different.
# 1. ungrouped data
crab = read.table("crab.txt") # The 'crab.dat' file should be in the working directory
names(crab) = c("color","spine","width","satell","weight")
crab$color = as.factor(crab$color)
obj.ungrp = glm(y ~ color, crab, family=binomial) 

# 2. grouped data
mat = as.matrix(with(crab, table(color, y)))
color.grp = as.factor(c(2,3,4,5))
obj.grp = glm(cbind(mat[,2], mat[,1]) ~ color.grp, family=binomial) 

# 3. Compare the two result of obj.ungrp and obj.grp
coef(obj.ungrp)
coef(obj.grp)
c(deviance(obj.ungrp), df.residual(obj.ungrp)) # LRT is not appropriate. Why?
c(deviance(obj.grp), df.residual(obj.grp)) # the saturated model

  