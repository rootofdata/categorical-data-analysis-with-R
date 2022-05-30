# Problem 4.13
# b.
arr = array(c(19,11,132,52,0,6,9,97), dim=c(2,2,2), 
            dimnames=list(Defend=c("W","B"),Death=c("Y","N"),Victim=c("W","B")))
tbl = as.table(arr)
# When the defendant's race is given as white
(ftbl = ftable(tbl, row.vars = c(1,3), col.vars=2))
ftbl[1:2,]
ftbl[1:2,][2,1] # observed count
chisq.test(ftbl[1:2,])$expected[2,1] # expected count under H_0

# When the victim's race is given as black
(ftbl = ftable(tbl, row.vars = c(3,1), col.vars=2))
ftbl[3:4,]
ftbl[3:4,][1,1] # observed count
chisq.test(ftbl[3:4,])$expected[1,1] # expected count under H_0
