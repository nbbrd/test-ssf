library(rjdssf)

library(tidyverse)
library(readxl)
emp<-read_excel("./Data/simulWaves.xlsx")
x<-as.matrix(emp[,1:5])

david<-rjdssf::model()
# bias
rjdssf::add(david, rjdssf::locallevel("b2",0, T))
rjdssf::add(david, rjdssf::locallevel("b3",0, T))
rjdssf::add(david, rjdssf::locallevel("b4",0, T))
rjdssf::add(david, rjdssf::locallevel("b5",0, T))
# correlation
ar=0.5
mar<-matrix(ar, nrow = 1, ncol=4)
mar[4]<-0.9

rjdssf::add(david, rjdssf::msae("sae", nwaves=5, ar=mar, fixedar = FALSE, lag=3))

# create the equations 
eq1<-rjdssf::equation("eq1")
rjdssf::add(eq1, "b2", -1)
rjdssf::add(eq1, "b3", -1)
rjdssf::add(eq1, "b4", -1)
rjdssf::add(eq1, "b5", -1)
rjdssf::add(eq1, "sae", coef=2,fixed=T,loading=rjdssf::loading(0))
rjdssf::add(david, eq1)

eq2<-rjdssf::equation("eq2")
rjdssf::add(eq2, "b2")
rjdssf::add(eq2, "sae",coef=1,fixed=T, loading=rjdssf::loading(1))
rjdssf::add(david, eq2)

eq3<-rjdssf::equation("eq3")
rjdssf::add(eq3, "b3")
rjdssf::add(eq3, "sae", coef=1,fixed=T,loading=rjdssf::loading(2))
rjdssf::add(david, eq3)

eq4<-rjdssf::equation("eq4")
rjdssf::add(eq4, "b4")
rjdssf::add(eq4, "sae", coef=1,fixed=T,loading=rjdssf::loading(3))
rjdssf::add(david, eq4)

eq5<-rjdssf::equation("eq5")
rjdssf::add(eq5, "b5")
rjdssf::add(eq5, "sae", coef=1,fixed=T,loading=rjdssf::loading(4))
rjdssf::add(david, eq5)

resltsDavid<-rjdssf::estimate(david, x, marginal=T, concentrated=F)

result(resltsDavid,"parameters") 
result(resltsDavid,"parametersname")