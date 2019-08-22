library(rjdssf)

library(tidyverse)
library(readxl)
emp<-read_excel("./Data/simulWaves.xlsx")
x<-as.matrix(emp[,1:5])

david<-jd3_ssf_model()
# bias
ssf.add(david, jd3_ssf_locallevel("b2",0, T))
ssf.add(david, jd3_ssf_locallevel("b3",0, T))
ssf.add(david, jd3_ssf_locallevel("b4",0, T))
ssf.add(david, jd3_ssf_locallevel("b5",0, T))
# correlation
ar=0.5
mar<-matrix(ar, nrow = 1, ncol=4)
mar[4]<-0.9

ssf.add(david, jd3_ssf_msae("sae", nwaves=5, ar=mar, fixedar = FALSE, lag=3))

# create the equations 
eq1<-jd3_ssf_equation("eq1")
ssf.add(eq1, "b2", -1)
ssf.add(eq1, "b3", -1)
ssf.add(eq1, "b4", -1)
ssf.add(eq1, "b5", -1)
ssf.add(eq1, "sae", coef=2,fixed=T,loading=jd3_ssf_loading(0))
ssf.add(david, eq1)

eq2<-jd3_ssf_equation("eq2")
ssf.add(eq2, "b2")
ssf.add(eq2, "sae",coef=1,fixed=T, loading=jd3_ssf_loading(1))
ssf.add(david, eq2)

eq3<-jd3_ssf_equation("eq3")
ssf.add(eq3, "b3")
ssf.add(eq3, "sae", coef=1,fixed=T,loading=jd3_ssf_loading(2))
ssf.add(david, eq3)

eq4<-jd3_ssf_equation("eq4")
ssf.add(eq4, "b4")
ssf.add(eq4, "sae", coef=1,fixed=T,loading=jd3_ssf_loading(3))
ssf.add(david, eq4)

eq5<-jd3_ssf_equation("eq5")
ssf.add(eq5, "b5")
ssf.add(eq5, "sae", coef=1,fixed=T,loading=jd3_ssf_loading(4))
ssf.add(david, eq5)

resltsDavid<-ssf.estimate(david, x, marginal=T, concentrated=F)

result(resltsDavid,"parameters") 
result(resltsDavid,"parametersname")