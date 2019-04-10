library(rjdssf)

library(tidyverse)
library(readxl)
emp<-read_excel("./Data/simulWaves.xlsx")
x<-as.matrix(emp[,1:5])

david<-jd3_ssf_model()
# bias
add(david, jd3_ssf_locallevel("b2",0, T))
add(david, jd3_ssf_locallevel("b3",0, T))
add(david, jd3_ssf_locallevel("b4",0, T))
add(david, jd3_ssf_locallevel("b5",0, T))
# correlation
ar=0.5
mar<-matrix(ar, nrow = 1, ncol=4)
mar[4]<-0.9

add(david, jd3_ssf_msae("sae", nwaves=5, ar=mar, fixedar = FALSE, lag=3))

# create the equations 
eq1<-jd3_ssf_equation("eq1")
add(eq1, "b2", -1)
add(eq1, "b3", -1)
add(eq1, "b4", -1)
add(eq1, "b5", -1)
add(eq1, "sae", coef=2,fixed=T,loading=jd3_ssf_loading(0))
add(david, eq1)

eq2<-jd3_ssf_equation("eq2")
add(eq2, "b2")
add(eq2, "sae",coef=1,fixed=T, loading=jd3_ssf_loading(1))
add(david, eq2)

eq3<-jd3_ssf_equation("eq3")
add(eq3, "b3")
add(eq3, "sae", coef=1,fixed=T,loading=jd3_ssf_loading(2))
add(david, eq3)

eq4<-jd3_ssf_equation("eq4")
add(eq4, "b4")
add(eq4, "sae", coef=1,fixed=T,loading=jd3_ssf_loading(3))
add(david, eq4)

eq5<-jd3_ssf_equation("eq5")
add(eq5, "b5")
add(eq5, "sae", coef=1,fixed=T,loading=jd3_ssf_loading(4))
add(david, eq5)

resltsDavid<-estimate(david, x, marginal=T, concentrated=F)

result(resltsDavid,"parameters") 
result(resltsDavid,"parametersname")