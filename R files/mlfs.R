library(rjdssf)

a<-read.csv("./Data/abs1.csv")

# ONS
# create the model
ons<-jd3_ssf_model()

# create the common components and add them to the model
# trend component
add(ons, jd3_ssf_locallineartrend("ll"))
# or, for a smooth trend: 
# add(bsm, jd3_ssf_locallineartrend("ll", levelVariance = 0, fixedLevelVariance = TRUE))

# seasonal component. Several specifcations available
add(ons, jd3_ssf_seasonal("s", 12, type="Dummy"))
# for instance: add(bsm, jd3_ssf_seasonal("s", 12, type="Trigonometric"))
# or my preferred one (trivial extensions to seasonal specific components)
# add(bsm, jd3_ssf_seasonal("s", 12, type="HarrisonStevens"))

# bias corrections (we use specific local levels)
add(ons, jd3_ssf_locallevel("b2"))
add(ons, jd3_ssf_locallevel("b3"))
add(ons, jd3_ssf_locallevel("b4"))
add(ons, jd3_ssf_locallevel("b5"))

# multivariate survey errors 
# 5 waves, ar(1) models, given r2...r5 (fixed here to .22... 25), lag = 3
ar<-matrix(.2+.01*(2:5),nrow = 1, ncol=4)
add(ons, jd3_ssf_msae("sae", nwaves=5, ar=ar, fixedar = TRUE, lag=3))

# survey errors (set to 1.x for wave x + some noise)
err<-matrix(data=1, nrow = 500, ncol=5)
for (i in 1:5){err[,i]<-err[,i]+(i*.1)}
err<-apply(err, MARGIN=2, FUN=function(x) {x+runif(n=length(x), min=-.1, max=.1)})

# create the equations (fix the variance to 0)
eq1<-jd3_ssf_equation("eq1")
add(eq1, "ll")
add(eq1, "s")
add(eq1, "b2", coeff=-1)
add(eq1, "b3", coeff=-1)
add(eq1, "b4", coeff=-1)
add(eq1, "b5", coeff=-1)
add(eq1, "sae", loading=jd3_ssf_varloading(0, err[,1]))
add(ons, eq1)

eq2<-jd3_ssf_equation("eq2")
add(eq2, "ll")
add(eq2, "s")
add(eq2, "b2")
add(eq2, "sae", loading=jd3_ssf_varloading(1, err[,2]))
add(ons, eq2)

eq3<-jd3_ssf_equation("eq3")
add(eq3, "ll")
add(eq3, "s")
add(eq3, "b3")
add(eq3, "sae", loading=jd3_ssf_varloading(2, err[,3]))
add(ons, eq3)

eq4<-jd3_ssf_equation("eq4")
add(eq4, "ll")
add(eq4, "s")
add(eq4, "b4")
add(eq4, "sae", loading=jd3_ssf_varloading(3, err[,4]))
add(ons, eq4)

eq5<-jd3_ssf_equation("eq5")
add(eq5, "ll")
add(eq5, "s")
add(eq5, "b5")
add(eq4, "sae", loading=jd3_ssf_varloading(4, err[,5]))
add(ons, eq5)

# For testing purposes
x<-cbind(a[,3], a[,8], a[,13], a[,18], a[,23])
x<-scale(log(x))
q<-estimate(ons, x)
plot(result(q, "ssf.smoothing.cmp(0)"), type="l")
lines(result(q, "ssf.filtering.cmp(0)"), col="red")
print(result(q, "parametersname"))
print(result(q, "parameters"))
# ABS
# create the model
abs<-jd3_ssf_model()

# create the common components and add them to the model
# trend component
add(abs, jd3_ssf_locallineartrend("ll"))
# or, for a smooth trend: 
# add(bsm, jd3_ssf_locallineartrend("ll", levelVariance = 0, fixedLevelVariance = TRUE))

# seasonal component. Several specifcations available
add(abs, jd3_ssf_seasonal("s", 12, type="Dummy"))
# for instance: add(bsm, jd3_ssf_seasonal("s", 12, type="Trigonometric"))
# or my preferred one (trivial extensions to seasonal specific components)
# add(bsm, jd3_ssf_seasonal("s", 12, type="HarrisonStevens"))

# bias corrections (we use specific local levels with fixed 0-variance)
add(abs, jd3_ssf_locallevel("b2", variance=0, fixed=TRUE))
add(abs, jd3_ssf_locallevel("b3", variance=0, fixed=TRUE))
add(abs, jd3_ssf_locallevel("b4", variance=0, fixed=TRUE))
add(abs, jd3_ssf_locallevel("b5", variance=0, fixed=TRUE))
add(abs, jd3_ssf_locallevel("b6", variance=0, fixed=TRUE))
add(abs, jd3_ssf_locallevel("b7", variance=0, fixed=TRUE))
add(abs, jd3_ssf_locallevel("b8", variance=0, fixed=TRUE))

# multivariate survey errors 
# 8 waves, ar(1)/ar(2) models 
# ar(1) (coeff=.3) applied on wave 1, ar(2) (coeff=.4, .2) applied on waves>1
ar<-matrix(nrow = 2, ncol=2)
ar[1,1]<-.3
ar[1,2]<-.4
ar[2,2]<-.2
add(abs, jd3_ssf_msae("sae", ar, nwaves=8, fixedar = TRUE))

# survey errors (set to 1.x for wave x + some noise)
err<-matrix(data=1, nrow = 500, ncol=8)
for (i in 1:8){err[,i]<-err[,i]+(i*.1)}
err<-apply(err, MARGIN=2, FUN=function(x) {x+runif(n=length(x), min=-.1, max=.1)})


# create the equations (fix the variance to 0)
eq1<-jd3_ssf_equation("eq1", 0, TRUE)
add(eq1, "ll")
add(eq1, "s")
add(eq1, "sae", loading=jd3_ssf_varloading(0, err[,1]))
add(abs, eq1)

eq2<-jd3_ssf_equation("eq2", 0, TRUE)
add(eq2, "ll")
add(eq2, "s")
add(eq2, "b2")
add(eq2, "sae", loading=jd3_ssf_varloading(1, err[,2]))
add(abs, eq2)

eq3<-jd3_ssf_equation("eq3", 0, TRUE)
add(eq3, "ll")
add(eq3, "s")
add(eq3, "b3")
add(eq3, "sae", loading=jd3_ssf_varloading(2, err[,3]))
add(abs, eq3)

eq4<-jd3_ssf_equation("eq4", 0, TRUE)
add(eq4, "ll")
add(eq4, "s")
add(eq4, "b4")
add(eq4, "sae", loading=jd3_ssf_varloading(3, err[,4]))
add(abs, eq4)

eq5<-jd3_ssf_equation("eq5", 0, TRUE)
add(eq5, "ll")
add(eq5, "s")
add(eq5, "b5")
add(eq5, "sae", loading=jd3_ssf_varloading(4, err[,5]))
add(abs, eq5)

eq6<-jd3_ssf_equation("eq6", 0, TRUE)
add(eq6, "ll")
add(eq6, "s")
add(eq6, "b6")
add(eq6, "sae", loading=jd3_ssf_varloading(5, err[,6]))
add(abs, eq6)

eq7<-jd3_ssf_equation("eq7", 0, TRUE)
add(eq7, "ll")
add(eq7, "s")
add(eq7, "b7")
add(eq7, "sae", loading=jd3_ssf_varloading(6, err[,7]))
add(abs, eq7)

eq8<-jd3_ssf_equation("eq8", 0, TRUE)
add(eq8, "ll")
add(eq8, "s")
add(eq8, "b8")
add(eq8, "sae", loading=jd3_ssf_varloading(7, err[,8]))
add(abs, eq8)

# For testing purposes
x<-cbind(a[,3], a[,8], a[,13], a[,18], a[,23], a[,28], a[,33], a[,38])
x<-scale(log(x))
qq<-estimate(abs, x)
plot(result(qq, "ssf.filtering.cmp(0)"), type="l")
lines(result(qq, "ssf.smoothing.cmp(0)"), col="red")
print(result(qq, "parametersname"))
print(result(qq, "parameters"))
#print(result(qq,"ssf.T"))
#print(result(qq,"ssf.V"))
#print(result(qq,"ssf.P0"))

