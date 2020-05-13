library(rjdssf)

a<-read.csv("./Data/abs1.csv")

# ONS
# create the model
ons<-rjdssf::model()

# create the common components and add them to the model
# trend component
rjdssf::add(ons, rjdssf::locallineartrend("ll"))
# or, for a smooth trend: 
# ssf.add(bsm, rjdssf::locallineartrend("ll", levelVariance = 0, fixedLevelVariance = TRUE))

# seasonal component. Several specifcations available
rjdssf::add(ons, rjdssf::seasonal("s", 12, type="Dummy"))
# for instance: rjdssf::add(bsm, rjdssf::seasonal("s", 12, type="Trigonometric"))
# or my preferred one (trivial extensions to seasonal specific components)
# rjdssf::add(bsm, rjdssf::seasonal("s", 12, type="HarrisonStevens"))

# bias corrections (we use specific local levels)
rjdssf::add(ons, rjdssf::locallevel("b2"))
rjdssf::add(ons, rjdssf::locallevel("b3"))
rjdssf::add(ons, rjdssf::locallevel("b4"))
rjdssf::add(ons, rjdssf::locallevel("b5"))

# multivariate survey errors 
# 5 waves, ar(1) models, given r2...r5 (fixed here to .22... 25), lag = 3
ar<-matrix(.2+.01*(2:5),nrow = 1, ncol=4)
rjdssf::add(ons, rjdssf::msae("sae", nwaves=5, ar=ar, fixedar = TRUE, lag=3))

# survey errors (set to 1.x for wave x + some noise)
err<-matrix(data=1, nrow = 500, ncol=5)
for (i in 1:5){err[,i]<-err[,i]+(i*.1)}
err<-apply(err, MARGIN=2, FUN=function(x) {x+runif(n=length(x), min=-.1, max=.1)})

# create the equations (fix the variance to 0)
eq1<-rjdssf::equation("eq1")
rjdssf::add(eq1, "ll")
rjdssf::add(eq1, "s")
rjdssf::add(eq1, "b2", coeff=-1)
rjdssf::add(eq1, "b3", coeff=-1)
rjdssf::add(eq1, "b4", coeff=-1)
rjdssf::add(eq1, "b5", coeff=-1)
rjdssf::add(eq1, "sae", loading=rjdssf::varloading(0, err[,1]))
rjdssf::add(ons, eq1)

eq2<-rjdssf::equation("eq2")
rjdssf::add(eq2, "ll")
rjdssf::add(eq2, "s")
rjdssf::add(eq2, "b2")
rjdssf::add(eq2, "sae", loading=rjdssf::varloading(1, err[,2]))
rjdssf::add(ons, eq2)

eq3<-rjdssf::equation("eq3")
rjdssf::add(eq3, "ll")
rjdssf::add(eq3, "s")
rjdssf::add(eq3, "b3")
rjdssf::add(eq3, "sae", loading=rjdssf::varloading(2, err[,3]))
rjdssf::add(ons, eq3)

eq4<-rjdssf::equation("eq4")
rjdssf::add(eq4, "ll")
rjdssf::add(eq4, "s")
rjdssf::add(eq4, "b4")
rjdssf::add(eq4, "sae", loading=rjdssf::varloading(3, err[,4]))
rjdssf::add(ons, eq4)

eq5<-rjdssf::equation("eq5")
rjdssf::add(eq5, "ll")
rjdssf::add(eq5, "s")
rjdssf::add(eq5, "b5")
rjdssf::add(eq4, "sae", loading=rjdssf::varloading(4, err[,5]))
rjdssf::add(ons, eq5)

# For testing purposes
x<-cbind(a[,3], a[,8], a[,13], a[,18], a[,23])
x<-scale(log(x))
q<-estimate(ons, x)
plot(result(q, "ssf.smoothing.cmp(0)"), type="l")
lines(result(q, "ssf.filtering.cmp(0)"), col="red")
print(result(q, "parametersnames"))
print(result(q, "parameters"))
# ABS
# create the model
abs<-rjdssf::model()

# create the common components and add them to the model
# trend component
rjdssf::add(abs, rjdssf::locallineartrend("ll"))
# or, for a smooth trend: 
# rjdssf::add(bsm, rjdssf::locallineartrend("ll", levelVariance = 0, fixedLevelVariance = TRUE))

# seasonal component. Several specifcations available
rjdssf::add(abs, rjdssf::seasonal("s", 12, type="Dummy"))
# for instance: rjdssf::add(bsm, rjdssf::seasonal("s", 12, type="Trigonometric"))
# or my preferred one (trivial extensions to seasonal specific components)
# rjdssf::add(bsm, rjdssf::seasonal("s", 12, type="HarrisonStevens"))

# bias corrections (we use specific local levels with fixed 0-variance)
rjdssf::add(abs, rjdssf::locallevel("b2", variance=0, fixed=TRUE))
rjdssf::add(abs, rjdssf::locallevel("b3", variance=0, fixed=TRUE))
rjdssf::add(abs, rjdssf::locallevel("b4", variance=0, fixed=TRUE))
rjdssf::add(abs, rjdssf::locallevel("b5", variance=0, fixed=TRUE))
rjdssf::add(abs, rjdssf::locallevel("b6", variance=0, fixed=TRUE))
rjdssf::add(abs, rjdssf::locallevel("b7", variance=0, fixed=TRUE))
rjdssf::add(abs, rjdssf::locallevel("b8", variance=0, fixed=TRUE))

# multivariate survey errors 
# 8 waves, ar(1)/ar(2) models 
# ar(1) (coeff=.3) applied on wave 1, ar(2) (coeff=.4, .2) applied on waves>1
ar<-matrix(nrow = 2, ncol=2)
ar[1,1]<-.3
ar[1,2]<-.4
ar[2,2]<-.2
rjdssf::add(abs, rjdssf::msae("sae", ar, nwaves=8, fixedar = TRUE))

# survey errors (set to 1.x for wave x + some noise)
err<-matrix(data=1, nrow = 500, ncol=8)
for (i in 1:8){err[,i]<-err[,i]+(i*.1)}
err<-apply(err, MARGIN=2, FUN=function(x) {x+runif(n=length(x), min=-.1, max=.1)})


# create the equations (fix the variance to 0)
eq1<-rjdssf::equation("eq1", 0, TRUE)
rjdssf::add(eq1, "ll")
rjdssf::add(eq1, "s")
rjdssf::add(eq1, "sae", loading=rjdssf::varloading(0, err[,1]))
rjdssf::add(abs, eq1)

eq2<-rjdssf::equation("eq2", 0, TRUE)
rjdssf::add(eq2, "ll")
rjdssf::add(eq2, "s")
rjdssf::add(eq2, "b2")
rjdssf::add(eq2, "sae", loading=rjdssf::varloading(1, err[,2]))
rjdssf::add(abs, eq2)

eq3<-rjdssf::equation("eq3", 0, TRUE)
rjdssf::add(eq3, "ll")
rjdssf::add(eq3, "s")
rjdssf::add(eq3, "b3")
rjdssf::add(eq3, "sae", loading=rjdssf::varloading(2, err[,3]))
rjdssf::add(abs, eq3)

eq4<-rjdssf::equation("eq4", 0, TRUE)
rjdssf::add(eq4, "ll")
rjdssf::add(eq4, "s")
rjdssf::add(eq4, "b4")
rjdssf::add(eq4, "sae", loading=rjdssf::varloading(3, err[,4]))
rjdssf::add(abs, eq4)

eq5<-rjdssf::equation("eq5", 0, TRUE)
rjdssf::add(eq5, "ll")
rjdssf::add(eq5, "s")
rjdssf::add(eq5, "b5")
rjdssf::add(eq5, "sae", loading=rjdssf::varloading(4, err[,5]))
rjdssf::add(abs, eq5)

eq6<-rjdssf::equation("eq6", 0, TRUE)
rjdssf::add(eq6, "ll")
rjdssf::add(eq6, "s")
rjdssf::add(eq6, "b6")
rjdssf::add(eq6, "sae", loading=rjdssf::varloading(5, err[,6]))
rjdssf::add(abs, eq6)

eq7<-rjdssf::equation("eq7", 0, TRUE)
rjdssf::add(eq7, "ll")
rjdssf::add(eq7, "s")
rjdssf::add(eq7, "b7")
rjdssf::add(eq7, "sae", loading=rjdssf::varloading(6, err[,7]))
rjdssf::add(abs, eq7)

eq8<-rjdssf::equation("eq8", 0, TRUE)
rjdssf::add(eq8, "ll")
rjdssf::add(eq8, "s")
rjdssf::add(eq8, "b8")
rjdssf::add(eq8, "sae", loading=rjdssf::varloading(7, err[,8]))
rjdssf::add(abs, eq8)

# For testing purposes
x<-cbind(a[,3], a[,8], a[,13], a[,18], a[,23], a[,28], a[,33], a[,38])
x<-scale(log(x))
qq<-rjdssf::estimate(abs, x)
plot(result(qq, "ssf.filtering.cmp(0)"), type="l")
lines(result(qq, "ssf.smoothing.cmp(0)"), col="red")
print(result(qq, "parametersnames"))
print(result(qq, "parameters"))
#print(result(qq,"ssf.T"))
#print(result(qq,"ssf.V"))
#print(result(qq,"ssf.P0"))

