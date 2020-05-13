library(rjdssf)

load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-ABS$X0.2.09.10.M
# create the model
bsm<-rjdssf::model()

# create the components and add them to the model
# trend component
rjdssf::add(bsm, rjdssf::locallineartrend("ll"))
# or, for a smooth trend: 
# rjdssf::add(bsm, rjdssf::locallineartrend("ll", levelVariance = 0, fixedLevelVariance = TRUE))

# seasonal component. Several specifcations available
rjdssf::add(bsm, rjdssf::seasonal("s", 12, type="Dummy"))
# for instance: rjdssf::add(bsm, rjdssf::seasonal("s", 12, type="Trigonometric"))
# or my preferred one (trivial extensions to seasonal specific components)
# rjdssf::add(bsm, rjdssf::seasonal("s", 12, type="HarrisonStevens"))

#sample error
rjdssf::add(bsm, rjdssf::sae("n", c(.3), lag=3))
# AR(3,6) with fixed parameters
# rjdssf::add(bsm, rjdssf::sae("n", c(.3, -2), fixedar=TRUE, lag=3))

# Survey std error
f<-10+runif(n=length(s), min = 0, max=10)

# create the equation (fix the variance to 0)
eq<-rjdssf::equation("eq", 0, TRUE)
rjdssf::add(eq, "ll")
rjdssf::add(eq, "s")
rjdssf::add(eq, "n", loading= rjdssf::varloading(0, f))
rjdssf::add(bsm, eq)

rslt<-rjdssf::estimate(bsm, s, concentrated=FALSE)
print(result(rslt, "likelihood.ll"))
print(result(rslt, "parameters"))

print(result(rslt, "ssf.cmppos"))
plot(result(rslt, "ssf.smoothing.cmp(0)"), type="l")
lines(result(rslt, "ssf.filtering.cmp(0)"), col="red")

plot(result(rslt, "ssf.smoothing.cmp(1)"), type="l")
plot(result(rslt, "ssf.smoothing.cmp(2)"), type="l")

