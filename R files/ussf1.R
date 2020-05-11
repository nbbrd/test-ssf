library(rjdssf)

load("./Data/retail.rda")
load("./Data/ABS.rda")

# Usual BSM with time varying trading days
bsm<-function(s, seasonal="HarrisonStevens", tdgroups=c(1,2,3,4,5,6,0), fixedtd=F){  # create the model
  # create the components and add them to the model
  m<-rjdssf::model()
  rjdssf::add(m, rjdssf::locallineartrend("ll"))
  rjdssf::add(m, rjdssf::seasonal("s", frequency(s), type=seasonal))
  rjdssf::add(m, rjdssf::td("td", frequency(s), start(s), length(s), tdgroups
                      , variance = if(fixedtd)0 else 1, fixed=fixedtd))
  rjdssf::add(m, rjdssf::noise("n"))
  #z<-rnorm(3*length(s))
  #x<-matrix(z, nrow=length(s), ncol=3)
  #ssf.add(m, jd3_ssf_reg("x", x, .1, F))
  #ssf.add(m, jd3_ssf_cycle("n"))
  # create the equation 
  eq<-rjdssf::equation("eq")
  rjdssf::add(eq, "ll")
  rjdssf::add(eq, "s")
  rjdssf::add(eq, "td")
  rjdssf::add(eq, "n")
  #ssf.add(eq, "x")
  rjdssf::add(m, eq)
  #estimate the model
  rslt<-rjdssf::estimate(m, s, concentrated=T)
  return (rslt)
}

printbsm<-function(rslt){
  s<-result(rslt, "scalingfactor")
  p<-result(rslt, "parameters")
  ll<-result(rslt, "loglikelihood")
  cat("\nloglikelihood =", ll, "\n\n")
  cat("variances:\n")
  cat("level: ", s*p[1], "\n")
  cat("slope: ", s*p[2], "\n")
  cat("seasonal: ", s*p[3], "\n")
  cat("td: ", s*p[4], "\n")
  cat("noise: ", s*p[5], "\n")
}

s<-log(ABS$X0.2.41.10.M)
frslt<-bsm(s, tdgroups=c(1,1,1,1,2,3,0), fixedtd = T)
printbsm(frslt)

rslt<-bsm(s, tdgroups=c(1,1,1,1,2,3,0))

printbsm(rslt)

ss<-rjdssf::smoothedstates(rslt)
fs<-rjdssf::filteredstates(rslt)

plot(fs[,1], type="l")
lines(ss[,1], col="red")
plot(-fs[,14]*4-fs[,15]-fs[,16], type="l")
lines(-ss[,14]*5-ss[,15]-ss[,16], col="red")
plot(fs[,16], type="l")
lines(ss[,16], col="red")

# Usual airline with time varying trading days
airline<-function(s, period, tdgroups=c(1,2,3,4,5,6,0)){
  # create the model
  airline<-rjdssf::model()
  # create the components and add them to the model
  rjdssf::add(airline, rjdssf::sarima("air", frequency(s), c(0,1,1), c(0,1,1)) )
  rjdssf::add(airline, rjdssf::td("td", frequency(s), start(s), length(s), tdgroups))
  # create the equation (fix the variance to 0)
  eq<-rjdssf::equation("eq", 0, TRUE)
  rjdssf::add(eq, "air")
  rjdssf::add(eq, "td")
  rjdssf::add(airline, eq)
  #estimate the model
  rslt<-rjdssf::estimate(airline, s, concentrated=T)
  return(rslt)
}

printairline<-function(rslt){
  p<-result(rslt, "parameters")
  s<-result(rslt, "scalingfactor")
  ll<-result(rslt, "loglikelihood")
  cat("\nloglikelihood =", ll, "\n\n")
  cat("variances:\n")
  cat("airline: ", s*p[1], "\n")
  cat("td: ", s*p[4], "\n")
  cat("\nAirline:\n")
  cat("theta: ", p[2], "\n")
  cat("btheta: ", p[3], "\n")
}

arslt<-airline(s, tdgroups=c(1,1,1,1,2,3,0))

printairline(arslt)

ass<-rjdssf::smoothedstates(arslt)
afs<-rjdssf::filteredstates(arslt)

plot(-ass[,15]*4-ass[,16]-ass[,17], type="l")
lines(-afs[,15]*4-afs[,16]-afs[,17], col="red")


