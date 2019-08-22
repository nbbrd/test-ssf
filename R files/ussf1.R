library(rjdssf)

load("./Data/retail.rda")
load("./Data/ABS.rda")

# Usual BSM with time varying trading days
bsm<-function(s, seasonal="HarrisonStevens", tdgroups=c(1,2,3,4,5,6,0), fixedtd=F){  # create the model
  # create the components and add them to the model
  m<-jd3_ssf_model()
  ssf.add(m, jd3_ssf_locallineartrend("ll"))
  ssf.add(m, jd3_ssf_seasonal("s", frequency(s), type=seasonal))
  ssf.add(m, jd3_ssf_td("td", frequency(s), start(s), length(s), tdgroups
                      , variance = if(fixedtd)0 else 1, fixed=fixedtd))
  ssf.add(m, jd3_ssf_noise("n"))
  #z<-rnorm(3*length(s))
  #x<-matrix(z, nrow=length(s), ncol=3)
  #ssf.add(m, jd3_ssf_reg("x", x, .1, F))
  #ssf.add(m, jd3_ssf_cycle("n"))
  # create the equation 
  eq<-jd3_ssf_equation("eq")
  ssf.add(eq, "ll")
  ssf.add(eq, "s")
  ssf.add(eq, "td")
  ssf.add(eq, "n")
  #ssf.add(eq, "x")
  ssf.add(m, eq)
  #estimate the model
  rslt<-ssf.estimate(m, s, concentrated=T)
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

ss<-jd3_smoothedstates(rslt)
fs<-jd3_filteredstates(rslt)

plot(fs[,1], type="l")
lines(ss[,1], col="red")
plot(-fs[,14]*4-fs[,15]-fs[,16], type="l")
lines(-ss[,14]*5-ss[,15]-ss[,16], col="red")
plot(fs[,16], type="l")
lines(ss[,16], col="red")

# Usual airline with time varying trading days
airline<-function(s, period, tdgroups=c(1,2,3,4,5,6,0)){
  # create the model
  airline<-jd3_ssf_model()
  # create the components and add them to the model
  ssf.add(airline, jd3_ssf_sarima("air", frequency(s), c(0,1,1), c(0,1,1)) )
  ssf.add(airline, jd3_ssf_td("td", frequency(s), start(s), length(s), tdgroups))
  # create the equation (fix the variance to 0)
  eq<-jd3_ssf_equation("eq", 0, TRUE)
  ssf.add(eq, "air")
  ssf.add(eq, "td")
  ssf.add(airline, eq)
  #estimate the model
  rslt<-estimate(airline, s, concentrated=T)
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

ass<-jd3_smoothedstates(arslt)
afs<-jd3_filteredstates(arslt)

plot(-ass[,15]*4-ass[,16]-ass[,17], type="l")
lines(-afs[,15]*4-afs[,16]-afs[,17], col="red")


