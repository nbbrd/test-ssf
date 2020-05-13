library(rjdssf)

#simple bsm (dummy seasonal)
ons_bsm<-function(x, nar=0){
  # create the model
  bsm<-rjdssf::model()
  # create the components and add them to the model
  rjdssf::add(bsm, rjdssf::locallineartrend("ll"))
  rjdssf::add(bsm, rjdssf::seasonal("s", 12, type="Dummy"))
  if (nar == 0){
    rjdssf::add(bsm, rjdssf::noise("n"))
  }else if (nar == 1){
    rjdssf::add(bsm, rjdssf::sae("n", .5, lag=3))
  }else{
    rjdssf::add(bsm, rjdssf::sae("n", c(.3, .2), lag=3))
  }
  # create the equation 
  eq<-rjdssf::equation("eq")
  rjdssf::add(eq, "ll")
  rjdssf::add(eq, "s")
  rjdssf::add(eq, "n", .01, fixed=F)
  rjdssf::add(bsm, eq)
  #estimate the model
  rslt<-rjdssf::estimate(bsm, x, optimizer="BFGS", concentrated=F)
}

print_ons_bsm<-function(rslt, nar=0){
  p<-result(rslt, "parameters")
  s<-result(rslt, "scalingfactor")
  ll<-result(rslt, "loglikelihood")
  cat("\nloglikelihood =", ll, "\n\n")
  cat("variances:\n")
  cat("level: ", s*p[1], "\n")
  cat("slope: ", s*p[2], "\n")
  cat("seasonal: ", s*p[3], "\n")
  np=length(p)
  if (nar>0)cat("ar1: ", p[4], "\n")
  if (nar>1)cat("ar2: ", p[5], "\n")
  cat("noise: ", s*p[np]*p[np], "\n")
}



plot_ons_bsm<-function(rslt){
  ss<-rjdssf::smoothedstates(rslt)
  fs<-rjdssf::filteredstates(rslt)
  
  
  plot(fs[,1], type="l")
  lines(ss[,1], col="red")
  
#  plot(sqrt(result(q, "ssf.smoothing.vcmp(0)")), type="l")
}

emp<-read.csv("c:/ons-lfs/employment16plus.csv")
x<- rowMeans(as.matrix(emp[,2:6]))

cat("\nemployment-N\n")
q<-ons_bsm(x)
print_ons_bsm(q)
plot_ons_bsm(q)

cat("\nemployment-AR(1)\n")
q<-ons_bsm(x, 1)
print_ons_bsm(q,1)
plot_ons_bsm(q)

cat("\nemployment-AR(2)\n")
q<-ons_bsm(x, 2)
print_ons_bsm(q,2)
plot_ons_bsm(q)


emp<-read.csv("c:/ons-lfs/inactivity16plus.csv")
x<- rowMeans(as.matrix(emp[,2:6]))

cat("\ninactivity-N\n")
q<-ons_bsm(x)
print_ons_bsm(q)
plot_ons_bsm(q)

cat("\ninactivity-AR(1)\n")
q<-ons_bsm(x, 1)
print_ons_bsm(q,1)
plot_ons_bsm(q)

cat("\ninactivity-AR(2)\n")
q<-ons_bsm(x, 2)
print_ons_bsm(q,2)
plot_ons_bsm(q)

emp<-read.csv("c:/ons-lfs/unemployment16plus.csv")
x<- rowMeans(as.matrix(emp[,2:6]))

cat("\nunemployment-N\n")
q<-ons_bsm(x)
print_ons_bsm(q)
plot_ons_bsm(q)

cat("\nunemployment-AR(1)\n")
q<-ons_bsm(x, 1)
print_ons_bsm(q,1)
plot_ons_bsm(q)

cat("\nunemployment-AR(2)\n")
q<-ons_bsm(x, 2)
print_ons_bsm(q,2)
plot_ons_bsm(q)


