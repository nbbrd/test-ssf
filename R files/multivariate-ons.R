library(rjdssf)

ons_msae<-function(x, ar=.7, e=NULL){
# create the model
  ons<-jd3_ssf_model()
  
  # create the common components and add them to the model
  # trend component
  add(ons, jd3_ssf_locallineartrend("ll"))
  # or, for a smooth trend: 
  # add(bsm, jd3_ssf_locallineartrend("ll", levelVariance = 0, fixedLevelVariance = TRUE))
  
  # seasonal component. Several specifcations available
  add(ons, jd3_ssf_seasonal("s", 12, type="Dummy"))

  # bias corrections (we use specific local levels)
  add(ons, jd3_ssf_locallevel("b2", variance = 0))
  add(ons, jd3_ssf_locallevel("b3", variance = 0))
  add(ons, jd3_ssf_locallevel("b4", variance = 0))
  add(ons, jd3_ssf_locallevel("b5", variance = 0))
  
  # multivariate survey errors 
  # 5 waves, ar(1) models, given r2...r5 (fixed here to .22... 25), lag = 3
  mar<-matrix(ar, nrow = 1, ncol=4)
  add(ons, jd3_ssf_msae("sae", nwaves=5, ar=mar, fixedar = TRUE, lag=3))
  
  # survey errors (set to 1.x for wave x + some noise)
  
  # create the equations 
  eq1<-jd3_ssf_equation("eq1")
  add(eq1, "ll")
  add(eq1, "s")
  add(eq1, "b2", -1)
  add(eq1, "b3", -1)
  add(eq1, "b4", -1)
  add(eq1, "b5", -1)
  if (is.null(e)){
    add(eq1, "sae", .1, fixed=F, jd3_ssf_loading(0))
  }else{
    add(eq1, "sae", .1, fixed=F, loading=jd3_ssf_varloading(0, e[,1]))
  }
  add(ons, eq1)
  
  eq2<-jd3_ssf_equation("eq2")
  add(eq2, "ll")
  add(eq2, "s")
  add(eq2, "b2")
  if (is.null(e)){
    add(eq2, "sae", .1, fixed=F, jd3_ssf_loading(1))
  }else{
    add(eq2, "sae", .1, fixed=F, loading=jd3_ssf_varloading(1, e[,2]))
  }
  add(ons, eq2)
  
  eq3<-jd3_ssf_equation("eq3")
  add(eq3, "ll")
  add(eq3, "s")
  add(eq3, "b3")
  if (is.null(e)){
    add(eq3, "sae", .1, fixed=F, jd3_ssf_loading(2))
  }else{
    add(eq3, "sae", .1, fixed=F, loading=jd3_ssf_varloading(2, e[,3]))
  }
  add(ons, eq3)
  
  eq4<-jd3_ssf_equation("eq4")
  add(eq4, "ll")
  add(eq4, "s")
  add(eq4, "b4")
  if (is.null(e)){
    add(eq4, "sae", .1, fixed=F, jd3_ssf_loading(3))
  }else{
    add(eq4, "sae", .1, fixed=F, loading=jd3_ssf_varloading(3, e[,4]))
  }
  add(ons, eq4)
  
  eq5<-jd3_ssf_equation("eq5")
  add(eq5, "ll")
  add(eq5, "s")
  add(eq5, "b5")
  if (is.null(e)){
    add(eq5, "sae", .1, fixed=F, jd3_ssf_loading(4))
  }else{
    add(eq5, "sae", .1, fixed=F, loading=jd3_ssf_varloading(4, e[,5]))
  }
  add(ons, eq5)
  
  return (estimate(ons, x, marginal=T, concentrated=T))
}

ons_msae2<-function(x, ar=.7){
  # create the model
  ons2<-jd3_ssf_model()
  
  # create the common components and add them to the model
  # trend component
  add(ons2, jd3_ssf_locallineartrend("l"))
  # seasonal component. Several specifcations available
  add(ons2, jd3_ssf_seasonal("s", 12, type="Dummy"))
  # bias corrections (we use specific local levels)
  add(ons2, jd3_ssf_locallevel("b2"))
  add(ons2, jd3_ssf_locallevel("b3"))
  add(ons2, jd3_ssf_locallevel("b4"))
  add(ons2, jd3_ssf_locallevel("b5"))
  # multivariate survey errors 
  # 5 waves
  mar<-matrix(ar, nrow = 1, ncol=4)
  add(ons2, jd3_ssf_msae2("sae", vars=array(0, 5), fixedvars=F, ar=mar, fixedar = TRUE, lag=3))
  
  # survey errors (set to 1.x for wave x + some noise)
  
  # create the equations 
  eq1<-jd3_ssf_equation("eq1")
  add(eq1, "l")
  add(eq1, "s")
  add(eq1, "b2", -1)
  add(eq1, "b3", -1)
  add(eq1, "b4", -1)
  add(eq1, "b5", -1)
  add(eq1, "sae", loading=jd3_ssf_loading(0))
  add(ons2, eq1)
  
  eq2<-jd3_ssf_equation("eq2")
  add(eq2, "l")
  add(eq2, "s")
  add(eq2, "b2")
  add(eq2, "sae", loading=jd3_ssf_loading(1))
  add(ons2, eq2)
  
  eq3<-jd3_ssf_equation("eq3")
  add(eq3, "l")
  add(eq3, "s")
  add(eq3, "b3")
  add(eq3, "sae", loading=jd3_ssf_loading(2))
  add(ons2, eq3)
  
  eq4<-jd3_ssf_equation("eq4")
  add(eq4, "l")
  add(eq4, "s")
  add(eq4, "b4")
  add(eq4, "sae", loading=jd3_ssf_loading(3))
  add(ons2, eq4)
  
  eq5<-jd3_ssf_equation("eq5")
  add(eq5, "l")
  add(eq5, "s")
  add(eq5, "b5")
  add(eq5, "sae", loading=jd3_ssf_loading(4))
  add(ons2, eq5)
  
  return (estimate(ons2, x, marginal=T, concentrated=T))
}


print_ons_msae<-function(rslt, se=F){
  p<-result(rslt, "parameters")
  s<-result(rslt, "scalingfactor") 
#  if (se){s <- 1} else{ s<-result(rslt, "scalingfactor")} 
  ll<-result(rslt, "loglikelihood")
  cat("\nloglikelihood =", ll, "\n\n")
  cat("variances:\n")
  cat("level: ", s*p[1], "\n")
  cat("slope: ", s*p[2], "\n")
  cat("seasonal: ", s*p[3], "\n")
  cat("bias2: ", s*p[4], "\n")
  cat("bias3: ", s*p[5], "\n")
  cat("bias4: ", s*p[6], "\n")
  cat("bias5: ", s*p[7], "\n")
  np=length(p)
 # if (!se){
    cat("e1: ", s*p[np-4]*p[np-4], "\n")
    cat("e2: ", s*p[np-3]*p[np-3], "\n")
    cat("e3: ", s*p[np-2]*p[np-2], "\n")
    cat("e4: ", s*p[np-1]*p[np-1], "\n")
    cat("e5: ", s*p[np]*p[np], "\n")
#  }
}

print_ons_msae2<-function(rslt){
  p<-result(rslt, "parameters")
  s<-result(rslt, "scalingfactor") 
  ll<-result(rslt, "loglikelihood")
  cat("\nloglikelihood =", ll, "\n\n")
  cat("variances:\n")
  cat("level: ", s*p[1], "\n")
  cat("slope: ", s*p[2], "\n")
  cat("seasonal: ", s*p[3], "\n")
  cat("bias2: ", s*p[4], "\n")
  cat("bias3: ", s*p[5], "\n")
  cat("bias4: ", s*p[6], "\n")
  cat("bias5: ", s*p[7], "\n")
  cat("e1: ", s*p[8], "\n")
  cat("e2: ", s*p[9], "\n")
  cat("e3: ", s*p[10], "\n")
  cat("e4: ", s*p[11], "\n")
  cat("e5: ", s*p[12], "\n")
}


plot_ons_msae<-function(x, rslt){
  ss<-jd3_smoothedstates(rslt)
  fs<-jd3_filteredstates(rslt)
  y<- rowMeans(x)
  
  plot(y, type="l", col="blue")
  lines(fs[,1], col="red")
  lines(ss[,1], col="green")
  
  plot(result(rslt, "ssf.smoothing.vcmp(0)"), type="l")
}

emp<-read.csv("c:/ons-lfs/employment16plus.csv")
x<-as.matrix(emp[,2:6])
e<-as.matrix(emp[,7:11])

q<-ons_msae(x, .7, NULL)
print_ons_msae(q, F) 
plot_ons_msae(x, q)

q2<-ons_msae2(x, .7)
print_ons_msae2(q2) 
plot_ons_msae(x, q2)

q<-ons_msae(x, .7, e)
print_ons_msae(q, T) 
plot_ons_msae(x, q)

emp<-read.csv("c:/ons-lfs/inactivity16plus.csv")
x<-as.matrix(emp[,2:6])
e<-as.matrix(emp[,7:11])

q<-ons_msae(x, .7, NULL)
print_ons_msae(q, F) 
plot_ons_msae(x, q)

q<-ons_msae2(x, .7)
print_ons_msae2(q) 
plot_ons_msae(x, q)

q<-ons_msae(x, .7, e)
print_ons_msae(q, T) 
plot_ons_msae(x, q)

emp<-read.csv("c:/ons-lfs/unemployment16plus.csv")
x<-as.matrix(emp[,2:6])
e<-as.matrix(emp[,7:11])

q<-ons_msae(x, .4, NULL)
print_ons_msae(q, F) 
plot_ons_msae(x, q)

q<-ons_msae2(x, .4)
print_ons_msae2(q) 
plot_ons_msae(x, q)

q<-ons_msae(x, .4, e)
print_ons_msae(q, T) 
plot_ons_msae(x, q)