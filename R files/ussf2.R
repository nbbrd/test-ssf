library(rjdssf)

load("./Data/retail.rda")

# create the model
bsm_td_periodic<-function(s, tdgroups, noisyperiod, contrast = F){
  bsm<-jd3_ssf_model()
  
  # create the components and add them to the model
  add(bsm, jd3_ssf_locallineartrend("ll"))
  add(bsm, jd3_ssf_seasonal("s", frequency(s), type="Crude"))
  add(bsm, jd3_ssf_td("td", frequency(s), start(s), length(s), tdgroups, contrast, variance = 0, fixed=T))
  add(bsm, jd3_ssf_noise("n"))
  for (i in 1:length(noisyperiod)){
    add(bsm, jd3_ssf_noise(paste("pn", i, sep=""), variance = .01, fixed=F))
  }
  # create the equation (fix the variance to 1)
  eq<-jd3_ssf_equation("eq")
  add(eq, "ll")
  add(eq, "s")
  add(eq, "td")
  add(eq, "n")
  for (i in 1:length(noisyperiod)){
    add(eq,paste("pn", i, sep=""), 1, TRUE, jd3_ssf_loading_periodic(frequency(s), noisyperiod[i]))
  }
  add(bsm, eq)
  #estimate the model
  rslt<-estimate(bsm, s, marginal=T, concentrated=T)
  return(rslt)
}

q<-bsm_td_periodic(log(retail$BookStores), c(1,1,1,1,2,3,0), c(1,7, 8))
ss<-result(q, "ssf.smoothing.states")

printperiodicbsm<-function(rslt){
  p<-result(rslt, "parameters")
  s<-result(rslt, "scalingfactor")
  ll<-result(rslt, "loglikelihood")
  cat("\nloglikelihood =", ll, "\n\n")
  cat("variances:\n")
  cat("level: ", s*p[1], "\n")
  cat("slope: ", s*p[2], "\n")
  cat("seasonal: ", s*p[3], "\n")
  cat("noise: ", s*p[5], "\n")
  for (i in 6:(length(p)-1)){
    cat("noisy period", (i-5), ": ", s*p[i], "\n")
  }  
}

printperiodicbsm(q)

w<-result(q, "ssf.cmppos")
plot(ss[,w[4]+1]+ss[,w[5]+1]+ss[,w[6]+1]+ss[,w[7]+1], type="l")
lines(ss[,w[4]+1], col="blue")
z<-ss[,w[5]+1]+ss[,w[6]+1]+ss[,w[7]+1]
z<-sapply(z, function(x){if (abs(x)==0)return(NA)else return(x)})
points(z, col="red", pch=16)
