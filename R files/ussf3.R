library(rjdssf)
library(KFAS)

load("./Data/ABS.rda")

bsm0<-function(s, seasonal="Dummy"){
  # create the model
  bsm<-jd3_ssf_model()
  # create the components and add them to the model
  ssf.add(bsm, jd3_ssf_locallineartrend("ll"))
  ssf.add(bsm, jd3_ssf_seasonal("s", frequency(s), type=seasonal))
  # create the equation 
  eq<-jd3_ssf_equation("eq", 0, F)
  ssf.add(eq, "ll")
  ssf.add(eq, "s")
  ssf.add(bsm, eq)
  #estimate the model
  rslt<-ssf.estimate(bsm, s, concentrated=T)
  return (rslt)
}

update_model<-function(pars, model){
  for (i in 1:3){
  model["Q"][i,i,1]<-pars[i]*pars[i]
  }
#  for (i in 3:13){
#    model["Q"][i,i,1]<-pars[3]*pars[3]
#  }
  model["H"]<-pars[4]*pars[4]
  model
}

check_model<-function(pars, model){
return (T)}

bsm1<-function(s, method="L-BFGS-B"){
  model<-SSModel(s~-1+SSMtrend(degree=2, Q=list(matrix(NA), matrix(NA)))+SSMseasonal(period=frequency(s), Q=matrix(NA)), H=matrix(NA))
  fmodel<-fitSSM(model, inits = c(0.1,0.1,0.1,0.1), updatefn = update_model, checkfn=check_model, method=method)
  return (fmodel)
  
}

bsm2<-function(s, method="L-BFGS-B"){
  model<-SSModel(s~-1+SSMtrend(degree=2, Q=list(matrix(NA), matrix(NA)))+SSMseasonal(period=frequency(s), Q=matrix(NA)), H=matrix(NA))
  fmodel<-fitSSM(model, inits = c(0.1,0.1,0.1,0.1), method=method)
  return (fmodel)
  
}

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:10){
  a<-bsm0(log(ABS$X0.2.06.10.M))
}

# Stop the clock
message("JD3")
print(proc.time() - ptm)


# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:10){
  a<-bsm1(log(ABS$X0.2.06.10.M))
}

# Stop the clock
message("KFAS-1")
print(proc.time() - ptm)

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:10){
  a<-bsm2(log(ABS$X0.2.06.10.M))
}

# Stop the clock
message("KFAS-2")
print(proc.time() - ptm)



    