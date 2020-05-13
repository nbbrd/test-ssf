library(rjdssf)
library(rjdbench)

load("./Data/retail.rda")


target<-rjdbench::aggregate(retail$AutomobileDealers, 1)
s<-rjdbench::aggregate(retail$BookStores, 4 )

fernandez2<-function(s, target, conversion, start=0){
  jmodel<-rjdssf::model()
  q<-rjdssf::reg("q", s, 1, T)
  rw<-rjdssf::locallevel("l", 1, F)
  agg<-rjdssf::aggregation("all", list(q, rw))
  qc<-rjdssf::cumul("qc", agg, conversion, start)
  
  etarget<-array(dim=length(s))
  etarget[start+conversion*1:length(target)]<-target
  
  rjdssf::add(jmodel, qc)
  eq<-rjdssf::equation("eq")
  rjdssf::add(eq, "qc")
  rjdssf::add(jmodel, eq)
  
  rslt<-rjdssf::estimate(jmodel, etarget)
  
  states<-rjdssf::smoothedstates(rslt)
  return (states[,2]*s+states[,3])
}

chowlin2<-function(s, target, conversion, start=0){
  jmodel<-rjdssf::model()
  q<-rjdssf::reg("q", cbind(1,s), c(0,1), T)
  ar<-rjdssf::ar("l", ar=.9, fixedar = F, variance = 1, fixedvariance = F)
  #  rw<-rjdssf::locallevel("l", 1, T)
  agg<-rjdssf::aggregation("all", list(q, ar))
  qc<-rjdssf::cumul("qc", agg, conversion, start)
  
  etarget<-array(dim=length(s))
  etarget[start+conversion*1:length(target)]<-target
  
  rjdssf::add(jmodel, qc)
  eq<-rjdssf::equation("eq")
  rjdssf::add(eq, "qc")
  rjdssf::add(jmodel, eq)
  
  rslt<-rjdssf::estimate(jmodel, etarget)
  return (rslt)
}

md<-fernandez2(s, target, 4, 0)
mcl<-chowlin2(s, target, 4, 0)

fernandez<-rjdbench::jd3_tempdisagg(target, indicators = list(s), model = "Rw")
f<-fernandez$estimation$disagg
cl<-rjdbench::jd3_tempdisagg(target, indicators = list(s), diffuse.regressors = T)

ts.plot(ts.union(f, md), col=c("red", "blue"))
