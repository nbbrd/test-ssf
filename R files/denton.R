library(rjdssf)
library(rjdbench)

load("./Data/retail.rda")


target<-rjdbench::aggregate(retail$AutomobileDealers, 1)
s<-rjdbench::aggregate(retail$BookStores, 4 )

modelBasedDenton<-function(s, target, conversion, start=0){
  jmodel<-rjdssf::model()
  q<-rjdssf::reg("q", s, 1, T)
  qc<-rjdssf::cumul("qc", q, conversion, start)
  
  etarget<-array(dim=length(s))
  etarget[start+conversion*1:length(target)]<-target
  
  rjdssf::add(jmodel, qc)
  eq<-rjdssf::equation("eq")
  rjdssf::add(eq, "qc")
  rjdssf::add(jmodel, eq)
  
  rslt<-rjdssf::estimate(jmodel, etarget)
  
  states<-rjdssf::smoothedstates(rslt)
  estates<-rjdssf::smoothedstatesstdev(rslt)
  return (list(bench=states[,2]*s, ebench=estates[,2]*abs(s)))
}

b1<-rjdbench::jd3_cholette(s, window(target, end=2008))
md<-modelBasedDenton(s, window(target, start=1995, end=2008), 4, 12)

fernandez<-rjdbench::jd3_tempdisagg(window(target, start=1995, end=2008), indicators = list(s), model = "Rw")
b2<-fernandez$estimation$disagg

ts.plot(ts.union(b1, b2, md$bench), col=c("red", "blue", "gray"))
ts.plot(ts.union(md$ebench, fernandez$estimation$edisagg), col=c("red", "blue"))
        