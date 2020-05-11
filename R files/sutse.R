library(rjdssf)
load("./Data/retail.rda")
a<-read.csv("./Data/abs1.csv")

sutse<-function(st, concentrated = TRUE){
  if (! inherits(st, "ts") || ! inherits(st, "matrix"))  
    stop(st, " is not a ts matrix")
  if (dim(st)[2]!= 2)
    stop(st, " must have two columns")
  s<-st[,1]
  t<-st[,2]
  freq<-frequency(st)
  start<-start(st)
  model<-rjdssf::model()

  # create the components and add them to the model
  rjdssf::add(model, rjdssf::locallevel("l1", initial = 0))
  rjdssf::add(model, rjdssf::locallineartrend("lt1", levelVariance = 0, fixedLevelVariance = T))
  rjdssf::add(model, rjdssf::seasonal("s1", freq, type="Trigonometric"))
  rjdssf::add(model, rjdssf::noise("n1"))
  rjdssf::add(model, rjdssf::locallevel("l2", initial = 0))
  rjdssf::add(model, rjdssf::locallineartrend("lt2", levelVariance = 0, fixedLevelVariance = T))
  rjdssf::add(model, rjdssf::seasonal("s2", freq, type="Trigonometric"))
  rjdssf::add(model, rjdssf::noise("n2"))
  
  # create the equation (fix the variance to 1)
  eq1<-rjdssf::equation("eq1")
  rjdssf::add(eq1, "l1")
  rjdssf::add(eq1, "lt1")
  rjdssf::add(eq1, "s1")
  rjdssf::add(eq1, "n1")
  rjdssf::add(model, eq1)
  eq2<-rjdssf::equation("eq2")
  rjdssf::add(eq2, "l2")
  rjdssf::add(eq2, "l1", 0, F)
  rjdssf::add(eq2, "lt2")
  rjdssf::add(eq2, "lt1", 0, F)
  rjdssf::add(eq2, "s2")
  rjdssf::add(eq2, "s1", 0, F)
  rjdssf::add(eq2, "n2")
  rjdssf::add(eq2, "n1", 0, F)
  rjdssf::add(model, eq2)
  rslt<-rjdssf::estimate(model, cbind(s,t), marginal=F, initialization="SqrtDiffuse", optimizer="MinPack", concentrated=concentrated, precision=1e-15)
  
  p<-result(rslt, "parameters")
  s<-result(rslt, "scalingfactor")
  cl<-p[11]
  csl<-p[12]
  cs<-p[13]
  cn<-p[14]
  names<-c("level", "slope", "seasonal", "noise")
  
  variable1<-data.frame(variance=c(s*p[1],s*p[3], s*p[4], s*p[5]), 
                        row.names = names)
  variable2<-data.frame(variance=c(s*(p[1]*cl*cl+p[6]),
                                   s*(p[3]*csl*csl+p[8]),  
                                   s*(p[4]*cs*cs+p[9]),
                                   s*(p[5]*cn*cn+p[10])), 
                        row.names = names)

  if (p[1]==0){
    lc<-0
  }else if (p[6] == 0){
    lc<-1
  }else{
    lc<-sign(cl)/(sqrt(1+p[6]/(cl*cl*p[1])))
  }
  if (p[3]==0){
    sc<-0
  }else if (p[8] == 0){
    sc<-1
  }else{
    sc<-sign(csl)/(sqrt(1+p[8]/(csl*csl*p[3])))
  }
  if (p[4]==0){
    ssc<-0
  }else if (p[9] == 0){
    ssc<-1
  }else{
    ssc<-sign(cs)/(sqrt(1+p[9]/(cs*cs*p[4])))
  }
  if (p[5]==0){
    nc<-0
  }else if (p[10] == 0){
    nc<-1
  }else{
    nc<-sign(cn)/(sqrt(1+p[10]/(cn*cn*p[5])))
  }
  correlations<-data.frame(variance=c(lc, sc, ssc, nc), 
                        row.names = names)
  
  specification<-list(
         concentrated=concentrated,
         marginal=T,
         optimizer="MinPack",
         initialization="SqrtDiffuse")
  models<-list(
    variable1=variable1,
    variable2=variable2,
    correlations=correlations
  )
  
  ll<-result(rslt, "likelihood.ll")
  ser<-result(rslt, "likelihood.ser")
  res<-result(rslt, "likelihood.residuals")
  
  likelihood<-list(loglikelihood=ll, ser=ser, residuals=res)
  
  sm<-rjdssf::smoothedstates(rslt)
  decomposition1<-list(
    level=ts(sm[,1]+sm[,2], frequency = freq, start = start),
    seas=ts(sm[,4], frequency = freq, start = start),
    noise=ts(sm[,15], frequency = freq, start = start)
  )
  decomposition2<-list(
    level=ts(sm[,1]*cl+sm[,2]*csl+sm[,16]+sm[,17], frequency = freq, start = start),
    seas=ts(sm[,4]*cs+sm[,19], frequency = freq, start = start),
    noise=ts(sm[,15]*cn+sm[,30], frequency = freq, start = start)
  )
  e<-list(decomposition1=decomposition1,
          decomposition2=decomposition2)
  
  return (structure(
    list(specification=specification,
         model=models,
         estimation=e,
         likelihood=likelihood)
  , class="JD3SUTSE"))
}



c1=153
c2=c1+5
#c3=c1+10

#q<-sutse(a[,c1], a[,c2], T)
q<-sutse(ts.union(log(retail$BookStores), log(retail$BuildingMatAndGardenEquipAndSupp)), T)
