#install.packages("microbenchmark")
library(microbenchmark)
library(ggplot2)

three_parm_function<-function(aa,bb,cc){
  return (aa*100+bb*10+cc)
}

mapply_function<-function(mA,mB,mC){
  mapply(three_parm_function,mA,mB,mC,SIMPLIFY = TRUE)
}

loop_for_mapply_function<-function(mA,mB,mC){
  mR<-replicate(length(mA),NA)
  for( i in 1:length(mA))
    mR[i]<-three_parm_function(mA[i],mB[i%%length(mB)+length(mB)*floor((floor(i/length(mB))/ceiling(i/length(mB))))],mC[i%%length(mC)+length(mC)*floor((floor(i/length(mC))/ceiling(i/length(mC))))])
  return(mR)
}

mmA <-replicate(10, rnorm(20))
mmB <- rnorm(10)
mmC <- rnorm(20)

benchmarkR<-microbenchmark(mapply_function(mmA,mmB,mmC),loop_for_mapply_function(mmA,mmB,mmC),times = 300L)
autoplot(benchmarkR,log=TRUE)
benchmarkR