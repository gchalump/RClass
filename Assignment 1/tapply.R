library(microbenchmark)
library(ggplot2)

tapply_function<-function(x){
  tapply(x$amount,x$type,sum)
}

loop_for_tapply_function<-function(x){
  mR<-summary(x$type)
  for( i in 1: length(mR)){
    mR[i]<-0
    for( j in 1:length(x$amount)){
      if(x$type[j] == names(mR[i]))
        mR[i]<-mR[i]+x$amount[i]
    }
  }
    return(mR)
}

mydata <-  data.frame(id = 1:1000,
                      amount = rnorm(1000, mean = 300, sd = 10),
                      type = gl(4, 250, labels = c("Groceries", "Electronic","Toy", "Other stuff")))

benchmarkR<-microbenchmark(tapply_function(mydata),loop_for_tapply_function(mydata),times = 300L)
autoplot(benchmarkR,log=TRUE)
benchmarkR