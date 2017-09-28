#I used sqrt function as a FUN
library(microbenchmark)
library(ggplot2)

rapply_function<-function(x){
  rapply(x,sqrt,how="replace")
}

loop_for_rapply_function<-function(x){
  mR<-x
  for( i in 1:length(x))
    for( j in 1 : length(x[[i]][[1]]))
      mR[[i]][[1]][[j]]<-sqrt(x[[i]][[1]][[j]])
  return(mR)
}

myList <- list()
for(i in 1:1000){
  myList[[length(myList)+1]] <- list(sample(1:100,sample(1:10,1)))
}

benchmarkR<-microbenchmark(rapply_function(myList),loop_for_rapply_function(myList),times = 300L)
autoplot(benchmarkR,log=TRUE)
benchmarkR