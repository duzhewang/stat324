rm(list=ls())

## data set
dead <- c(17.65, 20.83, 24.59, 18.52, 21.40, 23.78, 20.36, 18.83, 21.83, 20.06)
live <- c(23.76, 21.17, 26.13, 20.18, 23.01, 24.84, 19.34, 24.94, 27.14, 25.87, 18.95, 22.61)
all <- c(dead, live)

m=length(dead)
n=length(live)

# calculate the observation of test statistic

tobs=mean(dead)-mean(live)

# build a permutation test function
permutationtest=function(data,obs,b) {
  permutestat=NULL
  m=0
  for(i in 1:b) {
    permutationdata=all[sample.int(m+n)]
    firstsamp=permutationdata[1:m]
    secondsamp=permutationdata[(m+1):(m+n)]
    firstmean=mean(firstsamp)
    secondmean=mean(secondsamp)
    permutestat[i]=firstmean-secondmean
    if (isTRUE(permutestat[i]>=abs(obs)|permutestat[i]<=-abs(obs))){
      m=m+1
    } else {
      m=m
    }
  }
  pvalue=m/b
  return(pvalue)
}

# run b times
b = 10000
permutationtest(all,tobs,b)  



