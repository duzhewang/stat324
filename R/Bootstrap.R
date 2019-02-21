## STAT324-1, Spring 2019
## Bootstrap code for second hand smoke example 

rm(list=ls())


## create the original data set
data = c(29,30,53,75,34,21,12,58,117,119,115,134,253,289,287)
n = length(data) # sample size


################################################
## Step 1: calculate the sample mean and sample
## standard deviation of the original data set
################################################

xbar = mean(data)
s = sd(data)

#################################################
## Step 2: Bootstrap b times (b is user-defined)
#################################################


# build a bootstrap function
bootest=function(data,b) {
  bootstat=NULL
  truemean=mean(data)
  for(i in 1:b) {
    samp=sample(data, size = length(data), replace = T)
    bootmean=mean(samp)
    bootsd=sd(samp)
    bootstat[i]=(bootmean - truemean)/(bootsd/sqrt(length(data)))
  }
  return(bootstat)
}

# run bootstrap b times
b = 1000       # number of bootstrap samples
bootdata=bootest(data,b)  


#######################################
## Step 3: Creat a CI
#######################################

alpha=0.05   # 95% CI
lower=quantile(bootdata, probs=alpha/2)    # lower critical value
upper=quantile(bootdata, probs=1-alpha/2)  # upper critical value
CI_lower=xbar-upper*s/sqrt(n)
CI_upper=xbar-lower*s/sqrt(n) 
print(c(CI_lower, CI_upper))








