#Clear all the memories
rm(list=ls(all=TRUE))
#change the working directory to that of the current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(moments)

volatility=0.2
deltaT=1/252/1
spotUndelrying=810
nSteps=1*252
muGBM=0.1
rnorm(1)*sqrt(deltaT)
nTimes=100000

spotUndelrying_t=rep(NA,nSteps)
spotUndelrying_t[1]=spotUndelrying
for (i in 2:nSteps){
  spotUndelrying_t[i]=spotUndelrying_t[i-1]+muGBM*spotUndelrying_t[i-1]*deltaT+volatility*spotUndelrying_t[i-1]*rnorm(1)*sqrt(deltaT)
}

plot(spotUndelrying_t, type="l")

# simulate N times
spotUndelryingMatrix=matrix(NA, nrow = nSteps, ncol = nTimes)
spotUndelryingMatrix[1,]=spotUndelrying
trials <- seq(1, nTimes)
for (i in 1:nTimes){
  for (j in 2:nSteps){
    spotUndelryingMatrix[j,i]=spotUndelryingMatrix[j-1,i]+muGBM*spotUndelryingMatrix[j-1,i]*deltaT+volatility*spotUndelryingMatrix[j-1,i]*rnorm(1)*sqrt(deltaT)
  }
}


simulatedEndSample=spotUndelryingMatrix[nSteps,]

# density 
hist(simulatedEndSample, breaks = 100)
d <- density(simulatedEndSample)
d
plot(d)

# calculate first four moments
mean(simulatedEndSample)
sqrt(var(simulatedEndSample))
skewness(simulatedEndSample)
kurtosis(simulatedEndSample)

muS=log(spotUndelrying)+(muGBM-volatility^2/2)*nSteps*deltaT
sigmaS=sqrt(volatility^2*nSteps*deltaT)
exp(muS+sigmaS^2/2)
sqrt((exp(sigmaS^2)-1)*exp(2*muS+sigmaS^2))

simulatedEndSampleRet=log(simulatedEndSample/spotUndelrying)

# density 
hist(simulatedEndSampleRet, breaks = 100)
d <- density(simulatedEndSampleRet)
d
plot(d)

# calculate first four moments
mean(simulatedEndSampleRet)
sqrt(var(simulatedEndSampleRet))
skewness(simulatedEndSampleRet)
kurtosis(simulatedEndSampleRet)

(muGBM-volatility^2/2)*nSteps*deltaT
sqrt(volatility^2*nSteps*deltaT)
############################################################################
##  the price movement of S^2
spotUndelrying_t=rep(NA,nSteps)
spotUndelrying_t[1]=spotUndelrying
f_t=rep(NA,nSteps)
f_t[1]=spotUndelrying^2
for (j in 2:nSteps){
  dz=rnorm(1)*sqrt(deltaT)
  spotUndelrying_t[j]=spotUndelrying_t[j-1]+muGBM*spotUndelrying_t[j-1]*deltaT+volatility*spotUndelrying_t[j-1]*dz
  f_t[j]=f_t[j-1]+(muGBM*2*(spotUndelrying_t[j-1]^2)+volatility^2*(spotUndelrying_t[j-1]^2))*deltaT+volatility*2*(spotUndelrying_t[j-1]^2)*dz
}

plot(spotUndelrying_t, type="l")
plot(f_t, type="l")

simulatedEndSample=simulatedEndSample^2

# density 
hist(simulatedEndSample, breaks = 100)
d <- density(simulatedEndSample)
d
plot(d)

# calculate first four moments
mean(simulatedEndSample)
sqrt(var(simulatedEndSample))
skewness(simulatedEndSample)
kurtosis(simulatedEndSample)

muS=log(spotUndelrying^2)+(2*muGBM-volatility^2)*nSteps*deltaT
sigmaS=sqrt((2*volatility)^2*nSteps*deltaT)
exp(muS+sigmaS^2/2)
sqrt((exp(sigmaS^2)-1)*exp(2*muS+sigmaS^2))

simulatedEndSampleRet=log(simulatedEndSample/spotUndelrying^2)

# density 
hist(simulatedEndSampleRet, breaks = 100)
d <- density(simulatedEndSampleRet)
d
plot(d)

# calculate first four moments
mean(simulatedEndSampleRet)
sqrt(var(simulatedEndSampleRet))
skewness(simulatedEndSampleRet)
kurtosis(simulatedEndSampleRet)

(2*muGBM-volatility^2)*nSteps*deltaT
2*volatility
###################################################################
