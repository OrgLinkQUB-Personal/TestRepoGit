#Clear all the memories
rm(list=ls(all=TRUE))
#change the working directory to that of the current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#################################################################
### simulate the stochastic process GBM
#################################################################

# the same example as the binomial tree
volatility=0.2
deltaT=1/252/1
spotUndelrying=810
nSteps=1*252

rnorm(1)*sqrt(deltaT)

strike=800
interestRate=0.05
dividendYield=0.02
call_put=1 # 1 for all, -1 for put
muGBM=interestRate-dividendYield
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

optionSample=rep(NA,length(simulatedEndSample))

#################################################################
# based on the distribution of asset price at expiry, calculate option price at expiry
for (i in 1:length(simulatedEndSample)){
  optionSample[i]=max((simulatedEndSample[i]-strike)*call_put,0)
}
#################################################################
# discount option price by risk-free rate
optionPrice=mean(optionSample)*exp(-interestRate*deltaT*nSteps)
cat("option Price is based on S: ",optionPrice) 
#################################################################

#################################################################
### simulate the stochastic process lnS
#################################################################

# the same example as the binomial tree
volatility=0.2
deltaT=1/252/1
spotUndelrying=810
nSteps=1*252

rnorm(1)*sqrt(deltaT)

strike=800
interestRate=0.05
dividendYield=0.02
call_put=1 # 1 for all, -1 for put
muGBM=interestRate-dividendYield
nTimes=100000

simulatedEndSample=rep(NA,nTimes)
for (i in 1:nTimes){
  simulatedEndSample[i]=spotUndelrying*exp((muGBM-volatility^2/2)*T+volatility*rnorm(1)*sqrt(T))
}

optionSample=rep(NA,length(simulatedEndSample))

#################################################################
# based on the distribution of asset price at expiry, calculate option price at expiry
for (i in 1:length(simulatedEndSample)){
  optionSample[i]=max((simulatedEndSample[i]-strike)*call_put,0)
}
#################################################################
# discount option price by risk-free rate
optionPrice=mean(optionSample)*exp(-interestRate*deltaT*nSteps)
cat("option Price is based on lnS: ",optionPrice) 
#################################################################

#################################################################
# bsm model
ttm=deltaT*nSteps
d1=(log(spotUndelrying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))
d2=d1-volatility*sqrt(ttm)

callPrice=spotUndelrying*exp(-dividendYield*ttm)*pnorm(d1)-strike*exp(-interestRate*ttm)*pnorm(d2)
putPrice=strike*exp(-interestRate*ttm)*pnorm(-d2)-spotUndelrying*exp(-dividendYield*ttm)*pnorm(-d1)

#################################################################
#examine put call parity
callPrice+strike*exp(-interestRate*ttm)

spotUndelrying*exp(-dividendYield*ttm)+putPrice


