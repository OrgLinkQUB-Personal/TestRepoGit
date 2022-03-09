#Clear all the memories
rm(list=ls(all=TRUE))
#change the working directory to that of the current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(bizdays)

#################################################################
### binomial tree example: stock with continuous dividend yield
#################################################################
nSteps=2
interestRate=0.05
dividendYield=0.02
volatility=0.2
ttm=6/12
spotUndelrying=810
strike=800
call_put=1 #1 for call option, -1 for put option
americanOption=FALSE
displayBT=TRUE

deltaT=ttm/nSteps
uCRR=exp(volatility*sqrt(deltaT))
dCRR=exp(-volatility*sqrt(deltaT))
aCRR=exp((interestRate-dividendYield)*(deltaT))

pCRR=(aCRR-dCRR)/(uCRR-dCRR)

# state price at expiry
statesTemp=rep(NA,nSteps+1)
for (j in 1:(nSteps+1)){
  statesTemp[j]=spotUndelrying*uCRR^(j-1)*dCRR^(nSteps-(j-1))
}
print(statesTemp)

# option price at expiry
optionTemp=rep(NA,nSteps+1)
for (j in 1:(nSteps+1)){
  optionTemp[j]=max((statesTemp[j]-strike)*call_put,0)
}  
cat("state price at step ",nSteps+1," is: ",statesTemp,'\n') 
cat("Option price at step ",nSteps+1," is: ",optionTemp,'\n')

# state price and option price at each node
for (i in nSteps:1){
  optionTemp_1=rep(NA,i)
  statesTemp=rep(NA,i)
  for (j in 1:i){
    statesTemp[j]=spotUndelrying*uCRR^(j-1)*dCRR^(i-1-(j-1))
  }
  
  if (americanOption==TRUE){
    for (j in 1:i){
      optionTemp_1[j]=max(((1-pCRR)*optionTemp[j]+pCRR*optionTemp[j+1])*exp(-interestRate*deltaT),(statesTemp[j]-strike)*call_put)
    }    
  }  else{
    for (j in 1:i){
      optionTemp_1[j]=((1-pCRR)*optionTemp[j]+pCRR*optionTemp[j+1])*exp(-interestRate*deltaT)
    }     
  }
  
  optionTemp=optionTemp_1
  if (displayBT==TRUE){
    cat("state price at step ",i," is: ",statesTemp,'\n') 
    cat("Option price at step ",i," is: ",optionTemp,'\n')
  }
}

cat("Option price is: ",optionTemp)
#################################################################

#################################################################
### binomial tree example: stock with continuous dividend yield 1000 steps
#################################################################
nSteps=1000
interestRate=0.05
dividendYield=0.02
volatility=0.2
ttm=6/12
spotUndelrying=810
strike=800
call_put=1 #1 for call option, -1 for put option
americanOption=FALSE
displayBT=FALSE

deltaT=ttm/nSteps
uCRR=exp(volatility*sqrt(deltaT))
dCRR=exp(-volatility*sqrt(deltaT))
aCRR=exp((interestRate-dividendYield)*(deltaT))

pCRR=(aCRR-dCRR)/(uCRR-dCRR)

# state price at expiry
statesTemp=rep(NA,nSteps+1)
for (j in 1:(nSteps+1)){
  statesTemp[j]=spotUndelrying*uCRR^(j-1)*dCRR^(nSteps-(j-1))
}
print(statesTemp)

# option price at expiry
optionTemp=rep(NA,nSteps+1)
for (j in 1:(nSteps+1)){
  optionTemp[j]=max((statesTemp[j]-strike)*call_put,0)
}  
cat("state price at step ",nSteps+1," is: ",statesTemp,'\n') 
cat("Option price at step ",nSteps+1," is: ",optionTemp,'\n')

# state price and option price at each node
for (i in nSteps:1){
  optionTemp_1=rep(NA,i)
  statesTemp=rep(NA,i)
  for (j in 1:i){
    statesTemp[j]=spotUndelrying*uCRR^(j-1)*dCRR^(i-1-(j-1))
  }
  
  if (americanOption==TRUE){
    for (j in 1:i){
      optionTemp_1[j]=max(((1-pCRR)*optionTemp[j]+pCRR*optionTemp[j+1])*exp(-interestRate*deltaT),(statesTemp[j]-strike)*call_put)
    }    
  }  else{
    for (j in 1:i){
      optionTemp_1[j]=((1-pCRR)*optionTemp[j]+pCRR*optionTemp[j+1])*exp(-interestRate*deltaT)
    }     
  }
  
  optionTemp=optionTemp_1
  if (displayBT==TRUE){
    cat("state price at step ",i," is: ",statesTemp,'\n') 
    cat("Option price at step ",i," is: ",optionTemp,'\n')
  }

}

cat("Option price is: ",optionTemp)
#################################################################



#################################################################
### binomial tree example: futures
#################################################################
nSteps=3
interestRate=0.05
dividendYield=0.05
volatility=0.3
ttm=9/12
spotUndelrying=31
strike=30
call_put=-1 #1 for call option, -1 for put option
americanOption=TRUE
displayBT=TRUE

deltaT=ttm/nSteps
uCRR=exp(volatility*sqrt(deltaT))
dCRR=exp(-volatility*sqrt(deltaT))
aCRR=exp((interestRate-dividendYield)*(deltaT))

pCRR=(aCRR-dCRR)/(uCRR-dCRR)

# state price at expiry
statesTemp=rep(NA,nSteps+1)
for (j in 1:(nSteps+1)){
  statesTemp[j]=spotUndelrying*uCRR^(j-1)*dCRR^(nSteps-(j-1))
}
print(statesTemp)

# option price at expiry
optionTemp=rep(NA,nSteps+1)
for (j in 1:(nSteps+1)){
  optionTemp[j]=max((statesTemp[j]-strike)*call_put,0)
}  
cat("state price at step ",nSteps+1," is: ",statesTemp,'\n') 
cat("Option price at step ",nSteps+1," is: ",optionTemp,'\n')

# state price and option price at each node
for (i in nSteps:1){
  optionTemp_1=rep(NA,i)
  statesTemp=rep(NA,i)
  for (j in 1:i){
    statesTemp[j]=spotUndelrying*uCRR^(j-1)*dCRR^(i-1-(j-1))
  }
  
  if (americanOption==TRUE){
    for (j in 1:i){
      optionTemp_1[j]=max(((1-pCRR)*optionTemp[j]+pCRR*optionTemp[j+1])*exp(-interestRate*deltaT),(statesTemp[j]-strike)*call_put)
    }    
  }  else{
    for (j in 1:i){
      optionTemp_1[j]=((1-pCRR)*optionTemp[j]+pCRR*optionTemp[j+1])*exp(-interestRate*deltaT)
    }     
  }
  
  optionTemp=optionTemp_1
  if (displayBT==TRUE){
    cat("state price at step ",i," is: ",statesTemp,'\n') 
    cat("Option price at step ",i," is: ",optionTemp,'\n')
  }
}

cat("Option price is: ",optionTemp)
#################################################################


#################################################################
### historical volatility
#################################################################

# read the raw data
underlying <- read.csv("./sampleData/underlyingSampleData.csv", header=T,as.is = T)
# sample size
N<-length(underlying$SPX)


underlying$TradeDate=as.Date(underlying$TradeDate, format = "%Y-%m-%d")

# calculate simple and log returns of SPX
underlying$retSPX=c(NA,underlying$SPX[2:N]/underlying$SPX[1:N-1]-1)
underlying$logRetSPX=c(NA,log(underlying$SPX[2:N]/underlying$SPX[1:N-1]))

plot(underlying$TradeDate,underlying$retSPX, type="l")

# historical volatility
underlying$var=NA
m_days=60

for (i in ((m_days+1):N)){
  underlying[i,'var']=1/(m_days-1)*sum((underlying[(i-m_days-1):(i-1),'retSPX']-mean(underlying[(i-m_days-1):(i-1),'retSPX']))^2)
  
}
underlying$stdAnnual=(underlying$var*252/m_days)^0.5

plot(underlying$TradeDate[seq(from = 1, to = N, by = m_days)],underlying$stdAnnual[seq(from = 1, to = N, by = m_days)], type="l")

### volatility for binomial tree
selectedTradeDate="2020-01-10"

spotUndelrying=underlying[underlying$TradeDate==selectedTradeDate,'SPX']
volatility=underlying[underlying$TradeDate==selectedTradeDate,'stdAnnual']

# other inputs for binomial tree can be found in computer practice 1
#################################################################








