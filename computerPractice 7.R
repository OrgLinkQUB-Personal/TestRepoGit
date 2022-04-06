#Clear all the memories
rm(list=ls(all=TRUE))
#change the working directory to that of the current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read the raw data of underlying
underlying <- read.csv("./sampleData/underlyingSampleData.csv", header=T,as.is = T)
# sample size
N<-length(underlying$SPX)

underlying$TradeDate=as.Date(underlying$TradeDate, format = "%Y-%m-%d")


# calculate simple and log returns of SPX
underlying$retSPX=c(NA,underlying$SPX[2:N]/underlying$SPX[1:N-1]-1)
underlying$logRetSPX=c(NA,log(underlying$SPX[2:N]/underlying$SPX[1:N-1]))
#underlying=na.omit(underlying)    

plot(underlying$TradeDate,underlying$retSPX, type="l")


# historical volatility
underlying$var=NA
m_days=60

for (i in ((m_days+1):N)){
  underlying[i,'var']=1/m_days*sum(underlying[(i-m_days-1):(i-1),'retSPX']^2)
  
}
underlying$stdAnnual=(underlying$var*252)^0.5

plot(underlying$TradeDate,underlying$stdAnnual, type="l")

# EWMA
# Set the begin date and end date 
B.dates <- "2010-01-01"
E.dates <- "2020-12-31"

objective_EWMA <- function(lambdaEWMA) {
  sampleEWMA=with(underlying, underlying[(underlying$TradeDate >= B.dates & underlying$TradeDate <= E.dates), c('TradeDate','SPX','retSPX')])
  sampleEWMA$var=NA
  
  
  sampleEWMA[3,'var']=sampleEWMA[2,'retSPX']^2
  for (i in 4:N){
    sampleEWMA[i,'var']=lambdaEWMA*sampleEWMA[i-1,'var']+(1-lambdaEWMA)*sampleEWMA[i-1,'retSPX']^2
    
  }
  
  sampleEWMA$likeli=-log(sampleEWMA$var)-sampleEWMA$retSPX^2/sampleEWMA$var
  
  return(-sum(sampleEWMA$likeli,na.rm = TRUE))
}

result <- optimize(objective_EWMA, interval = c(0, 1), tol = 0.0001)

objective_EWMA(result$minimum)

sampleEWMA=with(underlying, underlying[(underlying$TradeDate >= B.dates & underlying$TradeDate <= E.dates), c('TradeDate','SPX','retSPX')])
sampleEWMA$var=NA


sampleEWMA[3,'var']=sampleEWMA[2,'retSPX']^2
for (i in 4:N){
  sampleEWMA[i,'var']=result$minimum*sampleEWMA[i-1,'var']+(1-result$minimum)*sampleEWMA[i-1,'retSPX']^2
  
}


plot(sampleEWMA$TradeDate,sampleEWMA$var^0.5*sqrt(252),type="l")


# GARCH 1,1
# Set the begin date and end date 
B.dates <- "2010-01-01"
E.dates <- "2020-12-31"


objective_GARCH <- function(param) {
  sampleData=with(underlying, underlying[(underlying$TradeDate >= B.dates & underlying$TradeDate <= E.dates), c('TradeDate','SPX','retSPX')])
  sampleData$var=NA
  
  N=length(sampleData$var)
  sampleData[3,'var']=sampleData[2,'retSPX']^2
  for (i in 4:N){
    sampleData[i,'var']=param[1]+param[2]*sampleData[i-1,'retSPX']^2+(1-param[1]-param[2])*sampleData[i-1,'var']
    
  }
  
  sampleData$likeli=-log(sampleData$var)-sampleData$retSPX^2/sampleData$var
  
  return(-sum(sampleData$likeli,na.rm = TRUE))
}

result <-optim(c(0.000001, 0.08),objective_GARCH,method="L-BFGS-B",lower=0,upper=1)

param=result$par

sampleGARCH=with(underlying, underlying[(underlying$TradeDate >= B.dates & underlying$TradeDate <= E.dates), c('TradeDate','SPX','retSPX')])
sampleGARCH$var=NA


sampleGARCH[3,'var']=sampleGARCH[2,'retSPX']^2
for (i in 4:N){
  sampleGARCH[i,'var']=param[1]+param[2]*sampleGARCH[i-1,'retSPX']^2+(1-param[1]-param[2])*sampleGARCH[i-1,'var']
  
}

plot(sampleGARCH$TradeDate,sampleGARCH$var^0.5*sqrt(252),type="l")
lines(sampleEWMA$TradeDate, sampleEWMA$var^0.5*sqrt(252),col="red")
lines(underlying$TradeDate,underlying$stdAnnual,col="blue")
# choose recursive or rolling window
# GARCH forecasted volatility