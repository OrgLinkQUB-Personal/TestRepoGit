#Clear all the memories
rm(list=ls(all=TRUE))
#change the working directory to that of the current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(bizdays)

# read the raw data
optionRaw <- read.csv("./sampleData/spxOptionDataEx20200619.csv", header=T,as.is = T,check.names=FALSE, colClasses=c("numeric","character","character",rep("numeric",3)))

optionRaw$TradeDate=as.Date(optionRaw$TradeDate, format = "%Y%m%d")
optionRaw$expiryDate=as.Date(optionRaw$expiryDate, format = "%Y%m%d")

# read the raw data of underlying
underlying <- read.csv("./sampleData/underlyingSampleData.csv", header=T,as.is = T)
underlying$TradeDate=as.Date(underlying$TradeDate, format = "%Y-%m-%d")

selectedStrike=3000
oneStrikeData=optionRaw[optionRaw$strike==selectedStrike,]

callOption=oneStrikeData[oneStrikeData$c_p==1,]
putOption=oneStrikeData[oneStrikeData$c_p==0,]

callOption=merge(x=callOption,y=underlying[,c('TradeDate','SPX')],by="TradeDate",all.x=TRUE)
callOption$IV=NA

# use the implied interest rate and dividend rate as an example
# it is better to use the implied rate on each day
interestRate=0.01881165
dividendYield=0.01832855 

business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))
callOption$ttm=NA
for (i in 1:length(callOption$ttm)){
  callOption[i,'ttm']=bizdays(callOption[i,'TradeDate'], callOption[i,'expiryDate'], cal = business_calendar)/252
}



for (i in 1:length(callOption$IV)){
  spotUndelrying=callOption[i,'SPX']
  marketPrice=callOption[i,'price']
  strike=callOption[i,'strike']
  ttm=callOption[i,'ttm']
  objective_Call_IV <- function(volatility) {
    d1=(log(spotUndelrying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))
    d2=d1-volatility*sqrt(ttm)
    
    callPrice=spotUndelrying*exp(-dividendYield*ttm)*pnorm(d1)-strike*exp(-interestRate*ttm)*pnorm(d2)
    
    return((callPrice-marketPrice)^2)
  }
  result <- optimize(objective_Call_IV, interval = c(0, 10), tol = 0.0001)
  
  callOption[i,'IV']=result$minimum
}

plot(callOption$IV)

callOption$delta=NA
callOption$gamma=NA
callOption$theta=NA
callOption$vega=NA
callOption$rho=NA

for (i in 1:length(callOption$IV)){
  spotUndelrying=callOption[i,'SPX']
  marketPrice=callOption[i,'price']
  strike=callOption[i,'strike']
  ttm=callOption[i,'ttm']
  volatility=callOption[i,'IV']
  d1=(log(spotUndelrying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))
  d2=d1-volatility*sqrt(ttm)
  
  callOption[i,'delta']=exp(-dividendYield*ttm)*pnorm(d1)
  callOption[i,'gamma']=(dnorm(d1)*exp(-dividendYield*ttm))/(spotUndelrying*volatility*sqrt(ttm))
  callOption[i,'theta']=-spotUndelrying*dnorm(d1)*volatility*exp(-dividendYield*ttm)/(2*sqrt(ttm))+
                        dividendYield*spotUndelrying*pnorm(d1)*exp(-dividendYield*ttm)-interestRate*strike*exp(-dividendYield*ttm)*pnorm(d2)
  callOption[i,'vega']=spotUndelrying*sqrt(ttm)*dnorm(d1)*exp(-dividendYield*ttm)
  callOption[i,'rho']=strike*ttm*exp(-interestRate*ttm)*pnorm(d2)
}

plot(callOption$IV)
plot(callOption$gamma)
plot(callOption$theta)
plot(callOption$vega)
plot(callOption$rho)

callOption$changeRep=NA
for (i in 2:length(callOption$changeRep)){
  callOption[i,'changeRep']=(callOption[i,'SPX']-callOption[i-1,'SPX'])*callOption[i-1,'delta']+
                            1/2*callOption[i-1,'gamma']*(callOption[i,'SPX']-callOption[i-1,'SPX'])^2+
                            callOption[i,'theta']*(-callOption[i,'ttm']+callOption[i-1,'ttm'])+
                            callOption[i,'vega']*(callOption[i,'IV']-callOption[i-1,'IV'])
}

callOption$changeRealise=NA
for (i in 2:length(callOption$changeRep)){
  callOption[i,'changeRealise']=(callOption[i,'price']-callOption[i-1,'price'])
}

callOption$deltaHedge=NA
for (i in 2:length(callOption$changeRep)){
  callOption[i,'deltaHedge']=(callOption[i,'SPX']-callOption[i-1,'SPX'])*callOption[i-1,'delta']
}

plot(callOption$changeRep,type="l",col="red")
lines(callOption$changeRealise,col="green")
lines(callOption$deltaHedge,col="orange")
