#Clear all the memories
rm(list=ls(all=TRUE))
#change the working directory to that of the current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages
library(moments)
library(tseries)
library(bizdays)

# read the raw data of underlying
underlying <- read.csv("./sampleData/underlyingSampleData.csv", header=T,as.is = T)
# sample size
N<-length(underlying$SPX)

underlying$TradeDate=as.Date(underlying$TradeDate, format = "%Y-%m-%d")

# calculate simple and log returns of SPX
underlying$retSPX=c(NA,underlying$SPX[2:N]/underlying$SPX[1:N-1]-1)
underlying$logRetSPX=c(NA,log(underlying$SPX[2:N]/underlying$SPX[1:N-1]))
underlying$retSPY=c(NA,underlying$SPY[2:N]/underlying$SPY[1:N-1]-1)
underlying$logRetSPY=c(NA,log(underlying$SPY[2:N]/underlying$SPY[1:N-1]))
underlying=na.omit(underlying)                   


# Set the begin date and end date 
B.dates <- "2010-01-01"
E.dates <- "2020-12-31"

selectedSample=with(underlying, underlying[(underlying$TradeDate >= B.dates & underlying$TradeDate <= E.dates), ])

plot(selectedSample$TradeDate,selectedSample$SPX,col="red", type="l")
lines(selectedSample$TradeDate,selectedSample$SPY*10,col="green")

selectedSample$diff=selectedSample$SPX-selectedSample$SPY*10
plot(selectedSample$TradeDate,selectedSample$diff,col="red", type="l")

selectedSample$diffP=(selectedSample$SPX/(selectedSample$SPY*10))-1
plot(selectedSample$TradeDate,selectedSample$diffP,col="red", type="l")

# density of SPX returns
hist(selectedSample$logRetSPX, breaks = 100)
d <- density(selectedSample$logRetSPX)
d
plot(d)

# calculate first four moments
mean(selectedSample$logRetSPX)*252
var(selectedSample$logRetSPX)
sqrt(var(selectedSample$logRetSPX)*252)
skewness(selectedSample$logRetSPX)
kurtosis(selectedSample$logRetSPX)

# compare empirical density and a normal distribution with the same mean and variance
xAxis=seq(-0.15,0.1,by=0.0001)
plot(d,xlim = c(-0.05,0.05))
lines(xAxis,dnorm(xAxis, mean(selectedSample$logRetSPX), sqrt(var(selectedSample$logRetSPX))),col="green")

# normality test
jarque.bera.test(selectedSample$logRetSPX)

###################################################################
# read the S&P 500 Emini cross section data on 20200110
spEminiCross=read.csv("./sampleData/spxEminiTrade20200110.csv", header=T,as.is = T,check.names=FALSE)
spEminiCross$TradeDate=as.Date(spEminiCross$TradeDate, format = "%d/%m/%Y")

spotIndex=underlying[underlying$TradeDate==spEminiCross$TradeDate,'SPX']
deliveryDate=as.Date(colnames(spEminiCross)[2:length(colnames(spEminiCross))], format = "%d/%m/%Y") 

# calculate business days
business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))
ttm=bizdays(spEminiCross$TradeDate, deliveryDate, cal = business_calendar)/252

# cost of carry
log(spEminiCross[,2:length(colnames(spEminiCross))]/spotIndex)/ttm

###################################################################
# read the S&P 500 Emini time series data 
spEminiTS=read.csv("./sampleData/spxEminiEx20200619.csv", header=T,as.is = T,check.names=FALSE)
spEminiTS$TradeDate=as.Date(spEminiTS$TradeDate, format = "%d/%m/%Y")

# basis 
spEminiTS=merge(x=spEminiTS,y=underlying[c('TradeDate','SPX')],by="TradeDate",all.x=TRUE)

spEminiTS$Basis=spEminiTS$`19/06/2020`-spEminiTS$SPX
spEminiTS$BasisP=spEminiTS$SPX/spEminiTS$`19/06/2020`-1

plot(spEminiTS$TradeDate,spEminiTS$Basis, type="l")
plot(spEminiTS$TradeDate,spEminiTS$BasisP, type="l")

###################################################################
# read the SPX cross section data on 20200110 
spxOptionCross=read.csv("./sampleData/spxOptionData20200110.csv", header=T,as.is = T,check.names=FALSE)

###################################################################
# read the SPX Panel data on 20200110 
spxOptionPD=read.csv("./sampleData/spxOptionDataEx20200619.csv", header=T,as.is = T,check.names=FALSE)

###################################################################
# read the SPY data 
spyOptionCross=read.csv("./sampleData/spyOptionData20200110.csv", header=T,as.is = T,check.names=FALSE)

###################################################################
# read the SPY data 
spyOptionPD=read.csv("./sampleData/spyOptionDataEx20200619.csv", header=T,as.is = T,check.names=FALSE)

