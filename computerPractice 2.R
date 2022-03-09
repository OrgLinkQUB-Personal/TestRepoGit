#Clear all the memories
rm(list=ls(all=TRUE))
#change the working directory to that of the current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(bizdays)

#################################################################
### put call parity 
#################################################################
# read the raw data
spxOptionCross=read.csv("./sampleData/spxOptionData20200110.csv", header=T,as.is = T,check.names=FALSE, colClasses=c("numeric","character","character",rep("numeric",3)))
spxOptionCross$TradeDate=as.Date(spxOptionCross$TradeDate, format = "%Y%m%d")
spxOptionCross$expiryDate=as.Date(spxOptionCross$expiryDate, format = "%Y%m%d")

sort(unique(spxOptionCross$expiryDate))
selectedExpiry="2020-06-19"
oneExpiryOptionData=spxOptionCross[spxOptionCross$expiryDate==selectedExpiry,]

# calculate business days
business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))
ttm=bizdays(oneExpiryOptionData$TradeDate[1], oneExpiryOptionData$expiryDate[1], cal = business_calendar)/252

callOptionData=oneExpiryOptionData[oneExpiryOptionData$c_p==1,]
callOptionData=callOptionData[order(callOptionData$strike),]
putOptionData=oneExpiryOptionData[oneExpiryOptionData$c_p==0,]
putOptionData=putOptionData[order(putOptionData$strike),]

group <- ifelse(oneExpiryOptionData$c_p==1, "Call options", "Put options")
plot(oneExpiryOptionData$strike,oneExpiryOptionData$price,pch = 19,col = factor(group), cex = 0.5)


# pair call and put
pairedOptions=data.frame(strike=sort(unique(oneExpiryOptionData$strike))) 

pairedOptions=merge(x=pairedOptions,y=callOptionData[,c('strike','price')],by="strike",all.x=TRUE)
colnames(pairedOptions)[colnames(pairedOptions)=='price'] <- "callPrice"
pairedOptions=merge(x=pairedOptions,y=putOptionData[,c('strike','price')],by="strike",all.x=TRUE)
colnames(pairedOptions)[colnames(pairedOptions)=='price'] <- "putPrice"

pairedOptions$impliedSPX_Pseudo=pairedOptions$callPrice+pairedOptions$strike-pairedOptions$putPrice
plot(pairedOptions$strike,pairedOptions$impliedSPX_Pseudo)

# implied futures, interest rate
sampleImpliedR=data.frame(strike=pairedOptions$strike,x=(pairedOptions$putPrice-pairedOptions$callPrice))
resIR=lm(strike ~ x, data = sampleImpliedR)
impliedSPXF=resIR$coefficients[1]
impliedR=log(resIR$coefficients[2])/ttm


pairedOptions$impliedSPXF=pairedOptions$callPrice*exp(impliedR*ttm)+pairedOptions$strike-pairedOptions$putPrice*exp(impliedR*ttm)
plot(pairedOptions$strike,pairedOptions$impliedSPXF)

# get the corresponding index level
underlying <- read.csv("./sampleData/underlyingSampleData.csv", header=T,as.is = T)
underlying$TradeDate=as.Date(underlying$TradeDate, format = "%Y-%m-%d")
spotUndelrying=underlying[underlying$TradeDate=="2020-01-10",'SPX']

# get the corresponding futures price
spEminiCross=read.csv("./sampleData/spxEminiTrade20200110.csv", header=T,as.is = T,check.names=FALSE)
spEminiCross$TradeDate=as.Date(spEminiCross$TradeDate, format = "%d/%m/%Y")
spotSPEmini=spEminiCross[1,'19/06/2020']

# implied dividend yield
impliedDividend=impliedR-log(impliedSPXF/spotUndelrying)/ttm

# implied index level
pairedOptions$impliedSPX=(pairedOptions$callPrice+pairedOptions$strike*exp(-impliedR*ttm)-pairedOptions$putPrice)*exp(impliedDividend*ttm)

plot(pairedOptions$strike,pairedOptions$impliedSPX)
impliedSPX=mean(pairedOptions$impliedSPX)
#################################################################


#################################################################
### Option bounds
#################################################################
#call options
callOptionData$upperBound=spotUndelrying
callOptionData$lowerBound=impliedSPX*exp(-impliedDividend*ttm)-callOptionData$strike*exp(-impliedR*ttm)
callOptionData$lowerBound[callOptionData$lowerBound<0]=0


plot(callOptionData$strike,callOptionData$price, cex = 0.5,ylim=c(0,spotUndelrying*1.2))
lines(callOptionData$strike,callOptionData$upperBound,col="green")
lines(callOptionData$strike,callOptionData$lowerBound,col="red")

#put option
putOptionData$upperBound=putOptionData$strike*exp(-impliedR*ttm)
putOptionData$lowerBound=putOptionData$strike*exp(-impliedR*ttm)-spotUndelrying*exp(-impliedDividend*ttm)
putOptionData$lowerBound[putOptionData$lowerBound<0]=0

plot(putOptionData$strike,putOptionData$price, cex = 0.5,ylim=c(0,spotUndelrying*1.2))
lines(putOptionData$strike,putOptionData$upperBound,col="green")
lines(putOptionData$strike,putOptionData$lowerBound,col="red")
#################################################################
