# Yashovardhan Jallan
# GT ID: 903206747
# Assignment 7
# Management of Financial Institutions
# Fall 2017

#library(sas7bdat)
library('dplyr')
library(Hmisc)
library(PerformanceAnalytics)
library(fGarch)

myGTid=90320647
set.seed(myGTid)
myYear=sample((1980:2010),1)
sprintf("The year generated is %d",myYear)
# The year generated is 2001 in this case

# Reading the SAS Dataset of 100 companies data between 2000 and 2010
# I have already random sampled 100 companies by PERMNO in SAS so I do not have to do it here
# This is done so that the size of dataset needed to be loaded in R is sizeably reduced
dsf<-read.sas7bdat("required_dsf.sas7bdat")
dsf<-read.csv("Subsetted_DSF.csv",stringsAsFactors=F)
head(dsf)

sortdsf<-dsf[order(dsf$DATE),]
head(sortdsf)

# to check type of the variable
sapply(sortdsf, typeof)
#all variable read as Double which is good

sortdsf$RET<-as.numeric(as.character((sortdsf$RET)))

#removing blanks and NAs
sortdsf<-sortdsf[!(is.na(sortdsf$RET)|sortdsf$RET==""),]

summary<-sortdsf %>% select(DATE,RET) %>% group_by(DATE) %>% summarise(ret=mean(RET))
head(summary)
#sort the summary dataframe by Returns
summary<-summary[order(summary$ret),]
head(summary)

#Assignment 7.1 Part (i)
# Keeping only data between 2001 and 2006
fiveYrDSF=summary[which(summary$DATE>=20010101 & summary$DATE<=20061231),]

p<-0.05
valuep<-100000000

#plotting the Returns as histogram
x<-fiveYrDSF$ret
h<-hist(x, breaks=80, col="red",xlab="Daily Returns", 
     main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=1000) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

VaR_absolute<-abs(quantile(x,p))
VaR_dollar<-abs(quantile(x,p)*valuep)
Expected_Shortfall<-abs(ES(x,p))
Expected_Shortfall_dollar<-Expected_Shortfall*valuep

sprintf("one-day 5 percent VaR: %f",VaR_absolute)
sprintf("$VaR: $%f",VaR_dollar)
sprintf("$Expected Shortfall: $%f",Expected_Shortfall_dollar)


#Assignment 7.1 Part (ii)
# Keeping all data between 2000 and 2010 as required by the question
tenYrDSF=summary

#plotting the Returns as histogram
x<-tenYrDSF$ret
h<-hist(x, breaks=100, col="red",xlab="Daily Returns", 
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=1000) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

VaR_absolute<-abs(quantile(x,p))
VaR_dollar<-abs(quantile(x,p)*valuep)
Expected_Shortfall<-abs(ES(x,p))
Expected_Shortfall_dollar<-Expected_Shortfall*valuep

sprintf("one-day 5 percent VaR: %f",VaR_absolute)
sprintf("$VaR: $%f",VaR_dollar)
sprintf("$Expected Shortfall: $%f",Expected_Shortfall_dollar)

#Assignment 7.2 Part (iii)
#Volatility Modeling

part_three<-summary
part_three$year<-substr(part_three$DATE,1,4)
#Checking variable type
sapply(part_three, typeof)

summaryvariance<-part_three%>%select(ret,year)%>%group_by(year)%>%summarise(std=sd(ret),returns=mean(ret))
#Checking variable type
sapply(summaryvariance, typeof)

summaryvariance$lagret<-lag(summaryvariance$returns,1)
summaryvariance$lagstd<-lag(summaryvariance$std,1)
summaryvariance$calculated_variance<-(0.94*(summaryvariance$lagstd)^2+0.06*(summaryvariance$lagret)^2)

#Plotting the calculated variance (Yearly) by risk Metrics approach:
y_variable=summaryvariance$calculated_variance
x_variable=summaryvariance$year
plot(x_variable,y_variable,type="b",col="blue",main="Plot of Annualized Variance Calculated in Part III",xlab="Year",ylab="Variance",ylim=c(0.0000,0.0006))
legend("top", legend=c("Risk Metrics-Part III"),col=c("blue"), lty=1:2, cex=0.8, bg='lightblue')


#Assignment 7.2 Part (iv)
#GARCH

part_four=summary[which(summary$DATE>=20010101 & summary$DATE<=20061231),]
part_four<-part_four[order(part_four$DATE),]
x.g<-garchFit(~garch(1,1),part_four$ret)
summary(x.g)
coef(x.g)

sprintf("The parameter Alpha is %f",coef(x.g)[3])
sprintf("The parameter Beta is %f",coef(x.g)[4])
sprintf("The parameter Omega is %f",coef(x.g)[2])

#Assignment 7.2 Part (iv)
#GARCH
part_five<-summaryvariance
part_five$garch_variance<-coef(x.g)[2]+coef(x.g)[4]*(part_five$lagstd)^2+coef(x.g)[3]*(part_five$lagret)^2

#Plotting the calculated variance (Yearly) by risk Metrics approach:
y1_variable=part_five$garch_variance
y2_variable=part_five$calculated_variance
x_variable=part_five$year
plot(x_variable,y1_variable,type="b",col="red",main="Plot of Annualized Variance Calculated in Part III and Part V",xlab="Year",ylab="Variance",ylim=c(0.0000,0.0006))
lines(x_variable,y2_variable,type="b",col="blue")
legend("top", legend=c("Garch Method-Part V", "Risk Metrics-Part III"),col=c("red", "blue"), lty=1:2, cex=0.8, bg='lightblue')
