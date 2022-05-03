##TSA Final Project##
library(TSA)
library(forecast)
library(tseries)
library(EnvStats)
library(dplyr)
library(astsa)

getwd()
setwd('~/Desktop/MA 4780')
rawdata <- read.csv("MORTGAGE30US.csv")
data <- ts(rawdata[,-1], frequency = 52)

###########Research Questions
#Did COVID affect the mortgage rates?
#Can our model accurately predict the current rate?

#### Plot of Time-Series Data
par(mfrow=c(1,1))
plot(data, xlab='Years After April 2nd, 1971', ylab='Mortgate Rate')
#Doesn't appear deterministic
## Additionally, the data does not appear to be affected by COVID. 
  # Seeing as we are just beginning to face the inflation from the pandemic, I believe it is too soon
  # to see result of it in the mortgage rates. Thus we will continue with our other research question
  # about the accuracy of our forecast model

######### Test if the mean is zero
### t-test
fm=lm(data~time(data))
summary(fm)
### HAC test
require(sandwich)	
require(lmtest)
coeftest(fm, vcov=vcovHAC(fm))

#### plot the data and the fitted constant trend function
plot(data,ylab="Mortgage Rate",xlab="Years after April 2, 1971")
abline(h=coef(fm),lty=2)


#######compare the standard errors
###standard error in 'lm' function
se1=summary(fm)$coeff[1,2]
se1
###standard error from HAC estimation
se2=coeftest(fm, vcov=vcovHAC(fm))[1,2]
se2
##Look at their ratio.
## Theoretical value for (se2/se1)^2 is 245.5919
(se2/se1)^2


ldata <- log(data)
######## Plot of Log Transformed Time-Series Data
plot(ldata,ylab="Log of Average Mortgage Rate",main="30-Year Fixed Rate")
#log transformation doesn't really change the shape of the data
#but still smooths it out a bit

######## SW test for Log Transformed Data
shapiro.test(ldata)
### Returns a significant P-vaues

########Creating linear model
model <- lm(ldata~c(1:length(data)))
plot(resid(model), type='l', main = 'Residual Plot of Log Transformed Data', ylab = 'Residuals', xlab = 'Weeks after April 2, 1971' )
summary(model)
## R^2 = 0.7159

#########Tests for log-transformed data
pp.test(ldata)
kpss.test(ldata)
adf.test(ldata)
## all statistically significant 


########Finding a model
acf(ldata)
pacf(ldata)
#PCAF and ACF suggest ARIMA(1,1,0)


auto.arima(ldata)
#suggests ARIMA(1,1,2)

eacf(ldata)
#suggests ARIMA(2,1,1) and 

##Model 1 
model1log <- Arima(ldata, order = c(1,1,0), xreg = time(ldata)) 
model1log 
    #AIC = -14917.22
    #AICc = -14917.21
    #BIC =-14899.55
model1 <- Arima(data, order = c(1,1,0), xreg = time(data)) 
model1
  #AIC = -4518.72
  #AICc = -4518.71
  #BIC = -4501.05


##Model 2
model2log <- Arima(ldata, order = c(1,1,2), xreg = time(ldata)) 
model2log
  #AIC = -14965.56
  #AICc = -14965.53
  #BIC = -14936.12
model2 <- Arima(data, order = c(1,1,2), xreg = time(ldata)) 
model2 
  #AIC = -4627.51
  #AICc = -4627.49
  #BIC = -4598.07


#Model 3
model3log <- Arima(ldata, order = c(2,1,1), xreg = time(ldata)) 
model3log
  #AIC = -14965.29
  #AICc = -14965.27
  #BIC =-14935.86
model3 <- Arima(data, order = c(2,1,1), xreg = time(data)) 
model3 
  #AIC = -4626.17
  #AICc = -4626.15
  #BIC = -4596.73


####### Best Model
### We can see that model 1, ARIMA(1,1,0), has the best AIC, AICc and BIC for both the log transformed data and the orginal data. We will then continue with this model. 

####### Residual Analysis for Model 1  
residualsm1 <- model1$residuals 
plot(residualsm1,type = 'p', main = 'Plot of Residuals for ARIMA(1,1,0)', ylab='Residuals') 
abline(h =0) 

###plot for normality
qqnorm(residualsm1) 
qqline(residualsm1) 

###SW Test
shapiro.test(residualsm1)
#statistically significant

###Runs Test
runs(residualsm1) 


###LBox Test
Box.test(residualsm1, type = 'Ljung-Box') 
#statistically significant


############Forecasting the ARIMA(1,1,0) model
forecastmodel <- Arima(data, order=c(1,1,0))
tsdiag(forecastmodel)


predx=predict(forecastmodel,n.ahead=10)
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

ymin=min(c(as.vector(lci),data))-1
ymax=max(c(as.vector(uci),data))+1


plot(data,xlim=c(52,53),ylim=c(ymin,ymax),main="Average Mortgage Forecast")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)

