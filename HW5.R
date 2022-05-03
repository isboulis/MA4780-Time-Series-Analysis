##Homework 5##
library(TSA)
set.seed(4780)
##8.6##
#A#
ar2 = arima(arima.sim(n=48, list(ar=c(1.5,-0.75))), order=c(2,0,0))
plot(rstandard(ar2),ylab ='Standardized Residuals', type='o');

#B#
qqnorm(rstandard(ar2))
qqline(rstandard(ar2))

#C#
acf(rstandard(ar2))

#D#
LB.test(ar2,lag=12)

##8.9##
data(robot)

model1=arima(robot,order=c(1,0,0));
res1=rstandard(model1);

model2=arima(robot,order=c(0,1,1));
res2=rstandard(model2);

plot(res1,ylab='AR(1) Residuals'); abline(h=0)
plot(res2,ylab='IMA(1,1) Residuals'); abline(h=0)

acf(residuals(model1), main='AR(1)')
acf(residuals(model2), main='IMA(1,1)')

shapiro.test(residuals(model2))

tsdiag(model2)

##9.16##
ima=(arima.sim(n=45,list(order=c(0,2,2),ma=c(-1,0.75)))[-1])[-1]
actual=window(ima,start=41); series=window(ima,end=40)
#A#
model=arima(ima ,order=c(0,2,2));

#B#
result=plot(model,n.ahead=5,ylab='Series & Forecasts',col=NULL,pch=19)

#C#
forecast=result$pred;
cbind(actual,forecast)
