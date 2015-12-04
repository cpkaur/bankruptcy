data <- read.csv("train.csv")
bank_rate <- ts(data$Bankruptcy_Rate, start= c(1987,1), end= c(2010,12),frequency= 12)
pop <- ts(data$Population, start= c(1987,1), end= c(2010,12),frequency= 12)
house <- ts(data$House_Price_Index, start= c(1987,1), end= c(2010,12),frequency= 12)
unemp <- ts(data$Unemployment_Rate, start= c(1987,1), end= c(2010,12),frequency= 12)

plot(bank_rate)
plot(decompose(bank_rate))

par(mfrow=c(4,1))
par(mar = c(3,3.1,3,3.1))
ts.plot(bank_rate)
ts.plot(pop)
ts.plot(unemp)
ts.plot(house)

#ordinary difference:
par(mfrow=c(2,1))
plot(diff(bank_rate))
acf(diff(bank_rate))
#order selection:
par(mfrow=c(2,1))
acf(diff(bank_rate))
pacf(diff(bank_rate))

#p=3, q=2 seems fine

#Fit an ARIMA(1,1,1) model
m1<-arima(bank_rate, order = c(2,1,3))
m2<-arima(bank_rate, order = c(2,1,2))
m3<-arima(bank_rate, order = c(2,1,1))
m4<-arima(bank_rate, order = c(1,1,1))
m5<-arima(bank_rate, order = c(1,1,2))
m6<-arima(bank_rate, order = c(1,1,3))

sigma2<-c(m1$sigma2,m2$sigma2,m3$sigma2,m4$sigma2,m5$sigma2,m6$sigma2)
loglik<-c(m1$loglik,m2$loglik,m3$loglik,m4$loglik,m5$loglik,m6$loglik)
AIC<-c(m1$aic,m2$aic,m3$aic,m4$aic,m5$aic,m6$aic)
d <- data.frame(sigma2,loglik,AIC)
d
#Fit an ARIMA(1,1,1) model with covariate information
m4_1<-arima(bank_rate, order = c(1,1,1), xreg = data.frame(pop,unemp,house))
#Fit an ARIMA(1,1,3) model with covariate information
m6_1<-arima(bank_rate, order = c(1,1,3), xreg = data.frame(pop,unemp,house))
m4_1$sigma2
m4_1$loglik
m4_1$aic
m4$sigma2
m4$loglik
m4$aic
m6_1$sigma2
m6_1$loglik
m6_1$aic
m6$sigma2
m6$loglik
m6$aic


