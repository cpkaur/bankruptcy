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

# transforming the data comparison
par(mfrow=c(2,1))
plot(diff(bank_rate))
plot(diff(log(bank_rate)))

#order selection:
par(mfrow=c(2,1))
acf(diff(log(bank_rate)), lag.max=128)
pacf(diff(log(bank_rate)), lag.max = 128)

#p=4, q=2
# P = 1, Q = 7

#Fit an ARIMA(1,1,1) model
m1 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
m2 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
m3 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
m4 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
m5 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
m6 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
m7 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
m8 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
m9 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
m10 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
m11 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
m12 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
m13 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
m14 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
m15 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
m16 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
m17 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
m18 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
m19 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
m20 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
m21 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
m22 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
m23 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
m24 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
m25 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
m26 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
m27 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
m28 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
m29 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
m30 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
m31 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
m32 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
m33 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
m34 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
m35 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
m36 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
m37 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
m38 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
m39 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
m40 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
m41 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
m42 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
m43 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
m44 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
m45 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
m46 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
m47 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
m48 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
m49 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,1), period = 12), method = "ML")



sigma2<-c(m1$sigma2,m2$sigma2,m4$sigma2,m5$sigma2,m6$sigma2,
          m7$sigma2,m8$sigma2,m9$sigma2,m10$sigma2, 
          m11$sigma2, m12$sigma2, m13$sigma2, m14$sigma2, 
          m15$sigma2,m16$sigma2, m17$sigma2, m18$sigma2, 
          m19$sigma2, m20$sigma2, m21$sigma2, m22$sigma2, 
          m23$sigma2, m24$sigma2, m25$sigma2, m26$sigma2, 
          m27$sigma2, m28$sigma2, m29$sigma2, m30$sigma2,
          m31$sigma2, m32$sigma2, m33$sigma2,
          m34$sigma2,m35$sigma2, m36$sigma2, m37$sigma2,
          m38$sigma2, m39$sigma2, m40$sigma2, m41$sigma2,
          m42$sigma2, m43$sigma, m44$sigma2, m45$sigma, 
          m46$sigma2, m47$sigma2, m48$loglik, m49$sigma2)
loglik<-c(m1$loglik,m2$loglik,m4$loglik,m5$loglik,
          m6$loglik, m7$loglik,m8$loglik,m9$loglik,m10$loglik,
          m11$loglik, m12$loglik, m13$loglik, m14$loglik, 
          m15$loglik,m16$loglik, m17$loglik, m18$loglik, 
          m19$loglik, m20$loglik, m21$loglik, m22$loglik, 
          m23$loglik, m24$loglik, m25$loglik, m26$loglik, 
          m27$loglik, m28$loglik, m29$loglik, m30$loglik,
          m31$loglik, m32$loglik, m33$loglik, m33$loglik, 
          m34$loglik,m35$loglik, m36$loglik, m37$loglik,
          m38$loglik, m39$loglik, m40$loglik, m41$loglik,
          m42$loglik, m43$loglok, m44$loglik, m45$loglik, 
          m46$loglik, m47$loglik, m48$loglik, m49$loglik)
AIC<-c(m1$aic,m2$aic,m4$aic,m5$aic,
       m6$aic,m7$aic,m8$aic,m9$aic,m10$aic,
       m11$aic, m12$aic, m13$aic, m14$aic, 
       m15$aic,m16$aic, m17$aic, m18$aic, 
       m19$aic, m20$aic, m21$aic, m22$aic, 
       m23$aic, m24$aic, m25$aic, m26$aic, 
       m27$aic, m28$aic, m29$aic, m30$aic,
       m31$aic, m32$aic, m33$aic, m33$aic, 
       m34$aic,m35$aic, m36$aic, m37$aic,
       m38$aic, m39$aic, m40$aic, m41$aic,
       m42$aic, m43$aic, m44$aic, m45$aic, 
       m46$aic, m47$aic, m49$aic)
library(qpcR)
Rmse_val <-c(RMSE(m1),RMSE(m2),RMSE(m4),RMSE(m5),
             RMSE(m6), RMSE(m7), RMSE(m8), RMSE(m9),
             RMSE(m10), RMSE(m11), RMSE(m12), RMSE(m13),
             RMSE(m14), RMSE(m15), RMSE(m16), RMSE(m17), 
             RMSE(m18),RMSE(m19),RMSE(m20), RMSE(m21), RMSE(m22),
             RMSE(m23), RMSE(m24), RMSE(m25), RMSE(m26), 
             RMSE(m27), RMSE(m28), RMSE(m29), RMSE(m30),
             RMSE(m31), RMSE(m32), RMSE(m33), RMSE(m34),
             RMSE(m35), RMSE(m36), RMSE(m37), RMSE(m38), 
             RMSE(m39), RMSE(m40), RMSE(m41), RMSE(m42),
             RMSE(m43), RMSE(m44), RMSE(m45), RMSE(m46),
             RMSE(m47), RMSE(m48), RMSE(m49))
d <- data.frame(sigma2, loglik, AIC, Rmse_val)
d

## min(sigma) <- m1
## min(aic) <- m33
## min(loglik) <- m36
## min(rmse) <- m1


#Fit an ARIMA model with covariate information
m1_1<- arima(bank_rate, order = c(4,1,2), seasonal = list(order = c(1,0,7), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")
m33_1<- arima(bank_rate, order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")
m36_1<- arima(bank_rate, order = c(1,1,1), seasonal = list(order = c(1,0,2), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")

m1$loglik
m1_1$loglik
m33$loglik
m33_1$loglik
m36$loglik
m36_1$loglik

m1$aic
m1_1$aic
m33$aic
m33_1$aic
m36$aic
m36_1$aic

m1$sigma2
m1_1$sigma2
m33$sigma2
m33_1$sigma2
m36$sigma2
m36_1$sigma2

# sigma2 -- m1_1
# aic --- 36
# loglik --- 36
# RMSE--- m1_1


## checking auto results
# m52 <- auto.arima(bank_rate, xreg = data.frame(pop,unemp,house))
# m52
# m51 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,8), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")


plot(residuals(m1_1))
plot(residuals(m36))
tsdiag(m1_1)
qqnorm(m1_1$residuals)
qqline(m1_1$residuals)


## Chosing m1_1 as the best model

## trying this by splitting the data set into training
## and test set also trying rolling window method 
## of prediction

