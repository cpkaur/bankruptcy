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

# #Fit an ARIMA(1,1,1) model
# m1 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
# m2 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
# m3 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
# m4 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
# m5 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
# m6 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
# m7 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,7), period = 12), method = "ML")
# m8 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
# m9 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
# m10 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
# m11 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
# m12 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
# m13 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
# m14 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
# m15 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
# m16 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
# m17 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
# m18 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
# m19 <- arima(log(bank_rate), order = c(4,1,1), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
# m20 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
# m21 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
# m22 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
# m23 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
# m24 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
# m25 <- arima(log(bank_rate), order = c(3,1,1), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
# m26 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
# m27 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
# m28 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
# m29 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
# m30 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
# m31 <- arima(log(bank_rate), order = c(2,1,1), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
# m32 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
# m33 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
# m34 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
# m35 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
# m36 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
# m37 <- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
# m38 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
# m39 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
# m40 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
# m41 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
# m42 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
# m43 <- arima(log(bank_rate), order = c(1,1,2), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
# m44 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,6), period = 12), method = "ML")
# m45 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
# m46 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,4), period = 12), method = "ML")
# m47 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,3), period = 12), method = "ML")
# m48 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
# m49 <- arima(log(bank_rate), order = c(1,1,3), seasonal = list(order = c(1,0,1), period = 12), method = "ML")
# 
# 
# 
# sigma2<-c(m1$sigma2,m2$sigma2,m4$sigma2,m5$sigma2,m6$sigma2,
#           m7$sigma2,m8$sigma2,m9$sigma2,m10$sigma2, 
#           m11$sigma2, m12$sigma2, m13$sigma2, m14$sigma2, 
#           m15$sigma2,m16$sigma2, m17$sigma2, m18$sigma2, 
#           m19$sigma2, m20$sigma2, m21$sigma2, m22$sigma2, 
#           m23$sigma2, m24$sigma2, m25$sigma2, m26$sigma2, 
#           m27$sigma2, m28$sigma2, m29$sigma2, m30$sigma2,
#           m31$sigma2, m32$sigma2, m33$sigma2,
#           m34$sigma2,m35$sigma2, m36$sigma2, m37$sigma2,
#           m38$sigma2, m39$sigma2, m40$sigma2, m41$sigma2,
#           m42$sigma2, m43$sigma, m44$sigma2, m45$sigma, 
#           m46$sigma2, m47$sigma2, m48$loglik, m49$sigma2)
# loglik<-c(m1$loglik,m2$loglik,m4$loglik,m5$loglik,
#           m6$loglik, m7$loglik,m8$loglik,m9$loglik,m10$loglik,
#           m11$loglik, m12$loglik, m13$loglik, m14$loglik, 
#           m15$loglik,m16$loglik, m17$loglik, m18$loglik, 
#           m19$loglik, m20$loglik, m21$loglik, m22$loglik, 
#           m23$loglik, m24$loglik, m25$loglik, m26$loglik, 
#           m27$loglik, m28$loglik, m29$loglik, m30$loglik,
#           m31$loglik, m32$loglik, m33$loglik, m33$loglik, 
#           m34$loglik,m35$loglik, m36$loglik, m37$loglik,
#           m38$loglik, m39$loglik, m40$loglik, m41$loglik,
#           m42$loglik, m43$loglok, m44$loglik, m45$loglik, 
#           m46$loglik, m47$loglik, m48$loglik, m49$loglik)
# AIC<-c(m1$aic,m2$aic,m4$aic,m5$aic,
#        m6$aic,m7$aic,m8$aic,m9$aic,m10$aic,
#        m11$aic, m12$aic, m13$aic, m14$aic, 
#        m15$aic,m16$aic, m17$aic, m18$aic, 
#        m19$aic, m20$aic, m21$aic, m22$aic, 
#        m23$aic, m24$aic, m25$aic, m26$aic, 
#        m27$aic, m28$aic, m29$aic, m30$aic,
#        m31$aic, m32$aic, m33$aic, m33$aic, 
#        m34$aic,m35$aic, m36$aic, m37$aic,
#        m38$aic, m39$aic, m40$aic, m41$aic,
#        m42$aic, m43$aic, m44$aic, m45$aic, 
#        m46$aic, m47$aic, m49$aic)
# library(qpcR)
# Rmse_val <-c(RMSE(m1),RMSE(m2),RMSE(m4),RMSE(m5),
#              RMSE(m6), RMSE(m7), RMSE(m8), RMSE(m9),
#              RMSE(m10), RMSE(m11), RMSE(m12), RMSE(m13),
#              RMSE(m14), RMSE(m15), RMSE(m16), RMSE(m17), 
#              RMSE(m18),RMSE(m19),RMSE(m20), RMSE(m21), RMSE(m22),
#              RMSE(m23), RMSE(m24), RMSE(m25), RMSE(m26), 
#              RMSE(m27), RMSE(m28), RMSE(m29), RMSE(m30),
#              RMSE(m31), RMSE(m32), RMSE(m33), RMSE(m34),
#              RMSE(m35), RMSE(m36), RMSE(m37), RMSE(m38), 
#              RMSE(m39), RMSE(m40), RMSE(m41), RMSE(m42),
#              RMSE(m43), RMSE(m44), RMSE(m45), RMSE(m46),
#              RMSE(m47), RMSE(m48), RMSE(m49))
# d <- data.frame(sigma2, loglik, AIC, Rmse_val)
# d
# 
# #Fit an ARIMA model with covariate information
# m1_1<- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,7), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")
# m33_1<- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")
# m36_1<- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,2), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")
# mb<- arima(log(bank_rate), order = c(4,1,4), seasonal = list(order = c(2,0,1), period = 1), xreg = data.frame(pop,unemp,house), method = "ML")
# m1$loglik
# m1_1$loglik
# mb$loglik
# m33$loglik
# m33_1$loglik
# m36$loglik
# m36_1$loglik
# 
# m1$aic
# m1_1$aic
# mb$aic
# m33$aic
# m33_1$aic
# m36$aic
# m36_1$aic
# 
# m1$sigma2
# m1_1$sigma2
# mb$sigma2
# m33$sigma2
# m33_1$sigma2
# m36$sigma2
# m36_1$sigma2
# 
# ## best models so far
# # sigma2 -- m1_1
# # aic --- 36
# # loglik --- 36
# # RMSE--- m1_1
# 
# 
# ## checking auto results
# # m52 <- auto.arima(bank_rate, xreg = data.frame(pop,unemp,house))
# # m52
# # m51 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,8), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")
# 
# # NORMALITY
# plot(residuals(m1_1))
# plot(residuals(m36))
# qqnorm(m1_1$residuals)
# qqline(m1_1$residuals)
# shapiro.test(m1_1$residuals) 
# hist(m1_1$residuals, n=100, main="Histogram",
#      xlab="Model Residuals")
# 
# # Zero Mean
# t.test(m1_1$residuals) 
# 
# # HOMOSCEDASTICITY
# library(lawstat)
# par(mfrow=c(1,1))
# plot(m1_1$residuals, main="Residuals vs t", ylab="")
# abline(v=c(1994,2002), lwd=3, col="red")
# #group into different sections for testing
# group <- c(rep(1,96),rep(2,96),rep(3,96)) # useful to try different groupings
# levene.test(m1_1$residuals,group) 
# bartlett.test(m1_1$residuals,group)
# 
# # ZERO CORRELATION
# # test for uncorrelatedness / randomness
# tsdiag(m1_1)
# runs.test(m1_1$residuals) 
# Box.test(m1_1$residuals, type = c("Ljung-Box"))

######################################################################

#### Splitting the data and validating the models

train <- read.csv('train.csv')[1:240,]
test <- read.csv('train.csv')[241:288,]
br_train <- ts(train$Bankruptcy_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
ur_train <- ts(train$Unemployment_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
pop_train <- ts(train$Population, start= c(1987,1), end= c(2006,12),frequency= 12)
hpi_train <- ts(train$House_Price_Index, start= c(1987,1), end= c(2006,12),frequency= 12)
br_test <- ts(test$Bankruptcy_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
pop_test <- ts(test$Population, start= c(2007,1), end= c(2010,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2007,1), end= c(2010,12),frequency= 12)

# # building models with training data 
# m1_1_tr<- arima(log(br_train), order = c(4,1,2), seasonal = list(order = c(1,0,7), period = 12), xreg = data.frame(pop_train,ur_train,hpi_train), method = "ML")
# m33_tr <- arima(log(br_train), order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
# m36_tr <- arima(log(br_train), order = c(1,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
# m33_1_tr <- arima(log(br_train), order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 12), xreg = data.frame(pop_train,ur_train,hpi_train), method = "ML")
# m36_1_tr <- arima(log(br_train), order = c(1,1,1), seasonal = list(order = c(1,0,2), period = 12), xreg = data.frame(pop_train,ur_train,hpi_train), method = "ML")
# mb_tr <- arima(log(br_train), order = c(4,1,4), seasonal = list(order = c(2,0,1), period = 1), xreg = data.frame(pop_train,ur_train,hpi_train), method = "ML")


# ## prediction and comparison
# par(mfrow =c(3,1))
# f <- predict(m1_1_tr, n.ahead=36, se.fit=T, interval="predict", newxreg = data.frame(pop_test,ur_test,hpi_test))
# f$upper <- f$pred + f$se * 1.96
# f$lower <- f$pred - f$se * 1.96
# plot(bank_rate, xlim=c(1987,2010), main ="m1_1_tr: (4,1,2)(1,0,7)")#, ylim=c(500,7000))
# abline(v = 2007, lwd = 2, col = "black")
# lines(exp(f$pred), col="blue")
# lines(exp(f$lower), col="red")
# lines(exp(f$upper), col="red")
# # this last one shows the fit of the model on the history.
# lines(exp(fitted(m1_1_tr)), col='green')
# rmspe <- sqrt(mean((as.numeric(exp(f$pred)) - br_test)^2))
# rmspe 
# 
# f <- predict(m33_1_tr, n.ahead=36, se.fit=T, interval="predict", newxreg = data.frame(pop_test,ur_test,hpi_test))
# f$upper <- f$pred + f$se * 1.96
# f$lower <- f$pred - f$se * 1.96
# plot(bank_rate, xlim=c(1987,2010), main = "m33_1_tr: (1,1,1)(1,0,5)")#, ylim=c(500,7000))
# abline(v = 2007, lwd = 2, col = "black")
# lines(exp(f$pred), col="blue")
# lines(exp(f$lower), col="red")
# lines(exp(f$upper), col="red")
# lines(exp(fitted(m33_1_tr)), col='green')
# rmspe <- sqrt(mean((as.numeric(exp(f$pred)) - br_test)^2))
# rmspe 
# 
# f <- predict(mb_tr, n.ahead=36, se.fit=T, interval="predict", newxreg = data.frame(pop_test,ur_test,hpi_test))
# f$upper <- f$pred + f$se * 1.96
# f$lower <- f$pred - f$se * 1.96
# plot(bank_rate, xlim=c(1987,2010), main = "mb_tr:(4,1,4)(2,0,1)")#, ylim=c(500,7000))
# abline(v = 2007, lwd = 2, col = "black")
# lines(exp(f$pred), col="blue")
# lines(exp(f$lower), col="red")
# lines(exp(f$upper), col="red")
# lines(exp(fitted(mb_tr)), col='green')
# rmspe <- sqrt(mean((as.numeric(exp(f$pred)) - br_test)^2))
# rmspe 
# ####

# rolling window

## m1_1 with all three covariates
train <- read.csv('train.csv')[1:240,]
test <- read.csv('train.csv')[241:288,]
br_train <- ts(train$Bankruptcy_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
ur_train <- ts(train$Unemployment_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
pop_train <- ts(train$Population, start= c(1987,1), end= c(2006,12),frequency= 12)
hpi_train <- ts(train$House_Price_Index, start= c(1987,1), end= c(2006,12),frequency= 12)
br_test <- ts(test$Bankruptcy_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
pop_test <- ts(test$Population, start= c(2007,1), end= c(2010,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2007,1), end= c(2010,12),frequency= 12)

data1 <- log(br_train)
preds <- c()
predsLower <- c()
predsUpper <- c()
for (i in 1:length(br_test)){
  m1_1_roll <- arima(data1, order=c(4,1,2), seasonal = list(order=c(1,0,7), period=1) , xreg = data.frame(pop_train,ur_train, hpi_train), method = "ML")
  one_ahead <- predict(m1_1_roll, n.ahead= 1, interval="predict", newxreg = data.frame(pop_test[1],ur_test[1], hpi_test[1]))
  preds <- ts(c(preds, one_ahead$pred), start=c(2007,1))
  predsLower <-ts(c(predsLower, one_ahead$pred - 1.96*one_ahead$se), start=c(2007,1))
  predsUpper <- ts(c(predsUpper, one_ahead$pred + 1.96*one_ahead$se), start=c(2007,1))
  data1 <- ts(c(data1, one_ahead$pred), start = (data1))
  pop_train <- ts(c(pop_train,pop_test[1]), start = (pop_train))
  pop_test <- pop_test[-1]
  ur_train <- ts(c(ur_train, ur_test[1]), start = (ur_train))
  ur_test <- ur_test[-1]
  hpi_train <- ts(c(hpi_train,hpi_test[1]), start = (hpi_train))
  hpi_test <- hpi_test[-1]
  }
tspred <- ts(preds, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredU <- ts(predsUpper, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredL <- ts(predsLower, start = c(2007,1), end= c(2010,12), frequency = 12)
par(mfrow=c(3,1))
par(mar= c(3,3.1,3,3.1))
plot(bank_rate, main = "m1_1_roll:(4,1,2)(1,0,7) Rolling window with all three cov")#, ylim=c(500,7000))
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspred), col="blue")
lines(exp(tspredU), col = "red")
lines(exp(tspredL), col = "red")
library(forecast)
fit.val<- ts(fitted(m1_1_roll), start=c(1987,1), end = c(2006,12),frequency = 12)
lines(exp(fit.val), col='green')

## m33_1
train <- read.csv('train.csv')[1:240,]
test <- read.csv('train.csv')[241:288,]
br_train <- ts(train$Bankruptcy_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
ur_train <- ts(train$Unemployment_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
pop_train <- ts(train$Population, start= c(1987,1), end= c(2006,12),frequency= 12)
hpi_train <- ts(train$House_Price_Index, start= c(1987,1), end= c(2006,12),frequency= 12)
br_test <- ts(test$Bankruptcy_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
pop_test <- ts(test$Population, start= c(2007,1), end= c(2010,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2007,1), end= c(2010,12),frequency= 12)

data12 <- log(br_train)
preds2 <- c()
predsLower2 <- c()
predsUpper2 <- c()
for (i in 1:length(br_test)){
  m33_1_roll <- arima(data12, order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 1), xreg = data.frame(pop_train,ur_train, hpi_train), method = "ML")
  one_ahead <- predict(m33_1_roll, n.ahead= 1, interval="predict", newxreg = data.frame(pop_test[1],ur_test[1], hpi_test[1]))
  preds2 <- ts(c(preds2, one_ahead$pred), start=c(2007,1))
  predsLower2 <-ts(c(predsLower2, one_ahead$pred - 1.96*one_ahead$se), start=c(2007,1))
  predsUpper2 <- ts(c(predsUpper2, one_ahead$pred + 1.96*one_ahead$se),  start=c(2007,1))
  data12 <- ts(c(data12, one_ahead$pred), start = (data12))
  pop_train <- ts(c(pop_train,pop_test[1]), start = (pop_train))
  pop_test <- pop_test[-1]
  ur_train <- ts(c(ur_train, ur_test[1]), start = (ur_train))
  ur_test <- ur_test[-1]
  hpi_train <- ts(c(hpi_train,hpi_test[1]), start = (hpi_train))
  hpi_test <- hpi_test[-1]
}

tspred2 <- ts(preds2, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredU2 <- ts(predsUpper2, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredL2 <- ts(predsLower2, start = c(2007,1), end= c(2010,12), frequency = 12)

plot(bank_rate, main = "m33_1_roll:(1,1,1)(1,0,5) Rolling window all three cov")#, ylim=c(500,7000))
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspred2), col="blue")
lines(exp(tspredU2), col = "red")
lines(exp(tspredL2), col = "red")
library(forecast)
fit.val2<- ts(fitted(m33_1_roll), start=c(1987,1), end = c(2006,12),frequency = 12)
lines(exp(fit.val2), col='green')

# mb_tr
train <- read.csv('train.csv')[1:240,]
test <- read.csv('train.csv')[241:288,]
br_train <- ts(train$Bankruptcy_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
ur_train <- ts(train$Unemployment_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
pop_train <- ts(train$Population, start= c(1987,1), end= c(2006,12),frequency= 12)
hpi_train <- ts(train$House_Price_Index, start= c(1987,1), end= c(2006,12),frequency= 12)
br_test <- ts(test$Bankruptcy_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
pop_test <- ts(test$Population, start= c(2007,1), end= c(2010,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2007,1), end= c(2010,12),frequency= 12)

data13 <- log(br_train)
preds3 <- c()
predsLower3 <- c()
predsUpper3 <- c()
for (i in 1:length(br_test)){
 
  mb_roll <- arima(data13, order = c(4,1,4), seasonal = list(order = c(2,0,1), period = 1), xreg = data.frame(pop_train,ur_train, hpi_train), method = "ML")
  one_ahead3 <- predict(mb_roll, n.ahead= 1, interval="predict", newxreg = data.frame(pop_test[1],ur_test[1], hpi_test[1]))
  preds3 <- ts(c(preds3, one_ahead$pred), start=c(2007,1))
  predsLower3 <-ts(c(predsLower3, one_ahead$pred - 1.96*one_ahead$se), start=c(2007,1))
  predsUpper3 <- ts(c(predsUpper3, one_ahead$pred + 1.96*one_ahead$se),  start=c(2007,1))
  data13 <- ts(c(data13, one_ahead$pred), start = (data13))
  pop_train <- ts(c(pop_train,pop_test[1]), start = (pop_train))
  pop_test <- pop_test[-1]
  ur_train <- ts(c(ur_train, ur_test[1]), start = (ur_train))
  ur_test <- ur_test[-1]
  hpi_train <- ts(c(hpi_train,hpi_test[1]), start = (hpi_train))
  hpi_test <- hpi_test[-1]
}

tspred3 <- ts(preds3, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredU3 <- ts(predsUpper3, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredL3 <- ts(predsLower3, start = c(2007,1), end= c(2010,12), frequency = 12)

plot(bank_rate, main = "mb_roll:(4,1,4)(2,0,1) Rolling window all three cov")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspred3), col="blue")
lines(exp(tspredU3), col = "red")
lines(exp(tspredL3), col = "red")
library(forecast)
fit.val3<- ts(fitted(mb_roll), start=c(1987,1), end = c(2006,12),frequency = 12)
lines(exp(fit.val3), col='green')
rmspe_rollb <- sqrt(mean((exp(tspred3) - br_test)^2))
rmspe_rollb
rmspe_roll2 <- sqrt(mean((exp(tspred2) - br_test)^2))
rmspe_roll2
rmspe_roll1 <- sqrt(mean((exp(tspred) - br_test)^2))
rmspe_roll1 # 0.00447627

### Moving window--- M1_1 with all three covariates

train <- read.csv('train.csv')[1:240,]
test <- read.csv('train.csv')[241:288,]
br_train <- ts(train$Bankruptcy_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
ur_train <- ts(train$Unemployment_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
pop_train <- ts(train$Population, start= c(1987,1), end= c(2006,12),frequency= 12)
hpi_train <- ts(train$House_Price_Index, start= c(1987,1), end= c(2006,12),frequency= 12)
br_test <- ts(test$Bankruptcy_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
pop_test <- ts(test$Population, start= c(2007,1), end= c(2010,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2007,1), end= c(2010,12),frequency= 12)

data1mv <- log(br_train)
predsmv <- c()
predsLowermv <- c()
predsUppermv <- c()

for (i in 1:length(br_test)){
  m1_1_mv <- arima(data1mv, order=c(4,1,2), seasonal = list(order=c(1,0,7), period=1) , xreg = data.frame(pop_train,ur_train, hpi_train), method = "ML")
  one_ahead <- predict(m1_1_mv, n.ahead= 1, interval="predict", newxreg = data.frame(pop_test[1],ur_test[1], hpi_test[1]))
  predsmv <- ts(c(predsmv, one_ahead$pred), start=c(2007,1))
  predsLowermv <-ts(c(predsLowermv, one_ahead$pred - 1.96*one_ahead$se), start=c(2007,1))
  predsUppermv <- ts(c(predsUppermv, one_ahead$pred + 1.96*one_ahead$se), start=c(2007,1))
  data1mv <- ts(c(data1mv[-1], one_ahead$pred), start = (data1mv[-1]))
  pop_train <- ts(c(pop_train[-1],pop_test[1]), start = (pop_train[-1]))
  pop_test <- pop_test[-1]
  ur_train <- ts(c(ur_train[-1], ur_test[1]), start = (ur_train[-1]))
  ur_test <- ur_test[-1]
  hpi_train <- ts(c(hpi_train[-1],hpi_test[1]), start = (hpi_train))
  hpi_test <- hpi_test[-1]
}

tspredmv <- ts(predsmv, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredUmv <- ts(predsUppermv, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredLmv <- ts(predsLowermv, start = c(2007,1), end= c(2010,12), frequency = 12)
par(mfrow=c(3,1))
plot(bank_rate, main = "m1_1_roll:(4,1,2)(1,0,7) Moving window")#, ylim=c(500,7000))
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspredmv), col="blue")
lines(exp(tspredUmv), col = "red")
lines(exp(tspredLmv), col = "red")
library(forecast)
# fit.valmv<- ts(fitted(m1_1_mv), start=c(1987,1), end = c(2006,12),frequency = 12)
# lines(exp(fit.valmv), col='green')
rmspe_rollmv <- sqrt(mean((exp(tspredmv) - br_test)^2))
rmspe_rollmv #0.005316885 (using2--- pop and ur)--- all three indices # 0.009615296

## m33_1
train <- read.csv('train.csv')[1:240,]
test <- read.csv('train.csv')[241:288,]
br_train <- ts(train$Bankruptcy_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
ur_train <- ts(train$Unemployment_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
pop_train <- ts(train$Population, start= c(1987,1), end= c(2006,12),frequency= 12)
hpi_train <- ts(train$House_Price_Index, start= c(1987,1), end= c(2006,12),frequency= 12)
br_test <- ts(test$Bankruptcy_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
pop_test <- ts(test$Population, start= c(2007,1), end= c(2010,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2007,1), end= c(2010,12),frequency= 12)

data1mv2 <- log(br_train)
predsmv2 <- c()
predsLowermv2 <- c()
predsUppermv2 <- c()

for (i in 1:length(br_test)){
  m33_1_mv <- arima(data1mv2, order = c(1,1,1), seasonal = list(order = c(1,0,5), period=1) , xreg = data.frame(pop_train,ur_train, hpi_train), method = "ML")
  one_ahead <- predict(m33_1_mv, n.ahead= 1, interval="predict", newxreg = data.frame(pop_test[1],ur_test[1], hpi_test[1]))
  predsmv2 <- ts(c(predsmv2, one_ahead$pred), start=c(2007,1))
  predsLowermv2 <-ts(c(predsLowermv2, one_ahead$pred - 1.96*one_ahead$se), start=c(2007,1))
  predsUppermv2 <- ts(c(predsUppermv2, one_ahead$pred + 1.96*one_ahead$se), start=c(2007,1))
  data1mv2 <- ts(c(data1mv2[-1], one_ahead$pred), start = (data1mv2[-1]))
  pop_train <- ts(c(pop_train[-1],pop_test[1]), start = (pop_train[-1]))
  pop_test <- pop_test[-1]
  ur_train <- ts(c(ur_train[-1], ur_test[1]), start = (ur_train[-1]))
  ur_test <- ur_test[-1]
  hpi_train <- ts(c(hpi_train[-1],hpi_test[1]), start = (hpi_train))
  hpi_test <- hpi_test[-1]
}

tspredmv2 <- ts(predsmv2, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredUmv2 <- ts(predsUppermv2, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredLmv2 <- ts(predsLowermv2, start = c(2007,1), end= c(2010,12), frequency = 12)
plot(bank_rate, main = "m33_1_roll:(1,1,2)(1,0,5) Moving window")#, ylim=c(500,7000))
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspredmv2), col="blue")
lines(exp(tspredUmv2), col = "red")
lines(exp(tspredLmv2), col = "red")
library(forecast)
# fit.valmv<- ts(fitted(m1_1_mv), start=c(1987,1), end = c(2006,12),frequency = 12)
# lines(exp(fit.valmv), col='green')
rmspe_rollmv2 <- sqrt(mean((exp(tspredmv2) - br_test)^2))
rmspe_rollmv2 # 0.006174575

## Mb_mv
train <- read.csv('train.csv')[1:240,]
test <- read.csv('train.csv')[241:288,]
br_train <- ts(train$Bankruptcy_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
ur_train <- ts(train$Unemployment_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
pop_train <- ts(train$Population, start= c(1987,1), end= c(2006,12),frequency= 12)
hpi_train <- ts(train$House_Price_Index, start= c(1987,1), end= c(2006,12),frequency= 12)
br_test <- ts(test$Bankruptcy_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
pop_test <- ts(test$Population, start= c(2007,1), end= c(2010,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2007,1), end= c(2010,12),frequency= 12)

data1mv3 <- log(br_train)
predsmv3 <- c()
predsLowermv3 <- c()
predsUppermv3 <- c()

for (i in 1:length(br_test)){
  mb_mv <- arima(data1mv3, order = c(4,1,4), seasonal = list(order = c(2,0,1), period=1) , xreg = data.frame(pop_train,ur_train, hpi_train), method = "ML")
  one_ahead <- predict(mb_mv, n.ahead= 1, interval="predict", newxreg = data.frame(pop_test[1],ur_test[1], hpi_train[1]))
  predsmv3 <- ts(c(predsmv3, one_ahead$pred), start=c(2007,1))
  predsLowermv3 <-ts(c(predsLowermv3, one_ahead$pred - 1.96*one_ahead$se), start=c(2007,1))
  predsUppermv3 <- ts(c(predsUppermv3, one_ahead$pred + 1.96*one_ahead$se), start=c(2007,1))
  data1mv3 <- ts(c(data1mv3[-1], one_ahead$pred), start = (data1mv3[-1]))
  pop_train <- ts(c(pop_train[-1],pop_test[1]), start = (pop_train[-1]))
  pop_test <- pop_test[-1]
  ur_train <- ts(c(ur_train[-1], ur_test[1]), start = (ur_train[-1]))
  ur_test <- ur_test[-1]
  hpi_train <- ts(c(hpi_train[-1],hpi_test[1]), start = (hpi_train[-1]))
  hpi_test <- hpi_test[-1]
}

tspredmv3 <- ts(predsmv3, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredUmv3 <- ts(predsUppermv3, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredLmv3 <- ts(predsLowermv3, start = c(2007,1), end= c(2010,12), frequency = 12)
plot(bank_rate, main = "mb_mv:(4,1,4)(2,0,1) Moving window")#, ylim=c(500,7000))
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspredmv3), col="blue")
lines(exp(tspredUmv3), col = "red")
lines(exp(tspredLmv3), col = "red")
library(forecast)
# fit.valmv<- ts(fitted(m1_1_mv), start=c(1987,1), end = c(2006,12),frequency = 12)
# lines(exp(fit.valmv), col='green')
rmspe_rollmv3 <- sqrt(mean((exp(tspredmv3) - br_test)^2))
rmspe_rollmv3 


## rolling window vs moving window without unemployment rate for M1_1
# rolling window
train <- read.csv('train.csv')[1:240,]
test <- read.csv('train.csv')[241:288,]
br_train <- ts(train$Bankruptcy_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
ur_train <- ts(train$Unemployment_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
pop_train <- ts(train$Population, start= c(1987,1), end= c(2006,12),frequency= 12)
hpi_train <- ts(train$House_Price_Index, start= c(1987,1), end= c(2006,12),frequency= 12)
br_test <- ts(test$Bankruptcy_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
pop_test <- ts(test$Population, start= c(2007,1), end= c(2010,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2007,1), end= c(2010,12),frequency= 12)

data1 <- log(br_train)
preds <- c()
predsLower <- c()
predsUpper <- c()
for (i in 1:length(br_test)){
  m1_1_roll <- arima(data1, order=c(4,1,2), seasonal = list(order=c(1,0,7), period=1) , xreg = data.frame(pop_train, hpi_train), method = "ML")
  one_ahead <- predict(m1_1_roll, n.ahead= 1, interval="predict", newxreg = data.frame(pop_test[1],hpi_test[1]))
  preds <- ts(c(preds, one_ahead$pred), start=c(2007,1))
  predsLower <-ts(c(predsLower, one_ahead$pred - 1.96*one_ahead$se), start=c(2007,1))
  predsUpper <- ts(c(predsUpper, one_ahead$pred + 1.96*one_ahead$se), start=c(2007,1))
  data1 <- ts(c(data1, one_ahead$pred), start = (data1))
  pop_train <- ts(c(pop_train,pop_test[1]), start = (pop_train))
  pop_test <- pop_test[-1]
  hpi_train <- ts(c(hpi_train,hpi_test[1]), start = (hpi_train))
  hpi_test <- hpi_test[-1]
}
tspred <- ts(preds, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredU <- ts(predsUpper, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredL <- ts(predsLower, start = c(2007,1), end= c(2010,12), frequency = 12)
par(mfrow=c(2,1))
plot(bank_rate, main = "m1_1_roll:(4,1,2)(1,0,7) Rolling window")#, ylim=c(500,7000))
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspred), col="blue")
lines(exp(tspredU), col = "red")
lines(exp(tspredL), col = "red")
library(forecast)
fit.val<- ts(fitted(m1_1_roll), start=c(1987,1), end = c(2006,12),frequency = 12)
lines(exp(fit.val), col='green')
# 0.007039486

# moving window
train <- read.csv('train.csv')[1:240,]
test <- read.csv('train.csv')[241:288,]
br_train <- ts(train$Bankruptcy_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
ur_train <- ts(train$Unemployment_Rate, start= c(1987,1), end= c(2006,12),frequency= 12)
pop_train <- ts(train$Population, start= c(1987,1), end= c(2006,12),frequency= 12)
hpi_train <- ts(train$House_Price_Index, start= c(1987,1), end= c(2006,12),frequency= 12)
br_test <- ts(test$Bankruptcy_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2007,1), end= c(2010,12),frequency= 12)
pop_test <- ts(test$Population, start= c(2007,1), end= c(2010,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2007,1), end= c(2010,12),frequency= 12)

data1mv <- log(br_train)
predsmv <- c()
predsLowermv <- c()
predsUppermv <- c()

for (i in 1:length(br_test)){
  m1_1_mv <- arima(data1mv, order=c(4,1,2), seasonal = list(order=c(1,0,7), period=1) , xreg = data.frame(pop_train, hpi_train), method = "ML")
  one_ahead <- predict(m1_1_mv, n.ahead= 1, interval="predict", newxreg = data.frame(pop_test[1], hpi_test[1]))
  predsmv <- ts(c(predsmv, one_ahead$pred), start=c(2007,1))
  predsLowermv <-ts(c(predsLowermv, one_ahead$pred - 1.96*one_ahead$se), start=c(2007,1))
  predsUppermv <- ts(c(predsUppermv, one_ahead$pred + 1.96*one_ahead$se), start=c(2007,1))
  data1mv <- ts(c(data1mv[-1], one_ahead$pred), start = (data1mv[-1]))
  pop_train <- ts(c(pop_train[-1],pop_test[1]), start = (pop_train[-1]))
  pop_test <- pop_test[-1]
  hpi_train <- ts(c(hpi_train[-1],hpi_test[1]), start = (hpi_train))
  hpi_test <- hpi_test[-1]
}

tspredmv <- ts(predsmv, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredUmv <- ts(predsUppermv, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredLmv <- ts(predsLowermv, start = c(2007,1), end= c(2010,12), frequency = 12)

plot(bank_rate, main = "m1_1_roll:(4,1,2)(1,0,7) Moving window with two cov")#, ylim=c(500,7000))
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspredmv), col="blue")
lines(exp(tspredUmv), col = "red")
lines(exp(tspredLmv), col = "red")
library(forecast)
# fit.valmv<- ts(fitted(m1_1_mv), start=c(1987,1), end = c(2006,12),frequency = 12)
# lines(exp(fit.valmv), col='green')
rmspe_rollmv <- sqrt(mean((exp(tspredmv) - br_test)^2))
rmspe_rollmv 
rmspe_roll <- sqrt(mean((exp(tspred) - br_test)^2))
rmspe_roll
