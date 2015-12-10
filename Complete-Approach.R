# complete approach used for Bankruptcy time series analysis 

#####################################################################################
#                           SARIMA APPROACH                                         #
#####################################################################################

library(tseries)
library(forecast)
library(lmtest)
library(lawstat)

train <- read.csv("train.csv")
bank_rate <- ts(train$Bankruptcy_Rate, 
                start= c(1987,1), end= c(2010,12),frequency= 12)


############################################################
# TRAINING and TEST set modeling
############################################################
bank.train <- ts(bank_rate[1:240])
bank.test <- ts(bank_rate[241:288])

par(mfrow=c(2,1))
plot(bank.train) # not stationary plus, ordinary difference 1 time
acf(bank.train) # evidence of heteroscedasticity --> try log transform

lbank.train <- ts(log(bank.train), start= c(1987,1), end= c(2006,12),frequency= 12)
plot(diff(lbank.train)) # looks homoscedastic now.
acf(diff(lbank.train), lag.max=48)

# choose p,q,P,Q
acf(diff(lbank.train), lag.max=48)
pacf(diff(lbank.train), lag.max=48)
# p <= 4, q<= 4, P <= 2, Q <= 2

# iteratate across all models.
# commented out models did not converge
m1 <- arima(lbank.train, order=c(4,1,4), seasonal = list(order=c(2,0,2), period=12))
# m2 <- arima(lbank.train, order=c(4,1,3), seasonal = list(order=c(2,0,2), period=12))
# m3 <- arima(lbank.train, order=c(4,1,2), seasonal = list(order=c(2,0,2), period=12))
# m4 <- arima(lbank.train, order=c(4,1,1), seasonal = list(order=c(2,0,2), period=12))
# m5 <- arima(lbank.train, order=c(3,1,4), seasonal = list(order=c(2,0,2), period=12))
m6 <- arima(lbank.train, order=c(3,1,3), seasonal = list(order=c(2,0,2), period=12))
# m7 <- arima(lbank.train, order=c(3,1,2), seasonal = list(order=c(2,0,2), period=12))
# m8 <- arima(lbank.train, order=c(3,1,1), seasonal = list(order=c(2,0,2), period=12))
m9 <- arima(lbank.train, order=c(2,1,4), seasonal = list(order=c(2,0,2), period=12))
m10 <- arima(lbank.train, order=c(2,1,3), seasonal = list(order=c(2,0,2), period=12))
m11 <- arima(lbank.train, order=c(2,1,2), seasonal = list(order=c(2,0,2), period=12))
m12 <- arima(lbank.train, order=c(2,1,1), seasonal = list(order=c(2,0,2), period=12))
m13 <- arima(lbank.train, order=c(1,1,4), seasonal = list(order=c(2,0,2), period=12))
m14 <- arima(lbank.train, order=c(1,1,3), seasonal = list(order=c(2,0,2), period=12))
# m15 <- arima(lbank.train, order=c(1,1,2), seasonal = list(order=c(2,0,2), period=12))
m16 <- arima(lbank.train, order=c(1,1,1), seasonal = list(order=c(2,0,2), period=12))
m17 <- arima(lbank.train, order=c(4,1,4), seasonal = list(order=c(2,0,1), period=12))
# m18 <- arima(lbank.train, order=c(4,1,3), seasonal = list(order=c(2,0,1), period=12))
m19 <- arima(lbank.train, order=c(4,1,2), seasonal = list(order=c(2,0,1), period=12))
m20 <- arima(lbank.train, order=c(4,1,1), seasonal = list(order=c(2,0,1), period=12))
m21 <- arima(lbank.train, order=c(3,1,4), seasonal = list(order=c(2,0,1), period=12))
m22 <- arima(lbank.train, order=c(3,1,3), seasonal = list(order=c(2,0,1), period=12))
m23 <- arima(lbank.train, order=c(3,1,2), seasonal = list(order=c(2,0,1), period=12))
m24 <- arima(lbank.train, order=c(3,1,1), seasonal = list(order=c(2,0,1), period=12))
m25 <- arima(lbank.train, order=c(2,1,4), seasonal = list(order=c(2,0,1), period=12))
m26 <- arima(lbank.train, order=c(2,1,3), seasonal = list(order=c(2,0,1), period=12))
m27 <- arima(lbank.train, order=c(2,1,2), seasonal = list(order=c(2,0,1), period=12))
m28 <- arima(lbank.train, order=c(2,1,1), seasonal = list(order=c(2,0,1), period=12))
m29 <- arima(lbank.train, order=c(1,1,4), seasonal = list(order=c(2,0,1), period=12))
m30 <- arima(lbank.train, order=c(1,1,3), seasonal = list(order=c(2,0,1), period=12))
m31 <- arima(lbank.train, order=c(1,1,2), seasonal = list(order=c(2,0,1), period=12))
m32 <- arima(lbank.train, order=c(1,1,1), seasonal = list(order=c(2,0,1), period=12))
m33 <- arima(lbank.train, order=c(4,1,4), seasonal = list(order=c(1,0,2), period=12))
# m34 <- arima(lbank.train, order=c(4,1,3), seasonal = list(order=c(1,0,2), period=12))
m35 <- arima(lbank.train, order=c(4,1,2), seasonal = list(order=c(1,0,2), period=12))
m36 <- arima(lbank.train, order=c(4,1,1), seasonal = list(order=c(1,0,2), period=12))
m37 <- arima(lbank.train, order=c(3,1,4), seasonal = list(order=c(1,0,2), period=12))
m38 <- arima(lbank.train, order=c(3,1,3), seasonal = list(order=c(1,0,2), period=12))
m39 <- arima(lbank.train, order=c(3,1,2), seasonal = list(order=c(1,0,2), period=12))
m40 <- arima(lbank.train, order=c(3,1,1), seasonal = list(order=c(1,0,2), period=12))
m41 <- arima(lbank.train, order=c(2,1,4), seasonal = list(order=c(1,0,2), period=12))
m42 <- arima(lbank.train, order=c(2,1,3), seasonal = list(order=c(1,0,2), period=12))
m43 <- arima(lbank.train, order=c(2,1,2), seasonal = list(order=c(1,0,2), period=12))
m44 <- arima(lbank.train, order=c(2,1,1), seasonal = list(order=c(1,0,2), period=12))
m45 <- arima(lbank.train, order=c(1,1,4), seasonal = list(order=c(1,0,2), period=12))
m46 <- arima(lbank.train, order=c(1,1,3), seasonal = list(order=c(1,0,2), period=12))
m47 <- arima(lbank.train, order=c(1,1,2), seasonal = list(order=c(1,0,2), period=12))
m48 <- arima(lbank.train, order=c(1,1,1), seasonal = list(order=c(1,0,2), period=12))
# m49 <- arima(lbank.train, order=c(4,1,4), seasonal = list(order=c(1,0,1), period=12))
# m50 <- arima(lbank.train, order=c(4,1,3), seasonal = list(order=c(1,0,1), period=12))
# m51 <- arima(lbank.train, order=c(4,1,2), seasonal = list(order=c(1,0,1), period=12))
m52 <- arima(lbank.train, order=c(4,1,1), seasonal = list(order=c(1,0,1), period=12))
m53 <- arima(lbank.train, order=c(3,1,4), seasonal = list(order=c(1,0,1), period=12))
m54 <- arima(lbank.train, order=c(3,1,3), seasonal = list(order=c(1,0,1), period=12))
m55 <- arima(lbank.train, order=c(3,1,2), seasonal = list(order=c(1,0,1), period=12))
m56 <- arima(lbank.train, order=c(3,1,1), seasonal = list(order=c(1,0,1), period=12))
# m57 <- arima(lbank.train, order=c(2,1,4), seasonal = list(order=c(1,0,1), period=12))
m58 <- arima(lbank.train, order=c(2,1,3), seasonal = list(order=c(1,0,1), period=12))
m59 <- arima(lbank.train, order=c(2,1,2), seasonal = list(order=c(1,0,1), period=12))
m60 <- arima(lbank.train, order=c(2,1,1), seasonal = list(order=c(1,0,1), period=12))
# m61 <- arima(lbank.train, order=c(1,1,4), seasonal = list(order=c(1,0,1), period=12))
# m62 <- arima(lbank.train, order=c(1,1,3), seasonal = list(order=c(1,0,1), period=12))
# m63 <- arima(lbank.train, order=c(1,1,2), seasonal = list(order=c(1,0,1), period=12))
m64 <- arima(lbank.train, order=c(1,1,1), seasonal = list(order=c(1,0,1), period=12))
m65 <- arima(lbank.train, order=c(4,1,0))

# 2,3,4,5,15,19,34,45,49,50,51,57,61,62,63 -- models did not converge
# 33 doesn't converge for all predictions

sigma2 <- c(m1$sigma2, NA, NA, NA, NA, 
            m6$sigma2, NA, NA, m9$sigma2, m10$sigma2, 
            m11$sigma2, m12$sigma2,
            m13$sigma2, m14$sigma2, NA, m16$sigma2, m17$sigma2,
            NA, m19$sigma2, m20$sigma2, m21$sigma2, m22$sigma2,
            m23$sigma2, m24$sigma2, m25$sigma2, m26$sigma2, m27$sigma2, 
            m28$sigma2, m29$sigma2, m30$sigma2, m31$sigma2, m32$sigma2,
            m33$sigma2, NA, m35$sigma2, m36$sigma2, m37$sigma2,
            m38$sigma2, m39$sigma2, m40$sigma2, m41$sigma2, m42$sigma2,
            m43$sigma2, m44$sigma2, NA, m46$sigma2, m47$sigma2,
            m48$sigma2, NA, NA, NA, m52$sigma2,
            m53$sigma2, m54$sigma2, m55$sigma2, m56$sigma2, NA,
            m58$sigma2, m59$sigma2, m60$sigma2, NA, NA,
            NA, m64$sigma2, m65$sigma2)

loglik <- c(m1$loglik, NA, NA, NA, NA, 
            m6$loglik, NA, NA, m9$loglik, m10$loglik, 
            m11$loglik, m12$loglik,
            m13$loglik, m14$loglik, NA, m16$loglik, m17$loglik,
            NA, m19$loglik, m20$loglik, m21$loglik, m22$loglik,
            m23$loglik, m24$loglik, m25$loglik, m26$loglik, m27$loglik, 
            m28$loglik, m29$loglik, m30$loglik, m31$loglik, m32$loglik,
            m33$loglik, NA, m35$loglik, m36$loglik, m37$loglik,
            m38$loglik, m39$loglik, m40$loglik, m41$loglik, m42$loglik,
            m43$loglik, m44$loglik, NA, m46$loglik, m47$loglik,
            m48$loglik, NA, NA, NA, m52$loglik,
            m53$loglik, m54$loglik, m55$loglik, m56$loglik, NA,
            m58$loglik, m59$loglik, m60$loglik, NA, NA,
            NA, m64$loglik, m65$loglik)

AIC <- c(m1$aic, NA, NA, NA, NA, 
         m6$aic, NA, NA, m9$aic, m10$aic, 
         m11$aic, m12$aic,
         m13$aic, m14$aic, NA, m16$aic, m17$aic,
         NA, m19$aic, m20$aic, m21$aic, m22$aic,
         m23$aic, m24$aic, m25$aic, m26$aic, m27$aic, 
         m28$aic, m29$aic, m30$aic, m31$aic, m32$aic,
         m33$aic, NA, m35$aic, m36$aic, m37$aic,
         m38$aic, m39$aic, m40$aic, m41$aic, m42$aic,
         m43$aic, m44$aic, NA, m46$aic, m47$aic,
         m48$aic, NA, NA, NA, m52$aic,
         m53$aic, m54$aic, m55$aic, m56$aic, NA,
         m58$aic, m59$aic, m60$aic, NA, NA,
         NA, m64$aic, m65$aic)

RMSE <- c(sqrt(mean((m1$residuals)^2)), NA, NA, NA,
          NA, sqrt(mean((m6$residuals)^2)),
          NA, NA,
          sqrt(mean((m9$residuals)^2)), sqrt(mean((m10$residuals)^2)),
          sqrt(mean((m11$residuals)^2)), sqrt(mean((m12$residuals)^2)),
          sqrt(mean((m13$residuals)^2)), sqrt(mean((m14$residuals)^2)),
          NA, sqrt(mean((m16$residuals)^2)),
          sqrt(mean((m17$residuals)^2)), NA,
          sqrt(mean((m19$residuals)^2)), sqrt(mean((m20$residuals)^2)),
          sqrt(mean((m21$residuals)^2)), sqrt(mean((m22$residuals)^2)),
          sqrt(mean((m23$residuals)^2)), sqrt(mean((m24$residuals)^2)),
          sqrt(mean((m25$residuals)^2)), sqrt(mean((m26$residuals)^2)),
          sqrt(mean((m27$residuals)^2)), sqrt(mean((m28$residuals)^2)),
          sqrt(mean((m29$residuals)^2)), sqrt(mean((m30$residuals)^2)),
          sqrt(mean((m31$residuals)^2)), sqrt(mean((m32$residuals)^2)),
          sqrt(mean((m33$residuals)^2)), NA,
          sqrt(mean((m35$residuals)^2)), sqrt(mean((m36$residuals)^2)),
          sqrt(mean((m37$residuals)^2)), sqrt(mean((m38$residuals)^2)),
          sqrt(mean((m39$residuals)^2)), sqrt(mean((m40$residuals)^2)),
          sqrt(mean((m41$residuals)^2)), sqrt(mean((m42$residuals)^2)),
          sqrt(mean((m43$residuals)^2)), sqrt(mean((m44$residuals)^2)),
          NA, sqrt(mean((m46$residuals)^2)),
          sqrt(mean((m47$residuals)^2)), sqrt(mean((m48$residuals)^2)),
          NA, NA, NA, sqrt(mean((m52$residuals)^2)),
          sqrt(mean((m53$residuals)^2)), sqrt(mean((m54$residuals)^2)),
          sqrt(mean((m55$residuals)^2)), sqrt(mean((m56$residuals)^2)),
          NA, sqrt(mean((m58$residuals)^2)),
          sqrt(mean((m59$residuals)^2)), sqrt(mean((m60$residuals)^2)),
          NA, NA,NA, sqrt(mean((m64$residuals)^2)),
          sqrt(mean((m65$residuals)^2)))

d <- data.frame(sigma2,loglik,AIC,RMSE)
d

# all say model 33 is the best
# but it doesn't converge for all predictions in the test set, 
# use m35 instead
which.min(d$sigma2)
which.max(d$loglik)
which.min(d$AIC)
which.min(d$RMSE)

## m1 # SARIMA(4,1,4)x(2,0,2)x12, all coefficients are significant -- these were the best when
# m17 <- arima(lbank.train, order=c(4,1,4), seasonal = list(order=c(2,0,1), period=12))
# m33 <- arima(lbank.train, order=c(4,1,4), seasonal = list(order=c(1,0,2), period=12))
## m35 # SARIMA(4,1,2)x(1,0,2)x12, all coefficients are significant -- training set was 1:200


# this model is the best when training set is 1:240

logD <- -2*(m35$loglik - m33$loglik)
pval <- 1-pchisq(logD,1)
print(c("Test Statistic:", round(logD, 5),"P-value:", round(pval, 5)))
# there isn't a statistical signifance btwn m35 and m33.
# choose m35 since is has less parameters

# ZERO MEAN
# visually check and formally test whether residuals have zero mean
par(mfrow=c(1,1))
plot(m35$residuals, main = "Residuals of SARIMA(4,1,2)x(1,0,2) \n LOG(bankRate)")
abline(h=0, col='red')
t.test(m35$residuals) 

# HOMOSCEDASTICITY
par(mfrow=c(1,1))
plot(m35$residuals, main="Residuals vs t", ylab="")
abline(v=c(1994,2000), lwd=3, col="red")
#group into different sections for testing
group <- c(rep(1,80),rep(2,80),rep(3,80)) # useful to try different groupings
levene.test(m35$residuals,group) 
bartlett.test(m35$residuals,group)

# ZERO CORRELATION
# test for uncorrelatedness / randomness
tsdiag(m35)
runs.test(m35$residuals) 
Box.test(m35$residuals, type = c("Ljung-Box"))

# NORMALITY
par(mfrow=c(1,1))
qqnorm(m35$residuals, main="QQ-plot of Residuals")
qqline(m35$residuals, col='red')
shapiro.test(m35$residuals) 
hist(m35$residuals, n=100, main="Histogram of SARIMA(4,1,2)x(1,0,2) \n LOG(bankRate)",
     xlab="Model Residuals")

# m33: ALL TESTS PASS
# but m33 doesn't converge for all predictions. use m35
#############################################
# FORECAST WITH m35 SARIMA(4,1,2)x(1,0,2)
#############################################

f <- predict(m35, n.ahead=48, se.fit=T, interval="predict")
f$upper <- f$pred + f$se * 1.96
f$lower <- f$pred - f$se * 1.96
f

par(mfrow=c(1,1))
plot(bank.train, xlim=c(0,300), ylim=c(0,0.07), main="Predictions SARIMA(4,1,2)x(1,0,2)")
abline(v = 240, lwd = 2, col = "black")
points(241:288,exp(f$pred), type = "l", col = "blue")
points(241:288,exp(f$lower), type = "l", col = "red")
points(241:288,exp(f$upper), type = "l", col = "red")
# this last one shows the fit of the model on the history.
lines(exp(fitted(m35)), col='green')
points(ts(bank_rate), type='l', col='black')

rmspe <- sqrt(mean((exp(as.numeric(f$pred)) - bank.test)^2))
rmspe

## m1 # SARIMA(4,1,4)x(2,0,2)x12, all coefficients are significant -- these were the best when
# m17 <- arima(lbank.train, order=c(4,1,4), seasonal = list(order=c(2,0,1), period=12))
# m33 <- arima(lbank.train, order=c(4,1,4), seasonal = list(order=c(1,0,2), period=12))
## m35 # SARIMA(4,1,2)x(1,0,2)x12, all coefficients are significant -- training set was 1:200
# m66 <- arima(lbank.train, order=c(4,1,2), seasonal = list(order=c(1,0,7), period=12))

# using moving window
data1 <- lbank.train
preds <- c()
predsLower <- c()
predsUpper <- c()
for (i in 1:length(bank.test)){
  m35.MV <- arima(data1, order=c(4,1,2), seasonal = list(order=c(1,0,2), period=12), 
                  method='ML', optim.control = list(maxit = 10000) )
  one_ahead <- predict(m35.MV, n.ahead=1)
  # data1 <- ts(c(data1, one_ahead$pred))
  data1 <- ts(c(data1[-1], one_ahead$pred))
  preds <- c(preds, one_ahead$pred)
  predsLower <- c(predsLower, one_ahead$pred - 1.96*one_ahead$se)
  predsUpper <- c(predsUpper, one_ahead$pred + 1.96*one_ahead$se)
}
#m33 doesn't converge for all predictions. use m35

tspred <- ts(preds, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredU <- ts(predsUpper, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredL <- ts(predsLower, start = c(2007,1), end= c(2010,12), frequency = 12)
ts.bank.train <- ts(bank.train, start= c(1987,1), end= c(2006,12),frequency= 12)

par(mfrow=c(1,1))
plot(bank_rate, xlim=c(1987,2012), 
     main="Moving Window SARIMA Model on Bankruptcy Rate",
     ylab="Bankruptcy Rate", xlab="Date")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspred), type = "l", col = "red")
lines(exp(tspredU), type = "l", col = "blue")
lines(exp(tspredL), type = "l", col = "blue")
# points(bank_rate, type='l', col='black')
lines(exp(fitted(m35)), col='red')
legend('topleft', c("Observations", "Predictions", "Confidence Interval"), lty=c(1,1,1),
       col=c("black", "blue", "red"), lwd=c(2.5,2.5), cex=0.65)
text(2000, 0.01, 'sub-training')
text(2010, 0.01, 'sub-test')


rmspe_MV <- sqrt(mean((exp(preds) - bank.test)^2))
rmspe_MV
# ZOOM
par(mfrow=c(1,1))
plot(ts.bank.train, xlim=c(2005,2012), ylim=c(0,0.04), 
     main="Predictions SARIMA(4,1,2)x(1,0,2) \n Rolling Window",
     ylab="Bankruptcy Rate")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspred), type = "l", col = "red")
lines(exp(tspredU), type = "l", col = "blue")
lines(exp(tspredL), type = "l", col = "blue")
lines(exp(fitted(m35)), col='red')
points(bank_rate, type='l', col='black')
legend('bottomright', c("Observations", "Predictions", "Confidence Interval"), lty=c(1,1,1),
       col=c("black", "blue", "red"), lwd=c(2.5,2.5), cex=0.65)

rmspe
rmspe_MV

#####################################################################################
#                           HOLT-WINTERS APPROACH                                         #
#####################################################################################

library(ggplot2)
library(gridExtra)
library(corrplot)
library(tseries)
library(forecast)
library(lawstat)

# read data
whole <- read.csv('train.csv')
# change Month into date format
mm <- whole$Month
mm1 <- sapply(mm, function(x) ifelse (nchar(x) == 5, paste("0", x, sep=""), x))
mm2 <- sapply(mm1, function(x) paste(substr(x, 1, 2), substr(x, 3, 6), sep="-01-"))
mm3 <- as.Date(mm2, "%m-%d-%Y")
whole$date <- mm3

train <- whole[1:240,]
test <- whole[241:288,]
br <- ts(train$Bankruptcy_Rate)
ur <- ts(train$Unemployment_Rate)
pop <- ts(train$Population)
hpi <- ts(train$House_Price_Index)

cor(train[,2:5]) # highly related to Population, related to House_Price_Index, may related to Unemployment
corrplot.mixed(cor(train[,2:5]), lower='number', upper='circle')

# holt-winter
br2 <-ts(br,start=1,freq=12) # Convert data to a 'ts' object


# rolling window
rolling_window <- function(data, freq, n, alpha) {
  # data is a time series with freq > 1
  # n is the number of predictions
  # return a time series of prediction data
  data <- ts(data, start=1, freq=freq)
  preds <- c()
  preds_upr <- c()
  preds_lwr <- c()
  for (i in 1:n) {
    hw <- HoltWinters(data, alpha = alpha)
    predicts <- predict(hw, n.ahead = 1, prediction.interval = T, level = 0.95)
    pred <- predicts[,1]
    pred_upr <- predicts[,2]
    pred_lwr <- predicts[,3]
    data <- ts(c(data, pred), start=1, freq=freq)
    preds <- c(preds, pred)
    preds_upr <- c(preds_upr, pred_upr)
    preds_lwr <- c(preds_lwr, pred_lwr)
  }
  return(data.frame(preds, preds_upr, preds_lwr))
}


# try alpha = 0.1, 0.2, 0.25, 0.3, 0.5, 0.75, 1
alpha <- c(0.1, 0.2, 0.25, 0.3, 0.5, 0.75, 1)
rmspe <- c()
pred_train <- data.frame(c(seq(1, nrow(test), 1)))
pred_train_upr <- data.frame(c(seq(1, nrow(test), 1)))
pred_train_lwr <- data.frame(c(seq(1, nrow(test), 1)))
for (al in alpha) {
  br2.pred <- rolling_window(br2, 12, 48, al)  
  pred_train <- data.frame(pred_train, br2.pred[,1])
  pred_train_upr <- data.frame(pred_train_upr, br2.pred[,2])
  pred_train_lwr <- data.frame(pred_train_lwr, br2.pred[,3])
  e <- sqrt(mean((br2.pred[,1]-test$Bankruptcy_Rate)^2)) # Root mean square error (RMSE)
  rmspe <- c(rmspe, e)
}

best.rmspe <- min(rmspe)
best.alpha <- alpha[which.min(rmspe)] # alpha = 0.5
best_pred_train <- pred_train[, which.min(rmspe) + 1]
best_pred_train_upr <- pred_train_upr[, which.min(rmspe) + 1]
best_pred_train_lwr <- pred_train_lwr[, which.min(rmspe) + 1]
best.rmse <- sqrt(sum((best_pred_train-whole$Bankruptcy_Rate)^2)/length(best_pred_train))

hw1 <- HoltWinters(br2, alpha = best.alpha, beta = F, gamma = F)
br3 <- c(br[1], hw1$fitted[,1], best_pred_train)

# plot observations and predictions (split to train and test)
plot(whole$Bankruptcy_Rate ~ whole$date, type="l",col="black", 
     xlab="Date", ylab="Brankruptcy Rate",
     main="Rolling Window Holt-Winters Model on Brankruptcy Rate")
abline(v=as.Date("2007-01-01"), col="black", lty=2)
lines(br3 ~ whole$date, col="red") # prediction
lines(best_pred_train_upr ~ test$date, col="blue") # upper 
lines(best_pred_train_lwr ~ test$date, col="blue") # lower
text(as.Date("2000-01-01"), 0.01, "sub-training")
text(as.Date("2009-10-01"), 0.01, "sub-test")
legend(as.Date("1987-01-01"), 0.045, c("Observations","Predictions", "Confidence Interval"), lty=c(1,1,1), lwd=c(1,1,1), col=c("black", "red", "blue"), cex = 0.5) 

# use whole data set to build the model
br_all <- whole$Bankruptcy_Rate
br_all <- ts(br_all,start=1,freq=12)
hw_all <- HoltWinters(br_all, alpha = best.alpha, beta = F, gamma = F)
pred_all <- c(br_all[1], hw_all$fitted[,1])

# plot observations and predictions on whole training set
plot(whole$Bankruptcy_Rate ~ whole$date, type="l",col="black", main="Holt_Winter Model on Brankruptcy Rate")
lines(pred_all ~ whole$date, col="red")
legend(as.Date("1987-01-01"), 0.045, c("Observations","Predictions"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black", "red")) 



# moving window
moving_window <- function(data, freq, n, alpha) {
  # data is a time series with freq > 1
  # n is the number of predictions
  # return a time series of prediction data
  data <- ts(data, start=1, freq=freq)
  preds <- c()
  preds_upr <- c()
  preds_lwr <- c()
  for (i in 1:n) {
    hw <- HoltWinters(data, alpha = alpha)
    predicts <- predict(hw, n.ahead = 1, prediction.interval = T, level = 0.95)
    pred <- predicts[,1]
    pred_upr <- predicts[,2]
    pred_lwr <- predicts[,3]
    data <- ts(c(data[-1], pred), start=1, freq=freq)
    preds <- c(preds, pred)
    preds_upr <- c(preds_upr, pred_upr)
    preds_lwr <- c(preds_lwr, pred_lwr)
  }
  return(data.frame(preds, preds_upr, preds_lwr))
}

# try alpha = 0.1, 0.2, 0.25, 0.3, 0.5, 0.75, 1
alpha <- c(0.1, 0.2, 0.25, 0.3, 0.5, 0.75, 1)
rmspe2 <- c()
pred_train2 <- data.frame(c(seq(1, nrow(test), 1)))
pred_train_upr2 <- data.frame(c(seq(1, nrow(test), 1)))
pred_train_lwr2 <- data.frame(c(seq(1, nrow(test), 1)))
for (al in alpha) {
  br2.pred2 <- moving_window(br2, 12, 48, al)  
  pred_train2 <- data.frame(pred_train2, br2.pred2[,1])
  pred_train_upr2 <- data.frame(pred_train_upr2, br2.pred2[,2])
  pred_train_lwr2 <- data.frame(pred_train_lwr2, br2.pred2[,3])
  e2 <- sqrt(mean((br2.pred2[,1]-test$Bankruptcy_Rate)^2)) # Root mean square error (RMSE)
  rmspe2 <- c(rmspe2, e2)
}

best.rmspe2 <- min(rmspe2)
best.alpha2 <- alpha[which.min(rmspe2)] # alpha = 0.5
best_pred_train2 <- pred_train2[, which.min(rmspe2) + 1]
best_pred_train_upr2 <- pred_train_upr2[, which.min(rmspe2) + 1]
best_pred_train_lwr2 <- pred_train_lwr2[, which.min(rmspe2) + 1]
best.rmse2 <- sqrt(mean((best_pred_train2-whole$Bankruptcy_Rate)^2))

hw2 <- HoltWinters(br2, alpha = best.alpha2, beta = F, gamma = F)
br3.2 <- c(br[1], hw2$fitted[,1], best_pred_train2)

# plot observations and predictions (split to train and test)
plot(whole$Bankruptcy_Rate ~ whole$date, type="l",col="black", 
     xlab="Date", ylab="Brankruptcy Rate",
     main="Moving Window Holt-Winters Model on Brankruptcy Rate")
abline(v=as.Date("2007-01-01"), col="black", lty=2)
lines(br3.2 ~ whole$date, col="red") # prediction
lines(best_pred_train_upr2 ~ test$date, col="blue") # upper 
lines(best_pred_train_lwr2 ~ test$date, col="blue") # lower
legend(as.Date("1987-01-01"), 0.045, c("Observations","Predictions", "Confidence Interval"), lty=c(1,1,1), lwd=c(2,2,2), col=c("black", "red", "blue")) 
text(as.Date("2000-01-01"), 0.01, "sub-training")
text(as.Date("2009-10-01"), 0.01, "sub-test")


#Forecasting on real test data
test.fct <- read.table('forecast_values.txt', head=T)
fct <- ts(test.fct[,1], start = c(2011,1), end= c(2011,12), frequency = 12)
fct.up <- ts(test.fct[,2], start = c(2011,1), end= c(2011,12), frequency = 12)
fct.low <- ts(test.fct[,3], start = c(2011,1), end= c(2011,12), frequency = 12)
fct.date <- seq(as.Date("2011-01-01"), by = "month", length.out = 12)
df <- data.frame(fct, fct.up, fct.low, fct.date)


# plot observations and predictions (split to train and test)
plot(whole$Bankruptcy_Rate ~ whole$date, type="l",col="black", 
     xlab="Date", ylab="Brankruptcy Rate", 
     xlim=c(as.Date(as.Date("1987-01-01")), as.Date(as.Date("2011-12-01"))),
     main="Moving Window SARIMA Model on Brankruptcy Rate")
abline(v=as.Date("2011-01-01"), col="black", lty=2)
lines(fct ~ fct.date, col="red")
lines(fct.up ~ fct.date, col="blue")
lines(fct.low ~ fct.date, col="blue")
legend(as.Date("1987-01-01"), 0.045, c("Observations","Predictions", "Confidence Interval"), lty=c(1,1,1), lwd=c(2,2,2), col=c("black", "red", "blue")) 
text(as.Date("2000-01-01"), 0.01, "training")
text(as.Date("2011-09-01"), 0.01, "test")


br <- ts(whole$Bankruptcy_Rate, start= c(1987,1), end= c(2010,12),frequency= 12)
hpi <- ts(whole$House_Price_Index, start= c(1987,1), end= c(2010,12),frequency= 12)
pop <- ts(whole$Population, start= c(1987,1), end= c(2010,12),frequency= 12)
lbr <- log(br)


# plot observations and predictions for real forecasting
plot(whole$Bankruptcy_Rate[144:288] ~ whole$date[144:288], type="l",col="black", 
     xlab="Date", ylab="Brankruptcy Rate", 
     xlim=c(as.Date(as.Date("1998-12-01")), as.Date(as.Date("2011-12-01"))),
     main="Moving Window SARIMA Model on Brankruptcy Rate")
abline(v=as.Date("2011-01-01"), col="black", lty=2)
lines(fit.br[144:288] ~ whole$date[144:288], col="green")
lines(fct ~ fct.date, col="red")
lines(fct.up ~ fct.date, col="blue")
lines(fct.low ~ fct.date, col="blue")
legend(as.Date("1999-01-01"), 0.045, c("Observations","Fitted Value", "Predictions", "Confidence Interval"), lty=c(1,1,1,1), lwd=c(2,2,2,2), col=c("black", "green", "red", "blue"), cex=0.75) 
text(as.Date("2006-01-01"), 0.02, "training")
text(as.Date("2011-8-01"), 0.02, "test")

#####################################################################################
#                           SARIMA with covariates APPROACH                                         #
#####################################################################################

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

# p=4, q=2
# P = 1, Q = 7
# Fit an ARIMA model

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

#Fitting an ARIMA model with covariate information from the best models 
# deduced from the ablove analysis. Model mb was additional suggestion by Binjie for comparison purposes
m1_1<- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,7), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")
m33_1<- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")
m36_1<- arima(log(bank_rate), order = c(1,1,1), seasonal = list(order = c(1,0,2), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")
mb<- arima(log(bank_rate), order = c(4,1,4), seasonal = list(order = c(2,0,1), period = 1), xreg = data.frame(pop,unemp,house), method = "ML")
m1$loglik
m1_1$loglik
mb$loglik
m33$loglik
m33_1$loglik
m36$loglik
m36_1$loglik

m1$aic
m1_1$aic
mb$aic
m33$aic
m33_1$aic
m36$aic
m36_1$aic

m1$sigma2
m1_1$sigma2
mb$sigma2
m33$sigma2
m33_1$sigma2
m36$sigma2
m36_1$sigma2

# ## best models so far
# # sigma2 <- m1_1
# # aic <- 36
# # loglik <-  36
# # RMSE <- m1_1
# 
# 
# ## checking auto results
# m52 <- auto.arima(bank_rate, xreg = data.frame(pop,unemp,house))
# m52
# m51 <- arima(log(bank_rate), order = c(4,1,2), seasonal = list(order = c(1,0,8), period = 12), xreg = data.frame(pop,unemp,house), method = "ML")

## Checking diagnostics for model m1_1
# NORMALITY
plot(residuals(m1_1))
plot(residuals(m36))
qqnorm(m1_1$residuals)
qqline(m1_1$residuals)
shapiro.test(m1_1$residuals) 
hist(m1_1$residuals, n=100, main="Histogram",
     xlab="Model Residuals")

# Zero Mean
t.test(m1_1$residuals) 

# HOMOSCEDASTICITY
library(lawstat)
par(mfrow=c(1,1))
plot(m1_1$residuals, main="Residuals vs t", ylab="")
abline(v=c(1994,2002), lwd=3, col="red")
#group into different sections for testing
group <- c(rep(1,96),rep(2,96),rep(3,96)) # useful to try different groupings
levene.test(m1_1$residuals,group) 
bartlett.test(m1_1$residuals,group)

# ZERO CORRELATION
# test for uncorrelatedness / randomness
tsdiag(m1_1)
runs.test(m1_1$residuals) 
Box.test(m1_1$residuals, type = c("Ljung-Box"))


# Splitting the training data set to validate models 

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

## Building models with training data 
## both with and without covariate information for comparison
m1_1_tr<- arima(log(br_train), order = c(4,1,2), seasonal = list(order = c(1,0,7), period = 1), xreg = data.frame(pop_train,ur_train,hpi_train), method = "ML")
m33_tr <- arima(log(br_train), order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 12), method = "ML")
m36_tr <- arima(log(br_train), order = c(1,1,1), seasonal = list(order = c(1,0,2), period = 12), method = "ML")
m33_1_tr <- arima(log(br_train), order = c(1,1,1), seasonal = list(order = c(1,0,5), period = 12), xreg = data.frame(pop_train,ur_train,hpi_train), method = "ML")
m36_1_tr <- arima(log(br_train), order = c(1,1,1), seasonal = list(order = c(1,0,2), period = 12), xreg = data.frame(pop_train,ur_train,hpi_train), method = "ML")
mb_tr <- arima(log(br_train), order = c(4,1,4), seasonal = list(order = c(2,0,1), period = 1), xreg = data.frame(pop_train,ur_train,hpi_train), method = "ML")


## Prediction and comparison
par(mfrow =c(3,1))
f <- predict(m1_1_tr, n.ahead=36, se.fit=T, interval="predict", newxreg = data.frame(pop_test,ur_test,hpi_test))
f$upper <- f$pred + f$se * 1.96
f$lower <- f$pred - f$se * 1.96
plot(bank_rate, xlim=c(1987,2010), main ="m1_1_tr: (4,1,2)(1,0,7)")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(f$pred), col="blue")
lines(exp(f$lower), col="red")
lines(exp(f$upper), col="red")
# This last one shows the fit of the model on the history.
lines(exp(fitted(m1_1_tr)), col='green')
rmspe <- sqrt(mean((as.numeric(exp(f$pred)) - br_test)^2))
rmspe 
 
f2 <- predict(m33_1_tr, n.ahead=36, se.fit=T, interval="predict", newxreg = data.frame(pop_test,ur_test,hpi_test))
f2$upper <- f2$pred + f2$se * 1.96
f2$lower <- f2$pred - f2$se * 1.96
plot(bank_rate, xlim=c(1987,2010), main = "m33_1_tr: (1,1,1)(1,0,5)")
lines(exp(f2$pred), col="blue")
lines(exp(f2$lower), col="red")
lines(exp(f2$upper), col="red")
lines(exp(fitted(m33_1_tr)), col='green')
rmspe <- sqrt(mean((as.numeric(exp(f2$pred)) - br_test)^2))
rmspe 

f3 <- predict(mb_tr, n.ahead=36, se.fit=T, interval="predict", newxreg = data.frame(pop_test,ur_test,hpi_test))
f3$upper <- f3$pred + f3$se * 1.96
f3$lower <- f3$pred - f3$se * 1.96
plot(bank_rate, xlim=c(1987,2010), main = "mb_tr:(4,1,4)(2,0,1)")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(f3$pred), col="blue")
lines(exp(f3$lower), col="red")
lines(exp(f3$upper), col="red")
lines(exp(fitted(mb_tr)), col='green')
rmspe <- sqrt(mean((as.numeric(exp(f3$pred)) - br_test)^2))
rmspe 


## Rolling window approach
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
plot(bank_rate, main = "m1_1_roll:(4,1,2)(1,0,7) Rolling window with all three cov")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspred), col="blue")
lines(exp(tspredU), col = "red")
lines(exp(tspredL), col = "red")
library(forecast)
fit.val<- ts(fitted(m1_1_roll), start=c(1987,1), end = c(2006,12),frequency = 12)
lines(exp(fit.val), col='green')

## Model m33_1 with Rolling Window and all three covariates
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

plot(bank_rate, main = "m33_1_roll:(1,1,1)(1,0,5) Rolling window all three cov")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspred2), col="blue")
lines(exp(tspredU2), col = "red")
lines(exp(tspredL2), col = "red")
library(forecast)
fit.val2<- ts(fitted(m33_1_roll), start=c(1987,1), end = c(2006,12),frequency = 12)
lines(exp(fit.val2), col='green')

# Model mb_tr with Rolling Window and all three covariates
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

# RMSPE values for these three models
rmspe_rollb <- sqrt(mean((exp(tspred3) - br_test)^2))
rmspe_rollb
rmspe_roll2 <- sqrt(mean((exp(tspred2) - br_test)^2))
rmspe_roll2
rmspe_roll1 <- sqrt(mean((exp(tspred) - br_test)^2))
rmspe_roll1 

### Moving window approach
## M1_1 with all three covariates and moving window approach

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
plot(bank_rate, main = "m1_1_roll:(4,1,2)(1,0,7) Moving window")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspredmv), col="blue")
lines(exp(tspredUmv), col = "red")
lines(exp(tspredLmv), col = "red")

rmspe_rollmv <- sqrt(mean((exp(tspredmv) - br_test)^2))
rmspe_rollmv 


## M33_1 with all three covariates and moving window approach
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
plot(bank_rate, main = "m33_1_roll:(1,1,2)(1,0,5) Moving window")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspredmv2), col="blue")
lines(exp(tspredUmv2), col = "red")
lines(exp(tspredLmv2), col = "red")
rmspe_rollmv2 <- sqrt(mean((exp(tspredmv2) - br_test)^2))
rmspe_rollmv2 

## Mb_mv with all three covariates and moving window approach
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
plot(bank_rate, main = "mb_mv:(4,1,4)(2,0,1) Moving window")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspredmv3), col="blue")
lines(exp(tspredUmv3), col = "red")
lines(exp(tspredLmv3), col = "red")
rmspe_rollmv3 <- sqrt(mean((exp(tspredmv3) - br_test)^2))
rmspe_rollmv3 


## Rolling window vs Moving window without unemployment rate for M1_1

# Rolling window without unemployment rate for M1_1
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
plot(bank_rate, main = "m1_1_roll:(4,1,2)(1,0,7) Rolling window")
abline(v = 2007, lwd = 2, col = "black")
lines(exp(tspred), col="blue")
lines(exp(tspredU), col = "red")
lines(exp(tspredL), col = "red")
library(forecast)
fit.val<- ts(fitted(m1_1_roll), start=c(1987,1), end = c(2006,12),frequency = 12)
lines(exp(fit.val), col='green')


# Moving window without unemployment rate for M1_1

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
  hpi_train <- ts(c(hpi_train[-1],hpi_test[1]), start = (hpi_train[-1]))
  hpi_test <- hpi_test[-1]
}

tspredmv <- ts(predsmv, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredUmv <- ts(predsUppermv, start = c(2007,1), end= c(2010,12), frequency = 12)
tspredLmv <- ts(predsLowermv, start = c(2007,1), end= c(2010,12), frequency = 12)

# Plotting (code to be consistent with other approach plots)
whole <- read.csv('train.csv')
# change Month into date format
mm <- whole$Month
mm1 <- sapply(mm, function(x) ifelse (nchar(x) == 5, paste("0", x, sep=""), x))
mm2 <- sapply(mm1, function(x) paste(substr(x, 1, 2), substr(x, 3, 6), sep="-01-"))
mm3 <- as.Date(mm2, "%m-%d-%Y")
whole$date <- mm3

m1_1 <- arima(data1, order=c(4,1,2), seasonal = list(order=c(1,0,7), period=1) , xreg = data.frame(pop_train, hpi_train), method = "ML")
fit.train <- exp(fitted(m1_1))
fit.all <- c(fit.train, exp(preds))

# plot observations and predictions (split to train and test)
plot(whole$Bankruptcy_Rate ~ whole$date, type="l",col="black", 
     xlab="Date", ylab="Brankruptcy Rate",
     main="Moving Window SARIMA Model (with covariates) on Brankruptcy Rate")
abline(v=as.Date("2007-01-01"), col="black", lty=2)
lines(fit.all ~ whole$date, col="red") # prediction
lines(exp(tspredU) ~ whole$date[241:288], col="blue") # upper 
lines(exp(tspredL) ~ whole$date[241:288], col="blue") # lower
legend(as.Date("1987-01-01"), 0.045, c("Observations","Predictions", "Confidence Interval"), lty=c(1,1,1), lwd=c(2,2,2), col=c("black", "red", "blue")) 
text(as.Date("2000-01-01"), 0.01, "sub-training")
text(as.Date("2009-10-01"), 0.01, "sub-test")

# RMSPE- values
rmspe_rollmv <- sqrt(mean((exp(tspredmv) - br_test)^2))
rmspe_rollmv 
rmspe_roll <- sqrt(mean((exp(tspred) - br_test)^2))
rmspe_roll

#####################################################################################
#                           DIAGNOSTIC TESTS OF OPTIMAL MODEL                      #
#####################################################################################


# Moving window
whole <- read.csv('train.csv')
br <- ts(whole$Bankruptcy_Rate, start= c(1987,1), end= c(2010,12),frequency= 12)
hpi <- ts(whole$House_Price_Index, start= c(1987,1), end= c(2010,12),frequency= 12)
pop <- ts(whole$Population, start= c(1987,1), end= c(2010,12),frequency= 12)
lbr <- log(br)

# Residual diagnostics
m1_1_mv <- arima(lbr, order=c(4,1,2), seasonal = list(order=c(1,0,7), period=1) , 
                 xreg = data.frame(pop, hpi), method = "ML")
ee <- m1_1_mv$residuals

# Test for heteroscedasticity
par(mfrow=c(1,1))
plot(ee, main="Residuals vs t", ylab="")
abline(v=c(72,144,216), lwd=3, col="red")
group <- c(rep(1,72),rep(2,72),rep(3,72),rep(4,72))
levene.test(ee,group) #Levene  # p value 0.28, not reject homo
bartlett.test(ee,group) #Bartlett   # p value 0.09, not reject homo

# test for uncorrelatedness / randomness
tsdiag(m1_1_mv) #ACF and Ljung-Box test all in one!  # Ljung-Box, not reject null
runs.test(ee) #Runs test for randomness

# test for normality
par(mfrow=c(1,1))
qqnorm(ee, main="QQ-plot of Residuals")
qqline(ee)
shapiro.test(ee) #SW test # p value is large, not reject the null. pass the normality.


#####################################################################################
#                           FORECASTING WITH ACTUAL DATA                            #
#####################################################################################

data <- read.csv("train.csv")
br_train <- ts(data$Bankruptcy_Rate, start= c(1987,1), end= c(2010,12),frequency= 12)
pop_train <- ts(data$Population, start= c(1987,1), end= c(2010,12),frequency= 12)
hpi_train <- ts(data$House_Price_Index, start= c(1987,1), end= c(2010,12),frequency= 12)
ur_train <- ts(data$Unemployment_Rate, start= c(1987,1), end= c(2010,12),frequency= 12)

test <- read.csv("test.csv")
pop_test <- ts(test$Population, start= c(2011,1), end= c(2011,12),frequency= 12)
hpi_test <- ts(test$House_Price_Index, start= c(2011,1), end= c(2011,12),frequency= 12)
ur_test <- ts(test$Unemployment_Rate, start= c(2011,1), end= c(2011,12),frequency= 12)

### Moving average forcasting using POP and HPI
data1mv <- log(br_train)
predsmv <- c()
predsLowermv <- c()
predsUppermv <- c()

for (i in 1:length(pop_test)){
  m1_1_mv <- arima(data1mv, order=c(4,1,2), seasonal = list(order=c(1,0,7), period=1) , xreg = data.frame(pop_train, hpi_train), method = "ML")
  one_ahead <- predict(m1_1_mv, n.ahead= 1, interval="predict", newxreg = data.frame(pop_test[1], hpi_test[1]))
  predsmv <- ts(c(predsmv, one_ahead$pred), start=c(2011,1))
  predsLowermv <-ts(c(predsLowermv, one_ahead$pred - 1.96*one_ahead$se), start=c(2011,1))
  predsUppermv <- ts(c(predsUppermv, one_ahead$pred + 1.96*one_ahead$se), start=c(2011,1))
  data1mv <- ts(c(data1mv[-1], one_ahead$pred), start = (data1mv[-1]))
  pop_train <- ts(c(pop_train[-1],pop_test[1]), start = (pop_train[-1]))
  pop_test <- pop_test[-1]
  hpi_train <- ts(c(hpi_train[-1],hpi_test[1]), start = (hpi_train))
  hpi_test <- hpi_test[-1]
}

tspredmv <- ts(predsmv, start = c(2011,1), end= c(2011,12), frequency = 12)
tspredUmv <- ts(predsUppermv, start = c(2011,1), end= c(2011,12), frequency = 12)
tspredLmv <- ts(predsLowermv, start = c(2011,1), end= c(2011,12), frequency = 12)

plot(br_train, main = "m1_1_roll:(4,1,2)(1,0,7) Moving window")
abline(v = 2011, lwd = 2, col = "black")
lines(exp(tspredmv), col="blue")
lines(exp(tspredUmv), col = "red")
lines(exp(tspredLmv), col = "red")

t <- data.frame(exp(predsmv),exp(predsUppermv), exp(predsLowermv))
t

