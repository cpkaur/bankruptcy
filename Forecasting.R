## Forecasting
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
library(forecast) 

t <- data.frame(Forecast = exp(predsmv), LowerLimit = exp(predsLowermv), UpperLimit = exp(predsUppermv))
t

write.table(t, file = "Forecast_values.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
