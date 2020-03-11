library(astsa)
library(bsts)
library(forecast)

#merge df
df <- read.csv("train.csv")
features <- read.csv("features.csv")
df <- aggregate(Weekly_Sales ~ Store + Date, df, sum)
df <- merge(x = df, y = features, by = c("Store", "Date"), x.all = TRUE)
df[is.na(df)]<-0
df$markdown <- (df$MarkDown1+df$MarkDown2+df$MarkDown3+df$MarkDown4+
                       df$MarkDown5)/5
View(df)

#convert timestamp and extract week of the year
df$Date <- as.POSIXct(df$Date, format = '%Y-%m-%d')
df$week <- strftime(df$Date,format="%Y-%W")
df<- df[order(df$Date),]

#set store#
df.1 <- df[df$Store==1,]
View (df.1)

#train,test split
train <- df.1[1:ceiling(nrow(df.1)*.8),]
test <- df.1[ceiling(nrow(df.1)*.8)+1:nrow(df.1),]

# convert to ts, initial insight with data viz and pattern discovery
ts.t <- ts(df.1$Weekly_Sales,  start = c(2010,5), frequency = 52)
ts <- ts(train$Weekly_Sales,  start = c(2010,5), frequency = 52)
ts.test <- ts(test$Weekly_Sales[1:28], start = c(2012,16), frequency = 52)
plot(log(ts))
acf2(log(ts), max.lag = 100)

#training arima model and model dignostic
out <- arima(log(ts), c(3,0,3), seasonal=list(order=c(0,0,2), period=52))
plot(out$residuals)
acf2(out$residuals,max.lag = 100)
qqnorm(out$residuals)
qqline(out$residuals)
sarima(out$residuals,0,0,0)
Box.test(out$residuals,type = "Ljung-Box")

#forecast, test-predict compare and viz
fore.test <- predict(out,n.ahead =28)
fore.test
up <- exp(fore.test$pred+ 1.96 * fore.test$se)
low <- exp(fore.test$pred- 1.96 * fore.test$se)
ts.plot(ts.t,ylab="Weekly Sales")
polygon(c(time(ts.test),rev(time(ts.test))), c(up,rev(low)),border=8,col=gray(.6,alpha=.2))
lines(exp(fore.test$pred),col= "Red")

#one step ahead forecast
one.step <- Arima(log(ts.test), model = out)
MSPE <- (accuracy(one.step)[,"RMSE"])^2
MSPE
one.step.predict <- exp(fitted(one.step))
up <- exp(fitted(one.step) + 1.96 * sqrt((one.step$sigma2)))
low <- exp(fitted(one.step) - 1.96 * sqrt((one.step$sigma2)))
plot(ts.t,ylab="Weekly Sales")
polygon(c(time(ts.test),rev(time(ts.test))), c(up,rev(low)),border=8,col=gray(.6,alpha=.2))
lines(one.step.predict,col= "Red")




### SARIMA WITH REGRESSORS ###

##EXPLORATORY DATA ANALYSIS

##Correlation Plot
xreg <- df[, c(1,3,4,5,6,7,8,9,10,11,12,14)]
res <- cor(xreg)
round(res, 2)

##Correlation Plot with p-values
rcorr(as.matrix(xreg))


##Scatter Plots
plot(df$Store, df$Weekly_Sales, main=" ",
     xlab="Store ", ylab="Weekly Sales ", pch=19)
plot(df$Temperature, df$Weekly_Sales, main=" ",
     xlab="Temperature ", ylab="Weekly Sales ", pch=19)
plot(df$Fuel_Price, df$Weekly_Sales, main=" ",
     xlab="Fuel_Price ", ylab="Weekly Sales ", pch=19)
plot(df$Markdown1, df$Weekly_Sales, main=" ",
     xlab="Markdown1 ", ylab="Weekly Sales ", pch=19)
plot(df$Markdown2, df$Weekly_Sales, main=" ",
     xlab="Markdown2 ", ylab="Weekly Sales ", pch=19)
plot(df$Markdown3, df$Weekly_Sales, main=" ",
     xlab="Markdown3 ", ylab="Weekly Sales ", pch=19)
plot(df$Markdown4, df$Weekly_Sales, main=" ",
     xlab="Markdown4 ", ylab="Weekly Sales ", pch=19)
plot(df$Markdown5, df$Weekly_Sales, main=" ",
     xlab="Markdown5 ", ylab="Weekly Sales ", pch=19)
plot(df$CPI, df$Weekly_Sales, main=" ",
     xlab="CPI ", ylab="Weekly Sales ", pch=19)
plot(df$Unemployment, df$Weekly_Sales, main=" ",
     xlab="Unemployment ", ylab="Weekly Sales ", pch=19)

##Training ARIMA Model and Model Diagnostics
xreg=cbind(df[, c(1)])
out = arima(log(ts), c(3,0,3), seasonal=list(order=c(0,0,2), period=52,xreg=xreg))
acf2(out$residuals,max.lag = 100)
qqnorm(out$residuals)
qqline(out$residuals)
sarima(out$residuals,0,0,0)
Box.test(out$residuals,type = "Ljung-Box")

##Forecast
fore.test <- predict(out,n.ahead =28)
fore.test
up <- exp(fore.test$pred+ 1.96 * fore.test$se)
low <- exp(fore.test$pred- 1.96 * fore.test$se)

ts.plot(ts.t,ylab="Weekly Sales")
polygon(c(time(ts.test),rev(time(ts.test))), c(up,rev(low)),border=8,col=gray(.6,alpha=.2))
lines(exp(fore.test$pred),col= "Red")

out %>%exp(forecast(h=28)) %>% autoplot()+autolayer(ts.test)


##One-Step Ahead Forecast
one.step <- Arima(log(ts.test), model = out)
accuracy(one.step)[,"MAPE"]
one.step.predict <- exp(fitted(one.step))
up <- exp(fitted(one.step) + 1.96 * sqrt((one.step$sigma2)))
low <- exp(fitted(one.step) - 1.96 * sqrt((one.step$sigma2)))
plot(ts.t,ylab="Weekly Sales")
polygon(c(time(ts.test),rev(time(ts.test))), c(up,rev(low)),border=8,col=gray(.6,alpha=.2))
lines(one.step.predict,col= "Red")


### BSTS ###

par(mfrow=c(1,1))
ll_ss <- list()
#ll_ss<- AddAr(state.specification = ll_ss, lags=1,  y = log(ts))
ll_ss <- AddLocalLevel(state.specification = ll_ss, y = log(ts))
#ll_ss <- AddLocalLinearTrend(state.specification = ll_ss, y = log(ts))
ll_ss <- AddSeasonal(state.specification = ll_ss, y = log(ts), nseasons = 52)
ll_fit <- bsts(log(ts), state.specification = ll_ss, niter = 1e3)

acf2(colMeans(ll_fit$one.step.prediction.errors))
Box.test(colMeans(ll_fit$one.step.prediction.errors),type = "Ljung-Box")
sarima(colMeans(ll_fit$one.step.prediction.errors),0,0,0)
plot(ll_fit, main = "Expectation of posterior")
ll_pred <- predict(ll_fit, h = 28)

ll_pred
plot(ll_pred)
lines(c(116:143), y = log(ts.test),col = "red", lwd = 2)


