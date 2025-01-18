library(forecast)
library(ggplot2)
library(TTR)

traking_signal = function(pre_data, prediction, n){
  for (i in 1:length(pre_data)) {
    err_df[i] = pre_data[i] - prediction[i]
  }
  err_df
  mfe = mean(err_df, na.rm = TRUE)
  mad = mean(abs(err_df), na.rm = TRUE)
  n = 3
  tsl = n * mfe / mad
  tsl
  cat('Traking Signal is: ', tsl)
}

#Loading the data set and making data frame
data = read.csv('Project_dataset.csv', stringsAsFactors = FALSE)
class(DF$Japan)
sum(is.na(DF$Japan)) 
data = as.data.frame(t(data))

# class(Japan)
# visualizing 2005-2022 for all three countries
ggplot(data=DF, aes(x=Year, y=Japan, group=1))+ 
  geom_line() +
  geom_point()+
  labs(x="Year", y = "Japan")
ggplot(data=DF, aes(x=Year, y=USA, group=1))+ 
  geom_line() +
  geom_point()+
  labs(x="Year", y = "USA")
ggplot(data=DF, aes(x=Year, y=Germany, group=1))+ 
  geom_line() +
  geom_point()+
  labs(x="Year", y = "Germany")


pred_df = data.frame(Year=2023:2028,SMA=c(0)*6 , WMA=c(0)*6 , SES=c(0)*6 ,SLR=c(0)*6)

# checking for missing values
sum(is.na(DF)) 


#making the initial data frame
tune_df <- data.frame(
  n = 1:5,
  ME = rep(0, 5),
  RMSE = rep(0, 5),
  MAE = rep(0, 5),
  MPE = rep(0, 5),
  MAPE = rep(0, 5),
  MASE = rep(0, 5),
  ACF1 = rep(0, 5)
)

DF = data.frame(
  Year = c(2005:2022),
  Japan = as.numeric(gsub(",", "", data[-1, 3])),
  USA = as.numeric(gsub(",", "", data[-1, 4])),
  Germany = as.numeric(gsub(",", "", data[-1, 5]))
)

fc_df = data.frame(
  japan = DF$Japan[13:18],
  usa = DF$USA[13:18],
  germany = DF$Germany[13:18]
)
attach(DF)
#--------------------------------Japan-------------------------------
#---------------------------Simple Moving Average---------------------
for (i in 1:5) {
  prediction = SMA(fc_df$japan, n = i)
  prediction = forecast(prediction, h = 6)
  tune_df[i,2:8] = accuracy(prediction)
}



# plotting the SMA with the best n
plot(DF$Japan, main = 'SMA (n = 4)', x = Year ,type = 'l', xlab = 'Year', ylab = 'Japan(sale)')
lines(SMA(DF$Japan, n = 4), x = DF$Year ,type = 'l', col = 'red')
legend('bottomleft', c('prediction'), fill = c('red'))


prediction = SMA(fc_df$japan, n = 4)
prediction = forecast(prediction, h = 6)
pred_df[1:6, 'SMA'] = prediction[2]
autoplot(prediction)
n = 4
traking_signal(fc_df$japan, prediction, n)

#---------------------------Weighted Moving Average-------------------------

prediction = WMA(fc_df$japan, n = 4 , seq(0.4,0.1,-0.1))
pred_df[1:6,'WMA'] = forecast(prediction, h = 6)[2]

# plotting the WMA with n = 4
plot(DF$Japan, main = 'WMA (n = 4)', x = Year, type = 'l', col = 'black',
     xlab = 'Year', ylab = 'Japan(sale)')
lines(WMA(DF$Japan, n = 4 , seq(0.4,0.1,-0.1)), type = 'l', col = 'red')
legend('bottomleft', c('prediction'), fill = c('red'))
autoplot(forecast(WMA(fc_df$japan ,n=5), h=6))

traking_signal(fc_df$japan, prediction, n)

#--------------------------Simple Exponential Smoothing---------------------

tune_df = data.frame(
  alpha = seq(0.1,0.9,by = 0.2),
  ME = rep(0, 5),
  RMSE = rep(0, 5),
  MAE = rep(0, 5),
  MPE = rep(0, 5),
  MAPE = rep(0, 5),
  MASE = rep(0, 5),
  ACF1 = rep(0, 5)
)

i = 1
for (alpha in tune_df$alpha) {
  prediction = ses(DF$Japan, h = 6, alpha = alpha)
  tune_df[i, 2:8] = accuracy(prediction)
  i = i + 1
}

# Use the best alpha 
prediction = ses(fc_df$japan, h=6,alpha = 0.9, lambda = NULL)
pred_df[1:6,"SES"] = forecast(prediction,h=6)[2]
autoplot(prediction)

traking_signal(fc_df$japan, as.numeric(unlist(prediction[2])), n)

#-------------------------Simple Linear Regression---------------------

prediction = lm(DF$Japan~Year)
prediction
summary(prediction)
plot(prediction)


b = unname(prediction$coefficients[1]) # Intercept
a = unname(prediction$coefficients[2]) # Period 

b
a

plot(y=DF$Japan, ylab = 'Japan(sale)' , x = Year, main = 'SLR', type="o", col="black")
lines(y=a*Year + b, x=Year,col = "red", type = 'l')
legend('bottomleft', c('regression'), fill = c('red'))

j = 1
for (i in 2023:2028){
  pred_df[j,'SLR'] = a * i + b
  j = j + 1
  print(j)
}




tune_df <- data.frame(
  n = 1:5,
  ME = rep(0, 5),
  RMSE = rep(0, 5),
  MAE = rep(0, 5),
  MPE = rep(0, 5),
  MAPE = rep(0, 5),
  MASE = rep(0, 5),
  ACF1 = rep(0, 5)
)


#making the initial data frame
tune_df <- data.frame(
  n = 1:5,
  ME = rep(0, 5),
  RMSE = rep(0, 5),
  MAE = rep(0, 5),
  MPE = rep(0, 5),
  MAPE = rep(0, 5),
  MASE = rep(0, 5),
  ACF1 = rep(0, 5)
)

traking_signal(DF$Japan, a*Year + b, n)


pred_df = data.frame(Year=2023:2028,SMA=c(0)*6 , WMA=c(0)*6 , SES=c(0)*6 ,SLR=c(0)*6)
fc_df = data.frame(
  japan = DF$Japan[13:18],
  usa = DF$USA[13:18],
  germany = DF$Germany[13:18]
)

#--------------------------------USA-------------------------------
#---------------------------Simple Moving Average---------------------
for (i in 1:5) {
  prediction = SMA(fc_df$usa, n = i)
  prediction = forecast(prediction, h = 6)
  tune_df[i,2:8] = accuracy(prediction)
}

# plotting the SMA with the best n
plot(DF$USA, main = 'SMA (n = 3)', x = Year ,type = 'l', xlab = 'Year', ylab = 'USA(sale)')
lines(SMA(DF$USA, n = 3), x = DF$Year ,type = 'l', col = 'red')
legend('bottomleft', c('prediction'), fill = c('red'))


prediction = SMA(fc_df$usa, n = 4)
prediction = forecast(prediction, h = 6)
pred_df[1:6, 'SMA'] = prediction[2]
traking_signal(fc_df$usa, prediction, n)
autoplot(prediction)
#---------------------------Weighted Moving Average-------------------------

prediction = WMA(fc_df$usa, n = 4 , seq(0.4,0.1,-0.1))
pred_df[1:6,'WMA'] = forecast(prediction, h = 6)[2]

# plotting the WMA with n = 4
plot(DF$USA, main = 'WMA (n = 4)', x = Year, type = 'l', col = 'black',
     xlab = 'Year', ylab = 'USA(sale)')
lines(WMA(DF$USA, n = 4 , seq(0.4,0.1,-0.1)), type = 'l', col = 'red')
legend('bottomleft', c('prediction'), fill = c('red'))
autoplot(forecast(WMA(fc_df$usa ,n=5), h=6))

traking_signal(fc_df$usa, prediction, n)

#--------------------------Simple Exponential Smoothing---------------------

tune_df = data.frame(
  alpha = seq(0.1,0.9,by = 0.2),
  ME = rep(0, 5),
  RMSE = rep(0, 5),
  MAE = rep(0, 5),
  MPE = rep(0, 5),
  MAPE = rep(0, 5),
  MASE = rep(0, 5),
  ACF1 = rep(0, 5)
)

i = 1
for (alpha in tune_df$alpha) {
  prediction = ses(DF$USA, h = 6, alpha = alpha)
  tune_df[i, 2:8] = accuracy(prediction)
  i = i + 1
}

# Use the best alpha 
prediction = ses(DF$USA, h=6 ,alpha = 0.9,lambda = NULL)
pred_df[1:6,"SES"] = forecast(prediction,h=6)[2]
autoplot(prediction)
traking_signal(fc_df$usa, as.numeric(unlist(prediction[2])), n)

#-------------------------Simple Linear Regression---------------------

prediction = lm(DF$USA~Year)
prediction
summary(prediction)
plot(prediction)


b = unname(prediction$coefficients[1]) # Intercept
a = unname(prediction$coefficients[2]) # Period 

plot(y=DF$USA, ylab = 'USA(sale)' , x = Year, main = 'SLR', type="o", col="black")
lines(y=a*Year + b, x=Year,col = "red", type = 'l')
legend('bottomleft', c('regression'), fill = c('red'))

j = 1
for (i in 2023:2028){
  pred_df[j,'SLR'] = a * i + b
  j = j + 1
  print(j)
}

traking_signal(DF$USA, a*Year + b, n)



#making the initial data frame
tune_df <- data.frame(
  n = 1:5,
  ME = rep(0, 5),
  RMSE = rep(0, 5),
  MAE = rep(0, 5),
  MPE = rep(0, 5),
  MAPE = rep(0, 5),
  MASE = rep(0, 5),
  ACF1 = rep(0, 5)
)


pred_df = data.frame(Year=2023:2028,SMA=c(0)*6 , WMA=c(0)*6 , SES=c(0)*6 ,SLR=c(0)*6)
fc_df = data.frame(
  japan = DF$Japan[13:18],
  usa = DF$USA[13:18],
  germany = DF$Germany[13:18]
)
#--------------------------------Germany-------------------------------
#---------------------------Simple Moving Average---------------------

for (i in 1:5) {
  prediction = SMA(fc_df$germany, n = i)
  prediction = forecast(prediction, h = 6)
  tune_df[i,2:8] = accuracy(prediction)
}

# plotting the SMA with the best n
plot(DF$Germany, main = 'SMA (n = 4)', x = Year ,type = 'l', xlab = 'Year', ylab = 'Germany(sale)')
lines(SMA(DF$Germany, n = 4), x = DF$Year ,type = 'l', col = 'red')
legend('bottomleft', c('prediction'), fill = c('red'))


prediction = SMA(fc_df$germany, n = 4)
prediction = forecast(prediction, h = 6)
pred_df[1:6, 'SMA'] = prediction[2]
traking_signal(fc_df$germany, prediction, n)
autoplot(prediction)
#---------------------------Weighted Moving Average-------------------------

prediction = WMA(fc_df$germany, n = 4 , seq(0.4,0.1,-0.1))
pred_df[1:6,'WMA'] = forecast(prediction, h = 6)[2]

# plotting the WMA with n = 4
plot(DF$Germany, main = 'WMA (n = 4)', x = Year, type = 'l', col = 'black',
     xlab = 'Year', ylab = 'Germany(sale)')
lines(WMA(DF$Germany, n = 4 , seq(0.4,0.1,-0.1)), type = 'l', col = 'red')
legend('bottomleft', c('prediction'), fill = c('red'))
autoplot(forecast(WMA(fc_df$germany ,n=4), h=6))

traking_signal(fc_df$germany, prediction, n)

#--------------------------Simple Exponential Smoothing---------------------

tune_df = data.frame(
  alpha = seq(0.1,0.9,by = 0.2),
  ME = rep(0, 5),
  RMSE = rep(0, 5),
  MAE = rep(0, 5),
  MPE = rep(0, 5),
  MAPE = rep(0, 5),
  MASE = rep(0, 5),
  ACF1 = rep(0, 5)
)

i = 1
for (alpha in tune_df$alpha) {
  prediction = ses(DF$Germany, h = 6, alpha = alpha)
  tune_df[i, 2:8] = accuracy(prediction)
  i = i + 1
}

# Use the best alpha 
prediction = ses(DF$Germany, h=6 ,alpha = 0.9,lambda = NULL)
pred_df[1:6,"SES"] = forecast(prediction,h=6)[2]
autoplot(prediction)
traking_signal(fc_df$germany, as.numeric(unlist(prediction[2])), n)

#-------------------------Simple Linear Regression---------------------

prediction = lm(DF$Germany~Year)
prediction
summary(prediction)
plot(prediction)


b = unname(prediction$coefficients[1]) # Intercept
a = unname(prediction$coefficients[2]) # Period 

plot(y=DF$Germany, ylab = 'Germany(sale)' , x = Year, main = 'SLR', type="o", col="black")
lines(y=a*Year + b, x=Year,col = "red", type = 'l')
legend('bottomleft', c('regression'), fill = c('red'))

j = 1
for (i in 2023:2028){
  pred_df[j,'SLR'] = a * i + b
  j = j + 1
  print(j)
}

traking_signal(DF$Germany, a*Year + b, n)

