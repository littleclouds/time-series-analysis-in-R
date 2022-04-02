install.packages("tseries")
library(tseries)
install.packages("forecast")
library(forecast)
#unit root test augumented dickey fuller test
#using Tau statistics
#using tseies packasges, calclated using ADF test
attach(time_series_data)
#file is ready to use

plot(time_series_data$GDP...2)
plot.ts(GDP...2)
#we find that mean and variance are shifting
adf.test(GDP...2)

#at 5% we are geting p= 0.2738 which is greater han 0.05. so we will accept null hypotheseis.
#that is there is presence of unit root data is non stationary.
adf.test(time_series_data$GDP...2)
RGDP= diff(log(GDP...2))
#we find gdp is lagged differences 

adf.test(RGDP)
#we get p value lesss tha 0.05 so we have stationary data
plot.ts(RGDP)
#we find mean is common. this means we have converted non staionary data into staionary data.

#data  series is non priced data we wil take simple difference data. otherwise, log differnce of data.

adf.test(PDI...3)
adf.test(PCE...4)
#in PCE p value is 0.01 which is less than 0.05 hence stationary.
#auto regressionmodel-when the lag value decide the futurestic value
#AR1(one time lag),AR2(two time lag)
#current situation also needs to be acknowledge. current devlopment will be considered in error term Ui
#when we forecaste using error term and its lagged values to forecaste this is called moving average process.
#MA(lagged of Ui)
#for correct analysis both AR-MA will be used. we use past values and future values.

#ARIMA i stands for intergated, integrated at what level.
#if original i=0 if level stayionary
#if i = 1 then stationary at level 1
#if i = 2 then staionary  at level 2
#ARIMA(1(laged values considered), 1(level of intergation at what level), 1(error term ka one time lag))
#IT INCLUDESS
#AR model auto regresssion. this includes lagged values.
#MA model moving avg. it uses error term lagged values.
#ARIMA inclues both lagged and error terms. this includes stationary condition.(must)

#box jinkension methodlogy- it is four step procedure.
#1. what is the identification
#2. estimation
#3. digaonstic checking
#4. forecasting

#1. how to decide which lagged valu to be considered. i.e first lagged or second lagged?? it willl AR1,2,3.
#similary for MA,, which error term will you use. i.e MA1,2,3 where MA3 is thrid time lagged.
#if u ur considering both AR&MA which ar or ma are being considered then we will se lagged value affecting significantly.
#how to decide which variabl?
#we will ACF and partial CF. if we consider all the variable are impacting than we will use auto correlation. but if we want to which one inparticular is impact.
#then we will use partial correlation. normally for AR, we use PACF, i.e partial correlation fu tion. 
#for MA we usacf
#corlogram plots lagged variable and autocorrelation.
#we plot corologram distrivution to find which model to be used.

acf(time_series_data$GDP...2, plot= T)
pacf(time_series_data$GDP...2, plot = T)

#(type of model) - (pattern of acf) - (pattern of pacf)
#AR(p) - there should be exotential decay. That is decline as values goes on.- there should be significant spike lag p.
#MA(q) - there should be significant spike lag p.- there should be exotential decay. or with damp.
#ARMA(p,q)- when there in expotential decay.- expotential decay in both cases.
#in realtity, we may not get fully exopotential and spikes. so we will use Ui(error term). 

#in the GDPseries we find acg is damping and pacf is spike. Hence appropriate model is AR(p) i.e Auto regression

library(FitAR)
install.packages(FitAR)


acf(time_series_data$GDP...2)
res <- LjungBoxTest(time_series_data$GDP...2, k=2, StartLag = 1)
plot(res[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(time_series_data$GDP...2)
qqline(time_series_data$GDP...2)


futureV <- forecast(time_series_data$GDP...2, h= 10, level=c(99.5))
plot(futureV)

acf(RGDP)
pacf(RGDP)
#if all the lagged values are affecting than its non staionary. 
#we will take all the lagged value which are over or touchig the blue line in the ACF.
auto.arima(RGDP)

#JNU prof Tulsi Ram murdaliya. 
#Harivash Rai Bachan
#arima is mean concerend. VAR is variance concered  model. so if data series has constant mean we can use Arima
