#Mengatur Working Directory 
setwd("C:/Users/ekama/Documents/PROJECT ADW")
#Membaca file 
Kapal<-scan("Kapal.txt")

#no 1.1
#Mengonversi data menjadi objek time series dengan fekuensi bulanan (12 Bulan)
Kapal.ts<-ts(Kapal,start=c(2021,1),frequency=12)
#Plot Deret Waktu Penumpang Kapal
plot(Kapal.ts, xlab="Tahun", ylab="Penumpang Kapal")
#Memeriksa apakah data ini time series atau bukan 
is.ts(Kapal.ts)
#Memanggil library
library (TSA)
#Membuat plot dengan label bulan
points (y=Kapal.ts, x=time(Kapal.ts), pch=as.vector(season(Kapal.ts)))

#no 1.2a
#Uji Normalitas
shapiro.test(Kapal.ts)

#no 1.2b
#Uji Kestasioneran dengan plot
par(mfrow=c(2,1))
acf(as.vector(Kapal.ts),lag.max=12)
pacf(as.vector(Kapal.ts),lag.max=12)
#Uji Kestasioneran dengan uji formalitas
library(tseries)
adf.test(Kapal.ts)
kpss.test(Kapal.ts)

#no 1.2c
#Menstasionerkan data dengan melakukan differencing terhadap tren
diff.tren.Kapal.ts <- diff(Kapal.ts,lag=1)
plot(diff.tren.Kapal.ts)
#Menstasionerkan data dengan melakukan differencing terhadap musim
diff.musim.Kapal.ts <- diff(diff.tren.Kapal.ts,lag=12)
plot(diff.musim.Kapal.ts)


#no 1.2d
#Plot ACF dan PACF untuk data yang telah di-differencing terhadap tren
par(mfrow=c(2,1))
acf(as.vector(diff.tren.Kapal.ts),lag.max=12)
pacf(as.vector(diff.tren.Kapal.ts),lag.max=12)
adf.test(diff.tren.Kapal.ts)
kpss.test(diff.tren.Kapal.ts)
#Plot ACF dan PACF untuk data yang telah di-differencing terhadap musim
par(mfrow=c(2,1))
acf(as.vector(diff.musim.Kapal.ts),lag.max=50)
pacf(as.vector(diff.musim.Kapal.ts),lag.max=50)
adf.test(diff.musim.Kapal.ts)
kpss.test(diff.musim.Kapal.ts)

#no 1.2e
#Identifikasi menggunakan EACF
library(forecast)
library(TSA)
eacf(diff.musim.Kapal.ts,ar.max=5, ma.max=5)

#no 1.2g
library(forecast)
#Kandidat model ARIMA
model1<-arima(Kapal.ts,order=c(0,1,1))
model1
model2<-arima(Kapal.ts,order=c(1,1,1))
model2
model3<-arima(Kapal.ts,order=c(1,1,0))
model3

#no 1.3
#Menggunakan kriteria informasi (AIC) untuk memilih kandidat model 
model1$aic
model2$aic
model3$aic
#Luaran model terpilih 
#Model dengan nilai AIC terkecil adalah model 1
model1

#no 1.4a
#Plot sisaan 
res.model1 <- model1$residuals
#Uji kenormalan sisaan 
shapiro.test(res.model1)

#no 1.4b
#Membuat residual kuadrat 
res.model1.sqr <- res.model1^2
#Membuat nilai absolut residual
res.model1.abs <- abs(res.model1)
library(FinTS)
AutocorTest(res.model1.sqr)
AutocorTest(res.model1.abs)

#no 1.4c
#Plot Kuantil-Kuantil (QQ Plot)
qqnorm(res.model1)
qqline(res.model1)


#no 1.5
#Akan melakukan peramalan untuk 12 bulan kedepan
forecast.Kapal <- predict(model1,12)
forecast.Kapal
#plot peramalan 
#batas atas
#forecast.Kapal$pred berisi nilai prediksi (ramalan)
#forecast.Kapal$se berisi galat standar (standard error)
U <- forecast.Kapal$pred + 2*forecast.Kapal$se
L <- forecast.Kapal$pred - 2*forecast.Kapal$se
#Batas minimum untuk sumbu y
min.y <- min(Kapal.ts,L)
#Batas maksimum untuk sumbu y
max.y <- max(Kapal.ts,U)
ts.plot(Kapal.ts,forecast.Kapal$pred,col=1:2,ylim=c(min.y,max.y))
lines(U,col="purple",lty="dashed")
lines(L,col="purple",lty="dashed")

