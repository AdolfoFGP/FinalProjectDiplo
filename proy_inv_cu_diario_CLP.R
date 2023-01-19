###############################
### DIARIO ###################
##############################
library(dplyr)
library(psych)
library(corrplot) 
library(ggcorrplot)
##Funcion profe 
source("summary.arima.R")
source("TS.diag.R")

Data <- rio::import("proyecto final/diario_omg.xlsx")

head(Data)
tail(Data)
colSums(is.na(Data))

Data <- na.omit(Data)
head(Data)
tail(Data)

colSums(is.na(Data))
Data$cobreCLP <- Data$dolar*(Data$cobre/100)
head(Data)
tail(Data)


##Estadisticos descriptivos

describe(Data$dolar)
summary(Data$dolar)
quantile(Data$dolar)
mean(Data$dolar)
sd(Data$dolar)

describe(Data$cobreCLP)
summary(Data$cobreCLP)
quantile(Data$cobreCLP)
mean(Data$cobreCLP)
sd(Data$cobreCLP)
##Descriptivo
##Boxplot
par(mfrow = c(1,2))
boxplot(Data$dolar,bty = "n", las = 1,
        ylab = "Dolar (CLP)")
#boxplot(Data$cobre, xlab = "cobre")
boxplot(Data$cobreCLP, bty = "n", las = 1,
        ylab = "Cobre (CLP/Lb)")
dev.off()

describe(Data$dolar)

##Histograma
par(mfrow = c(1,2))
hist(Data$dolar, main='', xlab='Dolar (CLP)',bty = "n", las = 1)
#hist(Data$cobre, main='', xlab='dolar (CLP)')
hist(Data$cobreCLP, main='', xlab='Cobre (CLP/Lb)',bty = "n", las = 1)
dev.off()


##ESTADISTICA BIVARIADA EXPLORATORIO

##dispersion entre variables bivariado

plot(Data$dolar, Data$cobreCLP, bty = "n", las = 1,
     ylab = "Cobre (CLP/Lb)", xlab = "Dolar (CLP)")
##Se observa baja correlacion entre variables

##Peor aun :
#plot(Data$dolar, Data$cobre, bty = "n", las = 1,
     #ylab = "TEMP_AVG", xlab = "")

##Correlaciones
df = data.frame(DOLAR = Data$dolar, COBRE= Data$cobreCLP)
#cor_datos = cor(df)
#corrplot(cor_datos)

para_corr.cor <- cor(df, method="pearson")
round(para_corr.cor, digits =2)

abs_corr <- abs(round(para_corr.cor, digits =2))
abs_corr <- as.data.frame(abs_corr)

#Grafico de correlaciones
corrplot(para_corr.cor)
corrplot(para_corr.cor, method="shade", shade.col =NA,
         tl.col ="black", tl.srt = 45)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))


#con esto pa mostrar de 0.7 arriba y dejar 0 os otros.
tmp <- abs(para_corr.cor)
tmp[ tmp < 0.1] = 0
corrplot(tmp, method="square", shade.col =NA,
         tl.col ="black", tl.srt = 45, col=col(200),
         addCoef.col = "black", order="AOE", type= "upper")


#___________________________SERIE DE TIEMPO________________

##Grafico dolar
library(xts)
Y <- xts(Data$dolar, Data$fecha)
plot(Y)

Y <- ts(Data$dolar)
plot(Y)

##Grafico cobre
J <- xts(Data$cobreCLP, Data$fecha)
plot(J)

J <- ts(Data$cobreCLP)
plot(J)

summary(Data)

##Veamos dolar autoarima
d = forecast::ndiffs(Y)
d
D = forecast::nsdiffs(Y)
D

Z <- diff(Y, lag = 12) #estacional cada 12 datos
par(mfrow = c(1,2), bty = "n")
acf(c(Z), ylim = c(-1,+1), lag.max = 60, na.action = na.pass)                  
pacf(c(Z), ylim = c(-1,+1), lag.max = 60, na.action = na.pass) 
dev.off()
## MA(9), AR(2)
## SMA(2), SAR(0)

fit01 <- forecast::auto.arima(y = Y, d = d, D = D, max.p = 2, max.q = 9, max.P = 0, max.Q = 2, allowdrift = T)
fit01

## Me dice  diff 1 MA(1)
summary_arima(fit01, fixed = c(NA))
##Ma1 es significativo al 95


par(mfrow = c(1,1), bty = "n", las = 1)
plot(Y, col = "gray", lwd = 2)
lines(fit01$fitted, col = "red", lwd = 2)

## Diagnostico de los residuos
Z <- fit01$res
TS.diag(Z) ## Se me cae, MA(5)
dev.off()


##Iteracion con Ma 5 y Ma1
fixed = c(NA,0,0,0,0,0,NA)
fit04 <- forecast::Arima(y = Y, order = c(0,1,7), seasonal = c(0,0,0),fixed=fixed, include.drift=F)
fit04

par(mfrow = c(1,1), bty = "n", las = 1)
plot(Y, xlab="Tiempo en días", ylab="Dolar (CLP)", las=1, lwd=2, col = "black")
lines(fit04$fitted, col = "red", lwd=2)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()

forecast::auto.arima(y = fit04$res)
TS.diag(fit04$res) ##Se cae en 7, hago MA con 7
##Paso blancura con estructura MA1, MA5, MA7
## UFF PASAMOS CON MA1 y MA7
dev.off()
summary_arima(fit04, fixed=fixed)
##Significativos 1 y 7, no el 5. lo voy a quitar y probar.
#PERFECT! PASAMOS BLANCURA CON MA1 Y MA7, ambos significativos



plot(fit04) ##Se puede hacer forecast


par(mfrow = c(1,1), bty = "n", las = 1)
plot(Y, col = "black", lwd = 2, xlab="Año", ylab="MWh", las=1)
lines(fit01$fitted, col = "red", lwd = 2)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea

dev.off()
##Test homocedasticidad
lmtest::bptest(lm(fit04$res ~ time(fit04$res)))
##Se rechaza homocedasticidad.

##Grafico residuos

plot(fit04$res, xlab="Tiempo en días", ylab="Dolar (CLP)", las=1, bty="n")

acf(fit04$res^2)

plot(fit04$res[1:134], type="l")


## Aqui si pasa homocedasticidad, REVISAR FECHAS
lmtest::bptest(lm(fit04$res[1:400] ~ time(fit04$res)[1:400]))

ks.test(scale(fit04$res), "pnorm")
##Pasamos raspando normalidad 

plot(fit04)
##Raices dentro del circulo, se puede predecir.

##Vamos a hacer un forecast a 10 días
pred <- forecast::forecast(fit04, 30, level = 0.95, fan=F)
plot(pred)
pred
##Apartir del 7mo día, valor cte.

par(mfrow = c(1,1), bty = "n", las = 1)
plot(pred, xlim=c(500,530), xlab="Tiempo en días", ylab="Dolar (CLP)", las=1)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()

## Desempeño?
summary(fit04)
## MAPE = 0,70%
1-var(Z, na.rm = T)/var(Y, na.rm = T)
## r2 = 98.88%

###############################
### DIARIO ###################
## CON COBRE##################
##############################

##Funcion profe 
source("summary.arima.R")
source("TS.diag.R")

##Fuente precio dolar:
##https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_TIPO_CAMBIO/MN_TIPO_CAMBIO4/DOLAR_OBS_ADO/TCB_505

Data <- rio::import("proyecto final/diario_omg.xlsx")
Data <- na.omit(Data)
Data$cobreCLP <- Data$dolar*(Data$cobre/100)
Data

Data[50,]
Data[150,]
Data[286,]


head(Data)
tail(Data)
Y <- ts(Data$dolar)
plot(Y,xlab="Tiempo en días", ylab="Dolar (CLP)", las=1, bty="n")


K <- ts(Data$cobreCLP)
plot(K, xlab="Tiempo en días", ylab="Cobre (CLP/Lb)", las=1, bty="n") ##Tabien

Data[200:260,]
psych::describe(Data$cobreCLP)
Data[242,]
describe(Data[410:480,2:4])
Data[426,]


##Correlacion
df_correlacion <- data.frame(USD=Data$dolar, COBRE=Data$cobreCLP)
cor(df_correlacion[,c("USD","COBRE")])
## Da una correlación de 0.428 

lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}


## Veamos con cobre rezago en t-1:
x<-df_correlacion$COBRE;
x
x<-(cbind(x, lagpad(x, 1), lagpad(x,-1)))[,2]
x
df_correlacion2 <- data.frame(USD=Data$dolar, COBRE=x)
df_correlacion2

cor(na.omit(df_correlacion2[,c("USD","COBRE")]))
## 0.429 la correlacion

## en t-2? 
x<-df_correlacion$COBRE;
x
x<-(cbind(x, lagpad(x, 2), lagpad(x,-2)))[,2]
x
df_correlacion3 <- data.frame(USD=Data$dolar, COBRE=x)
df_correlacion3

cor(na.omit(df_correlacion3[,c("USD","COBRE")]))
## 0.43


xreg = as.matrix(data.frame(COBRE = Data$cobreCLP))
fixed = c(NA,NA,0,0,0,0,0,NA,0,0,0,0,0,NA,NA,NA,NA)
fit04 <- forecast::Arima(y = Y, order = c(1,0,13), seasonal = c(0,0,0), fixed=fixed, xreg = xreg, include.drift=T)
fit04

par(mfrow = c(1,1), bty = "n", las = 1)
plot(Y, xlab="Tiempo en días", ylab="Dolar (CLP)", las=1, lwd=2)
lines(fit04$fitted, col = "red", lwd=2)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()

forecast::auto.arima(y = fit04$res)

##Para modelar los residuos, auto arima propone:
##ARIMA(1,0,1), lo itero 

##diagnostico residuos
TS.diag(fit04$res) ##Se me cae en MA5. y ahora en MA7 TAMO OK


summary_arima(fit04, fixed=fixed)
## QUEDA BIEN CON AR1 MA1 MA7 MA13 intercetp drift y cobre UFFFF

## Normalidad
ks.test(scale(fit04$res), "pnorm") ## se cumple la normalidad


## Homocedasticidad
lmtest::bptest(lm(fit04$res~time(fit04$res))) ## no pasa, revisar

dev.off()
plot(fit04$res)

##Aqui pasa, revisar.
lmtest::bptest(lm(fit04$res[1:154] ~ time(fit04$res)[1:154]))

plot(fit04) ## Se puede predecir raices dentro circulo

## FORECAST ###
## COCHILCO DICE QUE SE PROYECTA PRECIO DE: 3.95 promedio anual
## IPOM DICE:

##Considerando el precio dolar del ultimo día ya que dicen entre 900-1000
tail(Data$dolar)
3.95 * 935.29  ##3694.396 CLP/LB


##pred 30 dias, al 8vo se hace mas o menos cte el precio dolar estimado.
newxreg = data.frame(COBRE = c(rep(3694.396,30)))
newxreg = as.matrix(newxreg)
newxreg

pred <- forecast::forecast(fit04, xreg = newxreg, level = 0.95)

plot(pred)
pred

par(mfrow = c(1,1), bty = "n", las = 1)
plot(pred, xlim=c(500,545), xlab="Tiempo en días", ylab="Dolar (CLP)", las=1)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()

summary(fit04)

## MAPE del 0.6175%

Z <- fit04$res

1-var(Z, na.rm = T)/var(Y, na.rm = T)
## r2 = 99.148%

#####################################
##### AUTOARIMA AL COBRE         ####
#####################################

K
plot(K)

## forecast recomienda diferenciar?
d = forecast::ndiffs(K)
d
D = forecast::nsdiffs(K)
D

Z <- diff(K, lag = 12) #estacional cada 12 datos
par(mfrow = c(1,2), bty = "n")
acf(c(Z), ylim = c(-1,+1), lag.max = 60, na.action = na.pass)                  
pacf(c(Z), ylim = c(-1,+1), lag.max = 60, na.action = na.pass) 
dev.off()


fit01 <- forecast::auto.arima(y = K, d = d, D = D, max.p = 2, max.q = 9, max.P = 2, max.Q = 1, allowdrift = T)
fit01
##Dice basta con una diff y MA 1

#summary_arima(fit01, fixed = c(NA,NA,NA))

par(mfrow = c(1,1), bty = "n", las = 1)
plot(K, col = "gray", lwd = 2)
lines(fit01$fitted, col = "red", lwd = 2)
dev.off()

## Diagnostico de los residuos
Z <- fit01$res
TS.diag(Z) 
dev.off()

summary_arima(fit01, fixed = c(NA))
##Es significativo estructura M1 y con una diferenciacion

lmtest::bptest(lm(Z ~ time(Z))) ## No se rechaza la homocedasticidad
ks.test(scale(Z), "pnorm") ## No se rechaza la normalidad

dev.off()
##Pronostico a 30 dias
pred <- forecast::forecast(fit01, 30, level = 0.95, fan=F)
plot(pred)


plot(pred, xlim=c(500,550))
pred ##Es cte 3384.905 CLP/LB bajo respecto a 3694.396 cochilco proyecion


##Vuelvo a lo anterior para usar este nuevo reg

newxreg = data.frame(COBRE = c(pred)$mean)
newxreg = as.matrix(newxreg)


pred <- forecast::forecast(fit04, xreg = newxreg, level = 0.95)

plot(pred)
pred

par(mfrow = c(1,1), bty = "n", las = 1)
plot(pred, xlim=c(500,550), xlab="Año", ylab="MWh", las=1)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()

summary(fit04)

## MAPE del 0.6175% se mantiene

Z <- fit04$res

1-var(Z, na.rm = T)/var(Y, na.rm = T)
## r2 = 99.14% se mantiene.


#OSSSSSSSSSSSSSSSSSS









