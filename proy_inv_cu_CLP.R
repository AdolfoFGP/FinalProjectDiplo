#########################
##Proy inv evolucion#####
## Cobre - Mensual ######
#########################
library(dplyr)
library(psych)
library(corrplot) 
library(ggcorrplot)

##Funcion profe R.Olea.
source("summary.arima.R")
source("TS.diag.R")

Data <- rio::import("proyecto final/probando_mensual.xlsx")
head(Data)
tail(Data)

Cobre <- rio::import("proyecto final/cochilco_mes.xlsx")
head(Cobre)
tail(Cobre)

Cobre$cobreCLP <- Data$USD*(Cobre$COBRE)
head(Cobre)
tail(Cobre)


##Estadisticos descriptivos

describe(Data$USD)
summary(Data$USD)
quantile(Data$USD)
mean(Data$USD)
sd(Data$USD)

describe(Cobre$cobreCLP)
summary(Cobre$cobreCLP)
quantile(Cobre$cobreCLP)
mean(Cobre$cobreCLP)
sd(Cobre$cobreCLP)






##Descriptivo
##Boxplot
par(mfrow = c(1,2))
boxplot(Data$USD,bty = "n", las = 1,
        ylab = "Dolar (CLP)")
#boxplot(Data$cobre, xlab = "cobre")
boxplot(Cobre$cobreCLP, bty = "n", las = 1,
        ylab = "Cobre (CLP/Lb)")
dev.off()

describe(Data$dolar)

##Histograma
par(mfrow = c(1,2))
hist(Data$USD, main='', xlab='Dolar (CLP)',bty = "n", las = 1)
#hist(Data$cobre, main='', xlab='dolar (CLP)')
hist(Cobre$cobreCLP, main='', xlab='Cobre (CLP/Lb)',bty = "n", las = 1)
dev.off()

##Analisis serie temporal variables

Y <- ts(Data$USD, start = c(2009, 1), frequency = 12)
plot(Y, bty="n", las=1, xlab="Año", ylab="Dolar (CLP)")

##Variable cobre

K <- ts(Cobre$cobreCLP, start = c(2009, 1), frequency = 12)
plot(K, bty="n", las=1, xlab="Año", ylab="Cobre (CLP/Lb)")


##ESTADISTICA BIVARIADA EXPLORATORIO

##dispersion entre variables bivariado

plot(Data$USD, Cobre$cobreCLP, bty = "n", las = 1,
     ylab = "Cobre (CLP/Lb)", xlab = "Dolar (CLP)")
##Se observa baja correlacion entre variables

##Peor aun :
#plot(Data$dolar, Data$cobre, bty = "n", las = 1,
#ylab = "TEMP_AVG", xlab = "")

##Correlaciones
df = data.frame(DOLAR = Data$USD, COBRE= Cobre$cobreCLP)
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






Y <- ts(Data$USD, start = c(2009, 1), frequency = 12)
plot(Y)

## forecast recomienda diferenciar?
d = forecast::ndiffs(Y)
d
D = forecast::nsdiffs(Y)
D

## diferenciaciones d:1 , D:0

Z <- diff(Y, lag = 12) #estacional cada 12 datos
par(mfrow = c(1,2), bty = "n")
acf(c(Z), ylim = c(-1,+1), lag.max = 60, na.action = na.pass)                  
pacf(c(Z), ylim = c(-1,+1), lag.max = 60, na.action = na.pass) 
dev.off()
## MA(8), AR(1)
## SMA(0), SAR(0)

fit01 <- forecast::auto.arima(y = Y, d = d, D = D, max.p = 1, max.q = 8, max.P = 0, max.Q = 0, allowdrift = T)
fit01

## Me dice  diff 2 MA(1)
summary_arima(fit01, fixed = c(NA))
##Ma1 es significativo al 95


par(mfrow = c(1,1), bty = "n", las = 1)
plot(Y, col = "gray", lwd = 2)
lines(fit01$fitted, col = "red", lwd = 2)

## Diagnostico de los residuos
Z <- fit01$res
TS.diag(Z) 
dev.off()

plot(fit01) ##Se puede hacer forecast, raices dentro del graph


par(mfrow = c(1,1), bty = "n", las = 1)
plot(Y, col = "black", lwd = 2, xlab="Año", ylab="Dolar (CLP)", las=1)
lines(fit01$fitted, col = "red", lwd = 2)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()

##Test homocedasticidad
lmtest::bptest(lm(fit01$res ~ time(fit01$res)))
##Se rechaza homocedasticidad.

##Grafico residuos
plot(fit01$res)

plot(fit01$res[1:134], type="l")

Data$YEAR[1:135] ##Marzo 2020

## Aqui si pasa homocedasticidad, hasta 2020 febrero
lmtest::bptest(lm(fit01$res[1:134] ~ time(fit01$res)[1:134]))

##Incluyendo hasta marzo 2020, no pasa homocedasticidad
lmtest::bptest(lm(fit01$res[1:135] ~ time(fit01$res)[1:135]))

##Se pasa test normalidad
ks.test(scale(Z), "pnorm")


plot(fit01)
##Raices dentro del circulo, se puede predecir.

##Vamos a hacer un forecast a 2 año
pred <- forecast::forecast(fit01, 24, level = 0.95, fan=F)
plot(pred)
pred


par(mfrow = c(1,1), bty = "n", las = 1)
plot(pred, xlim=c(2022,2025), xlab="Año", ylab="Dolar (CLP)", las=1)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()

## Desempeño?
summary(fit01)
## MAPE = 1,90%
1-var(Z, na.rm = T)/var(Y, na.rm = T)
## r2 = 97.85%


#########################
##Proy inv evolucion#####
## Cobre - Mensual ######
### Ahora con COBRE  ####
####  TENDENCIA    ######
#########################

##Funcion profe 
source("summary.arima.R")
source("TS.diag.R")

##Fuente precio dolar:
##https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_TIPO_CAMBIO/MN_TIPO_CAMBIO4/DOLAR_OBS_ADO/TCB_505

Data <- rio::import("proyecto final/probando_mensual.xlsx")
head(Data)
tail(Data)
Y <- ts(Data$USD, start = c(2009, 1), frequency = 12)
plot(Y)

##Usando excel de cochilco
##Fuente:https://boletin.cochilco.cl/estadisticas/grafico.asp?tipo_metal=1

Cobre <- rio::import("proyecto final/cochilco_mes.xlsx")
head(Cobre)
tail(Cobre)

Cobre$cobreCLP <- Data$USD*(Cobre$COBRE)
head(Cobre)
tail(Cobre)


##Variable cobre

K <- ts(Cobre$cobreCLP, start = c(2009, 1), frequency = 12)
plot(K)

##Correlacion
df_correlacion <- data.frame(USD=Data$USD, COBRE=Cobre$cobreCLP)
cor(df_correlacion[,c("USD","COBRE")])
## Da una correlación super alta 0.6284


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
df_correlacion2 <- data.frame(USD=Data$USD, COBRE=x)
df_correlacion2

cor(na.omit(df_correlacion2[,c("USD","COBRE")]))
## 0.63249 la correlacion

## Veamos con cobre rezago en t-1:
x<-df_correlacion$COBRE;
x
x<-(cbind(x, lagpad(x, 2), lagpad(x,-2)))[,2]
x
df_correlacion3 <- data.frame(USD=Data$USD, COBRE=x)
df_correlacion3

cor(na.omit(df_correlacion3[,c("USD","COBRE")]))
## COR DE 0.6381


xreg = as.matrix(data.frame(COBRE = Cobre$cobreCLP))
fit04 <- forecast::Arima(y = Y, order = c(1,1,1), seasonal = c(0,0,0), xreg = xreg, include.drift=F)
fit04


par(mfrow = c(1,1), bty = "n", las = 1)
plot(Y, xlab="Año", ylab="Dolar (CLP)", las=1, lwd=2)
lines(fit04$fitted, col = "red", lwd=2)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()

forecast::auto.arima(y = fit04$res)

##Para modelar los residuos, auto arima propone:
##ARIMA(1,0,0), lo itero 

## Diagnostico residuos
TS.diag(fit04$res)

dev.off()
summary_arima(fit04, fixed=c(NA,NA,NA))
## Todo significativo AR 1 MA1, COBRE


## Normalidad
ks.test(scale(fit04$res), "pnorm") ## se cumple la normalidad


## Homocedasticidad
lmtest::bptest(lm(fit04$res~time(fit04$res))) ## no pasa 

plot(fit04$res, bty="n", las=1, xlab="Año", ylab="Residuos modelo")

## AHI SI PASA hasta septiembre 2021
lmtest::bptest(lm(fit04$res[1:153] ~ time(fit04$res)[1:153]))
## ACA no pasa hasta octubre 2021
lmtest::bptest(lm(fit04$res[1:154] ~ time(fit04$res)[1:154]))

##Debido volatilidad ultimos años

plot(fit04) ## Se puede predecir raices dentro circulo

## FORECAST ###
## COCHILCO DICE QUE SE PROYECTA PRECIO DE: 3.95 promedio anual
## IPOM DICE:
3.95 * 935.29  ##3694.396 CLP/LB



##Forecast 2 años
newxreg = data.frame(COBRE = c(rep(3694.396,24)))
newxreg = as.matrix(newxreg)
newxreg

pred <- forecast::forecast(fit04, xreg = newxreg, level = 0.95)

plot(pred)
pred

par(mfrow = c(1,1), bty = "n", las = 1)
plot(pred, xlim=c(2022,2025), xlab="Año", ylab="Dolar (CLP)", las=1)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()
summary(fit04)

## MAPE del 1.87% 

Z <- fit04$res

1-var(Z, na.rm = T)/var(Y, na.rm = T)
## r2 = 97.90%

#####################################
## PROBANDO AUTOARIMA PARA COBRE   ##
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

fit01 <- forecast::auto.arima(y = K, d = d, D = D, max.p = 0, max.q = 13, max.P = 0, max.Q = 0, allowdrift = F)
fit01
##ARIMA(0,1,2) with drift

summary_arima(fit01, fixed = c(NA,NA))
##Dice drift no significativo

## El experto me dice ARIMA(0,1,2) sin drift todo significativo

par(mfrow = c(1,1), bty = "n", las = 1)
plot(K, col = "gray", lwd = 2)
lines(fit01$fitted, col = "red", lwd = 2)
dev.off()
## Diagnostico de los residuos
Z <- fit01$res
TS.diag(Z) 
#podria levantarlo en 5 pero lo dejo asi noma si es autoarima pal cu noma
dev.off()


lmtest::bptest(lm(Z ~ time(Z))) ## No se rechaza la homocedasticidad
ks.test(scale(Z), "pnorm") ## No se rechaza la normalidad

## Forecast 2 años
pred <- forecast::forecast(fit01, 24, level = 0.95, fan=F)
plot(pred)
pred

##Me dice un valor de cobre 3328.020CLP/Lb bajo respecteo 3694.396 CLP/LB cochilco


plot(pred, xlim=c(2022.5,2025))
pred

##Vuelvo a lo anterior para usar este nuevo reg
## que es cobre en torno al 3.4 usd/libra, lejano al 3.95 previo.

newxreg = data.frame(COBRE = c(pred)$mean)
newxreg = as.matrix(newxreg)


pred <- forecast::forecast(fit04, xreg = newxreg, level = 0.95)

plot(pred)
pred

par(mfrow = c(1,1), bty = "n", las = 1)
plot(pred, xlim=c(2022,2024), xlab="Año", ylab="MWh", las=1)
grid(nx = NULL, ny = NULL,
     lty = 1,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
dev.off()
summary(fit04)

## MAPE del 1.87% se mantiene al anterior 

Z <- fit04$res

1-var(Z, na.rm = T)/var(Y, na.rm = T)
## r2 = 97.90% igua mismo.

