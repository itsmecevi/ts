# blatt4\_aufgabe5

![](../.gitbook/assets/1%20%281%29.PNG)

```r
################################

#Übungsblatt 4 aufgabe 5

################################

#Passen Sie ein AR-Modell an die Reihe "Jährliche Anzahl von Luchspelzen am
#McKenzie River, 1821-1934" (lynx). Uberprüfen Sie Ihr Modell mit Hilfe der
#Residuen. Wiederholen Sie das Ganze fur die Reihe log der Luchspelze.
install.packages("astsa")
install.packages("forecast")
library(astsa)
library(forecast) #fitted()
datasets::lynx
View(lynx)
str(lynx)
rm(list = ls())
###########################################
#1. ACF und PACF als erste identification
###########################################
par(mfrow=c(1,1))
plot(lynx)
acf2(lynx)

#Fazit: 
#Es kann mit Ar modell modelieren. Denn die ACF sit seasonale.
#Abstand der Season: 10 Jahren
#Kein Kandidat für Ordnung. Denn es ist nicht exponential Abklingelnd, sondern Seasonal.
#PACF: skript S. 5-41
#(PACF) gives the partial correlation of a time series with its own lagged values

############################################################
#2. Schatzen von Parametern 
#ar.ols : Conditional und Uncoditional Least Sum Square
#ar.mle: Unconditional Maximum Likelihood
#ar.ywm: Yule Walker
#ar.burg: Burg
#Skript S. 5-27
############################################################

#Ordnung suchen als kandidaten:

mle=ar.mle(lynx)
mle$order # Find the identified order 
yw=ar.yw(lynx)
yw$order # Find the identified order  #passt zum PACF kandidat
ols=ar.ols(lynx)
ols$order # Find the identified order 
burg=ar.burg(lynx)
burg$order # Find the identified order 

#Graphic Ordnung suchen als kandidaten (AIC):
aic=mle$aic  # For plotting below.
plot(c(0:(length(aic)-1)),aic,type='h',xlab='order',ylab='aic')
lines(0:(length(yw$aic)-1), yw$aic, lty=2, col=2)
lines(0:(length(ols$aic)-1), ols$aic, lty=2, col=3)
lines(0:(length(burg$aic)-1), burg$aic, lty=2, col=4)
lines(0:(length(aic)-1),aic,lty=2)
colors <- c("green","blue","red", "black")
legend("topright", c(paste("OLS"),
                     paste("Burg"),
                     paste("YW"),
                     paste("MLE")),
       lwd = 1, cex=1, col=colors, pt.lwd = 1, lty=2) 

#Grafischer Vergleich jeweils methoden
plot(lynx, main="Vergleich Modelle")
lines(fitted(yw),col="blue")
lines(fitted(mle),col="red")
lines(fitted(ols),col=6)
lines(fitted(burg),col=3)
colors <- c("blue","red",6, 3)
legend("top", c(paste("YW"),
                paste("MLE"),
                paste("OLS"),
                paste("BURG")),
       lwd = 1, cex=1, col=colors, pt.lwd = 1, lty=2) 


#################################################################################
#3. Signifikant test für jeweils Methoden und Besten Modell test mit AIC und BIC
#################################################################################

#Mit arima, Subsetmodell
#Also als Kandidat ist 8. Ordnung (bassiert auf die 4 methoden nur ein Kandidat)

sarima(lynx,8,0,0)
sarima(lynx,8,0,0)$ttable #3-7 sind nicht Signifikant
(sm=arima(lynx, c(8,0,0), fixed=c(NA,NA,0,0,0,0,0,NA, NA)) ) #ersetzt nicht signifikannt mit 0
a=sm$coef
a
b=c(1,-a[1:8]) 
b
Mod(polyroot(b)) 
coeftest(sm) # Es fehlt die Unsignifikant wert 3-7
tsdiag(sm) #Zeigt die Ljung box test und ACF


#Vergleich falls mehr als ein Kandidaten in 4 methoden, aber hier nicht!

#AIC
sm$aic; sm$aic; sm$aic; 

#BIC
BIC(sm); BIC(sm); BIC(sm)


#Grafischer Vergleich arima mit daten falls mehr als ein Kandidaten gibt, aber hier nicht!
plot(lynx, main="Vergleich Modelle")
lines(fitted(ols),col="blue")


#########################
#4. Test auf Residuen
#########################

arima8_0_0=sarima(lynx,8,0,0)
arima8_0_0$ttable
(model8_0_0=arima(lynx, c(8,0,0), fixed=c(NA,NA,0,0,0,0,0,NA, NA)) )
coeftest(model8_0_0) # Es fehlt die Unsignifikant wert
tsdiag(model8_0_0)
res=resid(arima8_0_0$fit)
shapiro.test(res)          # passt nicht (normality test)
lillie.test(res)          # passt nicht (normality test)


###########################
#5. Vorhersagen AR Modelle
###########################

#Fast
sarima.for(lynx, n.ahead=5,8,0,0)

# 5 nächstes Daten
sun.pr = predict(ols, n.ahead=5); 
sun.pr
```

[Lösung: 4uebungsblatt\_5](https://trello.com/c/qOq1bLlA/37-4uebungsblatt5)

