# blatt1\_aufgabe5

![](../.gitbook/assets/ue5.PNG)

**Gute der Anpassung ACF Stationarität:**

![](../.gitbook/assets/acf.PNG)



```r
install.packages("quantmod")
install.packages("astsa")

library(quantmod) #or require(...)
library(astsa)

rm(list=ls()) #delete all variable
?rnorm #help for the syntaks meaning


#Simulation of White Noise: iid folge with limited EW und Var

par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
wn1 = rnorm(500,0,1)
plot.ts(wn1, main="Gausschen White noise", ylab="")
wn2=rnorm(50,0,1)
plot.ts(wn2, ylab="")


#Autocorelation function
acf(wn1,20)
acf(wn2,20)
#its all about the outlier in KI 95%


#Vergleich
par(mfrow=c(4,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
wn1 = rnorm(500,0,1)
plot.ts(wn1, main="Gausschen White noise", ylab="")
acf(wn1,20)

wn2=rnorm(50,0,1)
plot.ts(wn2, ylab="")
acf(wn2,20)


#N=50 hat schlechtes ACF (nicht WN->nicht Stationär). Denn Es gibt die daten, die außerhalb 95% KI liegt


```

**Auswirken Anzahl der Beobachtungen:**

![](../.gitbook/assets/acf2.PNG)

R file:

[Lösung: 1uebungsblatt\_5](https://trello.com/c/ODEcbUEy/21-1uebungsblatt5)

