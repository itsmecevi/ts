# blatt1\_aufgabe5

Aufgabe 5. \[∗ 2 Punkte\] Simulieren Sie N = 500 Beobachtungen eines Gaußschen White Noise. Plotten Sie die Reihe und die empirische ACF bis zum Lag 20. Vergleichen Sie sie mit der theoretischen ACF. Wiederholen Sie die Aufgabe mit N = 50. Wie wirkt sich die Anzahl der Beobachtungen N auf das Ergebnis aus?

```r
install.packages("quantmod") install.packages("astsa")
library(quantmod) #or require(...) library(astsa)
rm(list=ls()) #delete all variable ?rnorm #help for the syntaks meaning
Simulation of White Noise: iid folge with limited EW und Var
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0)) wn1 = rnorm(500,0,1) plot.ts(wn1, main="Gausschen White noise", ylab="") wn2=rnorm(50,0,1) plot.ts(wn2, ylab="")
Autocorelation function
acf(wn1,20) acf(wn2,20)
its all about the outlier in KI 95%
Vergleich
par(mfrow=c(4,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0)) wn1 = rnorm(500,0,1) plot.ts(wn1, main="Gausschen White noise", ylab="") acf(wn1,20)
wn2=rnorm(50,0,1) plot.ts(wn2, ylab="") acf(wn2,20)
N=50 hat schlechtes ACF. Denn Es gibt die daten, die außerhalb 95 KI liegt
```

