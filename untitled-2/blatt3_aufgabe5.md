# blatt3\_aufgabe5

Aufgabe 5. \[∗ 3 Punkte\] Wenden Sie exponentielles Glätten nach Holt-Winters

a\) ohne Trend und Saison

b\) mit Trend ohne Saison

c\) ohne Trend mit Saison

d\) mit Trend und Saison

auf eine der Reihen

* • Unfalltote in USA 1973-1978 \("USAccDeaths"\)
* • Geburten in USA 1948-1979 \("births"\)

Erstellen Sie Vorhersagen, plotten und kommentieren Sie Ihre Ergebnisse.

```r
install.packages("astsa")
rm(list=ls())#löscht alle variablen
library(astsa)

USAccDeaths
View(USAccDeaths)

plot(USAccDeaths, main="Reihe Unfalltote in USA 1973-1978")
acf(USAccDeaths)

#Wenden Sie exponentielles Glätten nach Holt-Winters 
?HoltWinters
#alpha, je kleiner die alpha ist, folt mehr Glätten
#beta, Trend faktor
#gamma, Saison faktor
#optimal alpha=0.999 steht in skript 3-97

#a) ohne Trend und Saison 
(hwl2=HoltWinters(USAccDeaths, alpha=.2, beta=F, gamma=F))
p2=predict(hwl2, n.ahead = 20)
plot(hwl2, p2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2)))

(hwl=HoltWinters(USAccDeaths, beta=F, gamma=F))
p=predict(hwl, n.ahead = 20)
plot(hwl, p, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.999 ~ "(optimal)")))

par(mfrow=c(1,2))
plot(hwl2, p2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2)))
plot(hwl, p, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.999 ~ "(optimal)")))


#b)  mit Trend ohne Saison 
(hwlb2=HoltWinters(USAccDeaths, alpha=.2, beta=.2, gamma=F))
pb2=predict(hwlb2, n.ahead = 20)
par(mfrow=c(1,1))
plot(hwlb2, pb2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2 ~ "und"  ~ beta == 0.2)))

(hwlb=HoltWinters(USAccDeaths, gamma=F))
pb=predict(hwlb, n.ahead = 20)
plot(hwlb, pb, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 1 ~ "und" ~ beta == 0.11 ~ "(optimal)")))

par(mfrow=c(1,2))
plot(hwlb2, pb2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2 ~ "und"  ~ beta == 0.2)))
plot(hwlb, pb, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 1 ~ "und" ~ beta == 0.11 ~ "(optimal)")))


#c) ohne Trend mit Saison 
(hwlc2=HoltWinters(USAccDeaths, alpha=.2, beta=F, gamma=0.2))
pc2=predict(hwlc2, n.ahead = 20)
par(mfrow=c(1,1))
plot(hwlc2, pc2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2 ~ "und"  ~ gamma == 0.2)))

(hwlc=HoltWinters(USAccDeaths, beta=F))
pc=predict(hwlc, n.ahead = 20)
plot(hwlc, pc, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.69 ~ "und" ~ gamma == 0.87 ~ "(optimal)")))

par(mfrow=c(1,2))
plot(hwlc2, pc2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2 ~ "und"  ~ gamma == 0.2)))
plot(hwlc, pc, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.69 ~ "und" ~ gamma == 0.87 ~ "(optimal)")))


#d) mit Trend und Saison 
(hwld2=HoltWinters(USAccDeaths, alpha=.2, beta=0.2, gamma=0.2))
pd2=predict(hwld2, n.ahead = 20)
par(mfrow=c(1,1))
plot(hwld2, pd2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2 ~ "," ~ beta == 0.2 ~ "und" ~ gamma == 0.2 ~ "(optimal)")))

(hwld=HoltWinters(USAccDeaths))
pd=predict(hwld, n.ahead = 20)
plot(hwld, pd, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.73 ~ "," ~ beta == 0.02 ~ "und" ~ gamma == 1 ~ "(optimal)")))

par(mfrow=c(1,2))
plot(hwld2, pd2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2 ~ "," ~ beta == 0.2 ~ "und" ~ gamma == 0.2 ~ "(optimal)")))
plot(hwld, pd, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.73 ~ "," ~ beta == 0.02 ~ "und" ~ gamma == 1 ~ "(optimal)")))

#Ähnliche methode mit births dataset
```

[Lösung: 3uebungsblatt\_5](https://trello.com/c/uq8xFbUB/25-3uebungsblatt5)

