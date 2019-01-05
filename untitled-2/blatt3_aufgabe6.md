# blatt3\_aufgabe6

Aufgabe 6. \[∗ 2 Punkte\] Wenden Sie exponentielles Glätten nach Holt-Winters auf die Reihe rock, die den Durchmesser der Rocksäume in den Jahren 1866 bis 1911 beschreibt \(Data von Prof. Rob Hyndmann [http://robjhyndman.com/tsdldata/roberts/skirts.dat](http://robjhyndman.com/tsdldata/roberts/skirts.dat), \(original data from Hipel and McLeod, 1994\): 

rock=scan\("[http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5](http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)\) 

Entscheiden Sie, ob Sie exponentielles Glätten mit oder ohne Trend und/oder Saison anwenden. Plotten Sie Ihre Ergebnisse, machen Sie eine Vorhersage und untersuchen Sie die Residuen auf "Whiteness".

```r
install.packages("astsa")
rm(list=ls())#löscht alle variablen
library(astsa)
install.packages("data.table")
x<-read.table("https://robjhyndman.com/tsdldata/roberts/skirts.dat", header=T, sep="\t")

#Wenden Sie exponentielles Glätten nach Holt-Winters 
?HoltWinters
#alpha, je kleiner die alpha ist, folt mehr Glätten
#beta, Trend faktor
#gamma, Saison faktor
#optimal alpha=0.999 steht in skript 3-97

#a) ohne Trend und Saison 
(hwl2=HoltWinters(x, alpha=.2, beta=F, gamma=F))
p2=predict(hwl2, n.ahead = 20)
plot(hwl2, p2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2)))

(hwl=HoltWinters(x, beta=F, gamma=F))
p=predict(hwl, n.ahead = 20)
plot(hwl, p, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.999 ~ "(optimal)")))

par(mfrow=c(1,2))
plot(hwl2, p2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2)))
plot(hwl, p, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.999 ~ "(optimal)")))

#b)  mit Trend ohne Saison 
(hwlb2=HoltWinters(x, alpha=.2, beta=.2, gamma=F))
pb2=predict(hwlb2, n.ahead = 20)
par(mfrow=c(1,1))
plot(hwlb2, pb2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2 ~ "und"  ~ beta == 0.2)))

(hwlb=HoltWinters(x, gamma=F))
pb=predict(hwlb, n.ahead = 20)
plot(hwlb, pb, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 1 ~ "und" ~ beta == 0.11 ~ "(optimal)")))

par(mfrow=c(1,2))
plot(hwlb2, pb2, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 0.2 ~ "und"  ~ beta == 0.2)))
plot(hwlb, pb, main=as.expression(bquote("Exponentielle Glättung mit"  ~ alpha == 1 ~ "und" ~ beta == 0.11 ~ "(optimal)")))
```

[Lösung: 3uebungsblatt\_6](https://trello.com/c/T5h4eVsY/26-3uebungsblatt6)

