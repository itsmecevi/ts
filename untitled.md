# 1.a Einführung und Grundlagen

## Legende:

* ZV:= Zufallsvariablen
* WN:= White Noise
* EW:= Erwartungswert
* Var:= Varianz

## **Zeitreihe**: 

Skript Seite 1-1, bsp Seite. 1-2 bis Seite. 1-18, R-skript bsp Seite.8 / Skript-bsp.R \#Zeitreihe einführung

![](.gitbook/assets/1-zeitreihe.PNG)

## **Stochastischer Prozess**: 

Skript Seite 2-1

![](.gitbook/assets/2-stochpro.PNG)

und **zeireihe:** Skript Seite 2-1

![](.gitbook/assets/3-zeitreihe.PNG)

{% hint style="info" %}
Ein Zeitreihe: einen folgen

Ein stoch.prozesse: zeitlich geordnete Zufallsvariable in Zufallsprozesse

Zeitreihe mit stoch.prozesse: einen folgen, die Zufallsvariablen sind
{% endhint %}



## **Beispiel stoch.prozesse:** 

**Skript, Seite 2-2 bis Seite 2-12** 

**\#1-Gaußprozesse**: die ZV von folgen \(Zeitreihen\) sind [normalverteilt ](https://de.wikipedia.org/wiki/Normalverteilung)oder die Zeitreihe sind normalverteilt. 

![](.gitbook/assets/4-gauss-prozesse.PNG)

**\#2-White Noise**: die iid ZV von folgen \(Zeitreihen\) hat endlichen EW und Var. Oft sind WN zentriert sind, also EW=0 ist. Notation für WN : \(εt\) ∼ WN\(µ,σ2\). 

Skript Seite 2-4 bis Seite 2-5, R-skript Seite 2-7 / Skript-bsp.R: \#White Noise

![](.gitbook/assets/5-wn.PNG)

**\#3-Gausssche WN: 2. punkte: Denn Gauss selbst ist schon Unabhängig**

![](.gitbook/assets/6-gauss-wn.PNG)

{% hint style="info" %}
Fazit:

\#Gaußprozess: Die Folge sind Normalverteilt

\#Strikes WN: Endlichen EW und Endlichen Var, Oft: EW=0

\#Schwach WN: Gleichen Endlichen EW und Var, Unkorreliert

\#Ein Strikes WN ist automatisch Schwach WN. siehe definition

\#Gaussche WN: Strikes WN==Schwach WN
{% endhint %}

**\#4-Random Walk:** kumulierte WN

Skript Seite 2-10 & 2-11, R-skript Seite 2-12 / Skript-bsp.R \#Random Walk



![](.gitbook/assets/7-rw.PNG)

anderen Schreibweise von RW

----1

![](.gitbook/assets/8-rw2.PNG)

----2

![](.gitbook/assets/9-rw3.PNG)

## Wichtige Kennzahlen von Xt \(ZV von Folgen\):

\#EW und Var

![](.gitbook/assets/10-ew-var.PNG)

\#Kovarianz und Korrelation

Kovarianz: Lineare zusammenhäng von X und Y

Korrelation: Standardisierte Kovarianz

![Aus Unabh&#xE4;ngig kommt dann Unkorreliert](.gitbook/assets/a-kovarianz-korrelations%20%281%29.PNG)



\#Autokovarianzfunktion 

![](.gitbook/assets/11-autokovarianz.PNG)

\#Autokorrelationsfunktion

![](.gitbook/assets/12-autokorrelation.PNG)

\#Stationär

![](.gitbook/assets/13-stationaer.PNG)

![](.gitbook/assets/14-stationaer2.PNG)

\#Kurze Erinnerung: Kovarianzen

![](.gitbook/assets/15-kovarianzen.PNG)

\#Bedeutung der Autokorrelation 

![](.gitbook/assets/16-bedeutung-autokovarianz.PNG)

\#Wichtige Kennzahlen: Beispiele WN

![](.gitbook/assets/17-besp-wn.PNG)

\#Wichtige Kennzahlen: Beispiele RW ohne drift

![](.gitbook/assets/18-bsp-rw-ohne-drift.PNG)

\#Wichtige Kennzahlen: Beispiele Signal + rauschen

![](.gitbook/assets/19-besp-signal-rauschen.PNG)

\#Beispiele stochastischer Prozesse: Linear prozesse1

![](.gitbook/assets/20-linear-prozesse1.PNG)

\#Beispiele stochastischer Prozesse: Linear prozesse2

![](.gitbook/assets/21-linear-prozesse2.PNG)

\#Beispiele stochastischer Prozesse: Linear prozesse3

![](.gitbook/assets/22-linear-prozesse3.PNG)

