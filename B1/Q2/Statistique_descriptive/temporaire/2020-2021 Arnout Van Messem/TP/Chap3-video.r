###########################################
# Probabilité et Statistique I - Sc. Info #
# Exercices Chapitre 2                    #
###########################################


# Ex. 1 ----

S1<-c(3,5,2,6,5,9,5,2,8,6)
S2<-c(84,91,72,68,87,78,52,92,71,85,62,82,99)

# (a)
mean(S1); mean(S2)
which.max(table(S1))
sort(S1); sort(S2)
median(S1); median(S2)
summary(S1) ; summary(S2)



# Ex. 2 ----

Pommes<-0:12
Caisses<-c(1,0,1,3,5,7,14,33,54,66,72,78,66)

# (a)
sum(Pommes*Caisses)/sum(Caisses)

# (b)
which.max(Caisses)-1

# (c)
(freqCum <- cumsum(Caisses/sum(Caisses)))
cbind(Pommes, freqCum)

# (d)
x <- rep(Pommes, Caisses)
mean(x, trim=0.1)



# Ex. 3 ----

eff <- c(1,1,4,5,10)
val <- c(12500,6000,1200,950,600)
x <- rep(val,eff)

# (a)
mean(x)

# (b)
median(x)
val[which.max(eff)]
mean(x[-(1:2)])


# Ex. 4 ----

NbHab <- c(1835,1624,729,974)
NbVehicHab <- c(0.4583,0.4883,0.5048,0.4774)
sum(NbVehicHab*NbHab)/sum(NbHab)


# Ex. 5 ----
centres <- c(55,62.5,67.5,72.5,77.5,90)
eff <- c(12,10,12,14,5,7)
serieGr <- rep(centres, eff)

# (a)
mean(serieGr)

freq <- eff/sum(eff)
(freqCum <- cumsum(freq))

amplitudes <- c(10,5,5,5,5,20)
(freqAj <- freq/amplitudes)

(xM <- 3*68.33 - 2*mean(serieGr))

# (b)
c1<-c(52,52,55,56,57,58,59,60,60,60,60,60)
c4<-c(71,71,rep(72,6),rep(75,6))

mean(c1)
mean(c4)

(12*(55-mean(c1)) + 14* (72.5-mean(c4)))/60

# (c)
Poids<-c(68, 70, 67, 75, 72, 71, 67, 65, 60, 60, 65, 65, 77, 95, 85, 70, 70, 72, 66, 75, 90, 65, 62, 70,
         52, 60, 59, 65, 68, 71, 97, 65, 57, 75, 77, 75, 85, 56, 77, 67, 62, 52, 67, 72, 79, 60, 72,
         69, 58, 55, 75, 75, 78, 65, 95, 65, 90, 72, 72, 60)

sum(Poids<=61.5)
sum(Poids>=73.91)

# Ex. 7 -------------

eff<-c(9,26,19,24,14)
bornes<-c(10,20,40,50,80,100)

freq<-eff/sum(eff)
freqcum<-cumsum(freq)

windows()
plot(bornes, c(0,freqcum), type='b', xlab='Trajet (en km)', ylab='Fréquences cumulées')
lines(c(45.79,45.79),c(0,0.5), lty=2, col='red')
lines(c(0,45.79),c(0.5,0.5), lty=2, col='red')
savePlot('Ex_Chap3_7b', 'pdf')

# Ex. 8 --------------------

windows()
plot(c(-7,-1),c(-1,-1), lty=2, xlim=c(-6,6),ylim=c(-1.5,1.5), typ='l',xlab='x',ylab='')
lines(c(1,7),c(1,1),lty=2)
lines(c(-1,1),c(-1,1),lty=2)
abline(b=1/5,a=0, xlim=c(-6,6))
legend("topleft", c("Moyenne", "Médiane"), lty=1:2)
savePlot('Ex_Chap3_8', 'pdf')

# Ex. 12 -------------------------

Valeurs<-0:5
Effectifs<-c(8,19,32,17,14,10)

# (a)
sum(Valeurs*Effectifs)/sum(Effectifs)
(EffCum<-cumsum(Effectifs))

# (b)
x<-rep(Valeurs, Effectifs)
windows()
boxplot(x, ylab="Nombre d'emplois antérieurs")
savePlot('Ex_Chap3_12b', 'pdf')

# (c)
var(x)*(99/100)
sd(x)*sqrt(99/100)

# (d)
windows()
plot(Valeurs, Effectifs, type='h')
savePlot('Ex_Chap3_12d', 'pdf')

(m3<- mean((x-mean(x))^3))
(s3<-(sd(x)*sqrt(99/100))^3)
(Fisher<-m3/s3)

(Pearson<-(2.4-2)/1.407)

# Ex. 13 --------------------

Serie<-c(47,48,49,50,53,55,55,55,56,56,58,59,61,62,62,63,63,63,64,65,65,66)

# (a)
mean(Serie)
median(Serie)
table(Serie)

# (b)
sd(Serie)*sqrt(21/22)

mean(Serie)-2*sd(Serie)*sqrt(21/22)
mean(Serie)+2*sd(Serie)*sqrt(21/22)

# (c)
summary(Serie)

Serie_G<-c(68,70,67,75, 72,71,67,65, 60,60,65,65,77,95,85,70, 70,72,66,75, 90,65,62,70,
           52,60,59,65, 68,71,97,65, 57,75,77,75,85,56,77,67, 62,52,67,72, 79,60,72,69,
           58,55,75,75, 78,65,95,65, 90,72,72,60)
summary(Serie_G)

windows()
boxplot(Serie, Serie_G, ylab='Poids (en kg)', names=c('Filles', 'Garçons'))
savePlot('Ex_Chap3_13c', 'pdf')

(YK_F<-(55+63-2*58.5)/(63-55))
(YK_G<-(65+75-2*69.5)/(75-65))
(m3<- mean((Serie-mean(Serie))^3))
(s3<-(sd(Serie)*sqrt(21/22))^3)
(Fisher_F<-m3/s3)

# (d)

(22*mean(Serie) + 25*62)/47
(22*5.8^2 + 25*3.334^2)/47
(22*(57.95-60.1)^2 + 25*(62-60.1)^2)/47
21.66+4.08

4.08/25.74
21.66/25.74

# Ex. 14 -----------------

x1<-c(1500,1750,3000)
eff1<-c(50,85,5)
x2<-c(1200,1600,2500)
eff2<-c(10,20,30)

(moy1<-sum(x1*eff1)/sum(eff1))
(moy2<-sum(x2*eff2)/sum(eff2))
(moy<-(140*moy1 + 60*moy2)/200)

(s1<-sum(x1^2*eff1)/sum(eff1) - moy1^2)
(s2<-sum(x2^2*eff2)/sum(eff2) - moy2^2)

(140 * s1 + 60 * s2)/200
(140 * (moy1-moy)^2 + 60 * (moy2-moy)^2)/200
(140 * s1 + 60 * s2)/200 + (140 * (moy1-moy)^2 + 60 * (moy2-moy)^2)/200

((140 * s1 + 60 * s2)/200)/((140 * s1 + 60 * s2)/200 + (140 * (moy1-moy)^2 + 60 * (moy2-moy)^2)/200)
((140 * (moy1-moy)^2 + 60 * (moy2-moy)^2)/200)/((140 * s1 + 60 * s2)/200 + (140 * (moy1-moy)^2 + 60 * (moy2-moy)^2)/200)


# Ex. 15 ---------------------

lundi<-c(57,60,52,49,56,46,51,63,49,57,40,45,65,55)
samedi<-c(83,93,77,69,81,70,71,91,70,82,55,65,105,80)

sort(lundi)
sort(samedi)

summary(lundi)
summary(samedi)

windows()
boxplot(lundi, samedi, names=c('Lundi','Samedi'), ylab="Recette (en miliers d'euros)")
savePlot('Ex_Chap3_15', 'pdf')

