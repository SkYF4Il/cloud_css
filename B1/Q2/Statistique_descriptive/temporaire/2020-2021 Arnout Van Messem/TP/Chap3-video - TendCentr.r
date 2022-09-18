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