###########################################
# Probabilité et Statistique I - Sc. Info #
# Exercices Chapitre 2                    #
###########################################



# Ex. 1 ----

data<-as.data.frame(matrix(c(27.6,44,28.4,8.6,54.2,37.2),ncol=3,byrow=TRUE))
names(data)<-c("Intesive","Legere","Aucune")
row.names(data)<-c("Hommes","Femmes")
View(data)

# (b)
Hommes <- data[1,]
Femmes <- data[2,]

pie(as.numeric(Hommes/100), labels=names(data), main="Activité sportive des hommes")
pie(as.numeric(Femmes/100), labels=names(data), main="Activité sportive des femmes")

# (c)
barplot(as.matrix(data)/100, beside=TRUE,col=1:2, ylab="Fréquences", xlab="Activité sportive")
legend("topleft", c("Hommes","Femmes"), col=1:2, pch=15)


# Ex. 2 ----

Modalites<-c("ActifOccupe","Chomeur","Enfant","NonActifJeune", "NonActifVieux")
Effectifs<-c(4007,375,1805,2356,1670)

# (a)
col5<-c("#66FFFF","#33CCCC", "#009999","#006666","#003333")
pie(Effectifs/sum(Effectifs),col=col5, labels=round(Effectifs*100/sum(Effectifs),2)) 
legend("topleft", legend=Modalites, col=col5, pch=15)


# (b)
barplot(Effectifs, names.arg=Modalites, ylab="Effectifs (milliers)",col=c("#009999"))


# Ex. 3 ----

NbrePers<-c(0, 3, 1, 2, 4, 3, 3, 3, 4, 1, 4, 1, 3, 2, 2, 1, 5, 2, 5, 2, 4, 2, 0, 3, 3, 2, 4, 1, 5, 2, 5,
            2, 5, 1, 3, 2, 2, 2, 4, 4, 5, 1, 2, 4, 2, 1, 2, 2, 1, 3, 5, 1, 0, 3, 2, 3, 1, 4, 2, 4, 5, 4,
            1, 2, 0, 2, 1, 2, 2, 3, 5, 2, 1, 4, 2, 2, 4, 0, 3, 3, 2, 2, 2, 0, 4, 1, 2, 1, 3, 1, 1, 5, 3,
            1, 3, 2, 2, 0, 2, 0)

# (a)
eff <- table(NbrePers)
effCum <- cumsum(eff)
freq <- eff/sum(eff)
freqCum <- cumsum(freq)
cbind(eff,effCum,freq,freqCum)

# (b)
plot(eff, ylab='Effectifs', xlab='Nombre de personnes à charge')

# (c)

plot(ecdf(NbrePers),ylab='Fréquences', xlab='Nombre de personnes à charge',main='')


# Ex. 4 ----

Valeurs<-1:6
Effectifs<-c(361,453,227,209,83,59)

# (c)
sum(Valeurs*Effectifs)

# (d)
freq <- Effectifs/sum(Effectifs)
effCum <- cumsum(Effectifs)
freqCum <- cumsum(freq)
cbind(Effectifs,freq, effCum,freqCum)

# (f)
plot(freq, type="h", xlab="Nombre de personnes dans le ménage", ylab="Fréquences")

# (g)
x <- rep(Valeurs, Effectifs)
plot(ecdf(x), main="", xlab="Nombre de personnes dans le ménage", ylab="Fréquences cumulées")



# Ex. 5 ----

Poids<-c(68, 70, 67, 75, 72, 71, 67, 65, 60, 60, 65, 65, 77, 95, 85, 70, 70, 72, 66, 75, 90, 65, 62, 70,
         52, 60, 59, 65, 68, 71, 97, 65, 57, 75, 77, 75, 85, 56, 77, 67, 62, 52, 67, 72, 79, 60, 72,
         69, 58, 55, 75, 75, 78, 65, 95, 65, 90, 72, 72, 60)

# (a)
effectifs <- c(sum(Poids>=50 & Poids<=60), sum(Poids>60 & Poids<=70), sum(Poids>70 & Poids<=80), sum(Poids>80 & Poids<=90), sum(Poids>90 & Poids<=100))
classes <- c("[50, 60]", "]60, 70]", "]70, 80]", "]80, 90]", "]90, 100]")
effCum <- cumsum(effectifs)
freq <- effectifs/sum(effectifs)
freqCum <- cumsum(freq)
cbind(classes, effectifs, effCum,freq, freqCum)

h <- hist(Poids, breaks=5, freq=FALSE, xlim=c(45,105))
lines(c(45,h$mids,105), c(0,h$density,0), type="o", pch=16, col=2)

# (b)
effectifs <- c(sum(Poids>=50 & Poids<=60), sum(Poids>60 & Poids<=65), sum(Poids>65 & Poids<=70), sum(Poids>70 & Poids<=75), sum(Poids>75 & Poids<=80), sum(Poids>80 & Poids<=100))
classes <- c("[50, 60]", "]60, 65]", "]65, 70]", "]70, 75]", "]75, 80]", "]80, 100]")
effCum <- cumsum(effectifs)
freq <- effectifs/sum(effectifs)
freqCum <- cumsum(freq)
cbind(classes, effectifs, effCum, freq, freqCum)

bornes <- c(50,60,65,70,75,80,100)
h <- hist(Poids, breaks=bornes, freq=FALSE, xlim=c(45,105))
x <- seq(47.5,102.5, by=5)
y <- rep(c(0, h$density, 0), c(1,2,1,1,1,1,4,1))
lines(x, y, type="o", pch=16, col=2)


# (c)
plot(bornes, c(0,freqCum),xlab="Poids (en kg)",ylab="Fréquences cumulées", type="b")

12/2+14+5/2
sum(Poids>=67.5 & Poids <= 77.5)

sort(Poids)[30]

# Ex. 6 ----

Classes<-c("[0, 10]","]10, 15]","]15, 20]","]20, 25]","]25, 30]","]30, 35]","]35, 50]","]50, 60]","]60, 100]")
Effectifs<-c(181,116,89,39,28,60,91,58,65)

# (a)
freq <- Effectifs/sum(Effectifs)
effCum <- cumsum(Effectifs)
freqCum <- cumsum(freq)
cbind(Classes, Effectifs, effCum, round(freq,2), round(freqCum,2))

# (b)
centres <- c(5,12.5,17.5,22.5,27.5,32.5,42.5,55,80)
effectifs <- c(181,116,89,39,28,60,91,58,65)
bornes <- c(0,10,15,20,25,30,35,50,60,100)
x <- rep(centres, effectifs)

h <- hist(x, breaks=bornes, xlim=c(-2.5, 102.5),main='')
abs <- seq(-2.5,102.5, by=5)
ord <- rep(c(0,h$density,0), c(1,2,1,1,1,1,1,3,2,8,1))
lines(abs,ord, type="o", pch=16, col=2)

# (c)
181+116+89/2
39/2+28+60+91*2.5/15

# (d)
plot(bornes, c(0,freqCum), type="o", pch=16,xlab="Salaire (en miliers d'UM)", ylab="Fréquence cumulée")

