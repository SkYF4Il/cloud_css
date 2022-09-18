# Définition du répertoire courant
# setwd("...")

data <- read.table("DataTP6Info.csv", header=TRUE, row.names=1, sep=";")
data$Generosite<-as.factor(data$Generosite)
attach(data)

# Question 1  ----
summary(data)
row.names(data)[Gini<0]
row.names(data)[Gini>1]
row.names(data)[EspVie==max(EspVie)]

windows()
boxplot(EspVie, xlab='EspVie')
savePlot("Boxplot_EspVie",'pdf')

data$Gini[c(72,79)] <- NA
data$EspVie[22] <- NA
detach(data)
attach(data)


# Question 2  ----
# Imputation non conditionnelle :
mean(LogPIB, na.rm=TRUE)
median(LogPIB, na.rm=TRUE)

# Imputation conditionnelle :
cor(data[,c(1,2,3,5),], use="pairwise.complete.obs")

windows()
plot(EspVie, LogPIB)
savePlot("Nuage_EspVieLogPIB",'pdf')

reg <- lm(LogPIB ~ EspVie)
summary(reg)

row.names(data)[is.na(LogPIB)]
EspVie[is.na(LogPIB)]

0.67+0.14*74.1
0.67+0.14*66.6


data2 <- na.omit(data)
detach(data)
attach(data2)


# Question 3  ----
# (a)
effGen <- table(Generosite)

windows()
pie(effGen)
savePlot("Pie_Generosite",'pdf')

windows()
barplot(effGen)
savePlot("Barplot_Generosite",'pdf')

summary(Generosite)

# (b)
GiniBin <- as.integer(Gini >= median(Gini, na.rm=TRUE))
table(Generosite, GiniBin)
prop.table(table(Generosite, GiniBin), margin=2)


# Question 4  ----
# (a)
windows()
boxplot(LogPIB ~ Generosite)
savePlot("Boxplots_LogPIBGenerosite",'pdf')

h1 <- hist(LogPIB[Generosite=="G"], freq=FALSE)
h2 <- hist(LogPIB[Generosite=="PeuG"], freq=FALSE)
windows() 
plot(c(7.25,h1$mids,11.75), c(0,h1$density,0), type="o", pch=16, col=2, ylim=c(0,0.55), xlab="LogPIB", ylab="")
lines(c(7.25,h2$mids,11.75), c(0,h2$density,0), type="o", pch=16, col=3)
legend("topleft", legend=c("G", "PeuG"), col=2:3, lty=1) 
savePlot("PolygFreq_LogPIBGenerosite", "pdf")

# (b)
LogPIB_G <- LogPIB[Generosite=="G"]
LogPIB_PeuG <- LogPIB[Generosite=="PeuG"]
mean(LogPIB_G) ; mean(LogPIB_PeuG)
median(LogPIB_G) ; median(LogPIB_PeuG)
sd(LogPIB_G) ; sd(LogPIB_PeuG)
IQR(LogPIB_G) ; IQR(LogPIB_PeuG)
library(moments)
skewness(LogPIB_G) ; skewness(LogPIB_PeuG)

# (c)
mean(LogPIB)
var(LogPIB)
length(LogPIB_G) ; var(LogPIB_G)
length(LogPIB_PeuG) ; var(LogPIB_PeuG)

(34 * var(LogPIB_G) + 50 * var(LogPIB_PeuG))/84

(33/34) * var(LogPIB_G)
(49/50) * var(LogPIB_PeuG)

(34* (33/34) * var(LogPIB_G) + 50* (49/50) * var(LogPIB_PeuG))/84


# Question 5  ----
# (a)
1+(10/3)*log10(84)
stem(LogPIB)

windows()
h <- hist(LogPIB, breaks=c(7.6,8.4,9,9.4,9.8,10.2,10.6,11,11.4))
savePlot("Hist_LogPIB",'pdf')

(effLogPIB <- h$counts)
(effCumLogPIB <- cumsum(effLogPIB))
(freqLogPIB <- effLogPIB/84)
(freqCumLogPIB <- cumsum(freqLogPIB))

# (b)
skewness(LogPIB)
(mean(LogPIB)-median(LogPIB))/sd(LogPIB)
(quantile(LogPIB,0.25) + quantile(LogPIB,0.75) - 2*median(LogPIB))/(quantile(LogPIB,0.75) - quantile(LogPIB,0.25))

# (c)
sum(h$counts*h$mids)/84
mean(LogPIB)
sort(LogPIB)[1:9] - 8
sum(sort(LogPIB)[1:9] - 8)/84


# Question 6  ----
# (a)
windows()
plot(LogPIB, BienEtre)
cor(LogPIB, BienEtre)

# (b)
reg <- lm(BienEtre ~ LogPIB)
summary(reg)
abline(reg, col=2)
savePlot("Reg_LogPIBBienEtre",'pdf')

# (c)
windows()
plot(LogPIB, reg$residuals)
savePlot("Reg_LogPIBBienEtre_Residus",'pdf')
row.names(data)[which.max(abs(reg$residuals))]
max(abs(reg$residuals))
var(reg$residuals)

# (d)
summary(reg)
cor(LogPIB, BienEtre)^2

cor(LogPIB[Generosite=="G"], BienEtre[Generosite=="G"])
cor(LogPIB[Generosite=="PeuG"], BienEtre[Generosite=="PeuG"])

regG <- lm(BienEtre[Generosite=="G"] ~ LogPIB[Generosite=="G"])
regPeuG <- lm(BienEtre[Generosite=="PeuG"] ~ LogPIB[Generosite=="PeuG"])

windows()
plot(LogPIB, BienEtre, col=as.integer(Generosite)+1)
abline(regG, col=2)
abline(regPeuG, col=3)
legend("topleft", c("G","PeuG"), col=2:3, pch=16)
savePlot("Reg_LogPIBBienEtre_Generosite",'pdf')

cor(data2[,c(1,2,3,5)])
