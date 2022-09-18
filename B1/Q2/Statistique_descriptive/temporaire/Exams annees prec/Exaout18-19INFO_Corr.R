# -------------------#
# -------------------#
# Statistique descriptive - Sc. Info  ----
# Examen septembre 2019               ----    
# -------------------#
# -------------------#



# ---------------- #
# Analyse de données  ----
# ---------------- #

# ! Commencer par définir le répertoire courant !
data <- read.table("Cereales.txt", header=TRUE,  row.names=1)
attach(data)
View(data)

# 1.  ----
# On ne garde que les marques Kelloggs et GeneralMills
data2 <- data[Marque=="Kelloggs" | Marque=="GeneralMills",]
detach(data)
attach(data2)

# 1.(a) ----
GenMills <- data2[Marque=="GeneralMills",]
Kell <- data2[Marque=="Kelloggs",]

# Boîtes à moustaches :
boxplot(GenMills$Calories, Kell$Calories, names=c("GeneralMills","Kelloggs"))

# Polygones des fréquences :
hGenMills <- hist(GenMills$Calories, freq=FALSE, breaks=seq(40,160, by=10))
hKell <- hist(Kell$Calories, freq=FALSE, breaks=seq(40,160, by=10))

plot(c(35,hGenMills$mids,165), c(0,hGenMills$density,0), type="o", pch=16, ylab="Fréquences ajustées", xlab="Calories")
lines(c(35,hKell$mids,165), c(0,hKell$density,0), type="o", pch=16, col=2)
legend("topleft", legend=c("GeneralMills","Kelloggs"), col=1:2, lty=1)


# 1.(b) ----
Cal_GenMills <- GenMills$Calories
Cal_Kell <- Kell$Calories
mean(Cal_GenMills) ; mean(Cal_Kell)
median(Cal_GenMills) ; median(Cal_Kell)
sd(Cal_GenMills) ; sd(Cal_Kell)
IQR(Cal_GenMills) ; IQR(Cal_Kell)
library(moments)
skewness(Cal_GenMills) ; skewness(Cal_Kell)


# 1.(c) ----
nG <- length(Cal_GenMills) ; nK <- length(Cal_Kell)
moyG <- mean(Cal_GenMills) ; moyK <- mean(Cal_Kell)
varG <- var(Cal_GenMills) ; varK <- var(Cal_Kell)
s <- sqrt(varK/nK + varG/nG)
abs((moyK-moyG)/s)




# 2.  ----
# On reprend la BD initiale car on réintègre les céréales de type "Autre"
detach(data2)
attach(data)


# 2.(a) ----
cor(Potassium, data[,1:7])
min(abs(cor(Potassium, data[,1:7])))
max(abs(cor(Potassium, data[,1:7])))


# 2.(b) ----
reg <- lm(Potassium ~ Fibre)
summary(reg)
reg$residuals[row.names(data)=="Smacks"]
reg$fitted.values[row.names(data)=="Golden_Crisp"]


# 2.(c) ----
summary(reg)


# 2.(d) ----

cor(GenMills$Potassium, GenMills$Fibre)
cor(Kell$Potassium, Kell$Fibre)

reg_GenMills <- lm(GenMills$Potassium ~ GenMills$Fibre)
reg_Kell <- lm(Kell$Potassium ~ Kell$Fibre)
plot(Fibre, Potassium, pch=as.integer(Marque), col=as.integer(Marque))
legend("topleft", levels(Marque), pch=1:3, col=1:3)
abline(reg, col=4)
abline(reg_GenMills, col=2)
abline(reg_Kell, col=3)
legend("bottomright", c("3 marques","GeneralMills","Kelloggs"), lty=1, col=c(4,2,3))