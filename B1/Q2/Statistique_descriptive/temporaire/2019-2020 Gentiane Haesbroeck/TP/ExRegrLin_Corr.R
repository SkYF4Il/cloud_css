# ---------------------------------------- #
# Exercice supplémentaire : Régression LTS #
# ---------------------------------------- #

# Définition du répertoire courant ----
setwd("...") # où ... est remplacé par le chemin d'accès au dossier dans lequel se trouve le fichier à importer

# Importation des données ----
data <- read.table("ExRegrLin.txt", header=TRUE)
attach(data)

# Exercice 1  ----

# 1. Diagramme de dispersion
plot(Taille, Poids)
which(Poids < 20)
  # C'est l'observation numéro 20 qui est problématique.

# 2. Droite de régression par la technique des moindres carrés
reg <- lm(Poids ~ Taille)
abline(reg, col=2)
(coeffDet <- var(reg$fitted.values)/var(Poids))
  # Le coefficient de détermination, i.e. la part de la variabilité du Poids expliquée par la régression est très faible

# 3.(a) Régression par moindres carrés pondérés
regPond <- lm(Poids ~ Taille, weights=c(rep(1,19),0))
abline(regPond, col=3, lty=2)

# 3.(b) Régression LTS
library(robustbase)
regLTS <- ltsReg(Poids ~ Taille, alpha=39/40)
abline(regLTS, col=4, lty=3)

detach(data)



# Exercice 2  ----
data <- read.table("stars.txt", header=TRUE, row.names=1)
attach(data)

# 1. Diagramme de dispersion et droite des moindres carrés
plot(LogTemp, LogLight)
reg <- lm(LogLight ~ LogTemp)
abline(reg, col=2)

# 2. Boîte à moustaches
boxplot(LogTemp)
(indExt <- which(LogTemp < 4))

poids <- rep(1, length(LogTemp))
poids <- replace(poids, list=indExt, values=0)
  # On remplace, dans le vecteur rempli de 1, les observation des indices indExt par des 0
regPond <- lm(LogLight ~ LogTemp, weights=poids)
plot(LogTemp,LogLight)
abline(regPond, col=3)

# 3. Régression LTS
regLTS1 <- ltsReg(LogLight ~ LogTemp, alpha=46/47)
plot(LogTemp,LogLight)
abline(regLTS1, col=2)

regLTS2 <- ltsReg(LogLight ~ LogTemp, alpha=43/47)
abline(regLTS2, col=3, lty=2)

regLTS3 <- ltsReg(LogLight ~ LogTemp, alpha=42/47)
abline(regLTS3, col=4, lty=3)

regLTS4 <- ltsReg(LogLight ~ LogTemp, alpha=40/47)
abline(regLTS4, col=5, lty=4)

regLTS5 <- ltsReg(LogLight ~ LogTemp, alpha=35/47)
abline(regLTS5, col=6, lty=5)

detach(data)