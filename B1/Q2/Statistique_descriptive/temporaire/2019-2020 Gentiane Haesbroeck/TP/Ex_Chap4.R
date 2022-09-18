# Exercice 6  ----

x <- c(10,8,13,9,11,14,6,4,12,7,5)
y <- c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)

# (a)
plot(x,y)

# (b)
reg <- lm(y ~ x)
summary(reg)
reg$coefficients
reg$residuals



# Exercice 7  ----

annee <- seq(75, 87, by=1)
chomage <- c(4,4.4,5,5.2,5.9,6.3,7.4,8.1,8.3,9.7,10.2,10.4,10.5)

# (a)
plot(annee, chomage)

# (b)
reg <- lm(chomage ~ annee)
summary(reg)
reg$coefficients
cor(annee,chomage)
cor(annee,chomage)^2



# Exercice 8  ----

# (a)
y <- c(170,150,165,190,350,465,245,58,90,115,250)
boxplot(y)

# (b)
# i.
x <- c(455,510,380,1280,1115,1145,460,220,250,310,530)
plot(x,y)
# ii.
reg <- lm(y ~ x)
reg$coefficients
plot(x,y) ; abline(reg, col=2)
# v.
xEU <- x[-4]
yEU <- y[-4]
regEU <- lm(yEU ~ xEU)
regEU$coefficients

xGB <- x[-6]
yGB <- y[-6]
regGB <- lm(yGB ~ xGB)
regGB$coefficients

plot(x,y) ; abline(reg, col=2) ; abline(regEU, col=3, lty=2) ; abline(regGB, col=4, lty=3)
legend("bottomright", legend=c("Série S", "Série S_EU", "Série S_GB"), col=2:4, lty=1:3)
