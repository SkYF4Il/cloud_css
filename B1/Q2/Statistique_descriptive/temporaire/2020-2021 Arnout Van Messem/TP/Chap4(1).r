# Exercice 6  ----

x <- c(10,8,13,9,11,14,6,4,12,7,5)
y <- c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)

# (a)

windows()
plot(x,y)
savePlot("Chap4_Ex6a", 'pdf')

# (b)

sum(x)
sum(y)
sum(x^2)
sum(y^2)
sum(x*y)

mean(x)
mean(y)
var(x)*(10/11)
var(y)*(10/11)
cov(x,y)

reg <- lm(y ~ x)
summary(reg)
reg$coefficients
reg$residuals



# Exercice 7  ----

annee <- seq(75, 87, by=1)
chomage <- c(4,4.4,5,5.2,5.9,6.3,7.4,8.1,8.3,9.7,10.2,10.4,10.5)

# (a)
windows()
plot(annee, chomage)
savePlot("Chap4_Ex7a","pdf")

# (b)
reg <- lm(chomage ~ annee)
summary(reg)
reg$coefficients
cor(annee,chomage)
cor(annee,chomage)^2

reg$fitted.values
reg$residuals

chomage-reg$fitted.values


# Exercice 8  ----

# (a)
y <- c(170,150,165,190,350,465,245,58,90,115,250)
windows()
plot(ecdf(y), xlab='Taux de mortalité', ylab='Fréquences cumulées')
segments(0,0.25, 115,0.25, lty=2, col=2)
segments(115,0.25, 115,-1, lty=2, col=2)

segments(0,0.5, 170,0.5, lty=2, col=2)
segments(170,0.5, 170,-1, lty=2, col=2)

segments(0,0.75, 250,0.75, lty=2, col=2)
segments(250,0.75, 250,-1, lty=2, col=2)
# axis(1, at=c(115,170,250), labels=c(115,170,250), col.ticks=2, col.axis=2)
# axis(1, at=c(58,90,150,165,190,245,350,465), labels=c(58,90,150,165,190,245,350,465))

sort(y)

windows()
boxplot(y, ylab='Taux de mortalité')
savePlot("Chap4_Ex8a","pdf")


# (b)
# i.
x <- c(455,510,380,1280,1115,1145,460,220,250,310,530)
windows()
plot(x,y)
savePlot("Chap4_Ex8bi","pdf")

# ii.
reg <- lm(y ~ x)
reg$coefficients

windows()
plot(x,y)
abline(reg, col='red')
text(1145,y=455,label='GB')
text(1280,y=180,label='EU')
savePlot("Chap4_Ex8bii","pdf")

# iii.
cor(x,y)

# v.
xEU <- x[-4]
yEU <- y[-4]
regEU <- lm(yEU ~ xEU)
summary(regEU)
regEU$coefficients

xGB <- x[-6]
yGB <- y[-6]
regGB <- lm(yGB ~ xGB)
summary(regGB)
regGB$coefficients

windows()
plot(x,y) ; abline(reg, col=2) ; abline(regEU, col=3, lty=2) ; abline(regGB, col=4, lty=3)
legend("bottomright", legend=c("Série S", "Série S_EU", "Série S_GB"), col=2:4, lty=1:3)
text(1145,y=455,label='GB')
text(1280,y=180,label='EU')
savePlot("Chap4_Ex8bv","pdf")