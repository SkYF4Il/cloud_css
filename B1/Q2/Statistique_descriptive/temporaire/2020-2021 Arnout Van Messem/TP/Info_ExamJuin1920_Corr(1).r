data <- read.table("ExamAttrition.csv",header=TRUE,sep=",",colClasses=c("numeric","factor","numeric","factor","numeric","numeric","numeric","numeric","factor"))
str(data)
attach(data)

# --------------- #
# Stat. Descr. - Q1 ----
# --------------- #

data1 <- data[JobRole=="Sales Representative",]
detach(data) ; attach(data1)

# Formule de Sturges :
n <- dim(data1)[1] ; n
  1+10*log10(n)/3

stem(DistanceFromHome)
table(DistanceFromHome)

# Tableau statistique et histogramme :
bornes <- c(0,2.5,5,7.5,10,20,30)
h <- hist(DistanceFromHome, breaks=bornes)
cbind(h$counts, cumsum(h$counts),
      round(h$counts/n, 2), round(cumsum(h$counts/n), 2))

# Vraie médiane :
median(DistanceFromHome)

# Répartition dans la classe médiane :
table(DistanceFromHome[DistanceFromHome>7.5 & DistanceFromHome<=10])

detach(data1)



# ---------------- #
# Analyse de données - Q1 ----
# ---------------- #

attach(data)

# Graphiques :
boxplot(YearsAtCompany ~ Attrition)

Years_Yes <- YearsAtCompany[Attrition=="Yes"]
Years_No <- YearsAtCompany[Attrition=="No"]
h_Yes <- hist(Years_Yes, freq=FALSE)
h_No <- hist(Years_No, freq=FALSE)
plot(seq(-2.5,42.5, by=5), c(0,h_Yes$density,0,0), type="o", pch=16, ylab="Fréquences ajustées", xlab="YearsAtCompany", main="")
lines(seq(-2.5,42.5, by=5), c(0,h_No$density,0), type="o", pch=16, col=2)
legend("topright", c("Yes","No"), col=1:2, pch=16, lty=1)

# Paramètres :
  # Tendance centrale :
mean(Years_Yes) ; mean(Years_No)
median(Years_Yes) ; median(Years_No)
  # Dispersion :
sd(Years_Yes) ; sd(Years_No)
IQR(Years_Yes) ; IQR(Years_No)
  # Dissymétrie :
library(moments)
skewness(Years_Yes) ; skewness(Years_No)



# ---------------- #
# Analyse de données - Q2 ----
# ---------------- #


# (a)
plot(TotalWorkingYears, MonthlyIncome)
cor(TotalWorkingYears, MonthlyIncome)

# (b)
plot(TotalWorkingYears, MonthlyIncome)
reg <- lm(MonthlyIncome ~ TotalWorkingYears)
abline(reg, col=2)

library(robustbase)
sum(JobRole=="Retired")
regLTS <- ltsReg(MonthlyIncome ~ TotalWorkingYears, alpha=274/282)
abline(regLTS, col=3, lty=2)
legend("topleft", c("Moindres carrés","LTS"), col=2:3, lty=1:2)
