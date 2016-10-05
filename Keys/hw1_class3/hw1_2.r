rm(list=ls())

setwd("F:/Wu/Geostat2016Fall/keys/hw1_class3/")
data1 <- read.csv("hw1_2.csv",header=T)

data1$logCa <- log(data1$Calcium)
data1$logMg <- log(data1$Magnesium)

shapiro.test(data1$Calcium)
shapiro.test(data1$logCa)

shapiro.test(data1$Magnesium)
shapiro.test(data1$logMg)

library(gstat)
library(sp)

coordinates(data1) <- ~X+Y

bubble(data1, "logCa")
spplot(data1, "logCa")

sem_ca <- variogram(logCa ~ 1, data1, width=2, cutoff=30)
plot(sem_ca, type="l")

sem_mg <- variogram(logMg ~ 1,data1, width=2, cutoff=30)
plot(sem_mg, type="l")

#Ca shows spatial autocorrelation (25 becomes stable), not Mg.