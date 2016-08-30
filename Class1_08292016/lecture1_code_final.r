#USGS document
library(NADA)
data(package="NADA")
data(HgFish)  # Mercury concentrations in fish across the United States
?HgFish
attach(HgFish)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#general look at the data
summary(HgFish)

hist(Hg)
logHg <- log(Hg)
hist(logHg)
boxplot(Hg)
boxplot(logHg)
logHg_std <- (logHg-mean(logHg))/sd(logHg)
qqnorm(logHg_std)
abline(0,1)

shapiro.test(Hg)
shapiro.test(logHg)

Hg_Len <- Hg/Length
logHg_L <- log(Hg_Len)

shapiro.test(Hg_Len)
shapiro.test(logHg_L)

Hg_data <- data.matrix(HgFish) #Hg_data is all numerical not right

cor(Hg_data, use="pairwise.complete.obs")

cor.test(SedLOI, WatDOC, na.action= na.omit)

# subset data
Hg_AF <- subset(HgFish, LandUse == "AF")

#simple regression

# relation of Hg in Fish to Hg in water

plot(WatMeHg, logHg_L)
plot(log(WatMeHg), logHg_L)
reg2 <- lm(logHg_L ~ log(WatMeHg))
summary(reg2)

abline(reg2)
#abline(reg2$coef,lty=5)

fitted(reg2)
#predict(reg2)
predict(reg2, se.fit=TRUE)
resid(reg2)

plot(fitted(reg2),resid(reg2))
resid_reg2 <- resid(reg2)
resid_std <- (resid_reg2 - mean(resid_reg2)) / sd(resid_reg2)
qqnorm(resid_std)
abline(0,1)

coefficients(reg2)
anova(reg2)
df.residual(reg2)


# for regression between logHg_L and log(WatMeHg)
x <- cbind (1, log(HgFish[,"WatMeHg"]))
temp1 <- solve(t(x) %*% x)
beta <- temp1 %*% t(x)*logHg_L


logHg_L_Large <- logHg_L[Species=="LargemouthBass"]
logWatMeHg_L <- log(WatMeHg[Species=="LargemouthBass"])

plot(logWatMeHg_L, logHg_L_Large)
reg2 <- lm(logHg_L_Large ~ logWatMeHg_L)

summary(reg2)
abline(reg2)
#lines(reg2$fitted.value ~ log(WatMeHg))

plot(WatTotHg, logHg_L)
plot(log(WatTotHg), logHg_L)

reg3 <- lm(logHg_L ~ log(WatTotHg))
summary(reg3)

abline(reg3)

# relation of Hg in Fish to percent of wetland
plot(PctWetland, logHg_L)
reg1 <- lm(logHg_L ~ PctWetland)

summary(reg1)
abline(reg1)


# multiple regression

SedAVS_c <- SedAVS ^ (1/3)
reg_all <- lm(logHg_L ~ PctWetland + log(WatMeHg) + log(WatTotHg) + SedMeHg + log(SedTotHg) + log(WatDOC) + SedLOI + SedAVS_c)

summary(reg_all)

plot(fitted(reg_all),resid(reg_all))
resid_reg <- resid(reg_all)
resid_std <- (resid_reg - mean(resid_reg)) / sd(resid_reg)
qqnorm(resid_std)
abline(0,1)

plot(LandUse, log(WatMeHg))

LU_Urb <- ifelse(LandUse=="Urb", 1, 0)
LU_Bkg <- ifelse(LandUse=="Bkg", 1, 0)
LU_Mine <- ifelse(LandUse=="Mine", 1, 0)
LU_Ag <- ifelse(LandUse=="Ag", 1, 0)
LU_AF <- ifelse(LandUse=="AF", 1, 0)

reg_all2 <- lm(logHg_L ~ LU_Urb+LU_Mine+LU_Ag+LU_AF+PctWetland + log(WatMeHg) + log(WatTotHg) + SedMeHg + log(SedTotHg) + log(WatDOC) + SedLOI + SedAVS_c)
summary(reg_all2)

library(car)
vif(reg_all2)