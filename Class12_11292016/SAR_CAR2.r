library(spdep)
data(NY_data)
nylm <- lm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data =nydata)
nydata$lmresid <- residuals(nylm)
lm.morantest(nylm, listw_NY)

nysar <- spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data =nydata, listw=listw_NY)
summary(nysar)

#nydata$r2 <- residuals(nysar)

nylmw <- lm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data =nydata, weights=POP8)
summary(nylmw)

nydata$lmwresid <- residuals(nylmw)
lm.morantest(nylmw, listw_NY)

nysarw <- spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data =nydata, listw=listw_NY, weights=POP8)
summary(nysarw)

AIC(nylm, nysar, nylmw, nysarw)

nycar <- spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data =nydata, listw=listw_NY, family="CAR")
summary(nycar)

nycarw <- spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data =nydata, listw=listw_NY, weights=POP8, family="CAR")
summary(nycarw)

#CARBayes example
library(CARBayes)
library(CARBayesdata)
library(sp)
library(spdep)
data(pricedata)

missing.IG <- setdiff(rownames(GGHB.IG@data), pricedata$IG)
missing.IG.row <- which(missing.IG==rownames(GGHB.IG@data))
propertydata.spatial <- GGHB.IG[-missing.IG.row, ]
propertydata.spatial@data <- data.frame(propertydata.spatial@data, pricedata)

propertydata.spatial@data$logprice <- log(propertydata.spatial@data$price)
propertydata.spatial@data$logdriveshop <- log(propertydata.spatial@data$driveshop)

#A model with all the covariates is fitted to the data, where the crime rate variable is modelled
#as non-linear using a natural cubic spline with 3 degrees of freedom. This is achieved using
#the following R code:
library(splines)
form <- logprice~ns(crime,3)+rooms+sales+factor(type) + logdriveshop
model <- lm(formula=form, data=propertydata.spatial@data)

W.nb <- poly2nb(propertydata.spatial, row.names = rownames(propertydata.spatial@data))
W.list <- nb2listw(W.nb, style="B")
resid.model <- residuals(model)
moran.mc(x=resid.model, listw=W.list, nsim=1000)

W <- nb2mat(W.nb, style="B")
model.spatial <- S.CARleroux(formula=form, data=propertydata.spatial@data, family="gaussian", W=W, burnin=20000, n.sample=120000, thin=10)


crime.effect <- summarise.lincomb(model=model.spatial, columns=c(2,3,4), quantiles=c(0.5, 0.025, 0.975), distribution=FALSE)
plot(propertydata.spatial@data$crime, crime.effect$quantiles[ ,1], pch=19, ylim=c(-0.55,0.05), xlab="Number of crimes", ylab="Effect of crime")
points(propertydata.spatial@data$crime, crime.effect$quantiles[ ,2], pch=19, col="red")
points(propertydata.spatial@data$crime, crime.effect$quantiles[ ,3], pch=19, col="red")

