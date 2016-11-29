library(sp)
library(gstat)
data(meuse)
coordinates (meuse) <- ~ x+y
hist(meuse$zinc)
shapiro.test(meuse$zinc)

#data transformation
#Reasons to transform data
#-to more closely approximate a theoretical distribution that has nice statistical
#properties
#-to spread data out more evenly
#-to make data distributions more symmetrical
#-to make relationships between variables more linear
#-to make data more constant in variance (homoscedastic)

#Moderately positive skewness: x' = sqrt(x)
#Substantially positive skewness: x' = log(x) 
#Substantially postive skewness with 0: x'=log(x+c) where c is usually 1
#Moderately negative skewness: x'= sqrt(c-x)
#Substantially negative skewness: x'=log(c-x)
#x'=sqrt(x+0.5) used for data taken from Poisson dist., or for right-skewed data that includes values very small or zero.
#x'=arcsin(sqrt(x)) used for data that are proportiuons or converts the binomial distribution, also consider logit transformation

logZn <- log(meuse$zinc)
shapiro.test(logZn)

hist(logZn)
meuse$logZn <- logZn

spplot(meuse, "logZn")
bubble(meuse, "logZn")

#Explore spatial autocorrelation
#make scatter plots of pairs of Z(Si) and Z(Sj) grouped according to their
#separation hij= ||si-sj||
hscat(logZn ~ 1, meuse, (0:9)*100)

#Variogram
var1 <- variogram(logZn ~ 1, meuse)
plot(var1)

#cutoff, lag width, and direction

plot(variogram(logZn ~ 1, meuse, alpha=c(0,45,90,135)))
plot(variogram(logZn ~ 1, meuse, cutoff=1000, width=50))

#variogram modeling
var1.fit <- fit.variogram(var1, vgm(0.8,"Sph",800,0.1))
var1.fit
attr(var1.fit, "SSErr")

# We also talked about different fitting methods, nested structures, and how
# to use geoR to fit variograms
# Gaussian model add a nugget

#anisotropy 
v.dir <- variogram(logZn ~ 1, meuse, alpha=c(45,135))
v.anis <- vgm(0.6,"Sph",1600,0.05, anis=c(45,0.3))
plot(v.dir,v.anis)

data(meuse.grid)
coordinates(meuse.grid) <- ~ x+y

# the new grid can also be created using expand
#new.grid <- expand.grid(x = seq(minX, maxX, intervalX), y=seq(minY,maxY,intervalY))
#gridded(new.grid) <- True
#coordinates(new.grid) <- ~ x+y

#simple kriging
lz.sk <- krige(logZn ~ 1, meuse, meuse.grid, var1.fit, beta=5.9)
spplot(lz.sk["var1.pred"])
spplot(lz.sk["var1.var"])

#ordinary kriging
lz.ok <- krige(logZn ~ 1, meuse, meuse.grid, var1.fit)

#universal kriging
var2 <- variogram(logZn ~ sqrt(dist), meuse)
plot(var2)
var2.fit <- fit.variogram(var2, vgm(0.2,"Exp",300,0.05))
plot(var2, var2.fit)
lz.uk <- krige(logZn ~ sqrt(dist), meuse, meuse.grid, var2.fit)

#block kriging
lz.bk <- krige(logZn ~ 1, meuse, meuse.grid, var1.fit, block =c(40,40))

# simulation for quantifying uncertainties
lzn.sim <- krige(logZn ~ 1, meuse, meuse.grid, var1.fit, nsim=6, nmax=40)
spplot(lzn.sim)

#model diagnostics cross validation
length(meuse$logZn)

lz_cv <- krige.cv(logZn ~ 1, meuse, var1.fit, nfold=155)
bubble(lz_cv, "residual")
summary(lz_cv$residual)
names(lz_cv)

plot(meuse$logZn , lz_cv$var1.pred)
abline(0,1)
cor(meuse$logZn , lz_cv$var1.pred)

res <- lz_cv$residual

res_std <- (res-mean(res))/sd(res)

qqnorm(res_std)
abline(0,1)

removal <- sample(1:155, 5)
subset1 <- meuse[-removal,] 
subset2 <- meuse[removal,]

var3 <- variogram(logZn ~ 1, subset1)
plot(var3)
vgm3 <- vgm(0.5,"Sph",700,0.05)

var3.fit <- fit.variogram(var3, vgm3)
subset2.pred <- krige(logZn ~ 1, subset1, subset2, var3.fit)

subset2.res <- subset2$logZn - subset2.pred$var1.pred