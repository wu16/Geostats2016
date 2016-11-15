# long code for lecture 10 Nov. 15, 2016
library(gstat)
library(sp)

#Simple kriging
var1 <- variogram(log(zinc) ~ 1, meuse)
plot(var1)
vgm1 <- vgm(0.6, "Sph", 700, 0.05)
var1.fit <- fit.variogram(var1, vgm1)
plot(var1, var1.fit)
data(meuse.grid)
coordinates(meuse.grid) <- ~ x+y
simple1 <- krige(log(zinc) ~ 1, meuse, meuse.grid, var1.fit, beta=5.9)

# universal kriging
data(meuse)
data(meuse.grid)
coordinates(meuse) <- ~x+y
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
var4 <- variogram(log(zinc) ~ sqrt(dist), meuse)
vt.fit <- fit.variogram(var4, vgm(1, "Exp", 300, 1))
lz.uk <- krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid,vt.fit)
spplot(lz.uk["var1.pred"])

#Multivariate variogram modelling
data(meuse)
shapiro.test(meuse$cadmium)
shapiro.test(log(meuse$cadmium))

coordinates(meuse) <- ~ x+y

#plot.new()
#par(mfrow=c(2,2))
bubble(meuse, "cadmium")
bubble(meuse, "copper")
bubble(meuse, "lead")
bubble(meuse, "zinc")


g <- gstat(NULL, "logCd", log(cadmium) ~ 1, meuse)
g <- gstat(g, id="logCu", form=log(copper) ~ 1, data=meuse)
g <- gstat(g, "logPb", log(lead) ~ 1, meuse)
g <- gstat(g, "logZn", log(zinc) ~ 1, meuse)
g

vm <- variogram(g)
vm.fit <- fit.lmc(vm, g, vgm(1, "Sph", 800, 1))
plot(vm, vm.fit)

#cokriging
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
cok.maps <- predict(vm.fit, meuse.grid)

#stratified kriging

library(gstat)
library(sp)
data(meuse)
data(meuse.grid)
coordinates(meuse) <- ~x+y
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE

meuse$part.a <- idw(part.a ~ 1, meuse.grid, meuse, nmax=1)$var1.pred

var1 <- variogram(log(zinc) ~ 1, meuse[meuse$part.a==0,])
plot(var1)
vgm1 <- vgm(0.5, "Gau", 400, 0.1)
var1.fit <- fit.variogram(var1, vgm1)

var2 <- variogram(log(zinc) ~ 1, meuse[meuse$part.a==1,])
plot(var2)
vgm2 <- vgm(0.6, "Sph", 600)
var2.fit <- fit.variogram(var2, vgm2)

x1 <- krige(log(zinc) ~ 1, meuse[meuse$part.a==0,],
 meuse.grid[meuse.grid$part.a==0,], model=var1.fit,
 nmin=20, nmax=40, maxdist=1400)

x2 <- krige(log(zinc) ~ 1, meuse[meuse$part.a==1,],
 meuse.grid[meuse.grid$part.a==1,], model=var2.fit,
 nmin=20, nmax=40, maxdist=1000)

lz.stk <- rbind(as.data.frame(x1),as.data.frame(x2))
coordinates(lz.stk) <- ~x+y
spplot(lz.stk["var1.pred"])

# cross validation
library(gstat)
library(sp)
data(meuse)
coordinates(meuse) <- ~x+y
sel100 <- sample(1:155,100)
m.model <- meuse[sel100,]
m.valid <- meuse[-sel100,]

var.m <- variogram(log(zinc) ~ 1, m.model)
plot(var.m)
vgm.m <- vgm(0.5, "Sph", 700, 0.1)
var.mfit <- fit.variogram(var.m, vgm.m)
plot(var.m, var.mfit)
var.mfit
m.valid.pred <- krige(log(zinc) ~ 1, m.model, m.valid, model=var.mfit)

resid.kr <- log(m.valid$zinc) - m.valid.pred$var1.pred
summary(resid.kr)

resid.mean <- log(m.valid$zinc) - mean(log(m.valid$zinc))
R2 <- 1-sum(resid.kr^2) / sum(resid.mean^2)

m.valid.pred$res <- resid.kr
bubble(m.valid.pred, "res")

#To use the data to a fuller extent (leave-one-out cross validation nfold=155)
nfold <- 3
part <- sample(1:nfold, 155, replace=TRUE)

for (i in 1: nfold) {
   sel <- (part != i)
   m.model <- meuse[sel,]
   m.validate <- meuse[-sel,]
   # variogram modeling, kriging, residual
}

#a more automated way
v.fit <- vgm(0.59, "Sph", 897, 0.05)
cv155 <- krige.cv(log(zinc) ~ 1, meuse, v.fit, nfold=5)
bubble(cv155, "residual")
summary(cv155)

#stochastic simulation
v <- variogram(log(zinc) ~ 1, meuse)
v.fit <- fit.variogram(v, vgm(0.6, "Sph", 1000, 0.1))
lzn.sim <- krige(log(zinc) ~ 1, meuse, meuse.grid, v.fit, nsim = 6, nmax = 40)
spplot(lzn.sim)




