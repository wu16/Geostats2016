# nested structure

library(gstat)
library(sp)
data(jura)
coordinates(jura.val) <- ~Xloc + Yloc

var1 <- variogram(Ni~1, jura.val,width=0.15)

plot(var1)

vgm1 <- vgm(50, "Exp", 0.2,0)
vgm2 <- vgm(20, "Exp", 1.0, 0, add.to=vgm1)

var1.fit <- fit.variogram(var1,vgm2,fit.sill=FALSE)

plot(var1, var1.fit)


# anisotropy 
data(walker)
#coordinates(walker) <- ~ X+Y

var4 <- variogram(V ~ 1, walker, alpha=76, tol.hor=45)
var5 <- variogram(V ~ 1, walker, alpha=166, tol.hor=45)

var <- variogram(V ~ 1, walker, alpha=c(76,166), tol.hor=45)

v.anis <- vgm(60000, "Sph", 80, 40000, anis=c(166, 0.5))

plot(var, v.anis) 

v.fit <- fit.variogram(var, v.anis)

# indicator variogram model
jura_val <- as.data.frame(jura.val)
attach(jura_val)
Ni_ind <- ifelse(Ni>20,1,0)

new <- c(Xloc, Yloc, Ni_ind)
dim(new) <- c(length(Ni_ind),3)
new <- as.data.frame(new)
names(new) <- c("Xloc", "Yloc", "Ni")
coordinates(new) <- ~Xloc + Yloc

var2 <- variogram(Ni~1, new,width=0.15)

vgm1 <- vgm(0.2, "Exp", 0.1, 0)
vgm2 <- vgm(0.1, "Exp", 0.5, 0, add.to=vgm1)

var2.fit <- fit.variogram(var2,vgm2,fit.sill=FALSE)

plot(var2, var2.fit)

vgm1 <- vgm(0.15, "Exp", 0.2,0.05)
#vgm2 <- vgm(0.05, "Exp", 1.0, 0, add.to=vgm1)

var2.fit <- fit.variogram(var2,vgm1)

plot(var2, var2.fit)



