rm(list=ls())
library(sp)
library(gstat)

data(meuse)

meuse$logZn <- log(meuse$zinc)

coordinates(meuse) <- c("x","y")

var <- variogram(logZn ~ 1, meuse)
plot(var)

vgm(1, "Sph",300)
vgm(1, "Sph",300,0.5)
vgm(0.5,"Nug",0)


? fit.variogram

v.fit <- fit.variogram(var,vgm(1,"Sph",800,1))


fit.variogram(var, vgm(1,"Sph",10,1))
attr(v.fit, "SSErr")

v2.fit <- fit.variogram(var,vgm(1,"Exp",200,1))
attr(v2.fit,"SSErr")

v3.fit <- fit.variogram(var,vgm(1,"Gau",800,1))
attr(v3.fit, "SSErr")

# restricted maximum likelihood fittong of only partial sills, not of ranges (likfit in geoR and fitvario in RandomFields)

fit.variogram.reml(logZn ~ 1,meuse,model=vgm(0.6,"Sph",800,0.06))

v.dir <- variogram(logZn ~ 1, meuse, alpha=(0:3)*45)
v.vgm <- vgm(0.6,"Sph",1600,0.05,anis=c(45,0.3))
plot(v.dir,v.vgm)

plot(variogram(logZn ~ 1, meuse, map=TRUE, cutoff=1000, width=100))


