library(sp)
library(gstat)
data(meuse)
names(meuse)
coordinates(meuse) <- ~x+y
v <- variogram(log(zinc) ~ 1, meuse, alpha=c(45, 135))
vm = vgm(.25, "Sph", 1000, anis = c(45, 0.5)) 
plot(v, vm)
zonal = vgm(.5, "Sph", 1e9, anis = c(45, 1/1e6)) 
vm = vgm(.5, "Sph", 1000, add.to = zonal) 
plot(v, vm)

