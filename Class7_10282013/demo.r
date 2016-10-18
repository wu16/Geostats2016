#demo 1 using meuse data to show kriging
library(gstat)
library(sp)
data(meuse)
coordinates(meuse) <- ~x+y
var_zinc <- variogram(log(zinc) ~ 1, meuse)
fit1 <- fit.variogram(var_zinc, vgm(1, "Sph", 800, 1))
fit1
data(meuse.grid)
class(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
krig_zinc <- krige(log(zinc) ~ 1, meuse, meuse.grid, model=fit1)
spplot(krig_zinc["var1.pred"])
spplot(krig_zinc["var1.var"])