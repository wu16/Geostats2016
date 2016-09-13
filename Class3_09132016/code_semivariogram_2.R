library(sp)
data(meuse)
?data(meuse)
data(package="sp")

summary(meuse)
names(meuse)
histogran(meuse$zinc)
shapiro.test(meuse$zinc)

logzinc <- log(meuse$zinc)
shapiro.test(logzinc)

class(meuse)
coordinates(meuse) <- ~x+y
class(meuse)

spplot(meuse, "zinc")
bubble(meuse, "zinc")
hscat(log(zinc)~1, meuse, c(0, 80, 120, 250, 500, 1000))
variogram(log(zinc)~1, meuse)
var1 <- variogram(log(zinc)~1, meuse, width=10,cutoff=2000)
plot(var1, type="l")

var2 <- variogram(log(zinc)~1, meuse, width=50,cutoff=2000, cloud=T)
plot(var2)

var3 <- variogram(log(zinc)~1, meuse, width=50,cutoff=2000)
plot(var3, type="l")

var4 <- variogram(log(zinc)~1, meuse, width=100,cutoff=2000)
plot(var4,type="l")

var5 <- variogram(log(zinc)~1, meuse, width=500,cutoff=2000)
plot(var5,type="l")


# directional variogram:
var6 <- variogram(log(zinc)~1, meuse, width=100, cutoff=2000, alpha=c(0,45,90,135),tol.hor=10)
plot(var6,type="l")
