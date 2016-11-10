setwd("F:/Wu/geostat2016Fall/Class9_11102016/")
all <- read.csv("barn.csv", header=TRUE)
library(gstat)
library(sp)
coordinates(all) <- ~ East+North
var1 <- variogram(pH ~ 1, all)
plot(var1)
vgm1 <- vgm(0.35, "Exp", 2.5, 0.05)
plot(var1, vgm1)
var1.fit1 <- fit.variogram(var1, vgm1)
data.grid <- expand.grid(x=seq(8.5,8.5,0), y=seq(10.5,10.5,0))
names(data.grid) <- c("X", "Y")
coordinates(data.grid) <- ~X+Y
krig1_ok <- krige(pH ~ 1, all, data.grid, model=var1.fit1)
krig1_ok

#compare variances between OK and BK
krig1_block <- krige(pH ~ 1, all, data.grid, model=var1.fit1, 
block=c(2,2), set=list(nblockdiscr=4))
krig1_block

# the effect of block size
krig1_block2 <- krige(pH ~ 1, all, data.grid, model=var1.fit1,
block=c(3,3), set=list(nblockdiscr=4))
krig1_block2

#change discretization 
krig1_block3 <- krige(pH ~ 1, all, data.grid, model=var1.fit1,
block=c(3,3), set=list(nblockdiscr=6))
krig1_block3

#Again the effect of block size
krig1_block4 <- krige(pH ~ 1, all, data.grid, model=var1.fit1,
 block=c(7,7), set=list(nblockdiscr=4))
krig1_block4

krig1_block5 <- krige(pH ~ 1, all, data.grid, model=var1.fit1,
block=c(8,8), set=list(nblockdiscr=4))
krig1_block5

#new grid
data.grid2 <- expand.grid(x=seq(7.75,9.25,0.5),y=seq(9.75,11.25,0.5))
coordinates(data.grid2) <- ~x+y
gridded(data.grid2) <- T  #added in class
krig_grid2 <- krige(pH ~ 1, all, data.grid2, model=var1.fit1,
block=c(2,2),set=list(nblockdiscr=4))

spplot(krig_grid2["var1.pred"])

# block kriging is the average of point estimates
krig1 <- krige(pH ~ 1, all, data.grid, model=var1.fit1, block=c(2,2), set=list(nblockdiscr=2))
krig1

krig1 <- krige(pH ~ 1, all, data.grid, model=var1.fit1, block=c(2,2), set=list(nblockdiscr=3))
krig1

data.grid3 <- expand.grid(x=seq(7.833,9.167,0.667),y=seq(9.833,11.167,0.667))
coordinates(data.grid3) <- ~x+y
krig1 <- krige(pH ~ 1, all, data.grid3,model=var1.fit1)
krig1
summary(krig1)

krig1 <- krige(pH ~ 1, all, data.grid, model=var1.fit1, block=c(2,2), set=list(nblockdiscr=4))
krig1

data.grid4 <- expand.grid(x=seq(7.75,9.25,0.5),y=seq(9.75,11.25,0.5))
coordinates(data.grid4) <- ~x+y
krig1 <- krige(pH ~ 1, all, data.grid4, model=var1.fit1)
krig1
summary(krig1)

# indicator kriging
library(sp)
library(gstat)
rm(list=ls()) # remove all the variables
setwd("F:/Wu/geostat2016Fall/Class9_11102016/")
all <- read.csv("barn.csv", header=TRUE)
attach(all)

data.grid <- expand.grid(x=seq(8.5,8.5,0), y=seq(10.5,10.5,0))
names(data.grid) <- c("X", "Y")
coordinates(data.grid) <- ~X+Y

P_ind <- ifelse(P <= 1.6, 1,0)
P_ind_df <- c(East, North, P_ind)
dim(P_ind_df) <- c(length(P_ind),3)
P_ind_df <- as.data.frame(P_ind_df)
names(P_ind_df) <- c("x", "y", "P_ind")
coordinates(P_ind_df) <- ~ x+y
var2 <- variogram(P_ind ~ 1, P_ind_df)
plot(var2)
vgm2 <- vgm(0.08, "Exp", 1, 0.05)
var2.fit <- fit.variogram(var2, vgm2)
var2.fit
plot(var2, var2.fit)
krig3 <- krige(P_ind ~ 1, P_ind_df, data.grid, model=var2.fit)
krig3

P_ind <- ifelse(P <= 2.3, 1,0)
P_ind_df <- c(East, North, P_ind)
dim(P_ind_df) <- c(length(P_ind),3)
P_ind_df <- as.data.frame(P_ind_df)
names(P_ind_df) <- c("x", "y", "P_ind")
coordinates(P_ind_df) <- ~ x+y
var2 <- variogram(P_ind ~ 1, P_ind_df)
plot(var2)
vgm2 <- vgm(0.08, "Exp", 1, 0.05)
var2.fit <- fit.variogram(var2, vgm2)
var2.fit
plot(var2, var2.fit)
krig3 <- krige(P_ind ~ 1, P_ind_df, data.grid, model=var2.fit)
krig3

P_ind <- ifelse(P <= 2.9, 1,0)
P_ind_df <- c(East, North, P_ind)
dim(P_ind_df) <- c(length(P_ind),3)
P_ind_df <- as.data.frame(P_ind_df)
names(P_ind_df) <- c("x", "y", "P_ind")
coordinates(P_ind_df) <- ~ x+y
var2 <- variogram(P_ind ~ 1, P_ind_df)
plot(var2)
vgm2 <- vgm(0.08, "Exp", 1, 0.05)
var2.fit <- fit.variogram(var2, vgm2)
var2.fit
plot(var2, var2.fit)
krig3 <- krige(P_ind ~ 1, P_ind_df, data.grid, model=var2.fit)
krig3

P_ind <- ifelse(P <= 3.5, 1,0)
P_ind_df <- c(East, North, P_ind)
dim(P_ind_df) <- c(length(P_ind),3)
P_ind_df <- as.data.frame(P_ind_df)
names(P_ind_df) <- c("x", "y", "P_ind")
coordinates(P_ind_df) <- ~ x+y
var2 <- variogram(P_ind ~ 1, P_ind_df)
plot(var2)
vgm2 <- vgm(0.08, "Exp", 1, 0.05)
var2.fit <- fit.variogram(var2, vgm2)
var2.fit
plot(var2, var2.fit)
krig3 <- krige(P_ind ~ 1, P_ind_df, data.grid, model=var2.fit)
krig3

P_ind <- ifelse(P <= 4.3, 1,0)
P_ind_df <- c(East, North, P_ind)
dim(P_ind_df) <- c(length(P_ind),3)
P_ind_df <- as.data.frame(P_ind_df)
names(P_ind_df) <- c("x", "y", "P_ind")
coordinates(P_ind_df) <- ~ x+y
var2 <- variogram(P_ind ~ 1, P_ind_df)
plot(var2)
vgm2 <- vgm(0.08, "Exp", 1, 0.05)
var2.fit <- fit.variogram(var2, vgm2)
var2.fit
plot(var2, var2.fit)
krig3 <- krige(P_ind ~ 1, P_ind_df, data.grid, model=var2.fit)
krig3

P_ind <- ifelse(P <= 5.2, 1,0)
P_ind_df <- c(East, North, P_ind)
dim(P_ind_df) <- c(length(P_ind),3)
P_ind_df <- as.data.frame(P_ind_df)
names(P_ind_df) <- c("x", "y", "P_ind")
coordinates(P_ind_df) <- ~ x+y
var2 <- variogram(P_ind ~ 1, P_ind_df)
plot(var2)
vgm2 <- vgm(0.08, "Exp", 1, 0.05)
var2.fit <- fit.variogram(var2, vgm2)
var2.fit
plot(var2, var2.fit)
krig3 <- krige(P_ind ~ 1, P_ind_df, data.grid, model=var2.fit)
krig3

P_ind <- ifelse(P <= 7.23, 1,0)
P_ind_df <- c(East, North, P_ind)
dim(P_ind_df) <- c(length(P_ind),3)
P_ind_df <- as.data.frame(P_ind_df)
names(P_ind_df) <- c("x", "y", "P_ind")
coordinates(P_ind_df) <- ~ x+y
var2 <- variogram(P_ind ~ 1, P_ind_df)
plot(var2)
vgm2 <- vgm(0.08, "Exp", 1, 0.05)
var2.fit <- fit.variogram(var2, vgm2)
var2.fit
plot(var2, var2.fit)
krig3 <- krige(P_ind ~ 1, P_ind_df, data.grid, model=var2.fit)
krig3

x <- c(1.6,2.3,2.9,3.5)
y <- c(0.2421871,0.7317228,0.9740367,0.975735)
xy <- c(x,y)
dim(xy) <- c(4,2)
plot(xy, type="l")