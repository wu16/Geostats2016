setwd("f:/Wu/MixedModel/")
Boreality <- read.table("boreality.txt", header=T)
Boreality$Bor<-sqrt(1000*(Boreality$nBor+1)/(Boreality$nTot))
B.lm<-lm(Bor~Wet,data=Boreality)
summary(B.lm)

E<-rstandard(B.lm)
library(gstat)
library(sp)
mydata<-data.frame(E,Boreality$x,Boreality$y)
coordinates(mydata)<-c("Boreality.x","Boreality.y")
bubble(mydata,"E",col=c("black","grey"),
       main="Residuals",xlab="X-coordinates",
       ylab="Y-coordinates")

Vario1 = variogram(E ~ 1, mydata)
plot(Vario1)

Vario2 <- variogram(E ~ 1, mydata, alpha = c(0, 45, 90,135) )
plot(Vario2)


library(nlme)
D<-seq(0,1,by=0.1)
Mydata2<-data.frame(D=D)
cs1C <- corSpher(c(0.8,0.1), form = ~ D,nugget=T)
cs1C <- Initialize(cs1C,data=Mydata2)
v1C<-Variogram(cs1C)
plot(v1C,smooth=F,type="l",col=1)


library(nlme)
f1 <- formula(Bor ~ Wet)
B1.gls<-gls(f1, data = Boreality)
var1<-Variogram(B1.gls,form=~x+y,robust=TRUE,maxDist=2000,
             resType="pearson")
plot(var1,smooth=T)



B1A<-gls(f1,correlation=corSpher(form=~x+y,nugget=T),data=Boreality)
B1B<-gls(f1,correlation=corLin(form=~x+y,nugget=T),data=Boreality)
B1C<-gls(f1,correlation=corRatio(form=~x+y,nugget=T),data=Boreality)
B1D<-gls(f1,correlation=corGaus(form=~x+y,nugget=T),data=Boreality)
B1E<-gls(f1,correlation=corExp(form=~x+y,nugget=T),data=Boreality)
AIC(B1.gls,B1A,B1B,B1C,B1D,B1E)


Vario1E <- Variogram(B1E, form =~ x + y, robust = TRUE, maxDist = 2000,
                 resType = "pearson")
plot(Vario1E,smooth=FALSE)

Vario2E <- Variogram(B1E, form =~ x + y,
                     robust =  TRUE, maxDist = 2000,
                     resType = "normalized")
plot(Vario2E, smooth = FALSE)
