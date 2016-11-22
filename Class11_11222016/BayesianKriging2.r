library(geoR)
set.seed(345)
data(s100)
summary(s100)
plot(s100)
points(s100)
plot(variog(s100, max.dist=1.1))

# Use of krige.bayes() function
args(krige.bayes)
args(model.control)
args(prior.control)
args(output.control)

MC <- model.control()
PC <- prior.control(phi.discrete=seq(0,1,l=11))
OC <- output.control(n.pos=500, n.pred=100,quantile=c(.25,.5, .75), thres=1.5)

gp <- expand.grid(seq(0,1,l=30), seq(0,1,l=30))
s100.kb <- krige.bayes(s100, loc=gp, model=MC, prior=PC, out=OC)
names(s100.kb)

#examine the posteriori:
names(s100.kb$posterior)
s100.kb$posterior$beta
s100.kb$posterior$sigmasq
s100.kb$posterior$phi
s100.kb$posterior$samples
#examine the predictive distribution
names(s100.kb$predictive)

image(s100.kb,main="Bayes predictive mean")
contour(s100.kb,add=TRUE,levels=seq(0, 3, by=.5))
points(s100,add=T,pch="+")

# plot the prediction errors
image(s100.kb, val=sqrt(s100.kb$pred$variance),
 coords.data=s100$coords,main="Bayes predictive variance")

#examine and plot the quantiles
s100.kb$pred$quant[1:5,]
#plot the 25% & 75% quartile
image(s100.kb, val=s100.kb$pred$quant[,1],main="plot 25% quartile")
contour(s100.kb,val=s100.kb$pred$quant[,1],add=TRUE,levels=seq(0, 3, by=.5))
image(s100.kb, val=s100.kb$pred$quant[,3],main="plot 75% quartile")
contour(s100.kb,val=s100.kb$pred$quant[,3],add=TRUE,levels=seq(0, 3, by=.5))

#plot the probability P[S < 1.5]
image(s100.kb, val=s100.kb$pred$prob,main="plot the probability P[S < 1.5]")
contour(s100.kb,val=s100.kb$pred$prob,add=TRUE,levels=seq(0, 1, by=.25))

#bsp4 <- krige.bayes(s100, loc=loci, prior=prior.control(phi.discrete=seq(0,5,l=101),phi.prior="rec"), output=output.control(n.post=5000))

