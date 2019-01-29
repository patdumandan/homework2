curve(10*x*exp(-2*x),from=0,to=5)

x=seq(from=0,to=5,length=1000)

sets=rpois(1000,lambda=4)+1

y=rbinom(1000,prob=plogis(10*x*exp(-2*x)),size=sets)

y_m<-y/sets

plot(y_m~x,xlab="Detritus",ylab="Emergent adults",cex.lab=2)

-sum(dbinom(y,prob=plogis(10*x*exp(-2*x)),size=sets,log=T))
-sum(dbinom(y,prob=plogis(1.44-.19*x-0.21*x^2+0.04*x^3),size=sets,log=T))


sum(dbinom(y,prob=plogis(-50*x*exp(50*x)),size=sets),log=T)

recs<-cbind(y,sets-y)
# 
# (Intercept)           x      I(x^2)      I(x^3) 
# 1.38406170 -0.01199389 -0.28138336  0.04728792

plot(y_m~x,xlab="Detritus",ylab="Emergent adults",cex.lab=1.3)
curve(plogis(10*x*exp(-2*x)),col="green",add=T,lwd=8)
curve(plogis(1.38-.01199389*x+-0.28*x^2+0.05*x^3),add=T,col="blue",lwd=8)

mosquito_data<-data.frame("Emergent_adults"=y,Egg_Count=sets,Detritus=round(x,2))

save(mosquito_data,file="mosquito_data.csv")

m1<-glm(response~predictor,family=binomial)

x=runif(200)*100
y=rpois(n=200,lambda=exp(2+-2e-02*x))

plot(y~x,pch=21,bg="firebrick",cex=1.3,xlab="Elevation",
     ylab="Safe zones for firefighters")

curve(exp(2+-2e-02*x),add=T,lwd=5)
