## BOOSTING ##

set.seed(1234)
x=seq(0,pi,length=200)
Y=sin(2*x)+rnorm(200,0,0.2)
df=data.frame(x=x,Y=Y)
plot(x,Y)

grilleKmax <- c(2,10,100,1000,10000,100000)
for (ii in grilleKmax){
  m1 <- ibr(Y~x, data=df, df=1.1,Kmin=1,smoother="k",Kmax=ii) 
  prev <- predict(m1,df)
  lines(df$x, prev, col=1,lwd=2)
  stop(0.03)
}