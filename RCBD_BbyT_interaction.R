# Tukey test for nonadditivity
d=read.table('C:/.../tukey.txt',header=TRUE)
d$trt=factor(d$t)
d$blk=factor(d$b) 
interaction.plot(d$blk,d$trt,d$y)
summaryBy(y~trt,data=d,FUN=mean)
result6=aov(y~blk+trt,data=d); summary(result6)
# tukey nonadd test - p335, snedecor & cochran, 6th ed)
d$u=.5*(result6$fitted.values-mean(y))^2
res6a=aov(u~blk+trt,data=d)
d$add=u-res6a$fitted.values
res6b=aov(y~blk+trt+add,data=d)
interaction.plot(d$blk,d$trt,d$y)
summary(res6b)
res6b$coefficients
d$ytr=res6b$fitted.values
interaction.plot(d$blk,d$trt,d$ytr)
# transform per Tukey's suggestion: y^p where p=1-betahat*mean(y);
d$yt=d$y^(1-.0933*mean(d$y))
result7=aov(yt~blk+trt,data=d)
summary(result7)
tmeans=summaryBy(yt~trt,data=d,FUN=mean)
btmeans=exp(log((tmeans$y.tmeans)/(1-.0933*mean(d$y)))) ## wont work; 
d$ytback=exp(log(d$yt)/(1-.0933*mean(d$y)))
interaction.plot(d$blk,d$trt,d$ytback)
 result6=aov(y~blk+trt,data=d) 
 summary(result6)