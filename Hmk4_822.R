library(sp)
library(gstat)
library(purrr)
library(dplyr)
library(tidyr)
library(nlme)
library(emmeans)
library(lme4)

nin=read.table('Lab4.nin.spatial.dat.txt',head=TRUE)
attach(nin)
#nin$row=nin$x-3000
#nin$col=nin$y-3000;
nin$trt = nin$entry 
nin$trt = factor(nin$trt)
nin$iblk=factor(nin$iblk)
nin$rep=factor(nin$rep)
nin$yield=as.numeric(nin$yield);
## get variogram
nin_sp <- filter(nin, !is.na(yield));
coordinates(nin_sp) <- ~ col + row ;
vario1 = variogram(yield ~ iblk + trt, data = nin_sp)
plot(vario1)

nin$row <- nin$row + runif(nrow(nin), min = 5, max = 12)
nin$col <- nin$col + runif(nrow(nin), min = 5, max = 12)
## spherical fit with lme
fit.sph = lme(yield ~ trt, random = ~ 1|iblk, data = nin,
              corr = corSpatial(form = ~ row + col, type ="spherical", nugget = F), method = "ML")
anova(fit.sph)
summary(fit.sph)
emm.sph=emmeans(fit.sph,"trt")
## exponential fit with lme
fit.exp = lme(yield ~ trt, random=~1|iblk, data = nin,
              corr = corSpatial(form = ~row + col, type ="exponential", nugget = F), method = "ML")
anova(fit.exp)
summary(fit.exp)
emm.exp=emmeans(fit.exp,"trt")
## Gaussian fit with lme
fit.gau = lme(yield ~ trt, random=~1|iblk, data = nin,
              corr = corSpatial(form = ~row + col, type ="gaussian", nugget = F), method = "ML")
anova(fit.gau)
summary(fit.gau)
emm.gau=emmeans(fit.gau,"trt")






## Question number three
library(lme4)
library(car)
library(lmerTest)
library(emmeans)
library(reshape2)

pigs=read.csv('pig.wgts.corrected.csv')
##using reshape2 package from wide to long;
pigt=melt(pigs, id.vars=c("Anim","Sex"),measure.vars=c("Wk0", "wk4", "wk8", "wk12",
                                                       "wk16", "wk20"),variable.name="week",value.name="wgt")
dim(pigt);
pigt$week=ordered(factor(pigt$week))
pigt$Anim=factor(pigt$Anim)
pigt$Sex=factor(pigt$Sex)
interaction.plot(pigt$week,pigt$Sex, pigt$wgt, fun = mean)

lmer1 <- lmer( wgt ~ Sex + week + Sex*week + (1 | Anim), data=pigt)
summary(lmer1)

con <- list( week_lin =c(-5, -3, -1, 1, 3, 5, -5, -3, -1, 1, 3, 5),
             week_quad =c( 5, -1, -4, -4, -1, 5, 5, -1, -4, -4, -1, 5),
             weekLxsex =c(-5, -3, -1, 1, 3, 5, 5, 3, 1, -1, -3, -5),
             weekQxsex_ =c( 5, -1, -4, -4, -1, 5, -5, 1, 4, 4, 1, -5) )


## Question 3b
options(contrasts=c("contr.sum","contr.poly"));
res0=aov(wgt ~ Sex + week + Sex*week,data=pigt)
anova(res0)
Anova(res0,type="III")
res1=lmer(wgt ~ Sex + week + Sex*week + (1 | Anim)+(1|Anim:Sex),data=pigt);
print(VarCorr(res1),comp="Variance")
anova(res1,ddf = c("Kenward-Roger"))
emmeans(res1,"Sex")
rr1=ranef(res1)
rr1
res2=lmer(wgt ~ Sex + week + Sex*week + (1 | Anim),data=pigt)
anova(res2,res1)

