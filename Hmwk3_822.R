## pig weight rep.meas.example with LR test for cov. structure;
library(reshape2)
library(car)
library(lme4)
library(lmerTest)
library(emmeans)
library(nlme)
library(mmrm)


dogs <- data.frame(dog=c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,
                         5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8),
                   trt=c(1,1,1,1,2,2,2,2,3,3,3,3,1,1,1,1,2,2,2,2,3,3,3,3,1,1,1,1,2,2,2,2,3,3,3,3,1,1,1,1,2,2,2,2,3,3,3,3,
                         1,1,1,1,2,2,2,2,3,3,3,3,1,1,1,1,2,2,2,2,3,3,3,3,1,1,1,1,2,2,2,2,3,3,3,3,1,1,1,1,2,2,2,2,3,3,3,3),
                   time=c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,
                          1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4),
                   y=c(72,80,81,77,85,86,83,80,69,73,72,74,78,83,88,81,82,86,80,84,66,62,67,73,71,82,81,75,71,70,70,75,84,
                       90,88,87,72,83,83,69,83,88,79,81,80,81,77,72,66,79,77,79,86,85,76,76,72,72,69,70,74,83,84,77,85,82,
                       83,80,65,62,65,61,62,73,78,70,79,83,80,81,75,71,69,68,69,75,76,70,83,84,78,81,71,70,67,65))

dim(dogs)

dogs$time=ordered(factor(dogs$time))
dogs$dog=factor(dogs$dog)
dogs$trt=factor(dogs$trt)
interaction.plot(dogs$time,dogs$trt, dogs$y, fun = mean)
con <- list( time_lin =c(-3, -1, 1, 3, -3, -1, 1, 3),
             time_quad =c(1, -1, -1, 1,1, -1, -1, 1),
             timeLxtrt =c(-3,-1,1,3,1.5,0.5,-0.5,-1.5),
             timeQxtrt =c(1,-1,-1,1,-0.5,0.5,0.5,-0.5) )

### fitting with mmrm
#install.packages("lme4", type = "source")
## CS
lmer1 <- lmer( y ~ trt + time + trt*time + (1 | dog), data=dogs)
emmeans(lmer1,"trt")
mmrm.cs=mmrm( y ~ trt + time + trt*time + cs( time | dog), data=dogs)
cs.emm=emmeans(mmrm.cs,"trt")
emmeans(mmrm.cs, list(~ time+trt), contr = con)

## CSH
mmrm.csh=mmrm( y ~ trt + time + trt*time + csh( time | dog), data=dogs)
csh.emm=emmeans(mmrm.csh,"trt")
emmeans(mmrm.csh, list(~ time+trt), contr = con)

## AR1
mmrm.ar1=mmrm(y ~ trt + time + trt*time + ar1( time | dog), data=dogs)
emmeans(mmrm.ar1,"trt")
emmeans(mmrm.ar1, list(~ time+trt), contr = con)

## ARH1
mmrm.ar1h=mmrm( y ~ trt + time + trt*time + ar1h( time | dog), data=dogs)
emmeans(mmrm.ar1h,"trt")
emmeans(mmrm.ar1h, list(~ time+trt), contr = con)

## ANTE1
mmrm.ant1=mmrm( y ~ trt + time + trt*time + ad( time | dog), data=dogs)
emmeans(mmrm.ant1,"trt")
emmeans(mmrm.ant1, list(~ time+trt), contr = con)

## US - Unstructured
mmrm.us=mmrm( y ~ trt + time + trt*time + us( time | dog), data=dogs)
emmeans(mmrm.us,"trt")
emmeans(mmrm.us, list(~ time+trt), contr = con)

# ## toeplitz
# mmrm.toep=mmrm( wgt ~ Sex + week + Sex*week + toep( week | Anim), data=pigt)
# emmeans(mmrm.toep,"Sex")
# emmeans(mmrm.toep, list(~ week+Sex), contr = con)

