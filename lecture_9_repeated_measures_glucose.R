# Load required packages
library(nlme)
library(car)

# Load the data
glucose <- data.frame(
  diet = factor(rep(1:3, each=12)),
  rep = rep(rep(1:4, each=3), 3),
  subject = factor(rep(1:12, each=3)),
  time = rep(c(15, 30, 45), 12),
  glucose = c(
    28, 34, 32, 15, 29, 27, 12, 33, 28, 21, 44, 39,
    22, 18, 12, 23, 22, 10, 18, 16, 9, 25, 24, 15,
    31, 30, 39, 28, 27, 36, 24, 26, 36, 21, 26, 32
  )
)

# Create transformed time variables
glucose$thr <- glucose$time / 60
glucose$th2 <- glucose$thr^2
###########################################################################################################
# Fit the mixed model using Compund Symmetry (produces same results as Satterweith)
model.glucose.1 <- lmer(glucose ~ diet * thr + th2 + diet:th2 + (1 | subject), 
                        data = glucose)

# Summary of fixed effects with Satterthwaite df (like ddfm=satterth in SAS)
summary(model.glucose.1)

# Type III tests of fixed effects
anova(model.glucose.1, type = 3)

############################################################################################################
## Unstructured
library(nlme)

model.calf.un <- gls( glucose ~ diet * thr + th2 + diet:th2,
                      data = glucose,
                      correlation = corSymm(form = ~ 1 | subject),  
                      weights = varIdent(form = ~1 | time)
)
getVarCov(model.calf.un, individual = 3)
summary(model.calf.un)
Anova(model.calf.un, test="F", type="III")
############################################################################################################
model.calf.ar1 <- gls(glucose ~ diet * thr + th2 + diet:th2, 
                      corr=corAR1(form=~1|subject),
                      data= glucose )
getVarCov(model.calf.ar1, individual = 3)
Anova(model.calf.ar1, test="F", type="III") 
summary(model.calf.ar1)
###########################################################################################################

model.calf.ante <- gls(glucose ~ diet * thr + th2 + diet:th2, 
                      corr=corAR1(form=~1|subject),
                      data= glucose )
getVarCov(model.calf.ar1, individual = 3)
Anova(model.calf.ar1, test="F", type="III") 
summary(model.calf.ar1)
##########################################################################################################
library(nlme)
library(emmeans)
library(car)

# Define orthogonal polynomial contrasts
glucose$diet <- factor(glucose$diet)
contrasts(glucose$diet) <- contr.poly(3)  # Linear and quadratic

# Fit the mixed model with CS covariance and no intercept
model.cs <- lme(
  fixed = glucose ~ 0 + diet,  # no intercept (same as noint in SAS)
  random = ~1 | subject,
  correlation = corCompSymm(form = ~ 1 | subject),
  data = glucose,
  method = "REML"
)

summary(model.cs)
###########################################################################################################
