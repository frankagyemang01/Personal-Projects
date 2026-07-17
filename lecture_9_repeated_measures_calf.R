## Lecture 9 - repeated Measures Calf Example
# Load required packages
# Load necessary package
library(tidyr)
library(dplyr)
library(nlme)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)

# Create the data in wide format
calf <- tribble(
  ~calf, ~trt, ~time2, ~time4, ~time6, ~time8, ~time10,
  1, 1, 245, 271, 287, 287, 293,
  2, 1, 260, 290, 300, 313, 321,
  3, 1, 245, 285, 298, 319, 334,
  4, 1, 268, 308, 309, 324, 336,
  5, 1, 239, 282, 299, 321, 332,
  6, 1, 242, 263, 277, 299, 308,
  7, 1, 246, 279, 292, 299, 300,
  8, 1, 270, 302, 321, 334, 337,
  9, 1, 243, 272, 276, 289, 300,
  10, 1, 247, 275, 286, 302, 319,
  1, 2, 230, 259, 266, 292, 290,
  2, 2, 258, 277, 293, 323, 340,
  3, 2, 248, 297, 313, 340, 365,
  4, 2, 253, 292, 310, 333, 353,
  5, 2, 262, 300, 314, 331, 348,
  6, 2, 237, 271, 288, 316, 333,
  7, 2, 239, 268, 290, 313, 318,
  8, 2, 255, 293, 307, 336, 344,
  9, 2, 234, 256, 266, 300, 293,
  10, 2, 259, 294, 313, 247, 362
)

calf_long <- calf %>%
  pivot_longer(
    cols = starts_with("time"),  # Select columns to pivot (those starting with "time")
    names_to = "time",           # Name of the new column to store the original column names
    values_to = "weight"          # Name of the new column to store the values
  )

calf_long$calf <- as.factor(calf_long$calf)
calf_long$trt <- as.factor(calf_long$trt)
calf_long$time <- ordered(factor(calf_long$time))

############################################################################################################
# Fit the mixed model using Satherweith
model.calf.1 <- lmer(weight ~ trt * time + (1 | calf:trt), data = calf_long)

# Summary of fixed effects with Satterthwaite df (like ddfm=satterth in SAS)
summary(model.calf.1)

# Type III tests of fixed effects
anova(model.calf.1, type = 3)
interaction.plot(calf_long$time,calf_long$trt,calf_long$weight)
#########################################################################################################
#Fit the repeated measures model with compound symmetry
model.calf.cs <- gls(weight ~ trt * time, data = calf_long,
    correlation = corCompSymm(form = ~ 1 | calf))
getVarCov(model.calf.cs)
# Summary of fixed effects with Satterthwaite df (like ddfm=satterth in SAS)
summary(model.calf.cs)

# Type III tests of fixed effects
anova(model.calf.cs, type = 3)

##########################################################################################################
# Fit repeated measures model with unstructured covariance
model.calf.un <- gls( weight ~ trt * time,
                      data = calf_long,
                      correlation = corSymm(form = ~ 1 | calf),  
                      weights = varIdent(form = ~1 | time)
)
getVarCov(model.calf.un, individual = 3)
summary(model.calf.un)
Anova(model.calf.un, test="F", type="III")
####################################################################################################3
model.calf.ar1 <- gls(weight ~ trt * time, 
                      corr=corAR1(form=~1|calf),
                      data= calf_long )
getVarCov(model.calf.ar1, individual = 3)
Anova(model.calf.ar1, test="F", type="III") 
summary(model.calf.ar1)
####################################################################################################
# Fit model with Toeplitz correlation structure: Results not available
library(nlme)
model.calf.toeplitz <- gls(
  weight ~ trt * time, 
  correlation = corToeplitz(form = ~ 1 | calf),
  data = calf_long,
  method = "REML" # Restricted Maximum Likelihood is generally preferred for mixed models
)







