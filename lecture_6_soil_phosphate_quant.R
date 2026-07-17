# Load libraries
library(car)
library(dplyr)

# Data input
soil <- data.frame(
  block = factor(rep(1:7, each = 5)),
  phos = rep(c(0, 50, 100, 150, 200), times = 7),
  calcium = c(
    3.51, 3.68, 3.62, 3.75, 3.71,
    5.07, 3.94, 3.97, 4.03, 5.14,
    2.97, 2.86, 2.92, 2.93, 3.60,
    2.68, 2.50, 2.47, 2.45, 3.12,
    6.54, 7.25, 7.27, 7.13, 7.96,
    2.30, 3.51, 3.38, 3.40, 3.40,
    2.06, 2.06, 2.61, 2.06, 2.22
  )
)

# Add scaled phosphorus variable (like scalephos) and LOF term
soil <- soil %>%
  mutate(
    lof = phos,
    scalephos = phos / 100
  )
##############################################################################################################
model.soil <- lm(calcium ~ block + phos + I(phos^2) + I(phos^3) + I(phos^4), data = soil)
anova(model.soil)

# Lack of fit
soil$lof <- factor(soil$lof)
model.soil.lof <- lm(calcium ~ block + phos + lof, data = soil)
anova(model.soil.lof)

model.soil.2 <- lm(calcium ~ block + phos , data = soil)
anova(model.soil.2)
summary(model.soil.2)
############################################################################################################
#Treating Block as Random
model.soil.random.block <- lmer(calcium ~  phos + I(phos^2) + I(phos^3) + I(phos^4) + (1|block), data = soil)
anova(model.soil.random.block)
summary(model.soil.random.block)

model.soil.random.block.2 <- lmer(calcium ~  scalephos + I(scalephos^2) + I(scalephos^3) + I(scalephos^4) + (1|block), data = soil)
anova(model.soil.random.block.2)
# Solution for fixed effects
summary(model.soil.random.block.2)








