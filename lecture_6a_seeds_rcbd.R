## Lecture 6- Seeds Example RCBD
# Load packages
library(dplyr)
library(emmeans)
library(lme4)
library(car)

# Create the dataset
Chem <- rep(c("Check", "Arasan", "Spergon", "Semesan", "Fermate"), each = 5)
Rep  <- rep(1:5, times = 5)
Fail <- c(8,10,12,13,11, 2,6,7,11,5, 4,10,9,8,10, 3,5,9,10,6, 9,7,5,5,3)
Emerge <- 100 - Fail
seeds <- data.frame(Chem, Rep = factor(Rep), Fail, Emerge)
seeds$Chem <- as.factor(seeds$Chem)
seeds$Rep <- as.factor(seeds$Rep)


# Fit linear model with fixed Rep
model.seeds <- lm(Emerge ~ Chem + Rep, data = seeds)
summary(model.seeds)
# Type III ANOVA
anova(model.seeds)

# LSMeans for Chem
lsm <- emmeans(model.seeds, "Chem")

# Dunnett-adjusted pairwise comparisons vs. 'Check'
pairs(lsm, adjust = "dunnett", ref = "Check")
##########################################################################################################
model.seeds.random <- lmer(Emerge ~ Chem + (1|Rep), data = seeds)
summary(model.seeds.random)
# Dunnett-adjusted pairwise comparisons vs. 'Check'
lsm.random <- emmeans(model.seeds.random, "Chem")
pairs(lsm.random, adjust = "dunnett", ref = "Check")

