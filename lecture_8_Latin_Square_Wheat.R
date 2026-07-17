## Lecture 8 Latin Square Wheat Example
# Load necessary packages
library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)

# Create the dataset
df.latin <- data.frame(
  row = rep(1:4, each = 4),
  col = rep(1:4, times = 4),
  variety = c(
    "C", "D", "B", "A",
    "B", "A", "C", "D",
    "D", "C", "A", "B",
    "A", "B", "D", "C"
  ),
  yield = c(
    10.5, 7.7, 12.0, 13.2,
    11.1, 12.0, 10.3, 7.5,
    5.8, 12.2, 11.2, 13.7,
    11.6, 12.3, 5.9, 10.2
  )
)

# Convert grouping variables to factors
df.latin <- df.latin %>%
  mutate(
    row = factor(row),
    col = factor(col),
    variety = factor(variety)
  )
############################################################################################################

# Fit the mixed model: random row and col, fixed variety
model.latin.random <- lmer(yield ~ variety + (1 | row) + (1 | col), data = df.latin)
# Variance Components
summary(model.latin.random)
# Type III Tests of Fixed Effects
anova(model.latin.random, type = 3)
############################################################################################################

# Least Squares Means and pairwise differences
lsm <- emmeans(model.latin.random, ~ variety)
lsm
pairs(lsm)
###########################################################################################################
# Fit the model with fixed row, col and variety
model.latin.fixed <- lm(yield ~ variety +  row +  col, data = df.latin)
# Type III Tests of Fixed Effects
anova(model.latin.fixed)
# Least Squares Means and pairwise differences
lsm.fixed <- emmeans(model.latin.fixed, ~ variety)
lsm.fixed
pairs(lsm.fixed)
