## Lecture 8 - Replicated Latin Square
# Create the data frame in R
mpga <- data.frame(
  square = rep(c(1, 2, 1, 2, 1, 2, 1, 2), each = 4),
  driver = c(
    1, 2, 3, 4,
    5, 6, 7, 8,
    1, 2, 3, 4,
    5, 6, 7, 8,
    1, 2, 3, 4,
    5, 6, 7, 8,
    1, 2, 3, 4,
    5, 6, 7, 8
  ),
  model = rep(c("I", "II", "III", "IV"), each = 8),
  blend = c(
    "D", "B", "C", "A",
    "B", "A", "D", "C",
    "B", "C", "A", "D",
    "C", "D", "B", "A",
    "C", "A", "D", "B",
    "A", "B", "C", "D",
    "A", "D", "B", "C",
    "D", "C", "A", "B"
  ),
  mpg = c(
    15.5, 16.3, 10.8, 14.7,
    16.6, 15.1, 15.4, 10.0,
    33.9, 26.6, 31.1, 34.0,
    27.0, 34.6, 33.8, 30.5,
    13.2, 19.4, 17.1, 19.7,
    19.8, 21.0, 13.0, 16.4,
    29.1, 22.8, 30.3, 21.6,
    23.1, 22.4, 28.9, 29.8
  )
)

# Convert character columns to factors
mpga$model <- factor(mpga$model)
mpga$blend <- factor(mpga$blend)

# View the first few rows
head(mpga)
###########################################################################################################
# Load required packages
library(lme4)
library(lmerTest)  
library(car)       

# Assuming the `mpga` dataset is already created (from earlier step)
# If not, run the data creation code first

# Fit the model
model.latin.replicated <- lmer(
  mpg ~ square * blend + 
    (1 | square:driver) + 
    (1 | square:model),
  data = mpga
)

# Get covariance parameter estimates
summary(model.latin.replicated)

# Type III Tests of Fixed Effects
anova(model.latin.replicated, type = 3)
############################################################################################################
# Fit the model: Drop square * blend term
model.latin.replicated <- lmer(
  mpg ~ square + blend + 
    (1 | square:driver) + 
    (1 | square:model),
  data = mpga
)

# Get covariance parameter estimates
summary(model.latin.replicated)

# Type III Tests of Fixed Effects
anova(model.latin.replicated, type = 3)
############################################################################################################
# Least Squares Means and pairwise differences
lsm <- emmeans(model.latin.replicated, ~ blend)
lsm
pairs(lsm)
############################################################################################################
# Fit the linear model with nested fixed effects
mpga$driver <- factor(mpga$driver)
mpga$square <- factor(mpga$square)
model.latin.replicated.2 <- lm(mpg ~ square + blend + 
                                 square*blend +
                                square:driver + 
                                square:model ,
                                 data = mpga)

# Get Type III Tests of Fixed Effects
anova(model.latin.replicated.2)



