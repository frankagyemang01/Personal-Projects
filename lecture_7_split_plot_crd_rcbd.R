## CRD and RCBD Split Plot Example
# Load required libraries
library(lme4)
library(lmerTest)  
library(car)       

# Create the dataset (same as SAS datalines)
a_data <- data.frame(
  rep = rep(1:3, each = 4),
  a   = c(1,1,2,2, 1,1,2,2, 2,2,1,1),
  b   = c(1,2,2,1, 1,2,1,2, 2,1,1,2),
  y   = c(5.22,5.61,6.52,5.78, 6.13,6.14,5.77,6.23, 5.81,6.43,5.49,4.60)
)

# Convert factors
a_data$rep <- as.factor(a_data$rep)
a_data$a <- as.factor(a_data$a)
a_data$b <- as.factor(a_data$b)

###########################################################################################################
# Fit the mixed model
model.a_data <- lmer(y ~ a * b + (1 | rep:a), data = a_data)
# Type III Tests of Fixed Effects
anova(model.a_data, type = 3)
# Variance Components
summary(model.a_data)
#############################################################################################################

# Split Plot Designs RCBD
# Load necessary libraries
library(lme4)
library(lmerTest)
library(emmeans)
library(agricolae)

# Create the data
y_values <- c(
  21.7, 18.8, 25.0, 24.3,
  26.3, 19.8, 21.6, 25.7,
  17.5, 15.2, 15.5, 15.6,
  20.8, 14.5, 18.1, 22.0,
  23.1, 16.0, 20.0, 21.1,
  18.5, 14.6, 17.2, 17.2,
  18.2, 14.2, 18.7, 20.3,
  20.0, 22.5, 21.2, 23.1,
  21.2, 17.7, 19.9, 19.9,
  25.2, 17.9, 20.9, 23.0,
  20.3, 13.7, 18.0, 17.0,
  18.6, 13.5, 15.5, 15.5,
  17.8, 14.5, 15.9, 18.6,
  17.3, 14.4, 19.8, 16.3,
  13.0, 10.0, 15.0, 16.3
)

# Factors: 5 blocks × 3 A levels × 4 B levels = 60 observations
df.split <- expand.grid(
  block = factor(1:5),
  A = factor(1:3),
  B = factor(1:4)
)
df.split$y <- y_values
df.split <- as.data.frame(df.split)
############################################################################################################
# Degrees of freedom match that of SAS output and inference are the same 
# But Sum of Squares and p-values differ
#  Split-plot model with fixed blocks

mod.split.fixed <- with(df.split,sp.plot(block,A,B,y))
summary(mod.split.fixed)
print(VarCorr(mod.split.fixed), comp = "Variance")

##########################################################################################################
# Does not produce the same results as in SAS
# Split-plot model with random blocks (block and block*A random)
model_random <- lmer(y ~  + A * B + (1 | block) + (1| block:A), data = df.split)
summary(model_random)
anova(model_random, type = 3)
emmeans(model_random, pairwise ~ A)
emmeans(model_random, pairwise ~ B)
emmeans(model_random, pairwise ~ A:B)



############################################################################################################
# Split plot with random block by A
mod.split.random <- aov(y ~ block  + A * B + Error(block/A), data = df.split)
summary(mod.split.random)

# LSMeans (estimated marginal means)
emmeans(mod.split.random, pairwise ~ A)
emmeans(mod.split.random, pairwise ~ B)
emmeans(mod.split.random, pairwise ~ A:B)

# Satterthwaite approximation (default in lmerTest)
# Pairwise comparisons with p-values
emmeans(mod.split.random, pairwise ~ A, adjust = "tukey")
emmeans(mod.split.random, pairwise ~ B, adjust = "tukey")
emmeans(mod.split.random, pairwise ~ A:B, adjust = "tukey")
