# RCBD- Wheat Example
# Load required packages
library(lme4)
library(lmerTest)   
library(emmeans)    
library(car)        
library(dplyr)

# Recreate the data (as in SAS)
wheat <- data.frame(
  strain = rep(c("A", "B", "C", "D"), each = 5),
  block = rep(1:5, times = 4),
  yield = c(
    32.3, 35.0, 34.8, 35.0, 36.4,
    31.8, 33.0, NA, 36.8, 34.4,
    30.3, 33.3, 35.8, 32.3, 35.7,
    NA, 26.0, 29.8, 28.0, 28.8
  )
)

# Remove missing yield values
wheat <- na.omit(wheat)

# Treat factors
wheat$strain <- as.factor(wheat$strain)
wheat$block <- as.factor(wheat$block)
######################################################################################################
# Fixed Effects Model 
model.wheat <- lm(yield ~ strain + block, data = wheat)

# Type III ANOVA
Anova(model.wheat, type = 3)
##########################################################################################################
# Type I ANOVA
anova(model.wheat)

# Least Squares Means for strain
emm_fixed <- emmeans(model.wheat, ~ strain)
summary(emm_fixed)
# Optional: pairwise differences
pairs(emm_fixed)
###########################################################################################################
# Means by strain
wheat %>%
  group_by(strain) %>%
  summarise(mean_yield = mean(yield, na.rm = TRUE),
            sd_yield = sd(yield))

# Random Block Model (RCBD)
mod_mixed <- lmer(yield ~ strain + (1 | block), data = wheat)

# Type III Tests of Fixed Effects (from lmer)
anova(mod_mixed, type = "III")

# LSMeans for strain in mixed model
emm_mixed <- emmeans(mod_mixed, ~ strain)
summary(emm_mixed)
