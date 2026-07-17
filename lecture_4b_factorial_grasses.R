# Example 2 of factorial design
# Install packages if not already installed
#install.packages(c("tidyverse", "car", "emmeans"))

library(tidyverse)
library(car)
library(emmeans)




# Raw input data
factorial <- data.frame(
  method = rep(c("a", "b", "c"), each = 30),
  variety = rep(c("1", "2", "3", "4", "5"), each = 6),
  yield = c(
    221, 241, 191, 221, 251, 181,       
    271, 151, 206, 286, 151, 246,
    223, 258, 228, 283, 213, 183,
   198, 283, 268, 273, 268, 268,
   200, 170, 240, 225, 280, 225,
    135, 145, 115,  60, 270, 180,
    169, 174, 104, 194, 119, 154,
     157, 102, 167, 197, 182, 122,
     151,  65, 171,  76, 136, 211,
     218, 228, 188, 213, 163, 143,
     190, 220, 200, 145, 190, 160,
     200, 220, 255, 165, 180, 175,
     164, 144, 214, 199, 104, 214,
   245, 160, 110,  75, 145, 155,
     118, 143, 213,  63,  78, 138        
  )
)



factorial$method <- as.factor(factorial$method)
factorial$variety <- as.factor(factorial$variety)
factorial <- factorial %>%
  mutate(yield = yield / 10)
############################################################################################################
# Fit model with interaction + Type III ANOVA
model.grasses <- lm(yield ~ variety * method, data = factorial)
Anova(model.grasses, type = "III")  

# Variety*Method LSMeans
emm_vm <- emmeans(model.grasses, ~ variety * method)
with(factorial,
     interaction.plot(variety,method,yield)) 

# Method*Variety LSMeans
emm_mv <- emmeans(model.grasses, ~ method * variety)
summary(emm_mv)

# Slice by variety
emmeans(model.grasses, ~ method | variety) %>%
  contrast(method = "pairwise")  
# Slice by method
emmeans(model.grasses, ~ variety | method) %>%
  contrast(method = "pairwise")  

#########################################################################################################
# Simple Effect Comparisons Table
emm_all <- emmeans(model.grasses, ~ method * variety)

# Simple effects by variety
contrast(emm_all, method = "pairwise", by = "variety")

# Simple effects by method
contrast(emm_all, method = "pairwise", by = "method")


# Contrast: A vs B,C in V1
contrast(emm_all, method = list(
  "A vs B,C in V1" = c(2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0)
), adjust = "none")

contrast(emm_all, method = list(
  "A vs B,C in V1" = c(2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0),
  "A vs B,C in V2" = c(0, 2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0),
  "A vs B,C in V3" = c(0, 0, 2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0),
  "A vs B,C in V4" = c(0, 0, 0, 2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0),
  "A vs B,C in V5" = c(0, 0, 0, 0, 2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1)
), adjust = "none")

# V1 vs V5
contrast(emm_all, method = list(
  "A vs B,C by V1 vs V5" = c(1, 0, 0, 0, -1, -0.5, 0, 0, 0, 0.5, -0.5, 0, 0, 0, 0.5)
), adjust = "none")



















