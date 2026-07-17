## Factorial design using rat weight example
# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Raw input data
rats <- data.frame(
  source = rep(c("beef", "beef", "cereal", "cereal"), each = 10),
  amount = rep(c("low", "high", "low", "high"), each = 10),
  rat = rep(1:10, times = 4),
  weight = c(
    90, 76, 90, 64, 86, 51, 72, 90, 95, 78,       
    73, 102, 118, 104, 81, 107, 100, 87, 117, 111, 
    107, 95, 97, 80, 98, 74, 74, 67, 89, 58,       
    98, 74, 56, 111, 95, 88, 82, 77, 86, 92        
  )
)

# Get means by source and amount (cell means)
cell_means <- rats %>%
  group_by(source, amount) %>%
  summarise(meanwt = mean(weight), .groups = "drop")

# Get marginal means for Source 
marginal_source <- rats %>%
  group_by(source) %>%
  summarise(meanwt = mean(weight)) %>%
  mutate(amount = "Marginal")

#Get marginal means for amount
marginal_amount <- rats %>%
  group_by(amount) %>%
  summarise(meanwt = mean(weight)) %>%
  mutate(source = "Marginal")

# Combine all for a full table
meanwt_all <- bind_rows(cell_means, marginal_source, marginal_amount)


# Interaction plot from cell means
ggplot(cell_means, aes(x = amount, y = meanwt, group = source, shape = source)) +
  geom_line(aes(linetype = source), color = "black") +
  geom_point(size = 3, color = "black") +
  scale_shape_manual(values = c(16, 8)) +  # 16 = dot, 8 = star
  labs(
    title = "Plot of Cell Means to Detect Interaction",
    x = "Amount",
    y = "Mean Weight"
  ) +
  theme_minimal()
############################################################################################################
# Install if you don't have these packages
#install.packages(c("lme4", "car", "emmeans"))

library(lme4)
library(car)
library(emmeans)

rats <- data.frame(
  source = rep(c("beef", "beef", "cereal", "cereal"), each = 10),
  amount = rep(c("low", "high", "low", "high"), each = 10),
  rat = rep(1:10, times = 4),
  weight = c(
    90, 76, 90, 64, 86, 51, 72, 90, 95, 78,        # beef low
    73, 102, 118, 104, 81, 107, 100, 87, 117, 111, # beef high
    107, 95, 97, 80, 98, 74, 74, 67, 89, 58,       # cereal low
    98, 74, 56, 111, 95, 88, 82, 77, 86, 92        # cereal high
  )
)
rats$source <- factor(rats$source)
rats$amount <- factor(rats$amount)

# Fit the fixed effects model 
model.rat <- lm(weight ~ source * amount, data = rats)
Anova(model.rat, type = "III")  

# LSMeans (EMMeans) for source
emmeans(model.rat, ~ source)

# LSMeans for amount
emmeans(model.rat, ~ amount)

# LSMeans for source * amount
emm_interact <- emmeans(model.rat, ~ source * amount)
emm_interact

# Differences of source*amount Least Squares Means
pairs(emm_interact)

# diffplot-style plot: Not recommended
#plot(pairs(emm_interact), comparisons = TRUE)

# Differences of source*amount Least Squares Means
contrast(emm_interact, method = list("source x amount" = c(1, -1, -1, 1)), adjust = "none")
emmeans(model.rat, ~ amount | source) %>%
  pairs()
emmeans(model.rat, ~ source | amount) %>%
  pairs()

# Estimate: LSMean beef
contrast(emm_interact, method = list("LSMean beef" = c(0.5, 0.5, 0, 0)), adjust = "none")

# Estimate: LSMean cereal
contrast(emm_interact, method = list("LSMean cereal" = c(0, 0, 0.5, 0.5)), adjust = "none")

# Estimate: LSMean high
contrast(emm_interact, method = list("LSMean high" = c(0, 0.5, 0, 0.5)), adjust = "none")

# Estimate: LSMean low
contrast(emm_interact, method = list("LSMean low" = c(0.5, 0, 0.5, 0)), adjust = "none")

# Estimate: Effect of source in high (beef_high - cereal_high)
contrast(emm_interact, method = list("source in high" = c(0, 1, 0, -1)), adjust = "none")

# Estimate: Effect of source in low (beef_low - cereal_low)
contrast(emm_interact, method = list("source in low" = c(1, 0, -1, 0)), adjust = "none")

# Estimate: Effect of amount in beef (beef_high - beef_low)
contrast(emm_interact, method = list("amount in beef" = c(-1, 1, 0, 0)), adjust = "none")

# Estimate: Effect of amount in cereal (cereal_high - cereal_low)
contrast(emm_interact, method = list("amount in cereal" = c(0, 0, -1, 1)), adjust = "none")

# Estimate: Interaction contrast (beef_high - beef_low - cereal_high + cereal_low)
contrast(emm_interact, method = list("interaction" = c(-1, 1, 1, -1)), adjust = "none")





















