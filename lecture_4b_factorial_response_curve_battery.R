## Qualitative by Quantitative factorial designs
library(tidyverse)
library(car)
library(emmeans)

battery <- tribble(
  ~life,
  130, 155, 74, 180,
  34,  40, 80, 75,
  20,  70, 82, 58,
  150, 188, 159, 126,
  136, 122, 106, 115,
  25,  70, 58, 45,
  138, 110, 168, 160,
  174, 120, 150, 139,
  96, 104, 82, 60
)

# Assign material and temp to each observation
battery <- battery %>%
  mutate(
    material = rep(1:3, each = 12),
    temp = rep(rep(c(15, 70, 125), each = 4), 3)
  )

###############################################################################################################
# Type 1 Tests of Fixed Effects
battery <- battery %>%
  mutate(t2 = temp^2,
         material = factor(material))

# Type I (sequential) ANOVA
model.battery <- lm(life ~ material + temp + I(temp^2) + material:temp + material:I(temp^2), data = battery)
anova(model.battery)

# Parameter Estimates 
model.battery.2 <- lm(life ~ -1 + material + material:temp + material:I(temp^2), data = battery)
summary(model.battery.2)

battery_split <- battery %>%
  group_by(material) %>%
  group_split()

# Quadratic order for the quantitative factor
model.battery.3 <- lapply(battery_split, function(battery) {
  lm(life ~ temp + I(temp^2), data = battery)
})

coefs <- lapply(model.battery.3, coef)
names(coefs) <- paste0("Material_", 1:3)
coefs
############################################################################################################
plot_data <- map2_dfr(coefs, 1:3, ~ {
  tibble(
    material = .y,
    temp = seq(15, 125, by = 10),
    life = .x[1] + .x[2] * temp + .x[3] * temp^2
  )
})

ggplot(plot_data, aes(x = temp, y = life, color = factor(material))) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Quadratic Response Curve by Material",
       x = "Temperature",
       y = "Predicted Life",
       color = "Material") +
  theme_minimal()





