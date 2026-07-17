# Reading in data
afb1 <- read.csv("AFB1.csv")

## Removing IDs from the sample name
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)   
library(emmeans)    
library(car)        
library(dplyr)

afb1 <- afb1 %>%
  separate(Treatment, into = c("Treatment", "ID"), sep = "-")
afb1 <- afb1[,-3]
afb1$Treatment <- as.factor(afb1$Treatment)
afb1$Year <- as.factor(afb1$Year)
write.csv(afb1, "afb1_data.csv", row.names = FALSE)

###################################################################################################
## Descriptive statistics
library(ggplot2)

# Histogram of Response by Year
ggplot(afb1, aes(x = Response)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Year, scales = "free_y") +
  labs(
    title = "Histogram of AFB1 by Year",
    x = "AFB1",
    y = "Count"
  ) +
  theme_minimal()
## Overall histogram
library(ggplot2)

# Example: Histogram of the Response variable
ggplot(afb1, aes(x = Response)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of AFB1",
    x = "AFB1",
    y = "Count"
  ) +
  theme_minimal()
###################################################################################################
# Boxplot for each treatment in each year


ggplot(afb1, aes(x = Treatment, y = Response, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Year) +
  labs(
    title = "Boxplot of AFB1 by Treatment in Each Year",
    x = "Treatment",
    y = "AFB1"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Boxplot for overall treatment
ggplot(afb1, aes(x = Treatment, y = Response, fill = Treatment)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of AFB1 by Treatment",
       x = "Treatment",
       y = "AFB1") +
  scale_fill_brewer(palette = "Set2")
########################################################################################################
#######################################################################################################
# Model and analysis
# Getting a summary table
afb1 %>%
  group_by(Year) %>%
  summarise(mean_response = mean(Response, na.rm = TRUE),
            sd_yield = sd(Response))

# Fixed Block Model (RCBD)
model.afb1 <- lm(Response ~ Treatment + Year + Treatment*Year, data = afb1)

# Type III Tests of Fixed Effects
anova(model.afb1)

# Simple effects of Treatment within each Block
emm <- emmeans(model.afb1, ~ Treatment | Year)
summary(emm)

# Fit the model with a Gamma distribution
model.afb1.2 <- glm(Response ~ Treatment + Year + Treatment:Year,
                  family = Gamma(link = "log"),
                  data = afb1)

# Summary of the model
summary(model.afb1.2)

#########################################################################################################
emm_df <- as.data.frame(emm)

# Interaction plot
ggplot(emm_df, aes(x = Year, y = emmean, color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1) +
  labs(y = "Estimated Marginal Mean", title = "Interaction of Treatment and Year") +
  theme_minimal()
#######################################################################################################
# Diagnostic check for model when Gamma distribution is assumed
# Deviance residuals
plot(model.afb1.2$fitted.values, residuals(model.afb1.2, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance Residuals")
abline(h = 0, col = "red")

plot(model.afb1.2$fitted.values, sqrt(abs(residuals(model.afb1.2, type = "deviance"))),
     xlab = "Fitted values", ylab = "Sqrt(|Deviance Residuals|)")

## Diagnostic check for model when Normal Distribution is assumed
plot(model.afb1$fitted.values, residuals(model.afb1),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

#########################################################################################################
flour <- read.csv("flour_type.csv")
library(ggplot2)
## Histogram of APC
ggplot(flour, aes(x = APC)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.6) +
  labs(
    title = "Density Plot of APC",
    x = "APC",
    y = "Density"
  ) +
  theme_minimal()

## Histogram of Coliform
ggplot(flour, aes(x = coliform)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.6) +
  labs(
    title = "Density Plot of Coliform",
    x = "Coliform",
    y = "Density"
  ) +
  theme_minimal()

## Histogram of EB
ggplot(flour, aes(x = EB)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.6) +
  labs(
    title = "Density Plot of EB",
    x = "EB",
    y = "Density"
  ) +
  theme_minimal()

## Histogram of Yeast
ggplot(flour, aes(x = Yeast)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.6) +
  labs(
    title = "Density Plot of Yeast",
    x = "Yeast",
    y = "Density"
  ) +
  theme_minimal()

## Histogram of Mold
ggplot(flour, aes(x = Molds)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.6) +
  labs(
    title = "Density Plot of Molds",
    x = "Molds",
    y = "Density"
  ) +
  theme_minimal()
