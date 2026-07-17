## Doughnut example
# Load required libraries
library(nlme)        
library(car)         
library(emmeans)     
library(performance) 
library(ggplot2)     
library(dplyr)

dough1 <- data.frame(
  type = factor(c(
    1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4,
    1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4
  )),
  absorb = c(
    164, 178, 175, 155,
    172, 191, 193, 166,
    168, 197, 178, 149,
    177, 182, 171, 164,
    156, 185, 163, 170,
    195, 177, 176, 168
  )
)


model.doughnut <- gls(absorb ~ type,
    weights = varIdent(form = ~ 1 | type),
    data = dough1,
    method = "REML")
summary(model.doughnut)
# Type III ANOVA
anova.doughnut<- aov(absorb ~ type,data = dough1)
summary(anova.doughnut)


# LSMeans and pairwise differences
emm <- emmeans(model.doughnut, ~ type)
emm
pairs(emm)

# Custom contrasts (optional example)
contrast(emm, list("1 vs 2" = c(1, -1, 0, 0)))

# Residuals and Predictions ***
dough1$pred <- predict(gls_model)
dough1$resid <- residuals(gls_model)

# Residual diagnostics
# Residual Panel (mimic ODS graphics panel)
par(mfrow = c(2, 2))
plot(gls_model, which = 1:4)

#  ggplot2 version of residual plot
ggplot(dough1, aes(x = pred, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals")

# Normality check of residuals
# Histogram + Q-Q Plot
qqnorm(dough1$resid)
qqline(dough1$resid)

# Or with univariate-style summary:
library(nortest)
shapiro.test(dough1$resid)





