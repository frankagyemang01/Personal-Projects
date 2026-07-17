# Load required packages
library(emmeans)
library(ggplot2)
library(dplyr)
library(car)
library(gmodels)
library(doBy)

# Step 1: Create the dataset
cotton <- data.frame(
  percent = factor(rep(c(15, 20, 25, 30, 35), each = 5)),
  strength = c(
    7, 7, 15, 11, 9,
    12, 17, 12, 18, 18,
    15, 19, 19, 21, 19,
    19, 25, 22, 19, 23,
    7, 10, 11, 15, 11
  )
)


 
  
attach(cotton)
## Statistical model 
lm.out=lm(strength ~ percent,data=cotton) 
anova(lm.out)

## Polynomial contrasts to determine which of the orders are significant

s.linear  = fit.contrast(lm.out, "percent", c(-2, -1,  0,  1,  2))
s.linear 
s.quad  = fit.contrast(lm.out, "percent", c( 2, -1, -2, -1,  2))
s.quad  
s.cubic = fit.contrast(lm.out, "percent", c(-1,  2 , 0, -2,  1))
s.cubic  
s.quartic = fit.contrast(lm.out, "percent", c( 1, -4,  6, -4,  1))
s.quartic 
## Contrast shows the cubic term is significant

means = summaryBy(strength ~ percent,data=cotton,FUN=mean) 
## Mean plots
plot(means)

means$p = as.numeric(means$percent) + 1 
means$p
reg =lm(strength.mean ~ p,data=means)
summary(reg)


# Convert factor to numeric correctly
cotton$percent <- as.numeric(as.character(cotton$percent))
quartic_model <- lm(strength ~ percent + I(percent^2) + I(percent^3)+ I(percent^4), data = cotton)
Anova(quartic_model,type = "III")
summary(quartic_model)
cubic_model <- lm(strength ~ percent + I(percent^2) + I(percent^3), data = cotton)
Anova(cubic_model, type = "III")
summary(cubic_model)

# Quadratic response equation:
# y = -39.3714 + 4.5354*percent - 0.08743*percent^2 + 0.00760 * percent^3

# Create the data to plot the response curve within the inference space of 15 to 35
ploty <- data.frame(percent = seq(15, 35, by = 5))
ploty$y <- with(ploty, 59.5257 - 8.7257 * percent + 
                  0.4757 * percent^2 - 
                  0.00760 * percent^3)

# Print the data
print(ploty)

# plot for the response curve within the inference space of 15 to 35
library(ggplot2)

#Graph using the cubic term
ggplot(ploty, aes(x = percent, y = y)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 3) +
  labs(title = "Cubic Response Curve (15–35)", 
       x = "Percent", y = "Predicted y") +
  theme_minimal()

# Create the data plot for the response curve outside the inference space of 15 to 35
plotyw <- data.frame(percent = seq(0, 50, by = 5))
plotyw$y <- with(plotyw, 59.5257 - 8.7257 * percent + 
                   0.4757 * percent^2 - 
                   0.00760 * percent^3)

# plot for the response curve outside the inference space of 15 to 35
ggplot(plotyw, aes(x = percent, y = y)) +
  geom_line(color = "red", size = 1) +
  geom_point(size = 2) +
  labs(title = "Cubic Response Curve (0–50)", 
       x = "Percent", y = "Predicted y") +
  theme_minimal()

## Rerunning the model dropping cubic and quartic terms
quadratic_model <- lm(strength ~ percent + I(percent^2) , data = cotton)
Anova(quadratic_model, type = "III")
summary(quadratic_model)

# Create the data to plot the response curve within the inference space of 15 to 35
ploty <- data.frame(percent = seq(15, 35, by = 5))
ploty$y <- with(ploty, -43.07429 + 4.87829 * percent - 
                  0.09429 * percent^2 )

# Print the data
print(ploty)

# Graph using the quadratic term
ggplot(ploty, aes(x = percent, y = y)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 3) +
  labs(title = "Quadratic Response Curve (15–35)", 
       x = "Percent", y = "Predicted y") +
  theme_minimal()

# Create the data plot for the response curve outside the inference space of 15 to 35
plotyw <- data.frame(percent = seq(0, 50, by = 5))
plotyw$y <- with(plotyw, -43.07429 + 4.87829 * percent - 
                   0.09429 * percent^2 )

# plot for the response curve outside the inference space of 15 to 35
ggplot(plotyw, aes(x = percent, y = y)) +
  geom_line(color = "red", size = 1) +
  geom_point(size = 2) +
  labs(title = "Quadratic Response Curve (0–50)", 
       x = "Percent", y = "Predicted y") +
  theme_minimal()

# Lastly, plotting the quartic model
ploty <- data.frame(percent = seq(15, 35, by = 5))
ploty$y <- with(ploty, -2.804e+02 + 5.128e+01 * percent - 
                  3.358e+00 * percent^2 + 
                  9.773e-02 * percent^3 - 1.053e-03 * percent^4)
# Print the data
print(ploty)

# Graph using the quadratic term
ggplot(ploty, aes(x = percent, y = y)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 3) +
  labs(title = "Quartic Response Curve (15–35)", 
       x = "Percent", y = "Predicted y") +
  theme_minimal()
