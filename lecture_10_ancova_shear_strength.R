# Lecture 10 - Shear Strength

# Create the data
shearstrength <- data.frame(
  alloy = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
  y     = c(37.5, 57.5, 38.0, 40.5, 69.5, 44.5, 49.0, 87.0, 53.0,
            51.0, 92.0, 55.0, 61.5, 107.0, 58.5, 63.0, 119.5, 60.0),
  x     = c(12.5, 16.5, 15.5, 14.0, 17.5, 16.0, 16.0, 19.0, 19.0,
            15.0, 19.5, 18.0, 18.0, 24.0, 19.0, 19.5, 22.5, 20.5)
)

# Calculate the mean of x
xbar <- mean(shearstrength$x)

# Create a new data frame with centered covariate
shear2 <- transform(shearstrength, xbar = xbar, center = x - xbar)

# Print the resulting data frame
print(shear2)
#############################################################################################################
# Fit a linear model with alloy as a categorical (factor) predictor
# first check assumption about independence of treatment and covariate
shear2$alloy <- as.factor(shear2$alloy)

model.shear <- lm(center ~ alloy, data = shear2)
anova(model.shear)
############################################################################################################
library(car)

# Ensure 'alloy' is treated as a factor
shear2$alloy <- as.factor(shear2$alloy)

# Fit the linear model with interaction
model.shear2 <- lm(y ~ alloy * center, data = shear2)

# Display model coefficients (SAS solution option)
summary(model.shear2)

# Type III ANOVA (SAS /s)
Anova(model.shear2, type = 3)
############################################################################################################
model.shear3 <- lm(y ~ alloy * x, data = shear2)
summary(model.shear3)
Anova(model.shear3, type = 3)
############################################################################################################
# Create interaction term manually
shear2$alloy <- factor(shear2$alloy)
model.shear4 <- lm(y ~ 0 + alloy + center:alloy, data = shear2)
summary(model.shear4)
# No intercept
model.shear5 <- lm(y ~ alloy + center:alloy, data = shear2)
summary(model.shear5)
############################################################################################################
## Plots
# Create the data frame
trt <- rep(1:3, each = length(seq(12.5, 24, by = 0.5)))
x <- rep(seq(12.5, 24, by = 0.5), times = 3)

y <- numeric(length(trt))

for (i in seq_along(trt)) {
  if (trt[i] == 1) {
    y[i] <- 50.4154 + 3.92 * (x[i] - 15.8333)
  } else if (trt[i] == 2) {
    y[i] <- 88.7475 + 7.35 * (x[i] - 19.833)
  } else if (trt[i] == 3) {
    y[i] <- 51.5 + 4.1892 * (x[i] - 18)
  }
}

graph <- data.frame(trt = factor(trt), x = x, y = y)

# Print the data frame (similar to proc print)
print(graph)

# Plot similar to proc gplot with lines grouped by trt
library(ggplot2)

ggplot(graph, aes(x = x, y = y, color = trt)) +
  geom_line() +
  labs(title = "Plot of y vs x by Treatment",
       x = "x",
       y = "y",
       color = "Treatment") +
  theme_minimal()










