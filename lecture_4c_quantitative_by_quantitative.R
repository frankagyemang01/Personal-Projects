# Input surface values from the datalines
surface_values <- c(
  74, 64, 60, 79, 68, 73, 82, 88, 92, 99, 104, 96,
  92, 86, 88, 98, 104, 88, 99, 108, 95, 104, 110, 99,
  99, 98, 102, 104, 96, 95, 108, 110, 99, 114, 111, 107
)

# Initialize index
i <- 1

# Initialize empty vectors
surface <- numeric()
f <- numeric()
d <- numeric()
rep <- numeric()
lof <- numeric()
lofd <- numeric()
loff <- numeric()

# Loop over values of f and d as in the SAS code
for (f_val in c(0.2, 0.25, 0.3)) {
  for (d_val in c(1.5, 1.8, 2.0, 2.5)) {
    for (r in 1:3) {
      surface[i] <- surface_values[i]
      f[i] <- f_val
      d[i] <- d_val
      rep[i] <- r
      lof[i] <- f_val * d_val
      lofd[i] <- d_val
      loff[i] <- f_val
      i <- i + 1
    }
  }
}

# Create the data frame
finish <- data.frame(surface, f, d, rep, lof, lofd, loff)



#############################################################################################################
# Add interaction factor variables (to mimic SAS CLASS)
finish$loff <- as.factor(finish$f)
finish$lofd <- as.factor(finish$d)

                         
# Fit the model
mod.finish <- lm(surface ~ loff * lofd, data = finish)
summary(mod.finish)
anova(mod.finish)

# Step 3: Get LSMeans
lsm <- emmeans(mod.finish, ~ loff * lofd)
lsm_df <- as.data.frame(lsm)

# Convert loff and lofd back to numeric for plotting
lsm_df <- lsm_df %>%
  mutate(
    loff = as.numeric(as.character(loff)),
    lofd = as.numeric(as.character(lofd))
  )

# Step 4: Create surface plot (like SAS proc g3d)
plot_ly(
  data = lsm_df,
  x = ~loff, y = ~lofd, z = ~emmean,
  type = "surface",
  contours = list(z = list(show = TRUE))
) %>%
  layout(
    scene = list(
      xaxis = list(title = "loff"),
      yaxis = list(title = "lofd"),
      zaxis = list(title = "Estimate")
    )
  )

#############################################################################################################

grid$pred <- predict(final_mod, newdata = grid)

plot_ly(grid, x = ~f, y = ~d, z = ~pred,
        type = 'surface') %>%
  layout(scene = list(
    xaxis = list(title = "f"),
    yaxis = list(title = "depth"),
    zaxis = list(title = "Predicted Surface")
  ))
##########################################################################################################
mod.finish.2 <- lm(surface ~ f + I(f^2) + d + I(d^2) + f*d + loff:lofd, data = finish)

# Type I ANOVA: Sequential tests (same as htype=1)
anova(mod.finish.2)
###########################################################################################################
mod.finish.3 <- lm(surface ~ f + d + f*d + I(f * d^2) + I(f^2) + I(f^2 * d) + I(f^2 * d^2)  + I(d^2) + I(d^3) + loff:lofd, data = finish)

# ANOVA table with Type I (sequential) 
anova(mod.finish.3)
##########################################################################################################
mod.finish.final <- lm(surface ~ f + I(f^2) + d + I(f^2*d) + f*d , data = finish)

# Type I ANOVA: Sequential tests (same as htype=1)
anova(mod.finish.final)
summary(mod.finish.final)
##########################################################################################################
# Load required libraries
library(ggplot2)
library(plotly)

# Generate grid of values for f and depth
grid <- expand.grid(
  f = seq(0.20, 0.30, by = 0.01),
  depth = seq(1.5, 2.5, by = 0.1)
)

# Compute predicted values using the model equation
grid$pred <- with(grid,
                  -635.6 + 4970.4 * f -8622.6 * f^2 +
                    283.7 * depth -1923.3 * f * depth +
                    3396.2 * f^2 * depth)

# 3D surface plot using plotly
plot_ly(
  data = grid,
  x = ~depth,
  y = ~f,
  z = ~pred,
  type = "surface"
)



