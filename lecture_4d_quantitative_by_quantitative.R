# Required packages
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
finish$lofd <- as.factor(finish$lofd)
finish$loff <- as.factor(finish$loff)
###########################################################################################################
##########################################################################################################
# Fit the linear model
mod <- lm(surface ~ loff * lofd, data = finish)
summary(mod)
# Type I ANOVA (sequential sums of squares, like SAS htype=1)
anova(mod)

# LS-means for loff*lofd
emm <- emmeans(mod, ~ loff * lofd)
emm

# If you want pairwise comparisons (like SAS LSMEANS options)
pairs(emm)
#################################################################################################
library(emmeans)
library(plotly)

# LS-means for loff*lofd
emm <- emmeans(mod, ~ loff * lofd)

# Convert to data.frame
emm_df <- as.data.frame(emm)

# Surface plot with plotly
# Install and load the plot3D package
#install.packages("plot3D")
library(plot3D)

# Generate data
lofd <- seq(1.5, 2.5, length.out = 10)
loff <- seq(0.3, 1.5, length.out = 10)
f <- function(lofd, loff) { lofd*loff }

# Create surface plot
surf <- persp3D(lofd, loff, outer(lofd, loff, f), theta = 30, phi = 30,
                col = "lightblue", border = NA)

#####################################################################################################
# Second-order polynomial model
mod2 <- lm(surface ~ f + d  + poly(f,2) + poly(d,2) + loff *lofd , data= finish)
anova(mod2)   # Type I ANOVA like SAS htype=1
summary(mod2)

# Third-order polynomial model
mod3 <- lm(surface ~ f + d + f*d + poly(f,2) + poly(d,2) + 
             poly(d,2):poly(f,1) + poly(f,2):poly(d,1) + 
             poly(d,3) + loff*lofd , 
           data= finish)


anova(mod3)
summary(mod3)

########################################################################################################
## Final Model
mod.final <- lm(surface ~ f + d + f*d  + poly(f,2) + poly(f,2)*poly(d,1) , data= finish)
anova(mod.final)   # Type I ANOVA like SAS htype=1
summary(mod.final)


depth = seq(1.5, 2.5, by = 0.1)
f = seq(0.2, 0.30, by = 0.01)

funt <- function(depth,f) { -627.57 + 4898.21*f - 8462.26*f^2 +
    280.85*depth - 1897.80*f*depth +
    3339.62*f^2*depth}

# Create surface plot
surf <- persp3D(depth, f, outer(depth, f, funt), theta = 30, phi = 30,
                col = "lightblue", border = NA)





















