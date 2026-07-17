# Set display options
options(width = 80)
##### LECTURE SLIDE 1 R CODE

# Create the data frame
dough1 <- data.frame(
  type = rep(1:4, each = 6),  # Repeat values 1 to 4 for each observation set
  absorb = c(164, 178, 175, 155,
             172, 191, 193, 166,
             168, 197, 178, 149,
             177, 182, 171, 164,
             156, 185, 163, 170,
             195, 177, 176, 168)
)

# Print the data
print(dough1)



# Load necessary packages
library(lme4)
library(emmeans)
library(ggplot2)


# Make sure `method` and `variety` are treated as factors
factorial$method <- as.factor(factorial$method)
factorial$variety <- as.factor(factorial$variety)

# Fit the linear mixed model
model <- lmer(yield ~ method * variety + (1 | method:variety), data = factorial)

# Obtain least squares means (LSMeans)
lsm_results <- emmeans(model, ~ method * variety)

# Print LSMeans results
print(lsm_results)


# Generate interaction plot similar to SAS meanplot(sliceby=method join)
plot_data <- as.data.frame(lsm_results)

ggplot(plot_data, aes(x = variety, y = emmean, group = method, color = method)) +
  geom_point() +
  geom_line() +
  labs(title = "Interaction Plot of Method & Variety",
       x = "Variety",
       y = "Estimated Mean Yield") +
  theme_minimal()



# Load necessary packages
library(lme4)
library(emmeans)

# Convert categorical variables to factors
factorial$method <- as.factor(factorial$method)
factorial$variety <- as.factor(factorial$variety)

# Fit the linear model
model <- lmer(yield ~ method * variety + (1 | method:variety), data = factorial)

# Get estimated marginal means (LSMeans)
lsm <- emmeans(model, ~ method * variety)

# Contrast: A vs (B, C) in Variety 1 (V1)
contrast1 <- contrast(lsm, list("A vs B,C in V1 1" = c(1, 0, 0, 0, 0, -0.5, 0, 0, 0, 0, -0.5, 0, 0, 0, 0)))
print(contrast1)

# Contrast: A vs (B, C) in Variety 1 with divisor = 2 (same as divisor option in SAS)
contrast2 <- contrast(lsm, list("A vs B,C in V1 2" = c(2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0)), adjust = "none")
print(contrast2)



# Load necessary packages
library(lme4)
library(emmeans)

# Convert categorical variables to factors
factorial$method <- as.factor(factorial$method)
factorial$variety <- as.factor(factorial$variety)

# Fit the mixed-effects model
model <- lmer(yield ~ method * variety + (1 | method:variety), data = factorial)

# Get estimated marginal means (LSMeans)
lsm <- emmeans(model, ~ method * variety)

# Define contrasts
contrast_list <- list(
  "A vs B,C in V1" = c(2, -1, -1, 2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0),
  "A vs B,C in V2" = c(2, -1, -1, 0, 2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0),
  "A vs B,C in V3" = c(2, -1, -1, 0, 0, 2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0),
  "A vs B,C in V4" = c(2, -1, -1, 0, 0, 0, 2, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0) / 2  # Applying divisor=2
)

# Compute contrasts
contrast_results <- contrast(lsm, contrast_list)

# Print results
print(contrast_results)

##########################################################################################################

# Load necessary packages
library(lme4)
library(emmeans)
library(dplyr)
library(tidyr)

# Create the data frame
battery <- expand.grid(
  material = factor(1:3), 
  temp = c(15, 70, 125), 
  rep = 1:4
) %>%
  mutate(life = c(
    130, 155, 74, 180,  34, 40, 80, 75,   20, 70, 82, 58,  
    150, 188, 159, 126, 136, 122, 106, 115, 25, 70, 58, 45,  
    138, 110, 168, 160, 174, 120, 150, 139, 96, 104, 82, 60
  ))

# Fit a linear mixed-effects model (interaction terms included)
model <- lmer(life ~ material * temp + I(temp^2) + material * I(temp^2) + (1 | material), data = battery)

# Summary of the model
summary(model)

# Get estimated marginal means (LSMeans) for material at different temperatures
lsm <- emmeans(model, ~ material * temp)

# Print LSMeans
print(lsm)

# Optional: Perform pairwise comparisons
pairs(lsm)




# Fit model with no intercept and nesting via interaction
model <- lm(life ~ 0 + material + material:temp + material:t2, data = battery)

# View the summary
summary(model)

############################################################################################################

# Load required libraries
library(dplyr)
library(broom)   # for tidy model summaries
library(purrr)   # for mapping models





# Fit model by material
models_by_material <- battery %>%
  group_by(material) %>%
  group_map(~ tidy(lm(life ~ temp + t2, data = .x)), .keep = TRUE)

# Combine model summaries into one data frame
solf <- bind_rows(models_by_material, .id = "material")


# Print the estimates
print(solf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Step 1: Pivot wider to get estimates per material
solpl <- solf %>%
  select(material, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

# Step 2: Create prediction grid
ploteq <- solpl %>%
  rowwise() %>%
  mutate(pred_data = list(
    tibble(
      x = seq(15, 125, by = 10),
      life = `(Intercept)` + temp * x + t2 * x^2,
      material = material
    )
  )) %>%
  unnest(pred_data)

# Step 3: Plot predicted life vs x by material
ggplot(ploteq, aes(x = x, y = life, color = material)) +
  geom_line(size = 1) +
  geom_point(shape = 1) +
  labs(title = "Predicted Life by Material",
       x = "Temperature (x)",
       y = "Predicted Life") +
  theme_minimal()

##############################################################################################################


# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(plotly)

# Define the data manually
surface <- c(
  74, 64, 60, 79, 68, 73, 82, 88, 92, 99, 104, 96,
  92, 86, 88, 98, 104, 88, 99, 108, 95, 104, 110, 99,
  99, 98, 102, 104, 99, 95, 108, 110, 99, 114, 111, 107
)

# Build the full design
finish <- expand.grid(
  loff = c(0.2, 0.25, 0.3),
  lofd = c(1.5, 1.8, 2.0, 2.5),
  rep = 1:3
)
finish$surface <- surface
# Fit a linear model with interaction
model <- lm(surface ~ loff * lofd, data = finish)
library(emmeans)

# Compute LSMeans
lsm <- emmeans(model, ~ loff * lofd)
lsm_df <- as.data.frame(lsm)
# Create a surface plot
plot_ly(lsm_df, x = ~loff, y = ~lofd, z = ~emmean, type = "scatter3d", mode = "markers+lines",
        marker = list(size = 5), line = list(width = 2)) %>%
  layout(
    scene = list(
      xaxis = list(title = "loff"),
      yaxis = list(title = "lofd"),
      zaxis = list(title = "Estimated Surface")
    ),
    title = "3D Surface Plot of LSMeans"
  )
ggplot(lsm_df, aes(x = loff, y = lofd, fill = emmean)) +
  geom_tile() +
  geom_text(aes(label = round(emmean, 1)), color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Estimated Surface by loff and lofd",
       x = "loff", y = "lofd", fill = "Estimate") +
  theme_minimal()

######################################################################################################################
# Load required packages
library(lme4)
library(emmeans)
library(multcomp)

# Step 1: Create the dataset
powerdat <- data.frame(
  trt = rep(c(15, 20, 25, 30, 35), each = 5),
  mu = c(rep(11, 5), rep(12, 5), rep(15, 5), rep(18, 5), rep(19, 5))
)

# Convert treatment to factor
powerdat$trt <- factor(powerdat$trt)

# Step 2: Fit the linear model (GLMM with identity link can be approximated using lm)
mod <- lm(mu ~ trt, data = powerdat)
anova(mod)


dft=4
dfe = (5-1)*5    
sstr1= 250               
fcrit = qf(.95,dft,dfe,0)     
lambda = 5*sstr1/9         
power = 1 - pf(fcrit,dft,dfe,lambda) 
dft
dfe
sstr1
fcrit
lambda
power

###################################################################################################################
# Create the data
library(car)
library(emmeans)
library(lme4)

# Load data (example)
powerdat <- data.frame(
  trt = factor(rep(c(15, 20, 25,30,35), each = 5)),
  mu = rep(c(11,12,15,18,19),each=5)+rnorm(25)
)
# model fit
mod <- aov(mu ~ trt, data=powerdat)
summary(mod)
#emmeans
emm <- emmeans(mod, "trt")
emm
# Contrast: 20-25 vs 30-35
contrast_1 <- contrast(emm, list("20-25 vs 30-35" = c(0, 1, 1, -1, -1)))
# Contrast: 30 vs 35
contrast_2 <- contrast(emm, list("30 vs 35" = c(0, 0, 0, 1, -1)))
# View contrast results
summary(contrast_1)
summary(contrast_2)

#########################################################################################################
# Load power analysis package
library(pwr)

# Effect sizes for power analysis
means <- c(11, 12, 15, 18, 19)
n_per_group <- 5
sd <- 3

# Compute effect size for each contrast
effect1 <- (mean(means[2:3]) - mean(means[4:5])) / sd  # (20,25) vs (30,35)
effect2 <- (means[4] - means[5]) / sd                  # 30 vs 35

# Power for contrast 1
pwr.anova.test(k = 5, n = n_per_group, f = abs(effect1), sig.level = 0.05)

# Power for contrast 2
pwr.anova.test(k = 5, n = n_per_group, f = abs(effect2), sig.level = 0.05)


##########################################################################################################


# Load necessary library
library(lme4)

# Reconstruct the dataset
mpga <- read.table(header = FALSE, text = "
1 1 I D 15.5 
1 2 I B 16.3 
1 3 I C 10.8 
1 4 I A 14.7
2 5 I B 16.6 
2 6 I A 15.1 
2 7 I D 15.4 
2 8 I C 10.0
1 1 II B 33.9 
1 2 II C 26.6 
1 3 II A 31.1 
1 4 II D 34.0
2 5 II C 27.0 
2 6 II D 34.6 
2 7 II B 33.8 
2 8 II A 30.5
1 1 III C 13.2 
1 2 III A 19.4 
1 3 III D 17.1 
1 4 III B 19.7
2 5 III A 19.8 
2 6 III B 21.0 
2 7 III C 13.0 
2 8 III D 16.4
1 1 IV A 29.1 
1 2 IV D 22.8 
1 3 IV B 30.3 
1 4 IV C 21.6
2 5 IV D 23.1 
2 6 IV C 22.4 
2 7 IV A 28.9 
2 8 IV B 29.8
")

# Assign column names
colnames(mpga) <- c("square", "driver", "model", "blend", "mpg")

# Convert relevant variables to factors
mpga$square <- factor(mpga$square)
mpga$driver <- factor(mpga$driver)
mpga$model <- factor(mpga$model)
mpga$blend <- factor(mpga$blend)

# Fit the model using lme4::lmer
mod <- lmer(mpg ~ square * blend + 
              (1 | square:driver) + 
              (1 | square:model), 
            data = mpga)

# Display ANOVA table for fixed effects
anova(mod)

# Summary of model
summary(mod)


# Fit the model
mod2 <- lmer(mpg ~ square + blend + 
               (1 | square:driver) + 
               (1 | square:model), 
             data = mpga)

# Show summary
summary(mod2)

# ANOVA table
anova(mod2)

##########################################################################################################

# install.packages("agricolae")
library(agricolae)

# Set seed for reproducibility
set.seed(37430)

# Define row and column labels and treatment labels
row_labels <- c("Row 1", "Row 2", "Row 3", "Row 4")
col_labels <- c("Col 1", "Col 2", "Col 3", "Col 4")
treatment_labels <- c("Type 1", "Type 2", "Type 3", "Type 4")

# Generate Latin Square Design
latin_sq <- design.lsd(trt = treatment_labels, r = 1, seed = 37430)

# Replace default row and column numbers with custom labels
latin_sq$book$rows <- factor(rep(row_labels, each = 4))
latin_sq$book$cols <- factor(rep(col_labels, times = 4))

# Sort by rows and then columns
latin_sorted <- latin_sq$book[order(latin_sq$book$rows, latin_sq$book$cols), ]

# Print the resulting design
print(latin_sorted)





































