# Lecture 10 - ANCOVA
# Create the data
ancov <- read.table(header = FALSE, text = '
C 385 1.91
C 413 2.55
C 435 2.03
C 403 1.88
C 432 2.33
C 439 2.11
C 407 2.54
C 399 1.86
1 330 1.55
1 340 1.92
1 353 2.31
1 384 2.26
1 325 1.48
1 374 2.54
1 323 1.3
1 345 2.21
2 421 2.64
2 379 1.81
2 391 2.09
2 371 2.36
2 355 1.62
2 364 2.08
2 389 2.44
2 373 1.69
3 345 1.46
3 380 1.26
3 337 1.4
3 414 2.31
3 368 2.08
3 345 1.56
3 372 1.77
3 410 1.77
')

# Add column names
colnames(ancov) <- c("trt", "x", "y")

# Compute overall mean of x
xbar <- mean(ancov$x)

# Group summaries (like proc means by trt)
library(dplyr)

group_means <- ancov %>%
  group_by(trt) %>%
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    .groups = "drop"
  )

print("Groupwise means of x and y:")
print(group_means)

# Center x around the overall mean
ancov2 <- ancov %>%
  mutate(
    xbar = xbar,
    center = x - xbar
  )

# Display the updated dataset
print("Centered data with xbar:")
print(ancov2[, c("trt", "y", "x", "center", "xbar")])
############################################################################################################
## With no covariate
library(emmeans)

# Fit the linear model
model.ancova0 <- lm(y ~ trt, data = ancov2)

# Get estimated marginal means (LSMeans)
lsmeans <- emmeans(model.ancova0, ~ trt)

# Display LSMeans
print(lsmeans)

# Pairwise comparisons (trt differences)
pairwise_diff <- contrast(lsmeans, method = "pairwise")
print(pairwise_diff)
###########################################################################################################
## With Covariate

# Fit the linear model
model.ancova1 <- lm(y ~ trt + center, data = ancov2)

# Show model summary (similar to solution option in SAS)
summary(model.ancova1)

# Get LSMeans for trt and pairwise comparisons
emm <- emmeans(model.ancova1, "trt")
print(emm)

# Differences between LSMeans (trt/diff in SAS)
pairs(emm)
###########################################################################################################
# Load necessary package
library(dplyr)

# Step 1: Group means (xbari)
ancovd <- ancov2 %>%
  group_by(trt) %>%
  summarise(xbari = mean(x), .groups = "drop")

# Step 2: Merge group means into original dataset
ancov3 <- ancov2 %>%
  left_join(ancovd, by = "trt")

# Step 3: Compute centered variables
xbar <- mean(ancov2$x)  # Grand mean
ancov3 <- ancov3 %>%
  mutate(
    centi = x - xbari,
    centi2 = centi^2,
    centt = xbari - xbar,
    centt2 = centt^2
  )

# Step 4: Sum of centi2 and centt2
summary_totals <- ancov3 %>%
  summarise(
    sum_centi2 = sum(centi2),
    sum_centt2 = sum(centt2)
  )

# View results
print(summary_totals)
#########################################################################################################
# Load required package
library(emmeans)

# Fit the linear model
model.ancova2 <- lm(center ~ trt, data = ancov2)

# Show model summary
summary(model.ancova2)

# Get LSMeans (estimated marginal means) for trt
emm <- emmeans(model.ancova2, "trt")
print(emm)
#########################################################################################################
# Fit the model with interaction between center and trt
model_interaction <- lm(y ~ trt * center, data = ancov2)
# View the model summary (similar to /solution in SAS)
summary(model_interaction)

