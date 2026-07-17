## CO2 example - RCBD

# Load packages
library(lme4)       
library(lmerTest)   
library(emmeans)   

# Data
co2 <- data.frame(
  block = factor(rep(1:4, each = 2)),
  treat = factor(rep(c("a", "b"), 4)),
  yield = c(6.21, 6.41,
            6.25, 6.42,
            6.10, 6.26,
            6.14, 6.30)
)

# --- Model 1: Block as Random Effect ---
model.co2.random.block <- lmer(yield ~ treat + (1 | block), data = co2)

# ANOVA table (Type III test of fixed effects)
anova(model.co2.random.block)

# Covariance parameters (like SAS's covariance parameter estimates)
summary(model.co2.random.block)

# LSMeans for treat
emmeans(model.co2.random.block, ~ treat)
############################################################################################################
# --- Model 2: Block as Fixed Effect ---
model.co2.fixed.block <- lm(yield ~ block + treat, data = co2)

# ANOVA table
anova(model.co2.fixed.block)

# LSMeans for treat
emmeans(model.co2.fixed.block, ~ treat)
# Comparisons between treatments (if needed)
pairs(emmeans(model.co2.fixed.block, ~ treat))
pairs(emmeans(model.co2.fixed.block, ~ treat))
##########################################################################################################
diff_df <- co2 %>%
  arrange(block, treat) %>%
  group_by(block) %>%
  summarize(diff = first(yield) - last(yield), .groups = "drop")


# Summary statistics (equivalent to PROC MEANS)
summary_stats <- summary(diff_df$diff)
sd_diff <- sd(diff_df$diff)
n_diff <- length(diff_df$diff)

cat("Mean:", mean(diff_df$diff), "\n")
cat("Standard Deviation:", sd_diff, "\n")
cat("N:", n_diff, "\n")

# Run t-test (equivalent to PROC TTEST)
ttest_result <- t.test(diff_df$diff)
print(ttest_result)
############################################################################################################
# Load required package
library(ggplot2)

# Create the plot
ggplot(co2, aes(x = treat, y = yield, group = block, shape = block, linetype = block)) +
  geom_line(aes(color = block)) +
  geom_point(aes(color = block), size = 3) +
  labs(
    title = "Yield vs. Treatment by Block",
    x = "Treatment",
    y = "Yield"
  ) +
  scale_shape_manual(values = c(16, 15, 18, 1)) +       # circle, square, diamond, open circle
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "twodash")) +
  scale_color_manual(values = rep("black", 4)) +        # All black lines/symbols
  theme_minimal()



