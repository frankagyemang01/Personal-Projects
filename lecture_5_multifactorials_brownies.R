# Multifactorial - Brownies example


brownies_raw <- "
g s e 11 9 10 10 11 10 8 9
a s e 15 10 16 14 12 9 6 15
g m e 9 12 11 11 11 11 11 12
a m e 16 17 15 12 13 13 11 11
g s c 10 11 15 8 6 8 9 14
a s c 12 13 14 13 9 13 14 9
g m c 10 12 13 10 7 7 17 13 
a m c 15 12 15 13 12 12 9 14
"

# Convert to a data frame
brownies <- read.table(text = brownies_raw, header = FALSE,
                       col.names = c("pan", "stir", "mix",
                                     paste0("scrump", 1:8)))

# Reshape to long format like the SAS DO loop with `panel=1 to 8`
library(tidyr)
library(dplyr)

brownies_long <- brownies %>%
  pivot_longer(cols = starts_with("scrump"), names_to = "panel", values_to = "scrump") %>%
  mutate(panel = as.integer(gsub("scrump", "", panel)))

############################################################################################################
# Convert categorical variables to factors
brownies_long <- brownies_long %>%
  mutate(across(c(pan, stir, mix), as.factor))

# Fit full factorial model
model.brownies <- lm(scrump ~ pan * stir * mix, data = brownies_long)

# Get Type III Tests of Fixed Effects
anova(model.brownies)
############################################################################################################
# LSMeans (estimated marginal means) and pairwise differences
emm_pan   <- emmeans(model.brownies, ~ pan)
emm_stir  <- emmeans(model.brownies, ~ stir)
emm_mix   <- emmeans(model.brownies, ~ mix)

# View LSMeans
summary(emm_pan)
summary(emm_stir)
summary(emm_mix)

# Pairwise comparisons (differences)
pairs(emm_pan)
pairs(emm_stir)
pairs(emm_mix)
#############################################################################################################
## Data set 2
# Load libraries
library(dplyr)
library(emmeans)
library(ggplot2)
library(car)


dat.2 <- read.table(header = FALSE, text = "
1 1 1 228.2
1 1 1 216.9
1 1 1 220.4
1 1 2 249.7
1 1 2 232.1
1 1 2 248.7
1 2 1 239.5
1 2 1 243.9
1 2 1 235.9
1 2 2 252.0
1 2 2 241.3
1 2 2 249.9
2 1 1 264.5
2 1 1 253.7
2 1 1 246.4
2 1 2 283.4
2 1 2 263.3
2 1 2 264.5
2 2 1 227.1
2 2 1 225.6
2 2 1 243.5
2 2 2 245.6
2 2 2 236.6
2 2 2 254.8
")

colnames(dat.2) <- c("a", "b", "c", "y")

# Convert a, b, c to factors
dat.2 <- dat.2 %>%
  mutate(across(c(a, b, c), factor))

# Fit the model (full factorial)
mod.dat.2 <- lm(y ~ a * b * c, data = dat.2)

# Type III Tests of Fixed Effects
anova(mod.dat.2)

# LSMeans for c, and interaction a*b with slice comparisons
emm_c   <- emmeans(mod.dat.2, ~ c)
emm_ab  <- emmeans(mod.dat.2, ~ a * b)

# Pairwise differences
pairs(emm_c)
pairs(emm_ab)

# Sliced comparisons
# Slice b within levels of a
contrast(emm_ab, interaction = "pairwise", by = "a")

# Slice a within levels of b
contrast(emm_ab, interaction = "pairwise", by = "b")

# Interaction plot (a*b means by b)
emm_df <- as.data.frame(emm_ab)
ggplot(emm_df, aes(x = b, y = emmean, group = a, color = a)) +
  geom_point() +
  geom_line() +
  labs(title = "Interaction Plot: a*b", y = "LS Mean of y") +
  theme_minimal()
###########################################################################################################
# Create the data frame equivalent to the SAS 'three' dataset
dat.3 <- data.frame(
  a = c(rep(1, 12), rep(2, 12)),
  b = rep(c(1, 1, 2, 2), each = 3, times = 2),
  c = rep(c(1, 2), each = 3, times = 4),
  y = c(
    234.7, 221.3, 213.4,
    238.5, 241.8, 223.3,
    259.2, 227.9, 256.2,
    257.0, 266.0, 255.7,
    235.2, 247.6, 254.2,
    268.0, 281.1, 276.8,
    251.3, 277.9, 266.8,
    244.1, 259.2, 246.4
  )
)






dat.3$a <- factor(dat.3$a)
dat.3$b <- factor(dat.3$b)
dat.3$c <- factor(dat.3$c)

library(car)
model.dat.3 <- lm(y ~  a * b * c, data = dat.3)
Anova(model.dat.3, type = "III")
###########################################################################################################
library(emmeans)

# Full interaction
emm_abc <- emmeans(model.dat.3, ~ a * b * c)
pairs(emm_abc)

# LSMeans with plot (like `sliceby=a plotby=b` in SAS)
emmip(model.dat.3, a ~ c | b, CIs = TRUE, type = "response")

# Simple effects of a within each level of c
emmeans(model.dat.3, pairwise ~ a | c)
# Simple effects of b within each level of c
emmeans(model.dat.3, pairwise ~ b | c)

# Slicediff on a*b*c
emmeans(model.dat.3, pairwise ~ a * b * c,
        by = c("b", "c")) 
# slicediff = a*c
emmeans(model.dat.3, pairwise ~ a * b * c,
        by = c("a", "c")) 
# slicediff = a*b
emmeans(model.dat.3, pairwise ~ a * b * c,
        by = c("a", "b"))  











