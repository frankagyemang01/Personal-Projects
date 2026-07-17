## R code to get the same results for SAS

# Load required packages
library(car)       
library(emmeans)   
library(dplyr)     

# Creating the dataset or you can also read in the .csv file
temp <- data.frame(
  treatment = factor(c(rep("x", 6), rep("y", 6), rep("saline", 6))),
  tempdiff = c(0.3, 0.0, 0.6, 0.0, -0.3, 0.2,
               0.1, 0.1, 0.2, 0.3, 0.1, 0.1,
               2.2, 1.6, 0.8, 1.8, 1.4, 0.4)
)

# Summary statistics
summary(temp$tempdiff)
aggregate(tempdiff ~ treatment, data = temp, summary)

# Fit the model
model.rabbit <- lm(tempdiff ~ treatment, data = temp)

# Type III ANOVA (requires contrasts to be set to contr.sum)
options(contrasts = c("contr.sum", "contr.poly"))
Anova(model.rabbit, type = "III")

# To Get Least Squares Means
emm.rabbit <- emmeans(model.rabbit, ~ treatment)
emm.rabbit

# Differences of LSMeans (pairwise)
pairs(emm.rabbit)

#  Contrast (X vs Y)
# order: saline, x, y. SAS uses the first level as control 
contrast(emm.rabbit, list("X vs Y" = c(0, 1, -1)))  

#######################################################################################








