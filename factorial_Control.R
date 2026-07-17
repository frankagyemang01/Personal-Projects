set.seed(2025)

# Define levels
drug_type <- c("Soluble", "Non-soluble")
dosage <- c(10, 15)
replicates <- 3

# Generate factorial combinations
factorial_df <- expand.grid(Drug = drug_type, Dosage = dosage)
factorial_df <- factorial_df[rep(1:nrow(factorial_df), each = replicates), ]

# Simulate blood pressure (response variable)
factorial_df$BloodPressure <- with(factorial_df,
                                   130 -                           
                                     ifelse(Drug == "Soluble", 5, 0) +       
                                     ifelse(Dosage == 15, 3, 0) +            
                                     ifelse(Drug == "Soluble" & Dosage == 15, -2, 0) +  
                                     rnorm(nrow(factorial_df), mean = 0, sd = 5)        
)


# Add Control group
control_df <- data.frame(
  Drug = "control",
  Dosage = 0,
  BloodPressure = rnorm(replicates, mean = 135, sd = 5)
)

# Combine datasets
bp_data <- rbind(factorial_df, control_df)

# Convert to factors
bp_data$Drug <- as.factor(bp_data$Drug)
bp_data$Dosage <- as.factor(bp_data$Dosage)
library(dplyr)
bp_data <- bp_data %>%
  mutate(Control = ifelse( Dosage== 0, "y", "n"))
bp_data$Control <- as.factor(bp_data$Control)
write.csv(bp_data,"bpdata.csv",row.names = FALSE)



# Descriptive Statistics
## Histogram of Response
hist(bp_data$BloodPressure)



# Load ggplot2
library(ggplot2)
library(dplyr)


# Considering Species and Treatment
ggplot(bp_data, aes(x = Drug, y = BloodPressure, fill = Drug)) +
  geom_boxplot() +
  facet_wrap(~ Dosage) +
  labs(title = "Blood Pressure of Dosage and Drug Type") +
  theme_minimal()

bp_data_sub <- subset(bp_data, Drug != "Control")

# Base R interaction plot
interaction.plot(
  x.factor = bp_data_sub$Drug,
  trace.factor = bp_data_sub$Dosage,
  response = bp_data_sub$BloodPressure,
  type = "b",
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  pch = 16,
  ylab = "Average Blood Pressure",
  xlab = "Drug",
  trace.label = "Dosage",
  main = "Interaction Plot of Drug and Dosage (with Control Reference)"
)

# Add control mean line
abline(h = mean(bp_data$BloodPressure[bp_data$Drug == "Control"]),
       col = "darkgreen", lty = 2, lwd = 2)

legend("topleft", legend = "Control Mean", col = "darkgreen", lty = 2, lwd = 2, bty = "n")



# Model and Analysis
# Load necessary packages
library(lme4)
library(emmeans)
library(multcomp)

# Analyzing as a traditional 2 by 2 factorial
bp.m0 <- lm(BloodPressure ~ Dosage + Drug + Drug*Dosage, data = bp_data)
summary(bp.m0)
anova(bp.m0)
bp.m0.emmeans<- emmeans (bp.m0,  ~ Drug | Dosage)
bp.m0.emmeans



# Model with separate control effect
bp.m1 <- lm(BloodPressure ~ Control + Dosage + Drug + Drug*Dosage, data = bp_data)
summary(bp.m1)
anova(bp.m1)
bp.m1.emmeans<- emmeans (bp.m1,  ~ Drug | Dosage)
bp.m1.emmeans
em_drug <- emmeans(bp.m1, ~ Drug)

# Perform contrast: Soluble vs Non-Soluble
contrast(em_drug, method = "pairwise")






# Load required libraries
library(lme4)
library(dplyr)
library(emmeans)

# Original data with adjusted block-wise values
df <- data.frame(
  Block = rep(1:3, each = 5),
  Drug = c("Soluble", "Soluble", "Non-soluble", "Non-soluble", "Control"),
  Dosage = c(10, 15, 10, 15, 0),
  BloodPressure = c(
    128.1038 + 3, 127.9856 + 3, 136.3624 + 3, 136.5108 + 3, 132.8952 + 3,
    125.1782 - 3, 125.6001 - 3, 131.8549 - 3, 131.0215 - 3, 138.8245 - 3,
    128.8658, 124.2752, 129.1857, 124.2247, 140.3308
  ),
  Control = c("n", "n", "n", "n", "y",
              "n", "n", "n", "n", "y",
              "n", "n", "n", "n", "y")
)

# View the updated data
df

write.csv(df, "fact_block.csv", row.names = FALSE)


# Convert variables to factors
df$Block <- factor(df$Block)
df$Dosage <- factor(df$Dosage)
df$Drug <- factor(df$Drug)
df$Control <- factor(df$Control)

# Model: Block as random, Drug, Dosage and Control as fixed
model <- lmer(BloodPressure ~ Control + Drug * Dosage + (1 | Block), data = df)
summary(model)
# ANOVA table
anova(model)

# Estimated marginal means (EMMs) if needed
emmeans(model, ~ Drug * Dosage)
emmeans(model, ~ Control)




proc glimmix data = DrugData; 
title "BloodPressure Data";
class Drug Dosage Control;
model BloodPressure =  Control Drug(Control) Dosage(Control) Drug*Dosage(Control)/solution ddfm=kr;
lsmeans Control Drug(Control) Dosage(Control)  Drug*Dosage(Control)/diff adjust=dunnett;
run;

