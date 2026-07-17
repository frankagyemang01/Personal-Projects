# Treating pgc_biomass and weed_biomass as numeric
# Convert to numeric
# Replace comma with dot and convert to numeric
# pcg_clean$PGC_biomass_kgHA   <- as.numeric(gsub(",", ".", pcg_clean$PGC_biomass_kgHA))
# pcg_clean$Weed_biomass_kgHA  <- as.numeric(gsub(",", ".", pcg_clean$Weed_biomass_kgHA))

## Reading in the data 
pcg <- read.csv("PGC_clean.csv")
library(dplyr)

pcg <- pcg %>%
  filter(Grass_Trt != "ctl")
pcg_ctv <- subset(pcg, location_id == "FAB_CTV_keystone")
pcg_unl <- subset(pcg, location_id == "FAB_UNL_keystone")


# Load tidyr package
library(tidyr)
## Treating "location id", "Replicate", "Rotation", "Stover", "Grass_Trt" as factors
factor_vars <- c("location_id", "Replicate", "Rotation", "Stover", "Grass_Trt")

# Apply conversion to factor
pcg_ctv[factor_vars] <- lapply(pcg_ctv[factor_vars], as.factor)
pcg_unl[factor_vars] <- lapply(pcg_unl[factor_vars], as.factor)






## Descriptive statistics using pgc_biomass as response variable
# Distribution of pgc_biomass
hist(pcg$PGC_biomass_kgHA)
hist(pcg_ctv$PGC_biomass_kgHA) #Corteva location
hist(pcg_unl$PGC_biomass_kgHA) # UNL location

# Distribution of weed_biomass
hist(pcg$Weed_biomass_kgHA)
hist(pcg_ctv$Weed_biomass_kgHA) #Corteva location
hist(pcg_unl$Weed_biomass_kgHA) # UNL location

###########################################################################################################

## Boxplots

# Boxplot of pgc_biomass effect for Corteva location
library(ggplot2)
ggplot(pcg_ctv, aes(x = Grass_Trt, y = PGC_biomass_kgHA)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Boxplot of PGC Biomass by Rotation for Corteva",
    x = "Grass_trt",
    y = "PGC Biomass (kg/ha)"
  ) +
  theme_minimal() +
  theme()  



# Boxplot of biomass for rotation
ggplot(pcg_ctv, aes(x = Rotation, y = PGC_biomass_kgHA)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of PGC Biomass by Rotation for Corteva",
       x = "Rotation",
       y = "PGC Biomass (kg/ha)") +
  theme_minimal()

##########################################################################################################
# Boxplot of pgc_biomass effect for UNL location
library(ggplot2)
ggplot(pcg_unl, aes(x = Grass_Trt, y = PGC_biomass_kgHA)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot of PGC Biomass by Rotation for UNL",
       x = "Grass_trt",
       y = "PGC Biomass (kg/ha)") +
  theme_minimal()


# Boxplot of biomass for rotation
ggplot(pcg_unl, aes(x = Rotation, y = PGC_biomass_kgHA)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot of PGC Biomass by Rotation for UNL",
       x = "Rotation",
       y = "PGC Biomass (kg/ha)") +
  theme_minimal()



##########################################################################################################
# Interaction Plots

with(pcg, interaction.plot(x.factor = Rotation,
                                trace.factor = Grass_Trt,
                                response = PGC_biomass_kgHA,
                                fun = mean,   # use mean response
                                type = "b",   # points + lines
                                pch = c(1,19),
                                col = c("blue","red","green"),
                                xlab = "Rotation",
                                ylab = "Mean PGC_biomass_kgHA",
                                trace.label = "Grass Treatment"))

with(pcg, interaction.plot(x.factor = Rotation,
                           trace.factor = Grass_Trt,
                           response = Weed_biomass_kgHA,
                           fun = mean,   # use mean response
                           type = "b",   # points + lines
                           pch = c(1,19),
                           col = c("blue","red","green"),
                           xlab = "Rotation",
                           ylab = "Mean Weed_biomass_kgHA",
                           trace.label = "Grass Treatment"))




########################################################################################################
# Boxplot of weed_biomass effect for UNL location
library(ggplot2)
ggplot(pcg_unl, aes(x = Grass_Trt, y = Weed_biomass_kgHA)) +
  geom_boxplot(fill = "blue") +
  labs(
    title = "Boxplot of Weed Biomass by Rotation for UNL",
    x = "Grass_trt",
    y = "Weed Biomass (kg/ha)"
  ) +
  theme_minimal() +
  theme()  

# Boxplot of biomass for rotation
ggplot(pcg_unl, aes(x = Rotation, y = Weed_biomass_kgHA)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of Weed Biomass by Rotation for UNL",
       x = "Rotation",
       y = "Weed Biomass (kg/ha)") +
  theme_minimal()


############################################################################################################

model.1 <- aov( PGC_biomass_kgHA ~ Rotation * Grass_Trt * location_id, data = pcg)
summary(model.1)
anova(model.1)

#CPGC_SepDes_Rcm
model.2 <- aov( Weed_biomass_kgHA  ~ Rotation * Grass_Trt * location_id, data = pcg)
summary(model.2)
anova(model.2)













