## ANOVA General Structures
# Analysis for Conference
## Descriptive Statistics - across sites, and within sites (Histograms and boxplots) for response variables
## Reading in all data sets

standcount <- read.csv("Stand_count.csv")
standcount$X <- NULL
standcount$plot_id <- NULL
standcount$X.1 <- NULL

summary(standcount)
standcount$Crop <- as.factor(standcount$Crop)
standcount$Grass <- as.factor(standcount$Grass)
standcount$Rep <- as.factor(standcount$Rep)

cpgc_r <- read.csv("CPGC_Dst_R.csv")
cpgc_r$plot_id <- NULL
cpgc_r$Rep <- as.factor(cpgc_r$Rep)
cpgc_r$Crop <- as.factor(cpgc_r$Crop)
cpgc_r$Grass <- as.factor(cpgc_r$Grass)

cpgc_l <- read.csv("CPGC_Dst_L.csv")
cpgc_l$plot_id <- NULL
cpgc_l$Rep <- as.factor(cpgc_l$Rep)
cpgc_l$Crop <- as.factor(cpgc_l$Crop)
cpgc_l$Grass <- as.factor(cpgc_l$Grass)


#####################################################################################
####################################################################################


## Relative Frequency Density Plots
library(ggplot2)

ggplot(cpgc_r, aes(x = CPGC_SepDstR_cm)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "black") +
  labs(
    x = "CPGC_SepDstL_cm",
    y = "Relative Frequency (Density)",
    title = "Relative Frequency Density Plot of CPGC_SepDstL_cm"
  ) +
  theme_minimal()

##################################################################################
##################################################################################
ggplot(cpgc_l, aes(x = CPGC_SepDstL_cm)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "black") +
  labs(
    x = "CPGC_SepDstR_cm",
    y = "Relative Frequency (Density)",
    title = "Relative Frequency Density Plot of CPGC_SepDstR_cm"
  ) +
  theme_minimal()



################################################################################
###############################################################################

# Relative  Frequency Density Plot of Standcount

ggplot(standcount, aes(x = Standcount)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "black") +
  labs(
    x = "Standcount",
    y = "Relative Frequency (Density)",
    title = "Relative Frequency Density Plot of Standcount"
  ) +
  theme_minimal()

# Boxplot of Standcount

library(ggplot2)

ggplot(data = standcount, 
       aes(x = interaction(Crop, Grass), 
           y = Standcount, 
           fill = Grass)) +
  geom_boxplot(color = "black") +
  facet_wrap(~ location_id, scales = "free") +
  labs(
    x = "Crop and Grass Combination",
    y = "Stand Count",
    title = "Boxplot of Stand Count by Crop and Grass Across Locations",
    fill = "Grass"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


## Write data into .csv file
write.csv(standcount, "Stand_count.csv", row.names = FALSE)
zero_count <- sapply(standcount, function(x) mean(x == 0, na.rm = TRUE) * 100)
zero_count




# Boxplot of CPGC_R

library(ggplot2)

ggplot(data = cpgc_r, 
       aes(x = interaction(Crop, Grass), 
           y = CPGC_SepDstR_cm, 
           fill = Grass)) +
  geom_boxplot(color = "black") +
  facet_wrap(~ location_id, scales = "free") +
  labs(
    x = "Crop and Grass Combination",
    y = "CPGC_SepDstR_cm",
    title = "Boxplot of CPGC_SepDstR_cm by Crop and Grass Across Locations",
    fill = "Grass"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


## Write data into .csv file
write.csv(cpgc_r, "CPGC_Dst_R.csv", row.names = FALSE)
zero_count <- sapply(cpgc_r, function(x) mean(x == 0, na.rm = TRUE) * 100)
zero_count

# Boxplot of CPGC_L

library(ggplot2)

ggplot(data = cpgc_l, 
       aes(x = interaction(Crop, Grass), 
           y = CPGC_SepDstL_cm, 
           fill = Grass)) +
  geom_boxplot(color = "black") +
  facet_wrap(~ location_id, scales = "free") +
  labs(
    x = "Crop and Grass Combination",
    y = "CPGC_SepDstL_cm",
    title = "Boxplot of CPGC_SepDstL_cm by Crop and Grass Across Locations",
    fill = "Grass"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


## Write data into .csv file
write.csv(cpgc_l, "CPGC_Dst_L.csv", row.names = FALSE)
zero_count <- sapply(cpgc_l, function(x) mean(x == 0, na.rm = TRUE) * 100)
zero_count




################################################################################
###############################################################################


## Corn_height Data

corn_hgt <- read.csv("Corn_height_data_UNL_TLI.csv")
corn_hgt$Year <- NULL
corn_hgt$location_id <- as.factor(corn_hgt$location_id)
corn_hgt$plot_id <- NULL
corn_hgt$Crop <- NULL


## Relative Frequency Density Plot
ggplot(corn_hgt, aes(x = Corn_height_m)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "black") +
  labs(
    x = "Corn_height_m",
    y = "Relative Frequency (Density)",
    title = "Relative Frequency Density Plot of Corn_height_m"
  ) +
  theme_minimal()

# Boxplot of Corn height

ggplot(corn_hgt, aes(x = location_id, y = Corn_height_m, fill = location_id)) +
  geom_boxplot(color = "black") +
  labs(
    x = "location_id",
    y = "Corn_height_m",
    title = "Boxplot of Corn_height_m",
    fill = "location_id"
  ) +
  theme_minimal()


write.csv(corn_hgt, "Corn_height_data_UNL_TLI.csv", row.names = FALSE)


## Anova Table For Corn Height
library(lme4)
model.corn.height <- lm(Corn_height_m ~ location_id*Grass, data = corn_hgt) 
anova(model.corn.height)
####################################################################################################
## Plant Height
pgc_ht <- read.csv("PGC_height.csv")
ggplot(pgc_ht, aes(x = PGC_Hgt_cm)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "black") +
  labs(
    x = "PGC_height_cm",
    y = "Relative Frequency (Density)",
    title = "Relative Frequency Density Plot of PGC_height_cm"
  ) +
  theme_minimal()



######################################################################################################
######################################################################################################

## PGC_biomass and Weed Biomass Data
pgc_data <- read.csv("PGC_weed_biomass_UNL_CTV.csv")
pgc_data$location_id <- as.factor(pgc_data$location_id)
pgc_data$Crop <- as.factor(pgc_data$Crop)
pgc_data$Grass <- as.factor(pgc_data$Grass)
## Removing all control
library(dplyr)

pgc_data <- pgc_data %>%
  filter(Grass != "Control")

write.csv(pgc_data, "PGC_weed_biomass_UNL_CTV_Clean.csv", row.names = FALSE)



## Relative frequency density plot of PGC height and Weed Biomass

# PGC biomass
ggplot(pgc_data, aes(x = PGC_biomass_KgHa)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "black") +
  labs(
    x = "PGC_biomass_KgHa",
    y = "Relative Frequency (Density)",
    title = "Relative Frequency Density Plot of PGC_biomass_KgHa"
  ) +
  theme_minimal()

# Weed Biomass
ggplot(pgc_data, aes(x = Weed_biomass_KgHa)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "black") +
  labs(
    x = "Weed_biomass_KgHa",
    y = "Relative Frequency (Density)",
    title = "Relative Frequency Density Plot of Weed_biomass_KgHa"
  ) +
  theme_minimal()

ggplot(pgc_data, aes(x = Crop, y = Weed_biomass_KgHa, fill = Grass)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  facet_wrap(~ location_id, scales = "free_y") +
  labs(
    title = "Weed_biomass_KgHa by Crop and Grass Treatment Across Locations",
    x = "Crop",
    y = "Weed_biomass_KgHa",
    fill = "Grass Treatment"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )


ggplot(pgc_data, aes(x = Crop, y = PGC_biomass_KgHa, fill = Grass)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  facet_wrap(~ location_id, scales = "free_y") +
  labs(
    title = "PGC_biomass_KgHa by Crop and Grass Treatment Across Locations",
    x = "Crop",
    y = "PGC_biomass_KgHa",
    fill = "Grass Treatment"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )












