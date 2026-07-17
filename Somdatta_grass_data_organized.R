# Analysis for Conference
## Descriptive Statistics - across sites, and within sites (Histograms and boxplots) for response variables

org_data <- read.csv("Somdatta_organized_data.csv")
unl <- subset(org_data, location == "FAB_UNL_keystone")
ctv <- subset(org_data, location == "FAB_CTV_keystone")



# Summary Statistics
summary(org_data)
summary(unl)
summary(ctv)


library(tidyr)

org_data <- org_data %>%
  separate(
    col = treatment, 
    into = c("replicate", "rotation", "stover", "grass", "crop_stage"), 
    sep = "\\."
  )

# Treating multiple variables as factors 
org_data[c("location" ,"replicate", "rotation", "stover", "grass", "crop_stage")] <- lapply(org_data[c("location" ,"replicate", "rotation", "stover", "grass", "crop_stage")], factor)
org_data$stover <- NULL
org_data$crop_stage <- NULL
library(dplyr)

org_data <- org_data %>%
  filter(grass != "ctl")
##############################################################################################################
# Histograms
# Loop through each numeric column and plot histogram
for (col in names(org_data)) {
  if (is.numeric(org_data[[col]])) {
    hist(org_data[[col]], 
         main = paste("Histogram of", col),
         xlab = col,
         col = "skyblue", 
         border = "white")
  }
}

#############################################################################################################


# Boxplots
## Boxplot by location
library(ggplot2)
responses <- c("crop_stand_count", "PGC_hgt", "CPGC_SepDes_Rcm", "CPGC_SepDes_Lcm")  

#location
for (resp in responses) {
  p <- ggplot(org_data, aes_string(x = "location", y = resp, fill = "location")) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", resp, "by location")) +
    theme_minimal()
  print(p)
}

#rotation
for (resp in responses) {
  p <- ggplot(org_data, aes_string(x = "rotation", y = resp, fill = "location")) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", resp, "by location")) +
    theme_minimal()
  print(p)
}

#rotation
for (resp in responses) {
  p <- ggplot(org_data, aes_string(x = "grass", y = resp, fill = "location")) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", resp, "by location")) +
    theme_minimal()
  print(p)
}


###########################################################################################################
# Interaction plots
with(org_data, interaction.plot(x.factor = rotation,
                          trace.factor = grass,
                          response = crop_stand_count,
                          fun = mean,   # use mean response
                          type = "b",   # points + lines
                          pch = c(1,19),
                          col = c("blue","red","green"),
                          xlab = "Rotation",
                          ylab = "Mean crop_stand_count",
                          trace.label = "Grass Treatment"))

with(org_data, interaction.plot(x.factor = rotation,
                                trace.factor = grass,
                                response = PGC_hgt,
                                fun = mean,   # use mean response
                                type = "b",   # points + lines
                                pch = c(1,19),
                                col = c("blue","red","green"),
                                xlab = "Rotation",
                                ylab = "Mean crop_stand_count",
                                trace.label = "Grass Treatment"))


with(org_data, interaction.plot(x.factor = rotation,
                                trace.factor = grass,
                                response = CPGC_SepDes_Rcm,
                                fun = mean,   # use mean response
                                type = "b",   # points + lines
                                pch = c(1,19),
                                col = c("blue","red","green"),
                                xlab = "Rotation",
                                ylab = "Mean crop_stand_count",
                                trace.label = "Grass Treatment"))

with(org_data, interaction.plot(x.factor = rotation,
                                trace.factor = grass,
                                response = CPGC_SepDes_Lcm,
                                fun = mean,   # use mean response
                                type = "b",   # points + lines
                                pch = c(1,19),
                                col = c("blue","red","green"),
                                xlab = "Rotation",
                                ylab = "Mean crop_stand_count",
                                trace.label = "Grass Treatment"))
############################################################################################################
 # ANOVA
# PGCHeight
model.1 <- aov( PGC_hgt ~ rotation * grass + location, data = org_data)
summary(model.1)
anova(model.1)

#CPGC_SepDes_Rcm
model.2 <- aov( CPGC_SepDes_Rcm  ~ rotation * grass + location, data = org_data)
summary(model.2)
anova(model.2)

#CPGC_SepDes_Lcm
model.3 <- aov( CPGC_SepDes_Lcm  ~ rotation * grass + location, data = org_data)
summary(model.3)
anova(model.3)

# Stand Count assumed to follow Poisson Distribution
model.4 <- glm(crop_stand_count ~ rotation * grass + location,
             data = org_data,
             family = poisson(link = "log"))

summary(model.4)




