# Creating a dataset in R

dat_wheat <- data.frame(
  Variety = factor(c(rep("rawhide", 4), rep("durum", 4), rep("scout", 4))),
  Y = c(14.3, 14.5, 11.5, 13.6,
        18.0, 17.8, 12.6, 11.2,
        11.0, 12.1, 10.5, 12.8)
)

 #read.csv("AFB1.csv")
# View the data frame
print(dat_wheat)

# Descriptive Data Analysis
## Distribution of the response variable
hist(dat_wheat$Y)

## Boxplot
boxplot(Y~Variety, data = dat_wheat,
        xlab = "Variety",
        ylab = "Y")

# Summary statistics
summary(dat_wheat$Y)
aggregate(Y ~ Variety, data = dat_wheat, summary)

## Fit a model
model.wheat <- lm(Y ~ Variety, data = dat_wheat)


# Type III ANOVA 
anova(model.wheat)

Source 
