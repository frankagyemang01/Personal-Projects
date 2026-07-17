## Boxplot of Fungi Isolate and Hybrid for data 1 and 2
library(ggplot2)
treatments <- unique(hybrid$Treatment)

for (tr in treatments) {
  temp <- subset(hybrid, Treatment == tr)
  
  p <- ggplot(temp, aes(x = Hybrid, y = Crown.Rot.Percentage, fill = Hybrid)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste("Crown Rot Percentage for Treatment:", tr),
      x = "Hybrid",
      y = "Crown Rot (%)"
    )
  
  print(p)
}

## For Data Set 2
library(ggplot2)
treatments2 <- unique(hybrid2$Treatment)

for (tr in treatments2) {
  temp2 <- subset(hybrid2, Treatment == tr)
  
  p <- ggplot(temp2, aes(x = Hybrid, y = Crown.Rot.Percentage, fill = Hybrid)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste("Crown Rot Percentage for Treatment:", tr),
      x = "Hybrid",
      y = "Crown Rot (%)"
    )
  
  print(p)
}
