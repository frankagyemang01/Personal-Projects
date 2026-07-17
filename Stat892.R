# Stat 892 Independent Study
# Question 1c
library(MASS)
x <- matrix(c(1,-3,0,-3,1,-2,-1,2,2,-5,-1,-1), nrow = 3, ncol = 4, byrow = TRUE)
#print(x)
x_inv <- ginv(x)
print(x_inv)

