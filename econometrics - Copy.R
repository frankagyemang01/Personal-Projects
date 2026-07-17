
############################################################3
### Econometrics Question4
#######################################################

library(dplyr) 
load("wage2.RData", wage_env <- new.env() )

# Access individual variables in the RData file using '$' operator
wage <- wage_env$data 
#Selecting those variables which are to be used in the assignment
wage1 <- wage%>%
  select(wage,educ,exper)
## Variance Covariance matrix
cov(wage1)

##correlation matrix
cor(wage1)

# OLS for the data
wage_model <- lm(wage ~ educ + exper,data = wage1)
summary_result <-summary(wage_model)
cov(wage1$educ,wage1$exper)

standard_errors <- summary_result$coefficients[, "Std. Error"]
# List all of the variable names in RData:
#ls(wage_env)



##Coursera
theta = seq(from=0,to=1,by=0.01)
plot(theta,dbeta(theta,1,1),type="l")
plot(theta,dbeta(theta,4,2),type="l")
plot(theta,dbeta(theta,8,4),type="l")
1-pbeta(.25,8,4)
1-pbeta(.50,8,4)
1-pbeta(.80,8,4)

lines(theta,dbeta(theta,41,11))
plot(theta,dbeta(theta,41,11),type="l")
lines(theta,dbeta(theta,8,4),lty=2)

lines(theta,dbinom(33,sample=40,p=theta),lty=3)
lines(theta,44*dbinom(33,sample=40,p=theta),lty=3)
1-pbeta(.25,41,11)
1-pbeta(.50,41,11)
1-pbeta(.80,41,11)
qbeta(.025,32,20)

qbeta(.975,32,20)

qbeta(0.975,8,16)
1-pbeta(.35,8,41)

qgamma(0.05,67,6)

# 7) Estimate by simulation: draw 1,000 samples from each and see how often 
#    we observe theta1>theta2

theta1=rbeta(1000,41,11)
theta2=rbeta(1000,32,20)
mean(theta1>theta2)




