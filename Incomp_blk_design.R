##### R code to generate a Balance Incomplete Block Design
library(agricolae)
# 10 treatments and k=4 size block
trt<-c("A","B","C","D","E","F","G","H","I","J")
k<-4 ##specifies your block size
outdesign<-design.bib(trt,k,serie=2,seed =41,kinds ="Super-Duper") # seed = 41 #serie represents lambda
print(outdesign$parameters)
book<-outdesign$book
plots <-as.numeric(book[,1])
matrix(plots,byrow=TRUE,ncol=k)
print(outdesign$sketch)




#########################################################
###Analysis Of a BIB
########################################################
library(lsmeans)
library(car) 
b=read.csv('C:/. . ./bulls.csv', header=TRUE)
judge=factor(b$judge)
bull=factor(b$bull) 
res=lm(score~judge+bull,data=b)
Anova(res,type="III")
lsmeans(res,list(~bull,~judge))
bull13.vs.6.11.12 =fit.contrast(res, "bull", c(0,0,0,0,0,1/3,0,0,0,0,1/3,1/3,-1))
bull13.vs.6.11.12 
res=lm(score~judge+bull,data=b)
Anova(res,type="III")















##########################################################
###Kent's Code
#########################################################
library(lsmeans)
library(car) 
b=read.csv('C:/. . ./bulls.csv', header=TRUE)
judge=factor(b$judge)
bull=factor(b$bull) 
res=lm(score~judge+bull,data=b)
Anova(res,type="III")
lsmeans(res,list(~bull,~judge))
bull13.vs.6.11.12 =fit.contrast(res, "bull", c(0,0,0,0,0,1/3,0,0,0,0,1/3,1/3,-1))
bull13.vs.6.11.12 
res=lm(score~judge+bull,data=b)
Anova(res,type="III")
