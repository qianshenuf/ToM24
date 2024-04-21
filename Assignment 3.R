#install and import the package
install.packages("lavaan")
install.packages("semTools")
install.packages('readr')
install.packages("ltm")

library (readr)
library(lavaan)
library(semTools)
library("semTools")

# Uploading the data from github 

urlfile="https://raw.githubusercontent.com/jinnieshinufl/EDF6436-Spring2024-/main/Assignment3Data2.csv"
data<-read_csv(url(urlfile), col_names=FALSE) 
data<- as.data.frame(data) 

# Assignment 1 data  is called "data" with the column names X1 ~ X10


#Q1.1
model <- ' 
  latent_variable =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +
                     X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 +
                     X21 + X22 + X23 + X24
'
fit <- cfa(model, data=data, estimator="WLSMV")
summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Q1.5

reliability(fit)

#Q2.1
library(ltm)

fit_2PL <- ltm(data ~ z1)
summary(fit_2PL)

#Q2.2
item.fit(fit_2PL,simulate.p.value=T)

#Q2.3
estimates <- coef(fit_2PL)
estimates <- as.data.frame(estimates)
estimates

#Q2.5
plot(fit_2PL, type = 'ICC', items = 2)

#Q2.6
plot(fit_2PL,  type=c("IIC"), main="Item Information Function - All")




