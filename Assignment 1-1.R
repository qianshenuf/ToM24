#install and import the package
install.packages('readr')
library(readr)
library(psych)
library(truncnorm)
library(ggplot2)
library(CTT)
library(MASS)
library(msm)
library(ltm)
# Uploading the data from github 

urlfile="https://raw.githubusercontent.com/jinnieshinufl/EDF-6436/main/assignment/data/timss_g4_2015_10_reading_items.csv"
data<-read_csv(url(urlfile), col_names=FALSE) 
data<- as.data.frame(data) 

# Assignment 1 data  is called "data" with the column names X1 ~ X10
head(data)
avg_values <- colMeans(data, na.rm = TRUE)
max_avg_column <- names(which.max(avg_values))
max_avg_column


table(data$X1, data$X2)
table(data$X3, data$X5)

pjk1 = 289/831
pj1 = (320+289)/831
pk1 = (11+289)/831
qj1 = 1-pj1
qk1 = 1-pk1
rho1 = (pjk1-(pj1*pk1))/(sqrt(pj1*qj1*pk1*qk1))  
print(rho1)

pjk2 = 180/831
pj2 = (23+180)/831
pk2 = (521+180)/831
qj2 = 1-pj2
qk2 = 1-pk2
rho2 = (pjk2-(pj2*pk2))/(sqrt(pj2*qj2*pk2*qk2))  
print(rho2)

result = itemAnalysis(data)
result$itemReport$pBis[3]
result$itemReport$pBis[5]

cronbach.alpha(data)

TSS <- apply(data, 1, sum)
SEM <- sqrt(1-0.761)*sd(TSS, na.rm = TRUE)
SEM

9-1.96*SEM
9+1.96*SEM