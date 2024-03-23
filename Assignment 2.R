#install and import the package
library (readr)
library(psych)
library(EFAtools)
# Uploading the data from github 

urlfile="https://raw.githubusercontent.com/jinnieshinufl/EDF-6436/main/Assignment3Data1.csv"
data<-read_csv(url(urlfile), col_names=TRUE) 
data<- as.data.frame(data) 
data<- data[, -c(1, 2)]

BARTLETT(data)
KMO(data)

#Assumption Checking
scree(data)
fa.parallel(data, fm = "ml")

#bulid the model
factor <- fa(data,  # data
                 nfactors = 2, # number of factors 
                 fm = "pa",  # method = 
                 rotate = "Promax") # rotation 

print(factor, cut = .33, sort = TRUE, digits = 3)

fa.diagram(factor, digits = 3, main = "Factor Diagram", 
           cut = .33, 
           simple = F, 
           errors = T)
#Factor loadings
PattM   <- factor$loadings
print(PattM)

#communality and uniquenesses
factor$communality[12]
factor$uniquenesses[12]

#factor score table
factor_scores <- as.data.frame(factor$scores)
student_23_scores <- factor_scores[23, ]
student_30_scores <- factor_scores[30, ]
print("Student 23 Factor Scores:")
print(student_23_scores)
print("Student 30 Factor Scores:")
print(student_30_scores)
