#Empty the environment and re-run if needed
#rm(list=ls()) 

library(caret)
#library(mlbench)
library(haven) #Read dta files
library(stargazer)

#Source the working directory where you're located
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

lending_df <- read_dta('../Data/Clean/filtered_lending_club.dta')

#Set seed to make our code reproducible in the future, if needed
set.seed(42)

#Recursive Feature elimination

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(lending_df[sample(nrow(lending_df),5000),-4], as.vector(unlist(c(lending_df[sample(nrow(lending_df),5000), 4]))),
               sizes=c(1:15), rfeControl=control)

# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

grep("int_rate", colnames(lending_df))

