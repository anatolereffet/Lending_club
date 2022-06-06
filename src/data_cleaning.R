#Empty the environment and re-run if needed
#rm(list=ls()) 

library(dplyr)
library(haven) #Read dta files 
library(purrr) #Map NaN values across the dataset
library(caret) #Recursive Feature Elimination package
library(stargazer) #ASCII/Latex Format table 
library(matchmaker) #replicates the dictionary function of python, to allow mapping
library(readxl) #enables excel import
library(fastDummies) #dummy library
library(VIM) #count infinite values that are within our dataset

#Source the working directory where you're located
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Raw Import of the data and the two hand-labelled datasets 
lending_df <- read_dta('../Data/Raw/Lending_Club.dta')

#Hand-labelled data on 
#1 = Blue Collar, #2 = White Collar #3 = Pink Collar
dictionary <- read_excel('../Data/Clean/dictionary_emp_title.xlsx')

#Census data encoded
dict_big_city <- read_excel('../Data/Clean/dictionary_big_cities_census.xlsx')

#Inner Join on inflation rate, labor participation rate, employment-population ratio
add_econ_variables <- read_excel('../Data/Clean/add_econ_variables.xlsx')

#duplicate the column and slice it up 
sliced_1 <- lending_df[, 1:9]
sliced_1$encode_emp_title <- lending_df$emp_title
lending_df <- cbind(sliced_1, lending_df[, 10:147])

#remove the variable from our environment
rm(sliced_1)

#Map the employment dictionary
cleaned_lending <- match_df(lending_df,
                    dictionary = dictionary,
                    from = "Title",
                    to = "Encoding",
                    by = "column"
                    )

#Filter out the non mapped values
cleaned_lending <- filter(cleaned_lending, cleaned_lending$encode_emp_title < 4)

###Dummy creation
#Duplicate encoded employment column and apply it to blue collar
sliced_blue <- cleaned_lending[, 1:10]
sliced_blue$blue_worker <- cleaned_lending$encode_emp_title
cleaned_lending <- cbind(sliced_blue, cleaned_lending[, 11:148])

#remove the variable from our environment
rm(sliced_blue)

#Duplicate encoded employment column and apply it to white collar
sliced_white <- cleaned_lending[, 1:11]
sliced_white$white_worker <- cleaned_lending$encode_emp_title
cleaned_lending <- cbind(sliced_white, cleaned_lending[, 12:149])

#remove the variable from our environment
rm(sliced_white)

#Duplicate encoded employment column and apply it to pink collar
sliced_pink <- cleaned_lending[, 1:12]
sliced_pink$pink_worker <- cleaned_lending$encode_emp_title
cleaned_lending <- cbind(sliced_pink, cleaned_lending[, 13:150])

#remove the variable from our environment
rm(sliced_pink)

#Reaffecting values in the blue_worker binary column
cleaned_lending$blue_worker[cleaned_lending$blue_worker!=1] <- 0

#Reaffecting values in the white_worker binary column
cleaned_lending$white_worker[cleaned_lending$white_worker !=2] <- 0
cleaned_lending$white_worker[cleaned_lending$white_worker == 2] <- 1

#Reaffecting values in the pink_worker binary column
cleaned_lending$pink_worker[cleaned_lending$pink_worker != 3] <- 0
cleaned_lending$pink_worker[cleaned_lending$pink_worker == 3] <- 1

#Remove the initial encoded column
cleaned_lending$encode_emp_title <- NULL

final_lending <- left_join(cleaned_lending, dict_big_city, by=c('zip_code'))

#Replace any NaN value to 0 as it isn't a big city
final_lending <- final_lending %>% 
  mutate(big_city = if_else(is.na(big_city), 0, big_city))

#Create the second binary variable corresponding to the "countryside"
final_lending$countryside <- final_lending$big_city
final_lending$countryside[final_lending$countryside == 1] <- 2
final_lending$countryside[final_lending$countryside == 0] <- 1
final_lending$countryside[final_lending$countryside == 2] <- 0

#Inner join on Year_Issue and State of additional economical variables
final_lending <- left_join(final_lending, add_econ_variables, by = c('Year_Issue','State'))

#Fast dummies to create our necessary columns
final_lending <- dummy_cols(final_lending,
                            select_columns = c('term','home_ownership'),
                            remove_selected_columns = TRUE
                            )

#Removes columns we do not include in our paper
final_lending$home_ownership_ <- NULL
final_lending$home_ownership_NONE <- NULL
final_lending$home_ownership_ANY <- NULL

#Re-assign characters within the grade column
final_lending$grade[final_lending$grade == "A"] <- 1
final_lending$grade[final_lending$grade == "B"] <- 2
final_lending$grade[final_lending$grade == "C"] <- 3
final_lending$grade[final_lending$grade == "D"] <- 4
final_lending$grade[final_lending$grade == "E"] <- 5
final_lending$grade[final_lending$grade == "F"] <- 6
final_lending$grade[final_lending$grade == "G"] <- 7

#Re-assign characters within the sub-grade column
final_lending$sub_grade[final_lending$sub_grade == "A1"] <- 1
final_lending$sub_grade[final_lending$sub_grade == "A2"] <- 2
final_lending$sub_grade[final_lending$sub_grade == "A3"] <- 3
final_lending$sub_grade[final_lending$sub_grade == "A4"] <- 4
final_lending$sub_grade[final_lending$sub_grade == "A5"] <- 5
final_lending$sub_grade[final_lending$sub_grade == "B1"] <- 6
final_lending$sub_grade[final_lending$sub_grade == "B2"] <- 7
final_lending$sub_grade[final_lending$sub_grade == "B3"] <- 8
final_lending$sub_grade[final_lending$sub_grade == "B4"] <- 9
final_lending$sub_grade[final_lending$sub_grade == "B5"] <- 10
final_lending$sub_grade[final_lending$sub_grade == "C1"] <- 11
final_lending$sub_grade[final_lending$sub_grade == "C2"] <- 12
final_lending$sub_grade[final_lending$sub_grade == "C3"] <- 13
final_lending$sub_grade[final_lending$sub_grade == "C4"] <- 14
final_lending$sub_grade[final_lending$sub_grade == "C5"] <- 15
final_lending$sub_grade[final_lending$sub_grade == "D1"] <- 16
final_lending$sub_grade[final_lending$sub_grade == "D2"] <- 17
final_lending$sub_grade[final_lending$sub_grade == "D3"] <- 18
final_lending$sub_grade[final_lending$sub_grade == "D4"] <- 19
final_lending$sub_grade[final_lending$sub_grade == "D5"] <- 20
final_lending$sub_grade[final_lending$sub_grade == "E1"] <- 21
final_lending$sub_grade[final_lending$sub_grade == "E2"] <- 22
final_lending$sub_grade[final_lending$sub_grade == "E3"] <- 23
final_lending$sub_grade[final_lending$sub_grade == "E4"] <- 24
final_lending$sub_grade[final_lending$sub_grade == "E5"] <- 25
final_lending$sub_grade[final_lending$sub_grade == "F1"] <- 26
final_lending$sub_grade[final_lending$sub_grade == "F2"] <- 27
final_lending$sub_grade[final_lending$sub_grade == "F3"] <- 28
final_lending$sub_grade[final_lending$sub_grade == "F4"] <- 29
final_lending$sub_grade[final_lending$sub_grade == "F5"] <- 30
final_lending$sub_grade[final_lending$sub_grade == "G1"] <- 31
final_lending$sub_grade[final_lending$sub_grade == "G2"] <- 32
final_lending$sub_grade[final_lending$sub_grade == "G3"] <- 33
final_lending$sub_grade[final_lending$sub_grade == "G4"] <- 34
final_lending$sub_grade[final_lending$sub_grade == "G5"] <- 35

#Convert back the type char to numeric to be able to use it further down the line
final_lending <- transform(final_lending, 
                        big_city = as.numeric(big_city),
                        countryside = as.numeric(countryside),
                        grade = as.numeric(grade),
                        sub_grade = as.numeric(sub_grade),
                        blue_worker = as.numeric(blue_worker),
                        white_worker = as.numeric(white_worker),
                        pink_worker = as.numeric(pink_worker)
                        )

#Filter out the NaN values of encoded columns and get the final dataset
final_lending <- final_lending %>%
  filter(big_city < 2,
         countryside < 2,
         blue_worker < 2,
         white_worker < 2,
         pink_worker < 2)

#Specifying the characters columns that withhold empty cells, drop them
#Drop emp_title as it is already encoded
final_lending <- subset(final_lending,
                        select = -c(hardship_type,
                                    hardship_reason,
                                    hardship_status,
                                    hardship_start_date,
                                    hardship_end_date,
                                    payment_plan_start_date,
                                    hardship_loan_status,
                                    debt_settlement_flag_date,
                                    settlement_status,
                                    settlement_date,
                                    next_pymnt_d,
                                    emp_title)
                        )

#Show where the NaN are located in %
map(final_lending, ~mean(is.na(.))) 
#Remove columns with more than 20% of rows that are NaN
#Then remove NaN rows on the columns that are missing less than 20% of their data
final_lending <- final_lending %>% 
  purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=20) %>%
  na.omit(final_lending)

#Check if we have NaN values left 
#Use cat function to be able to print out on a new line our statement
if (table(is.na(final_lending))[1] == ncol(final_lending)*nrow(final_lending)){
  cat('We do not have any NaN values left \n proceed... ')
}

#Log our numeric values that we'll use afterwards
final_lending$log_total_rec_int <- log(final_lending$total_rec_int)
final_lending$log_bc_open_to_buy <- log(final_lending$bc_open_to_buy)
final_lending$log_total_rec_prncp <- log(final_lending$total_rec_prncp)
final_lending$log_total_bc_limit <- log(final_lending$total_bc_limit)
final_lending$log_total_pymnt <- log(final_lending$total_pymnt)
final_lending$log_annual_inc <- log(final_lending$annual_inc)

countInf(final_lending$log_total_rec_int)
countInf(final_lending$log_bc_open_to_buy)
countInf(final_lending$log_total_rec_prncp)
countInf(final_lending$log_total_bc_limit)
countInf(final_lending$log_total_pymnt)
countInf(final_lending$log_annual_inc)

#We only have 10 264 infinite values as it is a log(0), we'll drop them
#If we had a higher number we could've been looking into the asinh() function 
#which is more widely accepted than the log(x+1) or log(x+0.001) alternative
final_lending <- final_lending %>% 
  filter_all(all_vars(!is.infinite(.)))


#change the columns name to a correct format
names(final_lending)[names(final_lending) == 'term_.36.months'] <- 'term_36_months'
names(final_lending)[names(final_lending) == 'term_.60.months'] <- 'term_60_months'

#Correct the issue of storage size on the string length value
#https://tinyurl.com/bdd24f9a
final_lending <- final_lending %>%
  mutate(across(where(is.character), ~ substr(., 1, 1500)))

#Write out our file to the clean directory
haven::write_dta(final_lending, "../Data/Clean/filtered_lending_club.dta")
