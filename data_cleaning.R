#Empty the environment and re-run if needed
#rm(list=ls()) 

library(dplyr)
library(haven)
library(matchmaker) #replicates the dictionary function of python, to allow mapping
library(readxl) #enables excel import

#Source the working directory where you're located
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Raw Import of the data and the two hand-labelled datasets 
lending_df <- read_dta('Data/Raw/Lending_Club.dta')

#Hand-labelled data on 
#1 = Blue Collar, #2 = White Collar #3 = Pink Collar
dictionary <- read_excel('Data/Clean/dictionary_emp_title.xlsx')

#Census data encoded
dict_big_city <- read_excel('Data/Clean/dictionary_big_cities_census.xlsx')

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

#Re-order the columns and insert a duplicate for further manipulation
sliced_2 <- cleaned_lending[, 1:23]
sliced_2$big_city <- cleaned_lending$zip_code
cleaned_lending <- cbind(sliced_2, cleaned_lending[, 24:150])

#remove the variable from our environment
rm(sliced_2)

#Map the census dictionary
final_lending <- match_df(cleaned_lending,
                            dictionary = dict_big_city,
                            from = "Zip",
                            to = "Encoding",
                            by = "column"
)

#Replace any value that wasn't mapped to a 0 as it isn't within the dictionary
final_lending <- final_lending %>% 
  mutate(big_city = replace(big_city, big_city != 1, 0))

#Convert back the type char to numeric to be able to use it further down the line
final_lending <- transform(final_lending, 
                        big_city = as.numeric(big_city),
                        blue_worker = as.numeric(blue_worker),
                        white_worker = as.numeric(white_worker),
                        pink_worker = as.numeric(pink_worker))

#Filter out the NaN values of encoded columns and get the final dataset
final_lending <- final_lending %>%
  filter(big_city < 2,
         blue_worker < 2,
         white_worker < 2,
         pink_worker < 2)

#Specifying the characters columns that withhold empty cells, drop them
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
                                    settlement_date))

#Correct the issue of storage size on the string length value
#https://tinyurl.com/bdd24f9a
final_lending <- final_lending %>%
  mutate(across(where(is.character), ~ substr(., 1, 1500)))

#Write out our file to the clean directory
haven::write_dta(final_lending, "Data/Clean/filtered_lending_club.dta")
