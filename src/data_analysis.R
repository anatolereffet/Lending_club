#Empty the environment and re-run if needed
#rm(list=ls()) 

library(ggplot2) #Graph library
library(haven) #read dta file
library(stargazer) #format outputs
library(GMCM) #st dev



#Source the working directory where you're located
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

lending_df <- read_dta('../Data/Clean/filtered_lending_club.dta')


#Select columns for a summary tab
### Summary du dataset

summary_df <- subset(lending_df, select = c(int_rate,
                                             big_city,
                                             countryside,
                                             blue_worker,
                                             white_worker,
                                             pink_worker,
                                             grade,
                                             log_total_rec_int,
                                             log_bc_open_to_buy,
                                             log_total_rec_prncp,
                                             log_total_bc_limit,
                                             log_annual_inc,
                                             delinq_2yrs, 
                                             home_ownership_MORTGAGE,
                                             home_ownership_OWN,
                                             home_ownership_RENT,
                                             term_36_months, 
                                             labor_force_participation_rate,
                                             employment_population_ratio,
                                             unemployment_rate)
)

hist(summary_df$int_rate,
     main="Interest rate frequency",
     xlab="Interest rate in percentage (%)", 
     freq = TRUE
)

slices_count <- c(1130,6909,4356,13097,74801,10236,9269,1193,1718,40090,18586,2554,863,
                  24706,10413,4850,5602,6718,13231,13155,1167,15526,10022,9310,3470,1632,
                  14777,890,1943,28927,21752,2838,7765,44333,20289,5094,6031,20367,2493,
                  6693,1125,9203,49061,3665,15130,1254,10917,7643,1667,1206)

labelling <- c('AK','AL','AR','AZ','CA','CO','CT','DC','DE','FL','GA','HI','ID','IL',
               'IN','KS','KY','LA','MA','MD','ME','MI','MN','MO','MS','MT','NC','ND',
               'NE','NH','NJ','NM','NV','NY','OH','OK','OR','PA','RI','SC','SD','TN',
             'TX','UT','VA','VT','WA','WI','WV','WY')


pie(slices_count, labels = labelling, main="State representation of our data selection")

#Output classic summary tab
summary(summary_df)

st_dev <- summary_df %>%
  summarise_if(is.numeric, sd)


table(summary_df$white_worker)
table(summary_df$blue_worker)
table(summary_df$pink_worker)


