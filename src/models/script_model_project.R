#Empty the environment and re-run if needed
#rm(list=ls()) 

library(dplyr)
library(tibble)
library(ivreg)
library(stargazer)
library(caret)
library(mlbench)
library(haven) #Read dta files
library(car) #VIF tests 
library(mctest) #Farrar - Glauber test on Multicollinearity
library(memisc)

#Source the working directory where you're located
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Load the data
lending_df <- read_dta('../Data/Clean/filtered_lending_club.dta')



lending_corr <- cor(lending_df[, c("blue_worker",
                                   "white_worker",
                                   "pink_worker",
                                   "int_rate",
                                   "big_city",
                                   "grade",
                                   "log_total_rec_int",
                                   "log_bc_open_to_buy",
                                   "log_total_rec_prncp",
                                   "log_total_bc_limit",
                                   "log_annual_inc",
                                   "delinq_2yrs",
                                   "home_ownership_MORTGAGE",
                                   "home_ownership_OWN",
                                   "home_ownership_RENT",
                                   "term_36_months",
                                   "labor_force_participation_rate",
                                   "employment_population_ratio",
                                   "unemployment_rate")])


#Correlation matrix
lending_corr[upper.tri(lending_corr) ] <- NA
lending_corr
stargazer::stargazer(lending_corr,
                     digits = 3, 
                     title = "Correlation Matrix",
                     type = "HTML", 
                     output = "corr_matrix.html")

#1st Hypothesis
#Multi-linear regression with regards to int_rate
collar_reg <- lm(int_rate ~ blue_worker + white_worker + pink_worker +
                   grade + log_total_rec_int + log_bc_open_to_buy +
                   log_total_rec_prncp + log_total_bc_limit + log_annual_inc +
                   delinq_2yrs + home_ownership_MORTGAGE + home_ownership_OWN + 
                   home_ownership_RENT + term_36_months +
                   labor_force_participation_rate +
                   employment_population_ratio + unemployment_rate,
                 data = lending_df)

summary(collar_reg)

stargazer::stargazer(collar_reg, type = "text")

confint(collar_reg)


#OLS Assumption of the Linearity of the Data
#Is the red line more or less horizontal ? Yes, 
#There should be no pattern within the graph, however we do have one, 
plot(collar_reg,1)

#Multi-collinearity assumption

cormat <-  round(cor(lending_df[, c("blue_worker", "white_worker",
                                            "pink_worker","grade","log_total_rec_int",
                                            "log_bc_open_to_buy","log_total_rec_prncp",
                                            "log_total_bc_limit","log_annual_inc",
                                            "delinq_2yrs","home_ownership_MORTGAGE",
                                            "home_ownership_OWN","home_ownership_RENT",
                                            "term_36_months","labor_force_participation_rate",
                                            "employment_population_ratio","unemployment_rate")]),2)

findCorrelation(cormat, cutoff = 0.7, names = TRUE)
#3 variables, drop log_total_bc_limit, home_ownership_MORTGAGE, employment_population_ratio

#VIF >4 on our 3 encoded variables collars
vif(collar_reg)

omcdiag(collar_reg)

plot(collar_reg, 3)

#2nd Hypothesis
#Multi-linear regression with regards to area 
lm_geolocalisation <- lm(int_rate ~ big_city + grade + 
                           log_total_rec_int + log_bc_open_to_buy +
                           log_total_rec_prncp + log_total_bc_limit + 
                           log_annual_inc +home_ownership_MORTGAGE + 
                           home_ownership_OWN + home_ownership_RENT + 
                           term_36_months +delinq_2yrs + 
                           labor_force_participation_rate +
                           employment_population_ratio + unemployment_rate , 
                         data = lending_df)

summary(lm_geolocalisation)

stargazer::stargazer(lm_geolocalisation, type = "text")

confint(lm_geolocalisation)

plot(lm_geolocalisation, 1)

cormat_area <-  round(cor(lending_df[, c("big_city","grade","log_total_rec_int",
                                                 "log_bc_open_to_buy","log_total_rec_prncp",
                                                 "log_total_bc_limit","log_annual_inc",
                                                 "delinq_2yrs","home_ownership_MORTGAGE",
                                                 "home_ownership_OWN","home_ownership_RENT",
                                                 "term_36_months","labor_force_participation_rate",
                                                 "employment_population_ratio","unemployment_rate")]),2)

findCorrelation(cormat_area, cutoff = 0.7, names = TRUE)

vif(lm_geolocalisation)

omcdiag(lm_geolocalisation)

plot(lm_geolocalisation, 3)

#Output both summary results 

both_models <- mtable(collar_reg, lm_geolocalisation)

write.mtable(both_models,file="../plots/both_models.html",
             format=c("HTML"))


#Robustness tests 
#Removing cyclically outliers according to a Q1-1,5*IQR and Q1+1,5*IQR range

#log_total_rec_int
q <- quantile(lending_df$log_total_rec_int, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(lending_df$log_total_rec_int)
no_outl_lending_df<- subset(lending_df,
                    lending_df$log_total_rec_int > (q[1] - 1.5*iqr) & 
                    lending_df$log_total_rec_int < (q[2]+1.5*iqr)
                    )


#log_bc_open_to_buy
q <- quantile(no_outl_lending_df$log_bc_open_to_buy, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(no_outl_lending_df$log_bc_open_to_buy)
no_outl_lending_df<- subset(no_outl_lending_df,
                    no_outl_lending_df$log_bc_open_to_buy > (q[1] - 1.5*iqr) &
                      no_outl_lending_df$log_bc_open_to_buy < (q[2]+1.5*iqr)
                    )


#log_total_rec_prncp
q <- quantile(no_outl_lending_df$log_total_rec_prncp, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(no_outl_lending_df$log_total_rec_prncp)
no_outl_lending_df<- subset(no_outl_lending_df,
                            no_outl_lending_df$log_total_rec_prncp > (q[1] - 1.5*iqr) &
                              no_outl_lending_df$log_total_rec_prncp < (q[2]+1.5*iqr)
                    )

#log_total_bc_limit
q <- quantile(no_outl_lending_df$log_total_bc_limit, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(no_outl_lending_df$log_total_bc_limit)
no_outl_lending_df<- subset(no_outl_lending_df,
                            no_outl_lending_df$log_total_bc_limit > (q[1] - 1.5*iqr) &
                              no_outl_lending_df$log_total_bc_limit < (q[2]+1.5*iqr)
                    )


#log_annual_inc
q <- quantile(no_outl_lending_df$log_annual_inc, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(no_outl_lending_df$log_annual_inc)
no_outl_lending_df<- subset(no_outl_lending_df,
                            no_outl_lending_df$log_annual_inc > (q[1] - 1.5*iqr) &
                              no_outl_lending_df$log_annual_inc < (q[2]+1.5*iqr)
                    )

#labor_force_participation_rate
q <- quantile(no_outl_lending_df$labor_force_participation_rate, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(no_outl_lending_df$labor_force_participation_rate)
no_outl_lending_df<- subset(no_outl_lending_df,
                            no_outl_lending_df$labor_force_participation_rate > (q[1] - 1.5*iqr) &
                              no_outl_lending_df$labor_force_participation_rate < (q[2]+1.5*iqr)
                    )

#employment_population_ratio
q <- quantile(no_outl_lending_df$employment_population_ratio, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(no_outl_lending_df$employment_population_ratio)
no_outl_lending_df<- subset(no_outl_lending_df,
                            no_outl_lending_df$employment_population_ratio > (q[1] - 1.5*iqr) &
                              no_outl_lending_df$employment_population_ratio < (q[2]+1.5*iqr)
                    )

#unemployment_rate
q <- quantile(no_outl_lending_df$unemployment_rate, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(no_outl_lending_df$unemployment_rate)
no_outl_lending_df<- subset(no_outl_lending_df,
                            no_outl_lending_df$unemployment_rate > (q[1] - 1.5*iqr) &
                              no_outl_lending_df$unemployment_rate < (q[2]+1.5*iqr)
                    )

#1st Hypothesis
#Multi-linear regression with regards to int_rate
collar_reg_out <- lm(int_rate ~ blue_worker + white_worker + pink_worker +
                   grade + log_total_rec_int + log_bc_open_to_buy +
                   log_total_rec_prncp + log_total_bc_limit + log_annual_inc +
                   delinq_2yrs + home_ownership_MORTGAGE + home_ownership_OWN + 
                   home_ownership_RENT + term_36_months +
                   labor_force_participation_rate +
                   employment_population_ratio + unemployment_rate,
                 data = no_outl_lending_df)

summary(collar_reg_out)

stargazer::stargazer(collar_reg_out, type = "text")

confint(collar_reg_out)


#2nd Hypothesis
#Multi-linear regression with regards to area 
lm_geolocalisation_out <- lm(int_rate ~ big_city + grade + 
                           log_total_rec_int + log_bc_open_to_buy +
                           log_total_rec_prncp + log_total_bc_limit + 
                           log_annual_inc +home_ownership_MORTGAGE + 
                           home_ownership_OWN + home_ownership_RENT + 
                           term_36_months +delinq_2yrs + 
                           labor_force_participation_rate +
                           employment_population_ratio + unemployment_rate , 
                         data = no_outl_lending_df)

summary(lm_geolocalisation_out)

stargazer::stargazer(lm_geolocalisation_out, type = "text")

confint(lm_geolocalisation_out)


#Robustness test replacing grade with sub-grade

#1st Hypothesis
#Multi-linear regression with regards to int_rate
collar_reg_subgrade <- lm(int_rate ~ blue_worker + white_worker + pink_worker +
                       sub_grade + log_total_rec_int + log_bc_open_to_buy +
                       log_total_rec_prncp + log_total_bc_limit + log_annual_inc +
                       delinq_2yrs + home_ownership_MORTGAGE + home_ownership_OWN + 
                       home_ownership_RENT + term_36_months +
                       labor_force_participation_rate +
                       employment_population_ratio + unemployment_rate,
                     data = no_outl_lending_df)

summary(collar_reg_subgrade)

stargazer::stargazer(collar_reg_subgrade, type = "text")

confint(collar_reg_subgrade)


#2nd Hypothesis
#Multi-linear regression with regards to area 
lm_geolocalisation_subgrade <- lm(int_rate ~ big_city + sub_grade + 
                               log_total_rec_int + log_bc_open_to_buy +
                               log_total_rec_prncp + log_total_bc_limit + 
                               log_annual_inc +home_ownership_MORTGAGE + 
                               home_ownership_OWN + home_ownership_RENT + 
                               term_36_months +delinq_2yrs + 
                               labor_force_participation_rate +
                               employment_population_ratio + unemployment_rate , 
                             data = no_outl_lending_df)

summary(lm_geolocalisation_subgrade)

stargazer::stargazer(lm_geolocalisation_subgrade, type = "text")

confint(lm_geolocalisation_subgrade)


#Output both summary results 

robustness <- mtable(collar_reg_out,lm_geolocalisation_out,collar_reg_subgrade, lm_geolocalisation_subgrade)

write.mtable(robustness,file="../plots/robustness.html",
             format=c("HTML"))










