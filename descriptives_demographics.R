#--------- SCRIPT FOR DESCRIPTIVE STATISTICS - TRAJECTORY ANALYSIS ------------#
#
# Description:  This script reads the raw data demographic data from excel
#               sheets and calculates the descriptive statistics.
#
# Authors:      Xandra Plas
# Date:         Nov 2022
# Version:      1.0
# R.version:    4.2.1 (2022-06-23)
#
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
#                          Settings & Dependencies
#------------------------------------------------------------------------------#


# Import libraries
#--------------------------------#

# read files
library(readxl)
library(writexl)

# data manipulation
library(tidyverse)
library(dplyr)

# statistics
library(e1071)
library(psych)
library(gmodels)
library(pgirmess)

# visualizations
library(ggplot2)



# Functions
#--------------------------------#

# source("TrajAna_dataPrep_funcs.R")
source("~/Documents/PhD/Func_demoTable.R")




#------------------------------------------------------------------------------#
#                              Data Collection
#------------------------------------------------------------------------------#


# Read the xlsx files
#--------------------------------#

df_total <- read_excel("~/Documents/PhD/Trajectanalyse Depressie/prismo_trajectanalysis/df_total_demographics.xlsx")


#------------------------------------------------------------------------------#
#                               Demographics
#------------------------------------------------------------------------------#

df_demo_test <- df_total %>%
  dplyr::select("gender", "age_cat", "rank_cat", "education_cat", "work_function", "yr_deployment_cat", 
         "Prev_deployment_dummy", "all_na_in_outcome_var")


demo_table <- create_demoTable(df = df_demo_test, split_variable = "all_na_in_outcome_var")
demo_table <- cbind(rownames(demo_table), demo_table)
write_xlsx(demo_table, "demographics_table.xlsx")

# print statistics
for (var in c("gender", "age_cat", "rank_cat", "education_cat", "work_function", "yr_deployment_cat", 
              "Prev_deployment_dummy")){
  print(paste("Statistics: ", var))
  CrossTable(df_demo_test$all_na_in_outcome_var, df_demo_test[[var]], fisher = TRUE, chisq = TRUE,
             expected = TRUE, sresid = TRUE, format = "SPSS")
}


#------------------------------------------------------------------------------#
#                               Statistics
#------------------------------------------------------------------------------#

depr_table_descriptive <- data.frame()

for (wave in c("ASCL90_depr", "BSCL90_depr", "CSCL90_depr", "DSCL90_depr",
               "ESCL90_depr", "GSCL90_depr")) { 
  print(wave)
  n_wave <- nrow(na.omit(df_total[,wave]))
  
  # define subgroups of depressions scores
  df_low <- df_total %>%
    filter(get({{wave}}) < 20)
  df_medium <- df_total %>%
    filter(get({{wave}}) >= 20 & get({{wave}}) < 25)
  df_high <- df_total %>%
    filter(get({{wave}}) >= 25)
  
  # calculate n and percentages
  n_low <- nrow(df_low)
  perc_low <- (n_low * 100) / n_wave
  
  n_medium <- nrow(df_medium)
  perc_medium <- (n_medium * 100) / n_wave
  
  n_high <- nrow(df_high)
  perc_high <- (n_high * 100) / n_wave
  
  # fill data frame
  depr_table_descriptive[wave, "Depression low: n"] <- n_low
  depr_table_descriptive[wave, "Depression low: %"] <- 
    paste(round(perc_low), "%", sep = "")
  
  depr_table_descriptive[wave, "Depression medium: n"] <- n_medium
  depr_table_descriptive[wave, "Depression medium: %"] <- 
    paste(round(perc_medium), "%", sep = "")
  
  depr_table_descriptive[wave, "Depression high: n"] <- n_high
  depr_table_descriptive[wave, "Depression high: %"] <- 
    paste(round(perc_high), "%", sep = "")
}
depr_table_descriptive$wave <- rownames(depr_table_descriptive)
write_xlsx(depr_table_descriptive, "depresion_descriptives_table.xlsx")



# Distributions
#--------------------------------#

hist(df_total$ASCL90_depr, )
hist(df_total$BSCL90_depr)
hist(df_total$CSCL90_depr)
hist(df_total$DSCL90_depr)
hist(df_total$ESCL90_depr)
hist(df_total$GSCL90_depr)
