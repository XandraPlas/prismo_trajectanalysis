#--------- SCRIPT FOR DESCRIPTIVE STATISTICS - TRAJECTORY ANALYSIS ------------#
#
# Description:  This script reads the raw data demographic data from excel
#               sheets and calculates the descriptive statistics.
#
# Authors:      Plas
# Date:         March 2023
# Version:      1.0
# R.version:    4.2.2 (2022-10-31)
# Rstudio:      2023.03.0+386
#
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
#                          Settings & Dependencies
#------------------------------------------------------------------------------#

# Define path to get and save files
save_location = "~/Documents/PhD/p_PRISMO/Trajectanalyse Depressie/prismo_trajectanalysis/"

# Import libraries
#--------------------------------#

# read files
library(readxl)
library(writexl)

# data manipulation
library(tidyverse)
library(dplyr)
library(tidySEM)

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
source("~/Documents/PhD/R code/Func_demoTable.R")




#------------------------------------------------------------------------------#
#                              Data Collection
#------------------------------------------------------------------------------#


# Read the xlsx files
#--------------------------------#

df_total <- read_excel(paste(save_location, "df_total_demographics.xlsx", sep = ""))


#------------------------------------------------------------------------------#
#                               Demographics
#------------------------------------------------------------------------------#

# calculate total ZIL and DES scores
df_zil <- df_total[grepl("^.ZIL\\d+", names(df_total))]
names(df_zil) <- gsub("^(.)ZIL(\\d+)", "ZIL_\\1_\\2", names(df_zil))

# calculate ZIL total score
zil <- tidy_sem(as.data.frame(df_zil))
zil_scales <- create_scales(zil, totals = TRUE)
zil_scores <- zil_scales$scores
zil_scores <- replace(zil_scores, zil_scores == 0, NA)

df_total <- cbind(df_total, zil_scores)

# calculate DES/PES total score
df_pes <- df_total[grepl("^PES\\d+$", names(df_total))]
df_pes$sum <- rowSums(df_pes)

df_total$PESscore <- df_pes$sum

# calculate ZIL and DES scores for na and non-na group
# get variables
df_demo <- df_total %>%
  dplyr::select("all_na_in_outcome_var", "ZIL_A", "PESscore")


tapply(df_demo$ZIL_A, df_demo$all_na_in_outcome_var, summary)
tapply(df_demo$PESscore, df_demo$all_na_in_outcome_var, summary)

# Create table
#--------------------------------#
# get variables
df_demo_table <- df_total %>%
  dplyr::select("gender", "age_cat", "rank_cat", "education_cat", "work_function", "yr_deployment_cat", 
         "Prev_deployment_dummy", "all_na_in_outcome_var")


demo_table <- create_demoTable(df = df_demo_table, split_variable = "all_na_in_outcome_var")
demo_table <- cbind(rownames(demo_table), demo_table)
write_xlsx(demo_table, paste(save_location, "demographics_table.xlsx", sep = ""))

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
write_xlsx(depr_table_descriptive, paste(save_location, "depresion_descriptives_table.xlsx", sep = ""))



# Differences over time
#--------------------------------#
# Friedman's ANOVA (non-parametric alternative for repeated measures ANOVA)

dat <- df_total[grepl("^.SCL90_depr", names(df_total))]
friedman.test(as.matrix(dat))


# Distributions
#--------------------------------#

hist(df_total$ASCL90_depr, )
hist(df_total$BSCL90_depr)
hist(df_total$CSCL90_depr)
hist(df_total$DSCL90_depr)
hist(df_total$ESCL90_depr)
hist(df_total$GSCL90_depr)
