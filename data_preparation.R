#------------- SCRIPT FOR DATA PREPARATION - TRAJECTORY ANALYSIS --------------#
#
# Description:  This script reads the raw data (depression surveys: scl-90-r and
#               bsi and demographics (incl deployment experience and PTSD 
#               symptoms) of the sample) from excel sheets and replaces all 999 
#               for NA. Furthermore, outliers and nan's will be removed.
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

# # Getting the path of your current open file
# current_path <- rstudioapi::getActiveDocumentContext()$path
# setwd(dirname(current_path))

# Define path to save files
save_location = "~/Documents/PhD/p_PRISMO/Trajectanalyse Depressie/prismo_trajectanalysis/"

# Import libraries
#--------------------------------#

# read files
library(readxl)
library(writexl)

# data manipulation
library(tidyverse)
library(tidyr)
library(naniar)

# visualizations
library(ggplot2)

# statistics
library(e1071)

# missing values
library(mice)
library(VIM)


# Functions
#--------------------------------#
source("~/Documents/PhD/p_PRISMO/Trajectanalyse Depressie/prismo_trajectanalysis/TrajAna_dataPrep_funcs.R")




#------------------------------------------------------------------------------#
#                          Data Collection
#------------------------------------------------------------------------------#

setwd("/Volumes/heronderzoek-18/MGGZ/Xandra/PRISMO data/")

# Read the xlsx files
#--------------------------------#

# SCL-90-R
df_scl90r_0 <- read_excel("Databestanden vragenlijsten compleet/SCL-90-R/PRISMO_SCL90R_ABCDEG.xlsx")
scl90r_timepoints <- c("A", "B", "C", "D", "E", "G")


# Sample Demographics and life events
df_demo_0 <- read_excel("Databestanden vragenlijsten compleet/Demografie/PRISMO_Demografie_ABCDEF.xlsx")
df_life_changes_0 <- read_excel("Databestanden vragenlijsten compleet/Checklist belangrijke gebeurtenissen/PRISMO_BelangrijkeGebeurtenissen_DEFG.xlsx")
df_early_trauma_0 <- read_excel("Databestanden vragenlijsten compleet/ETISR-SF/PRISMO_ETISRSF_A.xlsx")

# Deployment experience (now DES)
df_pes_0 <- read_excel("Databestanden vragenlijsten compleet/PES/PRISMO_PES_B.xlsx")

# PTSD - ZIL(Dutch version of SRIP)
df_zil_0 <- read_excel("Databestanden vragenlijsten compleet/ZIL/PRISMO_ZIL_ABCDEFG.xlsx")
names(df_zil_0) <- gsub("^ZIL(\\d+)$", "AZIL\\1", names(df_zil_0)) # change col names to same format
names(df_zil_0) <- gsub("G_ZIL(\\d+)$", "GZIL\\1", names(df_zil_0)) # change col names to same format
zil_timepoints <- c("A", "B", "C", "D", "E", "F", "G")



#------------------------------------------------------------------------------#
#                      Data pre-processing & Exploration
#------------------------------------------------------------------------------#


# Replace 999 and outliers
#--------------------------------#

# replace 999 with NA and create back-up
df_scl90r <- replace_999(df_scl90r_0)

df_demo <- replace_999(df_demo_0)
df_life_changes <- replace_999(df_life_changes_0)
df_early_trauma <- replace_999(df_early_trauma_0)

df_pes <- replace_999(df_pes_0)

df_zil <- replace_999(df_zil_0)



# Prepare demographics
#--------------------------------#

df_demo <- df_demo %>%
  rename(work_function = "function") %>%
  
  # transform categories
  dplyr::mutate(age_cat = if_else(age <= 21, "<=21", ">21")) %>% 
  dplyr::mutate(education_cat = if_else(education == 1, "Low", 
                                 ifelse(education >= 5, "High", "Medium"))) %>% 
  dplyr::mutate(rank_cat = if_else(rank == 1, "Private", 
                            ifelse(rank == 2, "Corporal", 
                                   ifelse(rank >= 5, "Staff officer", "Non-commissioned")))) %>% 
  dplyr::mutate(yr_deployment_cat = if_else(yr_deployment <= 2006, "2005-2006", "2007-2008")) %>%
  
  # dplyr::select desired columns
  dplyr::select("moederfile", "Niet_op_uitzending", "gender", "age", "age_cat", "rank_cat", "education_cat", "work_function", "yr_deployment_cat", 
         "Prev_deployment_dummy")





# Prepare ZIL
#--------------------------------#

# impute missing values
zil <- df_zil[grepl("^.ZIL\\d+$", names(df_zil))]
names(zil) <- gsub("(.)ZIL(\\d+)$", "ZIL\\2_\\1", names(zil))
set.seed(1234)
df_zil_imp <- missRanger::missRanger(zil)
df_zil_imp$moederfile <- df_zil$moederfile






# Replace outliers
#--------------------------------#

## SCL-90
check_outliers(df_scl90r, cut_off = 5) # check it outliers exist

df_scl90r <- df_scl90r %>%
  # replace values that exceed the questionnaire scale
  mutate_at(vars(-"moederfile"), ~ replace(., . < 1, NA)) %>%
  mutate_at(vars(-"moederfile"), ~ replace(., . == 11, 1)) %>%
  mutate_at(vars(-"moederfile"), ~ replace(., . == 22, 2)) %>%
  mutate_at(vars(-"moederfile"), ~ replace(., . == 33, 3)) %>%
  mutate_at(vars(-"moederfile"), ~ replace(., . == 44, 4)) %>%
  mutate_at(vars(-"moederfile"), ~ replace(., . == 55, 5)) %>%
  mutate_at(vars(-"moederfile"), ~ replace(., . > 5, NA))

check_outliers(df_scl90r, cut_off = 5) # check it outliers still exist





# Select depression scores
#--------------------------------#

## SCL-90-R
scl90r_items_depr <- c("SCL3", "SCL5", "SCL14", "SCL15", "SCL19", "SCL20", 
                       "SCL22", "SCL26", "SCL29", "SCL30", "SCL31", "SCL32", 
                       "SCL51", "SCL54", "SCL59", "SCL79")

df_scl90r_depr <- df_scl90r %>%
  # dplyr::select all variables of the depression sub scale
  dplyr::select("moederfile", ends_with(scl90r_items_depr))

# calculate total score
for (timepoint in scl90r_timepoints) {
  # check time point
  print(paste("Timepoint:", timepoint))

  new_col_name <- paste(timepoint, "SCL90_depr", sep = "")
  df_scl90r_depr <- calc_subScore_scl90r(df_scl90r_depr,
                                         new_col = new_col_name,
                                         max_na = 2,
                                         subscale_items = scl90r_items_depr,
                                         time_point = timepoint)
}


# save total scores 
scl90r_scores <- df_scl90r_depr %>%
  dplyr::select("moederfile", ends_with("_depr"))
# create new col to see if participants missed all time points (A,B,C,D,E,G)
scl90r_scores <- scl90r_scores %>%
  column_to_rownames(., var = "moederfile") %>%
  mutate(all_na_in_outcome_var := ifelse(rowSums(is.na(.)) == ncol(.), 1, 0)) # 1: missed all
scl90r_scores$moederfile <- rownames(scl90r_scores) # recreate moederfile column

# save item scores
scl90r_items <- df_scl90r_depr %>%
  dplyr::select("moederfile", !ends_with("_depr"))
scl90r_items <- merge(scl90r_items, 
                      scl90r_scores %>% dplyr::select("moederfile", "all_na_in_outcome_var"),
                      by = "moederfile")




#------------------------------------------------------------------------------#
#                                  Save Files
#------------------------------------------------------------------------------#

# combine depression data and demographics
df_total_0 <- merge(df_demo, scl90r_items, by = "moederfile", all = TRUE)

# merge df_demo with PES/DES scores
df_total_0 <- merge(df_total_0, df_pes, by = "moederfile", all = TRUE)

# merge df_demo with Life Changes
df_total_0 <- merge(df_total_0, df_life_changes, by = "moederfile", all = TRUE)

# merge df_demo with Early Trauma
df_total_0 <- merge(df_total_0, df_early_trauma, by = "moederfile", all = TRUE)

# merge df_demo with imputed ZIL scores
df_total_ZILimp <- merge(df_total_0, df_zil_imp, by = "moederfile", all = TRUE)

# keep deployed participants
df_total_ZILimp <- df_total_ZILimp %>%
  filter(Niet_op_uitzending == 0)

write_xlsx(df_total_ZILimp, paste(save_location, "df_total.xlsx", sep = ""))


# save file for demographics and descriptive table (including total scores)
df_scores <- scl90r_scores
df_total_demographics <- merge(df_total_0, df_scores, by = "moederfile")

# merge df_demo with non imputed ZIL scores
df_total_demographics <- merge(df_total_demographics, df_zil, by = "moederfile", all = TRUE)

# keep deployed participants
df_total_demographics <- df_total_demographics %>%
  filter(Niet_op_uitzending == 0)

write_xlsx(df_total_demographics, paste(save_location, "df_total_demographics.xlsx", sep = ""))



#------------------------------------------------------------------------------#
#                           Explore Missing Values
#------------------------------------------------------------------------------#


# check distribution scl
for (timepoint in scl90r_timepoints) {
  y_col <- paste(timepoint, "SCL90_depr", sep = "")
  
  print(ggplot(scl90r_scores, aes_string(x=y_col)) +
    geom_histogram())
}
# SKEWED! Zero inflated, group at minimum

# Missing values
gg_miss_var(scl90r_scores)




# Patterns of missing values
meanA_nan <- scl90r_scores %>%
  bind_shadow() %>%
  group_by(ASCL90_depr_NA) %>%
  summarize(mean(BSCL90_depr, na.rm=TRUE), mean(CSCL90_depr, na.rm=TRUE),
            mean(DSCL90_depr, na.rm=TRUE), mean(ESCL90_depr, na.rm=TRUE),
            mean(GSCL90_depr, na.rm=TRUE))
meanB_nan <- scl90r_scores %>%
  bind_shadow() %>%
  group_by(BSCL90_depr_NA) %>%
  summarize(mean(ASCL90_depr, na.rm=TRUE), mean(CSCL90_depr, na.rm=TRUE),
            mean(DSCL90_depr, na.rm=TRUE), mean(ESCL90_depr, na.rm=TRUE),
            mean(GSCL90_depr, na.rm=TRUE))
meanC_nan <- scl90r_scores %>%
  bind_shadow() %>%
  group_by(CSCL90_depr_NA) %>%
  summarize(mean(ASCL90_depr, na.rm=TRUE), mean(BSCL90_depr, na.rm=TRUE),
            mean(DSCL90_depr, na.rm=TRUE), mean(ESCL90_depr, na.rm=TRUE),
            mean(GSCL90_depr, na.rm=TRUE))
meanD_nan <- scl90r_scores %>%
  bind_shadow() %>%
  group_by(DSCL90_depr_NA) %>%
  summarize(mean(ASCL90_depr, na.rm=TRUE), mean(BSCL90_depr, na.rm=TRUE),
            mean(CSCL90_depr, na.rm=TRUE), mean(ESCL90_depr, na.rm=TRUE),
            mean(GSCL90_depr, na.rm=TRUE))
meanE_nan <- scl90r_scores %>%
  bind_shadow() %>%
  group_by(ESCL90_depr_NA) %>%
  summarize(mean(ASCL90_depr, na.rm=TRUE), mean(BSCL90_depr, na.rm=TRUE),
            mean(CSCL90_depr, na.rm=TRUE), mean(DSCL90_depr, na.rm=TRUE),
            mean(GSCL90_depr, na.rm=TRUE))
meanG_nan <- scl90r_scores %>%
  bind_shadow() %>%
  group_by(GSCL90_depr_NA) %>%
  summarize(mean(ASCL90_depr, na.rm=TRUE), mean(BSCL90_depr, na.rm=TRUE),
            mean(CSCL90_depr, na.rm=TRUE), mean(DSCL90_depr, na.rm=TRUE),
            mean(ESCL90_depr, na.rm=TRUE))


# looks like the NA group in G scores a little higher ±1 in D, E
 
 


# Test NA/!NA differences
#--------------------------------#
# Does the NA group differ significantly from the !NA group?

df_scl90r_shadow <- scl90r_scores %>%
  bind_shadow()

# not normally distributed so use Wilcoxon rank-sum Test

wilcox.test(df_scl90r_shadow$DSCL90_depr ~ df_scl90r_shadow$GSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ESCL90_depr ~ df_scl90r_shadow$GSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$CSCL90_depr ~ df_scl90r_shadow$GSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$BSCL90_depr ~ df_scl90r_shadow$GSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ASCL90_depr ~ df_scl90r_shadow$GSCL90_depr_NA, paired = FALSE)
# NA group of time point G scores significantly higher than !NA group on D

wilcox.test(df_scl90r_shadow$GSCL90_depr ~ df_scl90r_shadow$ESCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$DSCL90_depr ~ df_scl90r_shadow$ESCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$CSCL90_depr ~ df_scl90r_shadow$ESCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$BSCL90_depr ~ df_scl90r_shadow$ESCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ASCL90_depr ~ df_scl90r_shadow$ESCL90_depr_NA, paired = FALSE)
# NA group of time point E scores significantly higher than !NA group on D

wilcox.test(df_scl90r_shadow$GSCL90_depr ~ df_scl90r_shadow$DSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ESCL90_depr ~ df_scl90r_shadow$DSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$CSCL90_depr ~ df_scl90r_shadow$DSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$BSCL90_depr ~ df_scl90r_shadow$DSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ASCL90_depr ~ df_scl90r_shadow$DSCL90_depr_NA, paired = FALSE)
# no difference for NA and !NA group of time point D

wilcox.test(df_scl90r_shadow$GSCL90_depr ~ df_scl90r_shadow$CSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ESCL90_depr ~ df_scl90r_shadow$CSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$DSCL90_depr ~ df_scl90r_shadow$CSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$BSCL90_depr ~ df_scl90r_shadow$CSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ASCL90_depr ~ df_scl90r_shadow$CSCL90_depr_NA, paired = FALSE)
# no difference for NA and !NA group of time point C

wilcox.test(df_scl90r_shadow$GSCL90_depr ~ df_scl90r_shadow$BSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ESCL90_depr ~ df_scl90r_shadow$BSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$DSCL90_depr ~ df_scl90r_shadow$BSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$CSCL90_depr ~ df_scl90r_shadow$BSCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ASCL90_depr ~ df_scl90r_shadow$BSCL90_depr_NA, paired = FALSE)
# no difference for NA and !NA group of time point B

wilcox.test(df_scl90r_shadow$GSCL90_depr ~ df_scl90r_shadow$ASCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$ESCL90_depr ~ df_scl90r_shadow$ASCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$DSCL90_depr ~ df_scl90r_shadow$ASCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$CSCL90_depr ~ df_scl90r_shadow$ASCL90_depr_NA, paired = FALSE)
wilcox.test(df_scl90r_shadow$BSCL90_depr ~ df_scl90r_shadow$ASCL90_depr_NA, paired = FALSE)
# no difference for NA and !NA group of time point A
