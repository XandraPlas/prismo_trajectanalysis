#--------------- SCRIPT FOR DATA ANALYSIS - TRAJECTORY ANALYSIS ---------------#
#
# Description:  This script performs group comparisons between the trajectory 
#               groups defined in manuscript.Rmd.
#
# Authors:      Xandra Plas
# Date:         Jan 2023
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
library(naniar)

# visualizations
library(ggplot2)
library(svglite)

# statistics
library(tidySEM)
library(lavaan)
library(gmodels)

# extra
library(worcs)


#------------------------------------------------------------------------------#
#                          Data Collection
#------------------------------------------------------------------------------#

res_final <- readRDS("manuscript/res_step.RData")
res_final_mod <- res_final[[3]]
cprobs <- data.frame(class_prob(res_final_mod, type = "individual"))

n <- nrow(cprobs)
paste("class 1: ", round(nrow(cprobs[cprobs$individual.predicted == 1,]) * 100 / n), "%")
paste("class 2: ", round(nrow(cprobs[cprobs$individual.predicted == 2,]) * 100 / n), "%")
paste("class 3: ", round(nrow(cprobs[cprobs$individual.predicted == 3,]) * 100 / n), "%")

df_demo <- read_excel("df_total.xlsx")
df_demo <- df_demo %>%
  filter(all_na_in_outcome_var == 0)


# Select variables
dat <- df_demo %>%
  dplyr::select("gender", "age", "rank", "education", "function", "yr_deployment", "Prev_deployment_dummy") %>%
  rename(work_function = "function")


# Manipulate variables
dat <- dat %>% 
  mutate(age_cat = if_else(age <= 21, "<=21", ">21")) %>% 
  mutate(education_cat = if_else(education == 1, "Low", 
                                 ifelse(education >= 5, "High", "Medium"))) %>% 
  mutate(rank_cat = if_else(rank == 1, "Private", 
                            ifelse(rank == 2, "Corporal", 
                                   ifelse(rank >= 5, "Staff officer", "Non-commissioned")))) %>% 
  mutate(yr_deployment_cat = if_else(yr_deployment <= 2006, "2005-2006", "2007-2008"))







dat$class <- cprobs$individual.predicted


dat$education[dat$education == 7] <- NA

res_bch <- BCH(res_final_mod, data = as.factor(dat$gender))
lr_test(res_bch)
res_bch <- BCH(res_final_mod, data = as.factor(dat$age_cat))
lr_test(res_bch)
res_bch <- BCH(res_final_mod, data = as.factor(dat$rank_cat))
lr_test(res_bch)
res_bch <- BCH(res_final_mod, data = as.factor(dat$education_cat))
lr_test(res_bch)
res_bch <- BCH(res_final_mod, data = as.factor(dat$work_function))
lr_test(res_bch)
res_bch <- BCH(res_final_mod, data = as.factor(dat$yr_deployment))
lr_test(res_bch)
res_bch <- BCH(res_final_mod, data = as.factor(dat$Prev_deployment_dummy))
lr_test(res_bch)

CrossTable(dat$class, dat$age_cat, fisher = TRUE, chisq = TRUE,
           expected = TRUE, sresid = TRUE, format = "SPSS")

# deployment experience scale (DES --? PES?)
pes_items <- paste("PES", 1:19, sep = '')
df_pes <- df_demo[grepl("^PES\\d+$", names(df_demo))]
df_pes$class <- cprobs$individual.predicted


res_bch <- BCH(res_final_mod, data = as.factor(df_pes$PES16))
lr_test(res_bch) # 6, 12, 15, 16, 17

variable <- "PES16"
CrossTable(df_pes$class, df_pes %>% pull(variable), fisher = TRUE, chisq = TRUE,
           expected = TRUE, sresid = TRUE, format = "SPSS")
