#------------- FUNCTIONS - TRAJ. ANA. DATA PREPARATION --------------#
#
# Description:  This script stores the functions needed for the data preparation
#               of the trajectory analysis.
#
# Authors:      Plas
# Date:         Nov 2022
# Version:      1.0
# R.version:    4.2.1 (2022-06-23) 
#
#------------------------------------------------------------------------------#


# change 999 to NA
#--------------------------------------------#
replace_999 <- function(df) {
  # replace all 999 in df with NA. Create and return new df(df_new)
  df_new <- df %>%
    mutate_all(~ na_if(., 999))
}



# check if values exceed scale
#--------------------------------------------#
check_outliers <- function(df, cut_off) {
  # check if df contains values > cut-off value
  if (sum(df %>% dplyr::select(!moederfile) > cut_off, na.rm = TRUE) == 0) {
    print(paste("CONGRATS: No values exceed the questionnaire scale (>", cut_off,")", sep = ""))
  } else {
    print(paste("WARNING: Some values exceed the questionnaire scale (>", cut_off,")", sep = ""))
  }
}



# calculate sub-scale score SCL-90
#--------------------------------------------#
calc_subScore_scl90r <- function(df, new_col, max_na, subscale_items, time_point) {
  
  # prepare match string for specific time point
  match_string_t <- paste(time_point, subscale_items, sep = "")
  match_string_t <- paste(match_string_t, collapse = "|")
  # check if match string is correct
  print(match_string_t)
  
  # calculate total sub scale score per time point if NA's < cut-off
  df <- df %>%
    rowwise() %>%
    mutate(!!new_col :=
             ifelse(sum(is.na(c_across(matches(match_string_t)))) <= max_na,
                    ifelse(sum(is.na(c_across(matches(match_string_t)))) == 0,
                           sum(c_across(matches(match_string_t)), na.rm = TRUE),
                           (sum(c_across(matches(match_string_t)), na.rm = TRUE) * 
                              length(subscale_items)) / 
                             (length(subscale_items) - sum(is.na(c_across(
                                matches(match_string_t)))))), 
                    NA))
}



# calculate sub-scale score BSI
#--------------------------------------------#
calc_subScore_bsi <- function(df, new_col, max_na, subscale_items, time_point) {
  
  # prepare match string for specific time point
  match_string_t <- paste(time_point, subscale_items, sep = "")
  match_string_t <- paste(match_string_t, collapse = "|")
  # check if match string is correct
  print(match_string_t)
  
  # calculate total sub scale score per time point if NA's < cut-off
  df <- df %>%
    rowwise() %>%
    mutate(!!new_col := 
             ifelse(sum(is.na(c_across(matches(match_string_t)))) <= max_na,
                    sum(c_across(matches(match_string_t)), na.rm = TRUE)
                              / length(subscale_items), 
                    NA))
}



# calculate score CES-D
#--------------------------------------------#
calc_subScore_cesd <- function(df, new_col, max_na, items, time_point) {
  
  # prepare match string for specific time point
  match_string_t <- paste(time_point, items, sep = "")
  match_string_t <- paste(match_string_t, collapse = "|")
  # check if match string is correct
  print(match_string_t)
  
  # calculate total sub scale score per time point if NA's < cut-off
  df <- df %>%
    rowwise() %>%
    mutate(!!new_col := 
             ifelse(sum(is.na(c_across(matches(match_string_t)))) <= max_na,
                    sum(c_across(matches(match_string_t))), 
                    NA))
}



# transform wide format to long format
#--------------------------------------------#
transform_to_long <- function(df, start_col, end_col) {
  # Create long data set
  df_new <- df %>%
    # Pivot data long
    pivot_longer(cols = start_col:end_col, names_to = "time", 
                 values_to = "value")
}









