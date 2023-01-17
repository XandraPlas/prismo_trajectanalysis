# In this file, write the R-code necessary to load your original data file
# (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# use the function open_data(your_data_frame) or closed_data(your_data_frame)
# to store the data.
library(svglite)
library(worcs)
library(readxl)
library(tidySEM)
library(ggplot2)
library(lavaan)
library(missRanger)
library(MASS)

df <- read_excel("df_total.xlsx")
df <- df[grepl("^.SCL\\d+$", names(df))]
names(df) <- gsub("^(.).+?(\\d+)$", "SCL_\\1_\\2", names(df))
df <- df[!grepl("SCL_F", names(df), fixed = T)]

closed_data(df)