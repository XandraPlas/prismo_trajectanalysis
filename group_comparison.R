#--------------- SCRIPT FOR DATA ANALYSIS - GROUP COMPARISON ------------------#
#
# Description:  This script performs group comparisons between the trajectory 
#               groups defined in manuscript_trajAnalysis.Rmd.
#
# Authors:      Plas
# Date:         June 2023
# Version:      1.0
# R.version:    4.2.2 (2022-10-31)
# Rstudio:      2023.03.0+386
#
#------------------------------------------------------------------------------#

# Define path to get and save files
save_location = "~/Documents/PhD/p_PRISMO/Trajectanalyse Depressie/prismo_trajectanalysis/"

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
library(ggpattern)
library(svglite)
library(ggsignif)
library(GGally)

# statistics
library(tidySEM)
library(lavaan)
library(gmodels)

# extra
library(worcs)


#------------------------------------------------------------------------------#
#                          Data Collection
#------------------------------------------------------------------------------#

res_final <- readRDS(paste(save_location, "manuscript/res_step2023-03-28.RData", sep = ""))
res_final_mod <- res_final[[4]]

cprobs <- data.frame(class_prob(res_final_mod, type = "individual"))

n <- nrow(cprobs)
paste("class 1: ", round(nrow(cprobs[cprobs$individual.predicted == 1,]) * 100 / n), "% with ", round(nrow(cprobs[cprobs$individual.predicted == 1,])), " participants")
paste("class 2: ", round(nrow(cprobs[cprobs$individual.predicted == 2,]) * 100 / n), "% with ", round(nrow(cprobs[cprobs$individual.predicted == 2,])), " participants")
paste("class 3: ", round(nrow(cprobs[cprobs$individual.predicted == 3,]) * 100 / n), "% with ", round(nrow(cprobs[cprobs$individual.predicted == 3,])), " participants")
paste("class 4: ", round(nrow(cprobs[cprobs$individual.predicted == 4,]) * 100 / n), "% with ", round(nrow(cprobs[cprobs$individual.predicted == 4,])), " participants")

df_total <- read_excel(paste(save_location, "df_total.xlsx", sep = ""))
df_total <- df_total %>%
  filter(all_na_in_outcome_var == 0)


# add class
df_total$class <- cprobs$individual.predicted






#----------------------------------------------------------------------------------------#
#                              COVARIATES OVERALL TESTS
#----------------------------------------------------------------------------------------#
set.seed(1234)

# DEMOGRAPHICS
#----------------------------------------------------------------------------------------#

# Select variables
df_demo <- df_total %>%
  dplyr::select("gender", "age", "rank_cat", "education_cat", "work_function", "yr_deployment_cat", "Prev_deployment_dummy", "class")
df_demo$education_cat[df_demo$education_cat == 7] <- NA

p_values <- list()
ll_dif <- list()
demographics <- c("gender", "age", "rank_cat", "education_cat", "work_function", "yr_deployment_cat", "Prev_deployment_dummy")

for (demo in demographics){
  if (demo == "age"){
    res_bch <- BCH(res_final_mod, data = df_demo[[demo]])
  } else {
    res_bch <- BCH(res_final_mod, data = as.factor(df_demo[[demo]]))
  }
  res <- lr_test(res_bch)
  
  new_col_p <- paste("p_", demo, sep = "")
  p_values[[new_col_p]] <- res[["overall"]][["p"]]
  ll_dif <- append(ll_dif, res[["overall"]][["LL_dif"]])
}

df_p_values <- data.frame(p_values)



# EARLY TRAUMA
#----------------------------------------------------------------------------------------#
df_early_trauma <- df_total[grepl("^ETI", names(df_total))]
df_early_trauma$sum <- rowSums(df_early_trauma, na.rm = TRUE)
df_early_trauma$class <- cprobs$individual.predicted


res_bch <- BCH(res_final_mod, data = as.integer(df_early_trauma$sum))
res <- lr_test(res_bch) 
df_p_values["p_early_trauma"] <- res[["overall"]][["p"]]
ll_dif <- append(ll_dif, res[["overall"]][["LL_dif"]])




# DEPLOYMENT EXPERIENCE
#----------------------------------------------------------------------------------------#
# deployment experience scale (DES(or PES))
df_pes <- df_total[grepl("^PES\\d+$", names(df_total))]
df_pes$sum <- rowSums(df_pes, na.rm = TRUE)
df_pes$class <- cprobs$individual.predicted

res_bch <- BCH(res_final_mod, data = as.integer(df_pes$sum))
res <- lr_test(res_bch) 
df_p_values["p_pes"] <- res[["overall"]][["p"]]
ll_dif <- append(ll_dif, res[["overall"]][["LL_dif"]])



# PTSD
#----------------------------------------------------------------------------------------#

df_zil <- df_total[grepl("^ZIL\\d+_.", names(df_total))]

timepoints <- c("A", "B", "C", "D", "E", "G")
for (timepoint in timepoints){
  
  # get data for specific time point
  cols <- paste("^ZIL\\d+_", timepoint, sep = "")
  df <- data.frame(df_zil[, grepl(cols, names(df_zil))])
  
  # calc sum and add class
  df$sum <- rowSums(df)
  df$class <- df_total$class
  
  # auxiliary model
  res_bch <- BCH(res_final_mod, data = df$sum)
  res <- lr_test(res_bch)
  
  new_col_p <- paste("p_zil_", timepoint, sep = "")
  df_p_values[new_col_p] <- res[["overall"]][["p"]]
  ll_dif <- append(ll_dif, res[["overall"]][["LL_dif"]])
}




# LIFE EVENTS
#------------------------------------------------------------------------------#

df_life_events <- df_total[grepl("event\\d+$", names(df_total))]
exclude_items <- c(2, 3, 5, 6, 8, 20, 23) #not necessarily negative

# exclude event 21 and 22 who are not available at G
exclude_cols <- c("Devent21", "Devent22", "Eevent21", "Eevent22", "Fevent21", "Fevent22")
df_life_events <- df_life_events[, -which(names(df_life_events) %in% exclude_cols)]

# combine F and G
for (i in 1:20){
  new_col <- paste("FGevent", i, sep = "")
  df_life_events[, new_col] <- df_life_events %>% pull(paste("Fevent", i, sep = "")) + df_life_events %>% pull(paste("Gevent", i, sep = ""))
}

for (timepoint in c("D", "E", "FG")){
  
  # get data for specific time point
  cols <- paste("^", timepoint, "event\\d+$", sep = "")
  df <- data.frame(df_life_events[, grepl(cols, names(df_life_events))])
  exclude_cols <- paste(timepoint, "event", exclude_items, sep = "") 
  df <- data.frame(df[, -which(names(df) %in% exclude_cols)])
  
  # # Remove identified non-negative columns before sum
  # df_life_events <- df_life_events[, -which(names(df_life_events) %in% exclude_columns)]
  
  # calc sum and add class
  df$sum <- rowSums(df)
  df$class <- df_total$class
  
  # auxiliary model
  res_bch <- BCH(res_final_mod, data = as.integer(df$sum))
  res <- lr_test(res_bch)
  
  new_col_p <- paste("p_life_events_", timepoint, sep = "")
  df_p_values[new_col_p] <- res[["overall"]][["p"]]
  ll_dif <- append(ll_dif, res[["overall"]][["LL_dif"]])
}



# ACTIVE DUTY
#----------------------------------------------------------------------------------------#
df_interview <- read_excel("/Volumes/heronderzoek-18/Groep Geuze/15-705_PRISMO/E_ResearchData/2_ResearchData/1. PRISMO algemeen/Questionnaire data files/10-jaars interview/PRISMO_interview_G.xlsx")
df_total <- merge(df_total, df_interview[, c("moederfile", "G_Defensie")], on='moederfile', how='inner')

res_bch <- BCH(res_final_mod, data = as.factor(df_total$G_Defensie))
res <- lr_test(res_bch) 
df_p_values["p_active_duty"] <- res[["overall"]][["p"]]
ll_dif <- append(ll_dif, res[["overall"]][["LL_dif"]])


# adjust p_values for multiple comparison
#----------------------------------------------------------------------------------------#
df_p_values["adjusted_p",] <- t(p.adjust(df_p_values[1,], method = "fdr"))
df_p_values["ll_dif",] <- round(data.frame(ll_dif),2)

rm(ll_dif)








#----------------------------------------------------------------------------------------#
#                              COVARIATES POST HOC
#----------------------------------------------------------------------------------------#


# DEMOGRAPHICS
#----------------------------------------------------------------------------------------#
# age was significant (p = 0.015)

res_bch <- BCH(res_final_mod, data = as.factor(df_demo$age_cat))
res <- lr_test(res_bch)

var <- rep("age", times = 6)
mod1 <- res[["pairwise"]][["Model1"]]
mod2 <- res[["pairwise"]][["Model2"]]
ll_dif <- res[["pairwise"]][["LL_dif"]]
p_values_ph <- res[["pairwise"]][["p"]]


# explore age
CrossTable(df_demo$class, df_demo$age_cat, fisher = TRUE, chisq = TRUE,
           expected = TRUE, sresid = TRUE, format = "SPSS")


# EARLY TRAUMA
#----------------------------------------------------------------------------------------#
# early trauma was significant (p < 0.001)

res_bch <- BCH(res_final_mod, data = as.integer(df_early_trauma$sum))
res <- lr_test(res_bch)

var <- append(var, rep("early_trauma", times = 6))
mod1 <- append(mod1, res[["pairwise"]][["Model1"]])
mod2 <- append(mod2, res[["pairwise"]][["Model2"]])
ll_dif <- append(ll_dif, res[["pairwise"]][["LL_dif"]])
p_values_ph <- append(p_values_ph, res[["pairwise"]][["p"]])

# Visualize with barplot
#-----------------------------------------#
fig_data <- df_early_trauma %>% group_by(class) %>%
  summarise(meanScore = mean(as.integer(sum)))
fig_data$class_string <- c("Resilient", "Intermediate-stable", "Symptomatic-chronic", "Late-onset-increasing")
fig_data$class_string <- factor(fig_data$class_string,                                  
                                levels = c("Resilient", "Intermediate-stable", "Symptomatic-chronic", "Late-onset-increasing"))

fig <- ggplot(fig_data, aes(y = meanScore, x = class_string)) +
  geom_col_pattern(aes(fill = class_string, pattern = class_string, pattern_angle = class_string, pattern_spacing = class_string),
                   pattern_density = 0.01, pattern_spacing = 0.01,
                   pattern_fill    = 'white', pattern_colour  = 'white') +
  theme_classic() + theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Early traumas/individual") + xlab("Class") + 
  scale_fill_manual(values=c("#7FC524","#e1700e", "#2494c5", "#0E4668")) +
  scale_pattern_angle_manual(name="Class", values = c(30, 0, -30, 0)) +
  scale_pattern_manual(values = c("Resilient" = "stripe", "Intermediate-stable" = "none", "Symptomatic-chronic" = "stripe", "Late-onset-increasing" = "none"),
                       guide = "none") +
  guides(fill = guide_legend(override.aes = list(pattern = c("stripe", "none", "stripe", "none")))) +
  theme(legend.title = element_text(size=11),
        legend.text = element_text(size=10)) +
  labs(fill = "Class") 

# add significance
fig <- fig +
  geom_signif(xmin = c(1, 1, 2, 2, 3.01), 
              xmax = c(2, 3, 2.99, 4, 4),
              y_position = c(4, 6.5, 5.6, 6.0, 5.6),  
              annotation = c("**", "**", "**", "*", "**"))

fig
ggsave(paste(save_location, "manuscript/earlyTrauma_bar.svg", sep = ""), fig, device = "svg", width = 210, height = 120, units = "mm")

# in grey scale
fig <- fig +  scale_fill_manual(values=c("#000000", "#7b7d7b","#969696", "#bfbfbf"))
ggsave(paste(save_location, "manuscript/earlyTrauma_bar_greyScale.svg", sep = ""), fig, device = "svg", width = 210, height = 120, units = "mm")



# DEPLOYMENT EXPERIENCE
#----------------------------------------------------------------------------------------#
# deployment experience was significant (p < 0.001)

res_bch <- BCH(res_final_mod, data = as.integer(df_pes$sum))
res <- lr_test(res_bch)

var <- append(var, rep("pes", times = 6))
mod1 <- append(mod1, res[["pairwise"]][["Model1"]])
mod2 <- append(mod2, res[["pairwise"]][["Model2"]])
ll_dif <- append(ll_dif, res[["pairwise"]][["LL_dif"]])
p_values_ph <- append(p_values_ph, res[["pairwise"]][["p"]])


# Visualize with barplot
#-----------------------------------------#
fig_data <- df_pes %>% group_by(class) %>%
  summarise(meanScore = mean(as.integer(sum)))
fig_data$class_string <- c("Resilient", "Intermediate-stable", "Symptomatic-chronic", "Late-onset-increasing")
fig_data$class_string <- factor(fig_data$class_string,                                  
                                levels = c("Resilient", "Intermediate-stable", "Symptomatic-chronic", "Late-onset-increasing"))

fig <- ggplot(fig_data, aes(y = meanScore, x = class_string)) +
  geom_col_pattern(aes(fill = class_string, pattern = class_string, pattern_angle = class_string, pattern_spacing = class_string),
                   pattern_density = 0.01, pattern_spacing = 0.01,
                   pattern_fill    = 'white', pattern_colour  = 'white') +
  theme_classic() + theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Deployment stressors/individual") + xlab("Class") + 
  scale_fill_manual(values=c("#7FC524","#e1700e", "#2494c5", "#0E4668")) +
  scale_pattern_angle_manual(name="Class", values = c(30, 0, -30, 0)) +
  scale_pattern_manual(values = c("Resilient" = "stripe", "Intermediate-stable" = "none", "Symptomatic-chronic" = "stripe", "Late-onset-increasing" = "none"),
                       guide = "none") +
  guides(fill = guide_legend(override.aes = list(pattern = c("stripe", "none", "stripe", "none")))) +
  theme(legend.title = element_text(size=11),
        legend.text = element_text(size=10)) +
  labs(fill = "Class") 

# add significance
fig <- fig +
  geom_signif(xmin = c(1, 1, 1), 
              xmax = c(2, 3, 4),
              y_position = c(4.4, 5.2, 5.5),  
              annotation = c("**", "**", "*"))

fig
ggsave(paste(save_location, "manuscript/pes_bar.svg", sep = ""), fig, device = "svg", width = 210, height = 120, units = "mm")

# in grey scale
fig <- fig +  scale_fill_manual(values=c("#000000", "#7b7d7b","#969696", "#bfbfbf"))
ggsave(paste(save_location, "manuscript/pes_bar_greyScale.svg", sep = ""), fig, device = "svg", width = 210, height = 120, units = "mm")




# PTSD
#----------------------------------------------------------------------------------------#
# zil(ptsd) score was significant at all time points (p < 0.001)

timepoints <- c("A", "B", "C", "D", "E", "G")
for (timepoint in timepoints){
  
  # get data for specific time point
  cols <- paste("^ZIL\\d+_", timepoint, sep = "")
  df <- data.frame(df_zil[, grepl(cols, names(df_zil))])
  
  # calc sum and add class
  df$sum <- rowSums(df)
  df$class <- df_total$class
  
  # auxiliary model
  res_bch <- BCH(res_final_mod, data = df$sum)
  res <- lr_test(res_bch)
  
  var <- append(var, rep(paste("zil_", timepoint, sep = ""), times = 6))
  mod1 <- append(mod1, res[["pairwise"]][["Model1"]])
  mod2 <- append(mod2, res[["pairwise"]][["Model2"]])
  ll_dif <- append(ll_dif, res[["pairwise"]][["LL_dif"]])
  p_values_ph <- append(p_values_ph, res[["pairwise"]][["p"]])
}


# Visualize ZIL over time
#----------------------------------------#
names(df_zil) <- gsub("^ZIL(\\d+)_(.)", "ZIL_\\2_\\1", names(df_zil))

# calculate ZIL total score
zil <- tidy_sem(as.data.frame(df_zil))
zil_scales <- create_scales(zil, totals = TRUE)
zil_scores <- zil_scales$scores
zil_scores$class <- cprobs$individual.predicted

# Change class values to class names
zil_scores$class[zil_scores$class == 1] <- "Resilient"
zil_scores$class[zil_scores$class == 2] <- "Intermediate-stable"
zil_scores$class[zil_scores$class == 3] <- "Symptomatic-chronic"
zil_scores$class[zil_scores$class == 4] <- "Late-onset-increasing"

# define levels
classes <- c("Resilient", "Intermediate-stable", "Symptomatic-chronic", "Late-onset-increasing")
zil_scores$class <- factor(zil_scores$class,                                  
                           levels = c("Resilient", "Intermediate-stable", "Symptomatic-chronic", "Late-onset-increasing"))

# transform to long format
df_zil_long <- reshape(data.frame(zil_scores), direction = "long", 
                       varying = c("ZIL_A", "ZIL_B", "ZIL_C", "ZIL_D", "ZIL_E", "ZIL_G"),
                       sep = "_")

df_zil_mean <- df_zil_long[,2:4] %>%
  dplyr::group_by(time, class) %>%
  dplyr::summarise(zil_mean = mean(ZIL), zil_sd = sd(ZIL))

# plot paper line
line_plot <- ggplot(df_zil_mean) +
  geom_line(aes(x = time, y = zil_mean, group = class, color = class, linetype = class), linewidth = 1.2) +
  geom_point(aes(x = time, y = zil_mean, group = class, color = class, shape = class), size = 4) +
geom_errorbar(aes(x = time, y = zil_mean, ymin = zil_mean - (zil_sd/sqrt(nrow(df_zil))), ymax = zil_mean + (zil_sd/sqrt(nrow(df_zil))), color = class), width = .2, position = position_dodge(.05))

line_plot <- line_plot + theme_bw() +
  xlab("Time relative to deployment") +
  scale_color_manual(values=c("#7FC524","#e1700e", "#2494c5", "#0E4668"), name = "Class", 
                     labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)")) +
  scale_linetype_manual(values=c("dashed", "solid", "dotdash", "longdash"), name = "Class", 
                        labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)")) + 
  scale_shape_manual(values=c(15, 16, 17, 18), name = "Class",
                     labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)")) +
  ylab("SRIP score") + xlab("Time relative to deployment") + 
  scale_x_discrete(labels=c("A" = "pre", "B" = "1m", "C" = "6m", "D" = "1y",
                            "E" = "2y", "G" = "10y"))

line_plot <- line_plot + 
  theme(legend.title = element_text(size=11),
        legend.text = element_text(size=10)) +
  labs(fill = "Class") +
  annotate("text", x = 3.5, y=34, label= "*", size = 20)

line_plot
ggsave(paste(save_location, "manuscript/ZIL_perClass_line_paperCol.svg", sep = ""), line_plot, device = "svg", width = 210, height = 120, units = "mm")
ggsave(paste(save_location, "manuscript/ZIL_perClass_line_paperCol.eps", sep = ""), line_plot, device = "eps", width = 210, height = 120, units = "mm")


# in grey scale
line_plot <- line_plot +  scale_color_manual(values=c("#000000", "#7b7d7b","#969696", "#bfbfbf"), name = "Class", 
                                             labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)"))
ggsave(paste(save_location, "manuscript/ZIL_perClass_line_greyScale.svg", sep = ""), line_plot, device = "svg", width = 210, height = 120, units = "mm")
ggsave(paste(save_location, "manuscript/ZIL_perClass_line_greyScale.eps", sep = ""), line_plot, device = "eps", width = 210, height = 120, units = "mm")






# LIFE EVENTS
#----------------------------------------------------------------------------------------#
# life events were significant at D, E and FG (p < 0.001)

timepoints <- c("D", "E", "FG")
for (timepoint in timepoints){
  
  # get data for specific time point
  cols <- paste("^", timepoint, "event\\d+$", sep = "")
  df <- data.frame(df_life_events[, grepl(cols, names(df_life_events))])
  # exclude columns that are not per obviously negative
  exclude_cols <- paste(timepoint, "event", exclude_items, sep = "") 
  df <- data.frame(df[, -which(names(df) %in% exclude_cols)])
  
  # calc sum and add class
  df$sum <- rowSums(df)
  df$class <- df_total$class
  
  # auxiliary model
  res_bch <- BCH(res_final_mod, data = as.integer(df$sum))
  res <- lr_test(res_bch)
  
  var <- append(var, rep(paste("life_events_", timepoint, sep = ""), times = 6))
  mod1 <- append(mod1, res[["pairwise"]][["Model1"]])
  mod2 <- append(mod2, res[["pairwise"]][["Model2"]])
  ll_dif <- append(ll_dif, res[["pairwise"]][["LL_dif"]])
  p_values_ph <- append(p_values_ph, res[["pairwise"]][["p"]])
}


# Visualize life events over time
#----------------------------------------#

# exclude columns that are not per obviously negative
exclude_cols <- c(paste("Devent", exclude_items, sep = ""), paste("Eevent", exclude_items, sep = ""), paste("FGevent", exclude_items, sep = ""))
df_life_events <- data.frame(df_life_events[, -which(names(df_life_events) %in% exclude_cols)])


df_life_events <- df_life_events %>%
  mutate(sum_D = rowSums(dplyr::select(., starts_with("D")))) %>%
  mutate(sum_E = rowSums(dplyr::select(., starts_with("E")))) %>%
  mutate(sum_FG = rowSums(dplyr::select(., starts_with("FG"))))
df_life_events$class <- as.factor(df_total$class)
df_life_events$id <- 1:nrow(df_life_events)

# Reshape the dataframe to long format
df_life_events_long <- df_life_events %>%
  dplyr::select("id", "sum_D", "sum_E", "sum_FG", "class") %>%
  gather(key = "time", value = "sum", 2:4)

# Calculate mean and standard deviation
fig_data <- df_life_events_long %>%
  group_by(time, class) %>%
  summarise(
    events_mean = mean(sum, na.rm = TRUE),
    events_sd = sd(sum, na.rm = TRUE)
  )

# plot paper line
line_plot <- ggplot(fig_data) +
  geom_line(aes(x = time, y = events_mean, group = class, color = class, linetype = class), linewidth = 1.2) +
  geom_point(aes(x = time, y = events_mean, group = class, color = class, shape = class), size = 4) #+
  #geom_errorbar(aes(x = time, y = events_mean, ymin = events_mean - (events_sd/sqrt(nrow(df_life_events))), ymax = events_mean + (events_sd/sqrt(nrow(df_life_events))), color = class), width = .2, position = position_dodge(.05))

line_plot

line_plot <- line_plot + theme_bw() +
  xlab("Time relative to deployment") +
  scale_color_manual(values=c("#7FC524","#e1700e", "#2494c5", "#0E4668"), name = "Class", 
                     labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)")) +
  scale_linetype_manual(values=c("dashed", "solid", "dotdash", "longdash"), name = "Class", 
                        labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)")) + 
  scale_shape_manual(values=c(15, 16, 17, 18), name = "Class",
                     labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)")) +
  ylab("Life events/individual") + xlab("Time relative to deployment") + 
  scale_x_discrete(labels=c("sum_D" = "0-1y", "sum_E" = "1-2y", "sum_FG" = "2-10y*"))

# line_plot <- line_plot + 
#   theme(legend.title = element_text(size=11),
#         legend.text = element_text(size=10)) +
#   labs(fill = "Class") +
#   annotate("text", x = 2, y=fig_data[fig_data["time"] == "sum_E" & fig_data["class"] == "1", "events_mean"][[1]], label= "*", size = 15) +
#   annotate("text", x = 3, y=fig_data[fig_data["time"] == "sum_FG" & fig_data["class"] == "1", "events_mean"][[1]], label= "*", size = 15)

line_plot
ggsave(paste(save_location, "manuscript/lifeEvents_line.svg", sep = ""), line_plot, device = "svg", width = 210, height = 120, units = "mm")



# in grey scale
line_plot <- line_plot +  scale_color_manual(values=c("#000000", "#7b7d7b","#969696", "#bfbfbf"), name = "Class", 
                                             labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)"))
ggsave(paste(save_location, "manuscript/lifeEvents_line_greyScale.svg", sep = ""), line_plot, device = "svg", width = 210, height = 120, units = "mm")








# Correct for multiple comparisons
#----------------------------------------------------------------------------------------#
df_p_values_ph <- data.frame(var, mod1, mod2, ll_dif, p_values_ph)
df_p_values_ph$adjusted_p <- round(p.adjust(df_p_values_ph$p_values, "fdr"), 4)









#----------------------------------------------------------------------------------------#
#                       DETAILED TABLE FOR PES AND LIFE EVENTS
#----------------------------------------------------------------------------------------#



# PTSD
#----------------------------------------------------------------------------------------#
df_percentage <- df_pes %>% 
  group_by(class) %>%
  summarise(across(starts_with("PES"), ~mean(. == 1, na.rm = TRUE) * 100, .names = "{.col}_percentage"))
df_percentage <- t(round(df_percentage, 0))




# LIFE EVENTS
#----------------------------------------------------------------------------------------#

df_percentage_D <- df_life_events %>% 
  group_by(class) %>%
  summarise(across(starts_with("Devent"), ~mean(. == 1, na.rm = TRUE) * 100, .names = "{.col}_percentage"))
df_percentage_D <- data.frame(sapply(df_percentage_D, as.numeric))
df_percentage_D <- t(round(df_percentage_D, 0))

df_percentage_E <- df_life_events %>% 
  group_by(class) %>%
  summarise(across(starts_with("Eevent"), ~mean(. == 1, na.rm = TRUE) * 100, .names = "{.col}_percentage"))
df_percentage_E <- data.frame(sapply(df_percentage_E, as.numeric))
df_percentage_E <- t(round(df_percentage_E, 0))

df_percentage_FG <- df_life_events %>% 
  group_by(class) %>%
  summarise(across(starts_with("FGevent"), ~mean(. == 1, na.rm = TRUE) * 100, .names = "{.col}_percentage"))
df_percentage_FG <- data.frame(sapply(df_percentage_FG, as.numeric))
df_percentage_FG <- t(round(df_percentage_FG, 0))








# SUPPLEMENTARY MATERIALS: RELATION DEPRESSION TRAJECTORIES, DEPLOYMENT STRESSORS AND PTSD
#-------------------------------------------------------------------------------------------#





