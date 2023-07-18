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

df_demo <- read_excel(paste(save_location, "df_total.xlsx", sep = ""))
df_demo <- df_demo %>%
  filter(all_na_in_outcome_var == 0)


# add class
df_demo$class <- cprobs$individual.predicted


# # visualize individual trajectories per class (not imputed)
# df_plot <- data.frame(df_demo %>% 
#                         filter(class == 1) %>%
#                         dplyr::select("ASCL90_depr":"GSCL90_depr"))
# 
# names(df_plot) <- gsub("(.)SCL90_depr", "SCL90_\\1", names(df_plot))
# df_plot_long <- reshape(df_plot, direction= "long", varying = names(df_plot), sep = "_")
# 
# ggplot(data = df_plot_long, aes(
#   x = time, y = SCL90, group = id)) +
#   geom_line(size = 0.1)





#------------------------------------------------------------------------------#
#                              COVARIATES
#------------------------------------------------------------------------------#
set.seed(1234)

# Select variables
dat <- df_demo %>%
  dplyr::select("gender", "age_cat", "rank_cat", "education_cat", "work_function", "yr_deployment_cat", "Prev_deployment_dummy", "class")
dat$education_cat[dat$education_cat == 7] <- NA


# Demographics
#--------------------------------#
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
res_bch <- BCH(res_final_mod, data = as.factor(dat$yr_deployment_cat))
lr_test(res_bch)
res_bch <- BCH(res_final_mod, data = as.factor(dat$Prev_deployment_dummy))
lr_test(res_bch)
# age shows significant differences


# explore age
CrossTable(dat$class, dat$age_cat, fisher = TRUE, chisq = TRUE,
           expected = TRUE, sresid = TRUE, format = "SPSS")


# Deployment experience
#--------------------------------#
# deployment experience scale (DES(or PES))
df_pes <- df_demo[grepl("^PES\\d+$", names(df_demo))]
df_pes$class <- cprobs$individual.predicted


res_bch <- BCH(res_final_mod, data = as.factor(df_pes$PES13))
lr_test(res_bch) # 6 (1-2, 1-3), 13(1-2), 15 (1 to all), 16(1 to all), 17 (1-2, 1-3)


variable <- "PES17"
CrossTable(df_pes$class, df_pes %>% pull(variable), fisher = TRUE, chisq = TRUE,
           expected = TRUE, sresid = TRUE, format = "SPSS")


# PTSS
#--------------------------------#

df_zil <- df_demo[grepl("^ZIL\\d+_.", names(df_demo))]
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

timepoints <- c("A", "B", "C", "D", "E", "G")
for (timepoint in timepoints){
  # prepare data
  df_zil_stats <- zil_scores %>% 
    dplyr::select(paste("ZIL", timepoint, sep = "_"), "class") %>%
    rename(ZIL = paste("ZIL", timepoint, sep = "_"))
  
  
  # test difference
  res_bch <- BCH(res_final_mod, data = df_zil_stats$ZIL)
  res <- lr_test(res_bch) 
  print(res)
  
  df_1 <- df_zil_stats[df_zil_stats$class == "Resilient", "ZIL"]
  df_2 <- df_zil_stats[df_zil_stats$class == "Intermediate-stable", "ZIL"]
  df_3 <- df_zil_stats[df_zil_stats$class == "Symptomatic-chronic", "ZIL"]
  df_4 <- df_zil_stats[df_zil_stats$class == "Late-onset-increasing", "ZIL"]
  print(summary(df_1)); print(summary(df_2)); print(summary(df_3)); print(summary(df_4))
}



# Visualize ZIL over time
#--------------------------------#
# transform to long format
df_zil_long <- reshape(data.frame(zil_scores), direction = "long", 
                       varying = c("ZIL_A", "ZIL_B", "ZIL_C", "ZIL_D", "ZIL_E", "ZIL_G"),
                       sep = "_")


df_zil_mean <- df_zil_long[,2:4] %>%
  dplyr::group_by(time, class) %>%
  dplyr::summarise(zil_mean = mean(ZIL), zil_sd = sd(ZIL))



# plot EC
fig <- ggplot(df_zil_mean, aes(fill = class, y = zil_mean, x = time)) + 
  geom_bar(position = "dodge", stat = "identity")  +
  geom_errorbar(aes(ymin = zil_mean - zil_sd, ymax = zil_mean + zil_sd), width = .2, position = position_dodge(.9)) +
  theme_classic() + theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_y_continuous(lim = c(0,55), expand = c(0,0)) +
  ylab("SRIP score") + xlab("Time relative to deployment") + 
  scale_x_discrete(labels=c("A" = "pre", "B" = "1m", "C" = "6m", "D" = "1y",
                            "E" = "2y", "G" = "10y")) + 
  scale_fill_manual(values=c("#7FC524","#e1700e", "#2494c5", "#0E4668"))

fig <- fig + 
  theme(legend.title = element_text(size=11),
        legend.text = element_text(size=10)) +
  labs(fill = "Class")

fig

ggsave(paste(save_location, "manuscript/ZIL_perClass_ECcolours.svg", sep = ""), fig, device = "svg", width = 210, height = 120, units = "mm")




# plot paper
fig <- ggplot(df_zil_mean, aes(fill = class, y = zil_mean, x = time)) +
  geom_col_pattern(position = "dodge", stat = "identity",
    aes(pattern = class, pattern_angle = class, pattern_spacing = class),
    pattern_density = 0.01, pattern_spacing = 0.01,
    pattern_fill    = 'white', pattern_colour  = 'white') +
  geom_errorbar(aes(ymin = zil_mean - zil_sd, ymax = zil_mean + zil_sd), width = .2, position = position_dodge(.9)) +
  theme_classic() + theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_y_continuous(lim = c(0,55), expand = c(0,0)) +
  ylab("SRIP score") + xlab("Time relative to deployment") + 
  scale_x_discrete(labels=c("A" = "pre", "B" = "1m", "C" = "6m", "D" = "1y",
                            "E" = "2y", "G" = "10y")) + 
  scale_fill_manual(values=c("#7FC524","#e1700e", "#2494c5", "#0E4668")) +
  # scale_fill_grey(start = 0, end = .7) +
  scale_pattern_angle_manual(name="Class", values = c(30, 0, -30, 0)) +
  scale_pattern_manual(values = c("Resilient" = "stripe", "Intermediate-stable" = "none", "Symptomatic-chronic" = "stripe", "Late-onset-increasing" = "none"),
                       guide = "none") +
  guides(fill = guide_legend(override.aes = list(pattern = c("stripe", "none", "stripe", "none"))))

fig <- fig + 
  theme(legend.title = element_text(size=11),
        legend.text = element_text(size=10)) +
  labs(fill = "Class") +
  annotate("text", x = 3.5, y=50, label= "*", size = 20)

fig

ggsave(paste(save_location, "manuscript/ZIL_perClass_paperCol.svg", sep = ""), fig, device = "svg", width = 210, height = 120, units = "mm")
# ggsave(paste(save_location, "manuscript/ZIL_perClass_greyCol.svg", sep = ""), fig, device = "svg", width = 210, height = 120, units = "mm")





# check with non-paramteric alternative of anova --> kruskal wallis
df_zil_test <- df_zil_long[df_zil_long$time == "D",]
kruskal.test(ZIL ~ class, data = df_zil_test)
pairwise.wilcox.test(df_zil_test$ZIL, df_zil_test$class,
                     p.adjust.method = "BH")



#------------------------------------------------------------------------------#
#               EXPLAIN TRAJECTORIES WITH MAJOR LIFE CHANGES?
#------------------------------------------------------------------------------#

# combine F and G
for (i in 1:20){
  new_col <- paste("FGevent", i, sep = "")
  df_demo[, new_col] <- df_demo %>% pull(paste("Fevent", i, sep = "")) + df_demo %>% pull(paste("Gevent", i, sep = ""))
}


# calculate differences
set.seed(1234)
for (timepoint in c("D", "E", "FG")){
  print(timepoint)
  
  # get specific cols
  cols <- paste("^", timepoint, "event\\d+$", sep = "")
  df_demo_lifeChange <- data.frame(df_demo[, grepl(cols, names(df_demo))])
  
  if (timepoint == "FG"){
    df_demo_lifeChange[df_demo_lifeChange == 2] <- 1
  }
  
  # calc sum and add class
  df_demo_lifeChange$sum <- rowSums(df_demo_lifeChange)
  df_demo_lifeChange$class <- df_demo$class

  for (i in 1:20){
    event <- paste(timepoint, "event", i, sep = "")
   
    # count participants per group
    n_1 <- sum(!is.na(df_demo_lifeChange[df_demo_lifeChange$class == 1, event]))
    n_2 <- sum(!is.na(df_demo_lifeChange[df_demo_lifeChange$class == 2, event]))
    n_3 <- sum(!is.na(df_demo_lifeChange[df_demo_lifeChange$class == 3, event]))
    n_4 <- sum(!is.na(df_demo_lifeChange[df_demo_lifeChange$class == 4, event]))
    
    # test differences
    res_bch <- BCH(res_final_mod, data = as.factor(df_demo_lifeChange %>% pull(event)))
    res <- lr_test(res_bch) 
    
    if (timepoint == "D" & i == 1){
      results <- data.frame(group1 = res[["pairwise"]][["Model1"]],
                            group2 = res[["pairwise"]][["Model2"]])
      count_table <- data.frame(class = c("class1", "class2", "class3", "class4"))
    }
    
    new_col_p <- paste("p_", event, sep = "")
    results[[new_col_p]] <- res[["pairwise"]][["p"]]
    
    # table with number of life changes per person
    new_col_count <- paste("count_", event, sep = "")
    count_table[count_table$class == "class1", new_col_count] <- 
      round(sum(df_demo_lifeChange[df_demo_lifeChange$class == "1", event], na.rm = TRUE) / n_1, 1)
    count_table[count_table$class == "class2", new_col_count] <- 
      round(sum(df_demo_lifeChange[df_demo_lifeChange$class == "2", event], na.rm = TRUE) / n_2, 1)
    count_table[count_table$class == "class3", new_col_count] <-
      round(sum(df_demo_lifeChange[df_demo_lifeChange$class == "3", event], na.rm = TRUE) / n_3, 1)
    count_table[count_table$class == "class4", new_col_count] <- 
      round(sum(df_demo_lifeChange[df_demo_lifeChange$class == "4", event], na.rm = TRUE) / n_4, 1)
    
  }
  
  # test differences of sum
  res_bch_sum <- BCH(res_final_mod, data = df_demo_lifeChange %>% pull("sum"))
  res_sum <- lr_test(res_bch_sum) 
  
  results[[paste("p_", timepoint, "sum", sep = "")]] <- res_sum[["pairwise"]][["p"]]
  
  
  # table with number of life changes per person
  count_table[count_table$class == "class1", paste("count_", timepoint, "sum", sep = "")] <- 
    round(sum(df_demo_lifeChange[df_demo_lifeChange$class == "1", "sum"], na.rm = TRUE) / n_1, 1)
  count_table[count_table$class == "class2", paste("count_", timepoint, "sum", sep = "")] <- 
    round(sum(df_demo_lifeChange[df_demo_lifeChange$class == "2", "sum"], na.rm = TRUE) / n_2, 1)
  count_table[count_table$class == "class3", paste("count_", timepoint, "sum", sep = "")] <- 
    round(sum(df_demo_lifeChange[df_demo_lifeChange$class == "3", "sum"], na.rm = TRUE) / n_3, 1)
  count_table[count_table$class == "class4", paste("count_", timepoint, "sum", sep = "")] <- 
    round(sum(df_demo_lifeChange[df_demo_lifeChange$class == "4", "sum"], na.rm = TRUE) / n_4, 1)
}

results[,3:ncol(results)] <- round(results[,3:ncol(results)], 2)
write_xlsx(results, paste(save_location, "manuscript/pValues_lifeChanges.xlsx", sep = ""))



fig_data <- count_table[, grepl("sum", names(count_table))]
names(fig_data) <- gsub("^count_(.+)sum$", "countSum_\\1", names(fig_data))

fig_data <- cbind(data.frame(class = c("Resilient", "Intermediate-stable", "Symptomatic-chronic", "Late-onset-increasing")), fig_data)
fig_data$class <- factor(fig_data$class,                                
                  levels = c("Resilient", "Intermediate-stable", "Symptomatic-chronic", "Late-onset-increasing"))

fig_data <- gather(fig_data, "time", "count", 2:4)

# Visualize with Barplot
fig <- ggplot(fig_data, aes(y = count, x = time)) +
  geom_col_pattern(position = "dodge",
                   aes(fill = class, pattern = class, pattern_angle = class, pattern_spacing = class),
                   pattern_density = 0.01, pattern_spacing = 0.01,
                   pattern_fill    = 'white', pattern_colour  = 'white') +
  theme_classic() + theme(axis.line.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Life events/individual") + xlab("Time relative to deployment") + 
  scale_x_discrete(labels=c("countSum_D" = "0-1y", "countSum_E" = "1-2y", "countSum_FG" = "2-10y*")) + 
  scale_fill_manual(values=c("#7FC524","#e1700e", "#2494c5", "#0E4668")) +
  scale_pattern_angle_manual(name="Class", values = c(30, 0, -30, 0)) +
  scale_pattern_manual(values = c("Resilient" = "stripe", "Intermediate-stable" = "none", "Symptomatic-chronic" = "stripe", "Late-onset-increasing" = "none"),
                       guide = "none") +
  guides(fill = guide_legend(override.aes = list(pattern = c("stripe", "none", "stripe", "none")))) +
  theme(legend.title = element_text(size=11),
        legend.text = element_text(size=10)) +
  labs(fill = "Class") 

# add signifiance
fig <- fig +
  geom_signif(xmin = c(0.65, 1.65, 1.65, 1.65, 2.65, 2.65, 2.65), 
              xmax = c(1.125, 1.875, 2.125, 2.35, 2.875, 3.125, 3.35),
              y_position = c(3.3, 3.3, 3.7, 4.1, 5.2, 5.6, 6.0),  
              annotation = c("*", "**", "**", "*", "**", "**", "**"))

fig

ggsave(paste(save_location, "manuscript/LifeEvents_bar.svg", sep = ""), fig, device = "svg", width = 210, height = 120, units = "mm")
