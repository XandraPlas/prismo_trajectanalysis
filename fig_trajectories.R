#--------------- SCRIPT FOR DATA ANALYSIS - TRAJECTORY PLOT ---------------#
#
# Description:  Script to customize trajectories figure from 
#               manuscript_trajAnalysis.R,d
#
# Authors:      Plas
# Date:         March 2023
# Version:      1.0
# R.version:    4.2.2 (2022-10-31)
# Rstudio:      2023.03.0+386
#
#------------------------------------------------------------------------------#

library(readxl)
library(dplyr)
library(tidySEM)
library(ggplot2)
library(MASS)

#------------------------------------------------------------------------------#
#                                  PREPARATION
#------------------------------------------------------------------------------

# Define path to get and save files
save_location = "~/Documents/PhD/p_PRISMO/Trajectanalyse Depressie/prismo_trajectanalysis/"


# Read data
#--------------------------------#
res_final <- readRDS(paste(save_location, "manuscript/res_step2023-03-28.RData", sep = ""))
res_final_mod <- res_final[[4]]




# Prepare individual data for plot
#--------------------------------#
# define models
submods <- names(res_final_mod@submodels)

# define items
items <- res_final_mod[[submods[1]]]$manifestVars

# get raw data
rawdata <- res_final_mod$data$observed[, items]


# add class probability
cprobs <- class_prob(res_final_mod, type = "individual")[["individual"]][,submods, drop = FALSE]
rawdata <- cbind(rawdata, cprobs)
names(rawdata)[match(submods, names(rawdata))] <- paste0("Probability.", seq_along(submods))


# transform data
rawdata <- reshape(rawdata, direction = "long", 
                   varying = paste0("Probability.", seq_along(submods)), 
                   timevar = "Class", idvar = "ID")
names(rawdata)[match(items, names(rawdata))] <- paste0("Value.", items)
rawdata <- reshape(rawdata, direction = "long", varying = paste0("Value.", items), 
                   timevar = "Time")[, c("ID", "Time", "Value", "Class", "Probability")]


# define time scale
time_scale <- seq_along(items)
rawdata$Time <- ordered(rawdata$Time, levels = items)
rawdata$Time <- time_scale[as.numeric(rawdata$Time)]

# change spacing between time points
rawdata <- rawdata %>%
  mutate(time_relative = ifelse(Time == 1, 0, 
                           ifelse(Time == 2, 2, 
                                  ifelse(Time == 3, 4,
                                         ifelse(Time == 4, 8,
                                                ifelse(Time == 5, 16,
                                                       32))))))
rawdata$Class <- ordered(rawdata$Class, labels = submods)
rawdata$ID <- paste(rawdata$Title, rawdata$Class, rawdata$ID, sep = "")



# Prepare trajetoriy data
#--------------------------------#
# select growth variables
growth_variables <- res_final_mod[[submods[1]]]$latentVars

loadings <- lapply(submods, function(thismod) {
  res_final_mod[[thismod]]$A$values[items, growth_variables]})
estimates <- lapply(submods, function(thismod) {
  matrix(res_final_mod[[thismod]]$M$values[1, growth_variables], nrow = nrow(loadings[[1]]), 
         ncol = length(growth_variables), byrow = TRUE)})

# create data object with predicted trajectories
predicted_trajectories <- do.call(rbind, lapply(1:length(submods), 
                                                function(x) {
                                                  data.frame(Time = time_scale, 
                                                             Value = rowSums(loadings[[x]] * estimates[[x]]), 
                                                             Class = submods[x])
                                                }))

predicted_trajectories$Class <- ordered(predicted_trajectories$Class)

# change spacing between time points
predicted_trajectories <- predicted_trajectories %>%
  mutate(time_relative = ifelse(Time == 1, 0, 
                           ifelse(Time == 2, 2, 
                                  ifelse(Time == 3, 4,
                                         ifelse(Time == 4, 8,
                                                ifelse(Time == 5, 16,
                                                       32))))))



#------------------------------------------------------------------------------#
#                                  VISUALISATION
#------------------------------------------------------------------------------#
col_blind = c("#d55e00", "#cc79a7", "#0072b2", "#f0e442")

alpha_range <- c(0, 0.1)

# get lambda for te-transforming data
out <- yaml::read_yaml(paste(save_location, "manuscript/out.yml", sep = ""))
invbc <- function(x, lambda){
  ((x*lambda)+1)^(1/lambda)
}
out$rng_bc <- as.numeric(out$rng_bc)
# DOES NOT WORK WITH THE OUT RANGE BC.... ONLY WITH THE OUT RANGE BC FROM MANUSCRIPT


# Prepare plot
#--------------------------------#
line_plot <- ggplot(NULL)


# individual trajectories
#--------------------------------#
line_plot <- line_plot + geom_path(data = rawdata, 
                                   aes(x = .data[["time_relative"]], y = .data[["Value"]], 
                                       group = .data[["ID"]], colour = .data[["Class"]], 
                                       linetype = .data[["Class"]],
                                       alpha = .data[["Probability"]]), linewidth = 0.5) + 
  scale_alpha_continuous(range = alpha_range, guide = "none")

# Model trajectories
#--------------------------------#
line_plot <- line_plot + geom_line(data = predicted_trajectories, aes(x = .data[["time_relative"]], y = .data[["Value"]], 
                                               group = .data[["Class"]], colour = .data[["Class"]], 
                                               linetype = .data[["Class"]]), linewidth = 1.2) +
  geom_point(data = predicted_trajectories, aes(x = .data[["time_relative"]], y = .data[["Value"]],
                                                group = .data[["Class"]], shape = .data[["Class"]],
                                                colour = .data[["Class"]], linetype = .data[["Class"]]), size = 4)



# Customize figure
#--------------------------------#

line_plot <- line_plot + theme_bw() +
  xlab("Time relative to deployment") +
  scale_color_manual(values=c("#7FC524","#e1700e", "#2494c5", "#0E4668"), name = "Class", 
                     labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)")) +
  scale_linetype_manual(values=c("dashed", "solid", "dotdash", "longdash"), name = "Class", 
                        labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)")) + 
  scale_shape_manual(values=c(15, 16, 17, 18), name = "Class",
                        labels=c("Resilient (65%)", "Intermediate-stable (20%)", "Symptomatic-chronic (9%)", "late-onset-increasing (6%)"))


# re-scale and change axis labels
p <- line_plot

brks <- seq(0, 1, length.out = 5)
labs <- round(invbc(scales::rescale(brks, from = c(0, 1), to = out$rng_bc), out$lambda))
p <- p + scale_y_continuous(breaks = seq(0, 1, length.out = 5), labels = labs) 
p <- p + ylab("SCL-90 depression score")  +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 16, 32), labels=c("1" = "pre", "2" = "1m", "4" = "6m", "8" = "1y",
                                                              "15" = "2y", "30" = "10y"),
                     expand = c(0,0))

p <- p + theme(legend.title = element_text(size=11),
               legend.text = element_text(size=10))
p

# save figure
ggsave(paste(save_location, "manuscript/plot_trajectories_paperCol.svg", sep = ""), p, device = "svg", width = 210, height = 120, units = "mm")
