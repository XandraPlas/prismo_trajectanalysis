#--------- SCRIPT FOR SUPPLEMENTARY ANALYSIS - TRAJECTORY ANALYSIS ------------#
#
# Description:  This script reads the scl data that is used for the lcga in
#               manuscript.Rmd and the total data to select the CES_D data.
#               Two new lcga's will be performed. One for the CES-D data obtained on
#               time point D, E, F, and G. And another LCGA with the
#               SCL90R data on time point D, E, and G only. These two analysis
#               can be compared.
#
# Authors:      Plas
# Date:         Feb 2023
# Version:      1.0
# R.version:    4.2.1 (2022-06-23)
#
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
#                          Settings & Dependencies
#------------------------------------------------------------------------------#

# Define path to get and save files
save_location = "~/Documents/PhD/p_PRISMO/Trajectanalyse Depressie/prismo_trajectanalysis/"




#------------------------------------------------------------------------------#
#                              Data Collection
#------------------------------------------------------------------------------#

# read scl data and select time point D, E, and F
dat_scl <- read.csv(paste(save_location, "lcga_dat_scl.csv", sep = ""), stringsAsFactors = FALSE)
dat_scl_DEG <- dat_scl[,4:6]

# read total data and get cesd data
df_total <- read_excel(paste(save_location, "df_total.xlsx", sep = ""))

dat_cesd_DEFG <- df_total %>%
  filter(all_na_in_cesd == 0)

dat_cesd_DEFG <- dat_cesd_DEFG[grepl("^.CESD\\d+$", names(dat_cesd_DEFG))]
names(dat_cesd_DEFG) <- gsub("^(.).+?(\\d+)$", "CESD_\\1_\\2", names(dat_cesd_DEFG))



#------------------------------------------------------------------------------#
#                              Data Manipulation
#------------------------------------------------------------------------------#

# Get descriptives
desc <- descriptives(dat_cesd_DEFG) # descriptives item level
desc <- desc[, !colSums(is.na(desc)) == nrow(desc)] # remove columns with NAs only


# Use single imputation
dat_cesd_DEFG <- dat_cesd_DEFG[, grepl("^CESD", names(dat_cesd_DEFG))]
names(dat_cesd_DEFG) <- gsub("^CESD_(.)_(\\d+)$", "CESD\\2_\\1", names(dat_cesd_DEFG))
set.seed(73274)
df_imp <- missRanger::missRanger(dat_cesd_DEFG)

# add 1 for transformations
df_imp <- df_imp + 1

saveRDS(df_imp, paste(save_location, "Extra/df_imp_cesd.RData", sep = ""))



# Compute scale scores
df_imp <- readRDS(paste(save_location, "Extra/df_imp_cesd.RData", sep = ""))
df_cesd <- df_imp
names(df_cesd) <- gsub("^CESD(\\d+)_(.)$", "CESD_\\2_\\1", names(df_cesd))
#df_cesd[] <- lapply(df_cesd, function(i){as.integer(as.character(i))})
if(anyNA(df_cesd)) stop("Requires complete data")
cesd <- tidy_sem(as.data.frame(df_cesd))
cesd_scales <- create_scales(cesd, totals = TRUE)
desc <- cesd_scales$descriptives
write.csv(desc, paste(save_location, "Extra/scale_desc_cesd.csv", sep = ""), row.names = FALSE)



# Transformation
df_scores_cesd <- cesd_scales$scores
df_scores_cesd <- reshape(df_scores_cesd, direction= "long", varying = names(df_scores_cesd), sep = "_")
rng_cesd <- range(df_scores_cesd$CESD)

df_scores_cesd$log <- scales::rescale(log(df_scores_cesd$CESD), to = c(0, 1))
df_scores_cesd$sqrt <- scales::rescale(sqrt(df_scores_cesd$CESD), to = c(0, 1))
df_scores_cesd$qrt <- scales::rescale(df_scores_cesd$CESD^.33, to = c(0, 1))
df_scores_cesd$reciprocal <- scales::rescale(1/df_scores_cesd$CESD, to = c(0, 1))
bc <- function(x, lambda){
  (((x ^ lambda) - 1) / lambda)
}
invbc <- function(x, lambda){
  ((x*lambda)+1)^(1/lambda)
}
b <- MASS::boxcox(lm(df_scores_cesd$CESD ~ 1))
lambda <- b$x[which.max(b$y)]

df_scores_cesd$boxcox <- bc(df_scores_cesd$CESD, lambda)
rng_bc <- range(df_scores_cesd$boxcox)
df_scores_cesd$boxcox <- scales::rescale(df_scores_cesd$boxcox, to = c(0, 1))
df_scores_cesd$CESD <- scales::rescale(df_scores_cesd$CESD, to = c(0, 1))

df_plot <- do.call(rbind, lapply(c("CESD", "log", "sqrt", "qrt", "boxcox"), function(n){
  data.frame(df_scores_cesd[c("time", "id")],
             Value = df_scores_cesd[[n]],
             Transformation = n)
}))
p_trans <- ggplot(df_plot, aes(x = Value, colour = Transformation)) + geom_density() + facet_wrap(~time) + scale_y_sqrt() + xlab("CESD (rescaled to 0-1)")
ggsave(paste(save_location, "Extra/transformations_cesd.svg", sep = ""), p_trans, device = "svg", width = 210, height = 120, units = "mm")



# prepare cesd data for lcga
dat_cesd <- df_scores_cesd[, c("id", "time", "boxcox")]
dat_cesd <- reshape(dat_cesd, direction = "wide", v.names = "boxcox", timevar = "time", idvar = "id")
names(dat_cesd) <- gsub("boxcox.", "cesd_", names(dat_cesd))

dat_cesd[["id"]] <- NULL
names(dat_cesd) <- paste0("cesd", 1:4)
dat_cesd <- data.frame(dat_cesd)


#------------------------------------------------------------------------------#
#                              Measurement Invariance
#------------------------------------------------------------------------------#


df_imp[] <- lapply(df_imp, function(i){
  ordered(round(i))
})

# Make data long for multilevel CFA
df_long <- reshape(as.data.frame(df_imp), direction = "long", varying = names(df_imp), sep = "_")
# Multilevel CFA
mod1 <- paste0("F =~ ", paste0(grep(
  "^CESD", names(df_long), value = T
), collapse = "+"))
res1 <-
  cfa(
    model = mod1,
    data = df_long[, -1],
    ordered = grep("^CESD", names(df_long), value = T),
    cluster = "id",
    std.lv = TRUE,
    auto.fix.first = FALSE,
    estimator ="WLSMV"
  )
saveRDS(res1, paste(save_location, "Extra/res_cesd1.RData", sep = ""))

# Configural invariance model
wavs <- LETTERS[4:7]
mod2 <- paste0(sapply(wavs, function(w){
  paste0("F", w, " =~ ", paste0(grep(
    paste0("^CESD.+_", w), names(df_imp), value = T
  ), collapse = "+"))}), collapse = "\n")

res2 <-
  cfa(
    model = mod2,
    data = df_imp,
    ordered = names(df_imp),
    std.lv = TRUE,
    auto.fix.first = FALSE,
    estimator ="WLSMV" 
  )
saveRDS(res2, paste(save_location, "Extra/res_cesd2.RData", sep = ""))

# Metric invariance model
mod3 <- paste0(sapply(wavs, function(w){
  paste0("F", w, " =~ ", paste0(paste0("l", 1:16, " * "), paste0(grep(
    paste0("^CESD.+_", w), names(df_imp), value = T
  )), collapse = "+"))}), collapse = "\n")

res3 <-
  cfa(
    model = mod3,
    data = df_imp,
    ordered = names(df_imp),
    std.lv = TRUE,
    auto.fix.first = FALSE,
    estimator ="WLSMV" 
  )
saveRDS(res3, paste(save_location, "Extra/res_cesd3.RData", sep = ""))

tab <- table_fit(res1)[c("Name", "Parameters", "chisq", "df", "cfi", "tli", "rmsea", "srmr")]
tab <- rbind(tab, table_fit(res2)[c("Name", "Parameters", "chisq", "df", "cfi", "tli", "rmsea", "srmr")])
tab <- rbind(tab, table_fit(res3)[c("Name", "Parameters", "chisq", "df", "cfi", "tli", "rmsea", "srmr")])
tab$Name <- c("CFA", "Configural", "Metric")
write_xlsx(tab, paste(save_location, "Extra/measurement_invariance_cesd.xlsx", sep = ""))


#------------------------------------------------------------------------------#
#                              LCGA Analysis
#------------------------------------------------------------------------------#

# SCL-90-R DEG
#--------------------------------#
# linear
set.seed(67426)
res_scl_DEG <- mx_growth_mixture(
  model =
    "i =~ 1*scl4 +1*scl5 +1*scl6
s =~ 1*scl4 + 2*scl5 + 10*scl6
scl4 ~~ vscl4*scl4
scl5 ~~ vscl5*scl5
scl6 ~~ vscl6*scl6
i ~~ 0*i
s ~~ 0*s
i ~~ 0*s", classes = 1:5,
  data = dat_scl_DEG)
# In case of convergence problems in the first model:
n <- 1
res_scl_DEG[[n]] <- mxTryHardWideSearch(res_scl_DEG[[n]], extraTries = 100)

saveRDS(res_scl_DEG, paste0(paste(save_location, "Extra/res_scl_DEG", sep = ""), Sys.Date(), ".RData"))




# quadratic
set.seed(67426)
res_scl_DEG_q <- mx_growth_mixture(
  model =
    "i =~ 1*scl4 +1*scl5 +1*scl6
s =~ 1*scl4 + 2*scl5 + 10*scl6
q =~ 1*scl4 + 4*scl5 + 100*scl6
scl4 ~~ vscl4*scl4
scl5 ~~ vscl5*scl5
scl6 ~~ vscl6*scl6
i ~~ 0*i
s ~~ 0*s
q ~~ 0*q
i ~~ 0*s
i ~~ 0*q
s ~~ 0*q", classes = 1:5,
  data = dat_scl_DEG)

# In case of convergence problems in the first model:
n <- 5
res_scl_DEG_q[[n]] <- mxTryHardWideSearch(res_scl_DEG_q[[n]], extraTries = 200)

saveRDS(res_scl_DEG_q, paste0(paste(save_location, "Extra/res_scl_DEG_q", sep = ""), Sys.Date(), ".RData"))

# table fit
res <- c(res_scl_DEG, res_scl_DEG_q)
class(res) <- c("mixture_list", "list")
names(res) <- c(paste0("linear", 1:length(res_scl_DEG)),
                paste0("quadratic", 1:length(res_scl_DEG_q)))
tab_fit <- table_fit(res)
write.csv(tab_fit, paste(save_location, "Extra/tab_fit_res_sclDEG.csv", sep = ""), row.names = FALSE)
write_xlsx(tab_fit, paste(save_location, "Extra/tab_fit_res_sclDEG.xlsx", sep = ""))


# plot
tidySEM::plot_growth(res_scl_DEG[[2]], rawdata = TRUE, alpha_range = c(0, .05))
tidySEM::plot_growth(res_scl_DEG_q[[3]], rawdata = TRUE, alpha_range = c(0, .05))

# scree plot
p <- tidySEM:::plot.tidy_fit(tab_fit, statistics = c("AIC", "BIC", "saBIC"))
ggsave(paste(save_location, "Extra/plot_scree_sclDEG.svg", sep = ""), p, device = "svg", width = 210, height = 120, units = "mm")




# CES-D DEG
#--------------------------------#
# linear
set.seed(67426)
res_cesd <- mx_growth_mixture(
  model =
    "i =~ 1*cesd1 +1*cesd2 +1*cesd3 +1*cesd4
s =~ 1*cesd1 + 2*cesd2 + 5*cesd3 +10*cesd4
cesd1 ~~ vcesd1*cesd1
cesd2 ~~ vcesd2*cesd2
cesd3 ~~ vcesd3*cesd3
cesd4 ~~ vcesd4*cesd4
i ~~ 0*i
s ~~ 0*s
i ~~ 0*s", classes = 1:5,
  data = dat_cesd)
# In case of convergence problems in the first model:
n <- 1
res_cesd[[n]] <- mxTryHardWideSearch(res_cesd[[n]], extraTries = 100)

saveRDS(res_cesd, paste0(paste(save_location, "Extra/res_cesd", sep = ""), Sys.Date(), ".RData"))


# quadratic
set.seed(67426)
res_cesd_q <- mx_growth_mixture(
  model =
    "i =~ 1*cesd1 +1*cesd2 +1*cesd3 +1*cesd4
s =~ 1*cesd1 + 2*cesd2 + 5*cesd3 +10*cesd4
q =~ 1*cesd1 + 4*cesd2 + 25*cesd3 +100*cesd4
cesd1 ~~ vcesd1*cesd1
cesd2 ~~ vcesd2*cesd2
cesd3 ~~ vcesd3*cesd3
cesd4 ~~ vcesd4*cesd4
i ~~ 0*i
s ~~ 0*s
q ~~ 0*q
i ~~ 0*s
i ~~ 0*q
s ~~ 0*q", classes = 1:5,
  data = dat_cesd)

# In case of convergence problems in the first model:
n <- 2
res_cesd_q[[n]] <- mxTryHardWideSearch(res_cesd_q[[n]], extraTries = 500)

saveRDS(res_cesd_q, paste0(paste(save_location, "Extra/res_cesd_q", sep = ""), Sys.Date(), ".RData"))





# table fit
res_CESD <- c(res_cesd, res_cesd_q)
class(res_CESD) <- c("mixture_list", "list")
names(res_CESD) <- c(paste0("linear", 1:length(res_cesd)),
                paste0("quadratic", 1:length(res_cesd_q)))
tab_fit_cesd <- table_fit(res_CESD)
write.csv(tab_fit_CESD, paste(save_location, "Extra/tab_fit_res_cesd.csv", sep = ""), row.names = FALSE)
write_xlsx(tab_fit_CESD, paste(save_location, "Extra/tab_fit_res_cesd.xlsx", sep = ""))





# scree plot
p <- tidySEM:::plot.tidy_fit(tab_fit_cesd, statistics = c("AIC", "BIC", "saBIC"))
ggsave(paste(save_location, "Extra/plot_scree_cesd.svg", sep = ""), p, device = "svg", width = 210, height = 120, units = "mm")


# plot trajectories
p <- tidySEM::plot_growth(res_cesd[[3]], rawdata = TRUE, alpha_range = c(0, .05))
brks <- seq(0, 1, length.out = 5)
labs <- round(invbc(scales::rescale(brks, from = c(0, 1), to = rng_bc), lambda)) - 20
p <- p + scale_y_continuous(breaks = seq(0, 1, length.out = 5), labels = labs) + ylab("CES-D (rescaled from Box-Cox)")
ggsave(paste(save_location, "Extra/lcga_trajectories_cesd.svg", sep = ""), p, device = "svg", width = 210, height = 120, units = "mm")

p_q <- tidySEM::plot_growth(res_cesd_q[[3]], rawdata = TRUE, alpha_range = c(0, .05))
p_q <- p_q + scale_y_continuous(breaks = seq(0, 1, length.out = 5), labels = labs) + ylab("CES-D (rescaled from Box-Cox)")
ggsave(paste(save_location, "Extra/lcga_trajectories_cesd_q.svg", sep = ""), p_q, device = "svg", width = 210, height = 120, units = "mm")






# parameters and waldtest
tab_res <- table_results(res_cesd[[3]])
write_xlsx(tab_res, paste(save_location, "Extra/tab_res_cesd.xlsx", sep = ""))

tab_res_q <- table_results(res_cesd_q[[3]])
write_xlsx(tab_res_q, paste(save_location, "Extra/tab_res_cesd_q.xlsx", sep = ""))


# wald_tests <- tidySEM::wald_test(res_cesd[[3]], 
#                                  "class1.M[1,7] = class2.M[1,7]&class1.M[1,7] = class3.M[1,7];class1.M[1,8] = class2.M[1,8]&class1.M[1,8] = class3.M[1,8]")
# wald_tests$Hypothesis <- c("Mean i", "Mean slope")
# write.csv(wald_tests, paste(save_location, "Extra/tab_wald_res_cesd_3.csv", sep = ""), row.names = FALSE)









res_final_mod <- res_cesd_q[[3]]
cprobs <- data.frame(class_prob(res_final_mod, type = "individual"))

n <- nrow(cprobs)
paste("class 1: ", round(nrow(cprobs[cprobs$individual.predicted == 1,]) * 100 / n), "%. 
      Participants:", round(nrow(cprobs[cprobs$individual.predicted == 1,])))
paste("class 2: ", round(nrow(cprobs[cprobs$individual.predicted == 2,]) * 100 / n), "%. 
      Participants:", round(nrow(cprobs[cprobs$individual.predicted == 2,])))
paste("class 3: ", round(nrow(cprobs[cprobs$individual.predicted == 3,]) * 100 / n), "%. 
      Participants:", round(nrow(cprobs[cprobs$individual.predicted == 3,])))




