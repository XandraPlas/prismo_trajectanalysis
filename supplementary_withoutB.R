# Supplementary without B

# linear with step
set.seed(67426)
res_notB <- mx_growth_mixture(
  model =
    "i =~ 1*scl1 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
step =~ 0*scl1 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
s =~ 0*scl1 + 0*scl3 + 1*scl4 + 2*scl5 + 10*scl6
scl1 ~~ vscl1*scl1
scl3 ~~ vscl3*scl3
scl4 ~~ vscl4*scl4
scl5 ~~ vscl5*scl5
scl6 ~~ vscl6*scl6
i ~~ 0*i
step ~~ 0*step
s ~~ 0*s

i ~~ 0*step
i ~~ 0*s

step ~~ 0*s", classes = 1:5,
  data = dat[, -2])

tab_fit_notB <- table_fit(res_notB)
tab_fit_notB$pers_per_parameter <- tab_fit_notB[, "n_min"] * nrow(dat[,-2]) / tab_fit_notB[, "Parameters"]

tidySEM::plot_growth(res_notB[[3]], rawdata = TRUE, alpha_range = c(0, .05))
saveRDS(res_notB, paste0("res_notB", Sys.Date(), ".RData"))


# quadratic with step
set.seed(67426)
res_notB_q <- mx_growth_mixture(
  model =
    "i =~ 1*scl1 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
step =~ 0*scl1 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
s =~ 0*scl1 + 0*scl3 + 1*scl4 + 2*scl5 + 10*scl6
q =~ 0*scl1 + 0*scl3 + 1*scl4 + 4*scl5 + 100*scl6
scl1 ~~ vscl1*scl1
scl3 ~~ vscl3*scl3
scl4 ~~ vscl4*scl4
scl5 ~~ vscl5*scl5
scl6 ~~ vscl6*scl6
i ~~ 0*i
step ~~ 0*step
s ~~ 0*s
q ~~ 0*q

i ~~ 0*step
i ~~ 0*s
i ~~ 0*q

step ~~ 0*s
step ~~ 0*q

s ~~ 0*q", classes = 1:5,
  data = dat[, -2])

tab_fit_notB_q <- table_fit(res_notB_q)
tab_fit_notB_q$pers_per_parameter <- tab_fit_notB_q[, "n_min"] * nrow(dat[,-2]) / tab_fit_notB_q[, "Parameters"]

tidySEM::plot_growth(res_notB_q[[2]], rawdata = TRUE, alpha_range = c(0, .05))
saveRDS(res_notB_q, paste0("res_notB_q", Sys.Date(), ".RData"))






