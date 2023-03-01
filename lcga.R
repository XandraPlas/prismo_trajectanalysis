library(worcs)
library(tidySEM)
set.seed(699648)
load_data()
dat[["id"]] <- NULL
names(dat) <- paste0("scl", 1:6)
dat <- as.data.frame(dat)

# Standard LCGA
res <- mx_growth_mixture(
  model =
"i =~ 1*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
s =~ 0*scl1 + 1*scl2 + 2*scl3 +3*scl4 +4*scl5 +5*scl6
scl1 ~~ vscl1*scl1
scl2 ~~ vscl2*scl2
scl3 ~~ vscl3*scl3
scl4 ~~ vscl4*scl4
scl5 ~~ vscl5*scl5
scl6 ~~ vscl6*scl6
i ~~ 0*i
s ~~ 0*s
i ~~ 0*s", classes = 1:3,
  data = dat)
# In case of convergence problems in the first model:
res[[1]] <- mxTryHardWideSearch(res[[1]])
saveRDS(res, "lcga.RData")
table_fit(res)


# LCGA with step function for effect of 
res_step <- mx_growth_mixture(
  model =
"i =~ 1*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
step =~ 0*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
s =~ 0*scl1 + 0*scl2 + 1*scl3 +2*scl4 +3*scl5 +4*scl6
scl1 ~~ vscl1*scl1
scl2 ~~ vscl2*scl2
scl3 ~~ vscl3*scl3
scl4 ~~ vscl4*scl4
scl5 ~~ vscl5*scl5
scl6 ~~ vscl6*scl6
i ~~ 0*i
step ~~ 0*step
s ~~ 0*s
i ~~ 0*s
i ~~ 0*step
s ~~ 0*step", classes = 1:3,
  data = dat)
# In case of convergence problems in the first model:
# res[[1]] <- mxTryHardWideSearch(res[[1]])
saveRDS(res_step, "res_step.RData")
table_fit(res_step)
table_results(res_step[[2]])
