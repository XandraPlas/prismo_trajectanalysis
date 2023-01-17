# linearl without step
#------------------------------------------------------------------------------#
res_lin <- mx_growth_mixture(
  model =
    "i =~ 1*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
s =~ 0*scl1 + 1*scl2 + 6*scl3 + 12*scl4 + 24*scl5 + 120*scl6
scl1 ~~ vscl1*scl1
scl2 ~~ vscl2*scl2
scl3 ~~ vscl3*scl3
scl4 ~~ vscl4*scl4
scl5 ~~ vscl5*scl5
scl6 ~~ vscl6*scl6
i ~~ 0*i
s ~~ 0*s
i ~~ 0*s", classes = 1:6,
  data = dat)





# quad, step
#------------------------------------------------------------------------------#
res_quad_step <- mx_growth_mixture(
  model =
    "i =~ 1*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
step =~ 0*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
s =~ 0*scl1 + 0*scl2 + 1*scl3 + 2*scl4 + 4*scl5 + 20*scl6
q =~ 0*scl1 + 0*scl2 + 1*scl3 + 4*scl4 + 16*scl5 + 400*scl6
scl1 ~~ vscl1*scl1
scl2 ~~ vscl2*scl2
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

s ~~ 0*q", classes = 1:6,
  data = dat)