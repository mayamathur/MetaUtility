

library(testthat)
library(metafor)


d = metafor::escalc(measure="RR", ai=tpos, bi=tneg,
                    ci=cpos, di=cneg, data=metafor::dat.bcg)

# fit random-effects model
# note that metafor package returns on the log scale
m = metafor::rma.uni(yi= d$yi, vi=d$vi, knha=TRUE,
                     measure="RR", method="REML" )

# pooled point estimate (RR scale)
exp(m$b)

# estimate the proportion of effects stronger than RR = 0.80
# no bootstrapping will be needed
prop_stronger( q = log(0.8),
               M = as.numeric(m$b),
               t2 = m$tau2,
               se.M = as.numeric(m$vb),
               se.t2 = m$se.tau2,
               CI.level = 0.95,

               estimate.method = "calibrated",
               ci.method = "calibrated",

               dat = d,
               yi.name = "yi",
               vi.name = "vi",
               tail = "below",
               R = 100,
               bootstrap = "ifneeded")

######## test ########

# calculate calibrated estimates manually to test
# with a different method of estimating Mhat and t2
meta = rma.uni( yi = d$yi,
                vi = d$vi,
                method = "REML")
my.ens = as.numeric( c(meta$b) + sqrt( c(meta$tau2) / ( c(meta$tau2) + d$vi ) ) * (d$yi - c(meta$b) ) )

expect_equal( my.ens,
              calib_ests(yi = d$yi,
                         sei = sqrt(d$vi),
                         method = "REML") )

# calculate calibrated estimates manually to test
# default (DL) method
meta = rma.uni( yi = d$yi,
                   vi = d$vi,
                   method = "DL")
my.ens = as.numeric( c(meta$b) + sqrt( c(meta$tau2) / ( c(meta$tau2) + d$vi ) ) * (d$yi - c(meta$b) ) )

expect_equal( my.ens,
              calib_ests(yi = d$yi,
                         sei = sqrt(d$vi) ) )



# Phat: lower tail
Phat = prop_stronger( q = log(0.8),
                      M = as.numeric(m$b),
                      t2 = m$tau2,
                      se.M = as.numeric(m$vb),
                      se.t2 = m$se.tau2,
                      CI.level = 0.95,

                      estimate.method = "calibrated",
                      ci.method = "calibrated",

                      dat = d,
                      yi.name = "yi",
                      vi.name = "vi",
                      tail = "below")

expect_equal( Phat$Est,
              mean(my.ens < log(.8)) )

# Phat: upper tail
Phat = prop_stronger( q = log(.8),
                      M = as.numeric(m$b),
                      t2 = m$tau2,
                      se.M = as.numeric(m$vb),
                      se.t2 = m$se.tau2,
                      CI.level = 0.95,

                      estimate.method = "calibrated",
                      ci.method = "calibrated",

                      dat = d,
                      yi.name = "yi",
                      vi.name = "vi",
                      tail = "above")

expect_equal( Phat$Est,
              mean(my.ens > log(.9)) )







