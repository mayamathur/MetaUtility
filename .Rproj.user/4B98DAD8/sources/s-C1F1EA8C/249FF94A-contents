

library(testthat)
library(metafor)
library(purrr)


d = metafor::escalc(measure="RR", ai=tpos, bi=tneg,
                    ci=cpos, di=cneg, data=metafor::dat.bcg)

# fit random-effects model
# note that metafor package returns on the log scale
m = metafor::rma.uni(yi= d$yi, vi=d$vi, knha=TRUE,
                     measure="RR", method="REML" )

# pooled point estimate (RR scale)
exp(m$b)


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



# Phat, calibrated: lower tail
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

# Phat, calibrated: upper tail
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


# Phat, sign test, upper tail
q = log(.8)
Phat = prop_stronger( q = q,

                      estimate.method = "calibrated",
                      ci.method = "calibrated",

                      dat = d,
                      yi.name = "yi",
                      vi.name = "vi",
                      tail = "below")

# sanity check: get sign test p-value of the CI limits
pct.vec = seq( 0, 1, 0.001 )

pvals = pct.vec %>% map( function(x) phi( theta = d$yi,
                                          theta.sd = sqrt(d$vi),
                                          mu = q,
                                          pct = x ) ) %>%
  unlist # return a double-type vector instead of list


plot(pct.vec, pvals)


prop_stronger_sign(q = q,
                   yi = d$yi,
                   vi = d$vi,
                   tail = "below",
                   return.vectors = FALSE )

# Phat values for this q that are rejected at the alpha = 0.05 level
Phat.keep = pct.vec[pvals>0.05]
min(Phat.keep)
max(Phat.keep)

pvals[ pct.vec == .92 ]

phi(theta = d$yi,
    theta.sd = sqrt(d$vi),
    mu = q,
    pct = .025)


# should give warning about parametric inference
# Phat, sign test, upper tail
q = -.6
prop_stronger( q = q,
                      M = as.numeric(m$b),
                      t2 = m$tau2,
                      se.M = as.numeric(m$vb),
                      se.t2 = m$se.tau2,
                      CI.level = 0.95,

                      estimate.method = "calibrated",
                      #ci.method = "sign.test",

                      dat = d,
                      yi.name = "yi",
                      vi.name = "vi",
                      tail = "above")


######## example code using package ######
# example

