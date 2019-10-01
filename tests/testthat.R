


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
               ci.method = "parametric",

               dat = d,
               yi.name = "yi",
               vi.name = "vi",
               tail = "below",
               bootstrap = "ifneeded")
