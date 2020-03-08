# Test the stationarity of time series
# Yang Xu
# 1/27/2017

library(data.table)
library(ggplot2)
library(fpp)
library(forecast)
library(lme4)


# read data
dt = readRDS('map.dt.ent_swbd.rds')

# test
dt.test = dt[, {
        res1 = Box.test(ent_swbd)
        res2 = adf.test(ent_swbd)
        res3 = kpss.test(ent_swbd)
        res4 = pp.test(ent_swbd)
        .(boxpval = res1$p.value, adfpval = res2$p.value, kpsspval = res3$p.value, pppval = res4$p.value)
    }, by = .(observation, who)]

# how many series passed stationarity tests?
nrow(dt.test[boxpval<.05,]) # 64, 25%
nrow(dt.test[adfpval<.05,]) # 211, 82.4%
nrow(dt.test[kpsspval>.05,]) # 245, 95.7%
nrow(dt.test[pppval<.05,]) # 256, 100%


####
# return to the good ole question: does entropy increase with sentence position?
dt.new = dt[, {
        .(utterID, ent_swbd, tsId = .GRP)
    }, by = .(observation, who)]
m = lmer(ent_swbd ~ utterID + (1|tsId), dt.new)
summary(m)
# n.s., t = 1.24

m = lm(ent_swbd ~ utterID, dt)
summary(m)
# 2.357   0.0185 *
# significant result
# beta = 2.299e-03, Adjusted R-squared:  0.0001681
