library(data.table)
library(ggplot2)
library(lme4)
library(MASS)
library(pracma)


# Read maptask data
dt = readRDS('data/map.dt.ent_swbd.rds')
setkey(dt, observation, who)

# Read the dataset that contains pathdev info
dt.dev = fread('data/moves_and_deviation.csv')
setnames(dt.dev, 'Observation', 'observation')
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)


# Phase shift at the peak freqs
dt.peakPS = dt[, {
        y_a = ent_swbd[who=='f']
        y_b = ent_swbd[who=='g']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at all peaks
        i_max_a = which(diff(sign(diff(spec$spec[,1])))<0) + 1
        i_max_b = which(diff(sign(diff(spec$spec[,2])))<0) + 1
        peakPS = spec$phase[,1][union(i_max_a, i_max_b)]
        # return
        .(peakPS = peakPS)
    }, by = observation]
dt.peakPS = dt.peakPS[dt.dev[, .(observation, pathdev)], nomatch=0]

# Compute mean, median and max values for peakPS
dt.peakPS.mean = dt.peakPS[, {
        .(peakPSmean = mean(abs(peakPS)), peakPSmedian = median(abs(peakPS)), peakPSmax = max(abs(peakPS)))
    }, by = observation]
dt.peakPS.mean = dt.peakPS.mean[dt.dev[, .(observation, pathdev)], nomatch=0]


# Model
# Table 3.
m = lm(pathdev ~ peakPSmean + peakPSmedian + peakPSmax, dt.peakPS.mean)
step = stepAIC(m)
step$anova
# Final Model:
# pathdev ~ peakPSmax
m = lm(pathdev ~ peakPSmax, dt.peakPS.mean)
summary(m)
# peakPSmax     -64.86      30.33  -2.138  0.03463 *
# Adjusted R-squared:  0.03039
