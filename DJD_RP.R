library(data.table)
library(MASS)
library(pracma)
library(ggplot2)

# Read data
dt = fread('data/all_pairs_entropy.txt')
setnames(dt, c('pairId', 'who', 'ent'))
setkey(dt, pairId, who)

# Read performance data
dt.pf = fread('data/PerformanceData.tsv')
setkey(dt.pf, Pair)


# Relative phase (RP) at the peak freqs
dt.peakPS = dt[, {
        y_a = ent[who=='A']
        y_b = ent[who=='B']
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
    }, by = pairId]
dt.peakPS = dt.peakPS[dt.pf, nomatch=0]

# Compute mean, median, and max peak relative phase
dt.peakPS.mean = dt.peakPS[, {
        .(peakPSmean = mean(abs(peakPS)), peakPSmedian = median(abs(peakPS)), peakPSmax = max(abs(peakPS)), peakN = .N)
    }, by = pairId]
dt.peakPS.mean = dt.peakPS.mean[dt.pf, nomatch=0]


# Model
# Table 3.
m = lm(CollectivePerformance ~ peakPSmean + peakPSmedian + peakPSmax, dt.peakPS.mean)
summary(m)
step = stepAIC(m)
step$anova
# Final Model:
# CollectivePerformance ~ peakPSmean + peakPSmedian + peakPSmax
# peakPSmean     15.646      5.677   2.756   0.0174 *
# peakPSmedian   -7.429      3.609  -2.058   0.0619 .
# peakPSmax     -11.451      7.176  -1.596   0.1366
# Adjusted R-squared:  0.3541
# F-statistic: 3.742 on 3 and 12 DF,  p-value: 0.04158

