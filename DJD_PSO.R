library(data.table)
library(MASS)
library(pracma)
library(ggplot2)

# read data
dt = fread('data/all_pairs_entropy.txt')
# dt = fread('data/all_pairs_entropy_new.txt')
setkey(dt, pairId, who)

# read performance data
dt.pf = fread('data/PerformanceData.tsv')
setkey(dt.pf, Pair)

##
# analysis
dt.spec = dt[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.pso = dt.spec[, {
        x_A = freq[who=='A']
        y_A = spec[who=='A']
        x_B = freq[who=='B']
        y_B = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_A, x_B))
        approx_A = approx(x_A, y_A, xout = x_out)
        approx_B = approx(x_B, y_B, xout = x_out)
        # find min ys and remove NAs
        x_out_A = x_out[!is.na(approx_A$y)]
        y_out_A = approx_A$y[!is.na(approx_A$y)]
        x_out_B = x_out[!is.na(approx_B$y)]
        y_out_B = approx_B$y[!is.na(approx_B$y)]
        y_min = pmin(approx_A$y, approx_B$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        y_max = pmax(approx_A$y, approx_B$y)
        x_max = x_out[!is.na(y_max)]
        y_max = y_max[!is.na(y_max)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        AUV_max = trapz(x_max, y_max)
        # PSO = AUV_min / (AUV_A + AUV_B)
        PSO = AUV_min / AUV_max
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.pso = dt.pso[dt.pf, nomatch=0]


# Model
# Table 2.
m = lm(CollectivePerformance ~ PSO, dt.pso)
summary(m)
# PSO          -23.154      9.011  -2.569  0.02226 *
# Adjusted R-squared:  0.2711
# F-statistic: 6.602 on 1 and 14 DF, p-value: 0.02226


# Plot regression line
# Figure 4. (b)
p = ggplot(dt.pso, aes(x=PSO, y=CollectivePerformance)) +
    geom_point() +
    geom_smooth(method = lm, color='#D55E00') +
    theme_bw()
pdf('figs/CollectivePerformance_vs_PSO.pdf', 4, 4)
plot(p)
dev.off()
