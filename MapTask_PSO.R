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


# Compute spectral data
dt.ent_swbd = dt[, {
        if (.N > 0) {
            specval = spec.pgram(ent_swbd, taper=0, log='no', plot=FALSE)
            .(spec = specval$spec, freq = specval$freq)
        }
    }, by = .(observation, who)]

##
# Compute the area under curve (AUV) for spectral plots
# and then compute the ratio of the common area (power spectral overlap, PSO)
dt.ent_swbd_pso = dt.ent_swbd[, {
        x_g = freq[who=='g']
        y_g = spec[who=='g']
        x_f = freq[who=='f']
        y_f = spec[who=='f']
        # linear interpolation
        x_out = sort(union(x_g, x_f))
        approx_g = approx(x_g, y_g, xout = x_out)
        approx_f = approx(x_f, y_f, xout = x_out)
        # find min ys and remove NAs
        x_out_g = x_out[which(!is.na(approx_g$y))]
        y_out_g = approx_g$y[which(!is.na(approx_g$y))]
        x_out_f = x_out[which(!is.na(approx_f$y))]
        y_out_f = approx_f$y[which(!is.na(approx_f$y))]
        y_min = pmin(approx_g$y, approx_f$y)
        x_min = x_out[which(!is.na(y_min))]
        y_min = y_min[which(!is.na(y_min))]
        y_max = pmax(approx_g$y, approx_f$y)
        x_max = x_out[!is.na(y_max)]
        y_max = y_max[!is.na(y_max)]
        # compute AUVs and PSO
        AUV_g = trapz(x_out_g, y_out_g)
        AUV_f = trapz(x_out_f, y_out_f)
        AUV_min = trapz(x_min, y_min)
        AUV_max = trapz(x_max, y_max)
        # PSO = AUV_min / (AUV_g + AUV_f)
        PSO = AUV_min / AUV_max
        # return PSO
        .(PSO = PSO, AUVg = AUV_g, AUVf = AUV_f, AUVmin = AUV_min)
    }, by = observation]

dt.ent_swbd_pso = dt.ent_swbd_pso[dt.dev[, .(observation, pathdev)], nomatch=0]


# Model
# Table 2.
m = lm(pathdev ~ PSO, dt.ent_swbd_pso)
summary(m)
# PSO           124.83      49.39   2.527  0.01287 *
# Adjusted R-squared:  0.04513
# F-statistic: 6.388 on 1 and 113 DF,  p-value: 0.01287


# Plot regression line
# Figure 4 (a)
p = ggplot(dt.ent_swbd_pso, aes(x = PSO, y = pathdev)) +
    geom_point() +
    geom_smooth(method = 'lm', color='#0072B2') + ylab('PATHDEV') +
    theme_bw() +
    theme(legend.position='none')
pdf('figs/pathdev_vs_PSO.pdf', 4, 4)
plot(p)
dev.off()


# Test heteroskedasticity
car::ncvTest(m1)
# Chisquare = 7.235384    Df = 1     p = 0.00714805
# we can reject the null hypothesis that the variance of the residuals is constant
# and infer that heteroscedasticity is indeed present


# Box-Cox transform to pathdev
pdBCMod = caret::BoxCoxTrans(dt.ent_swbd_pso$pathdev)
dt.ent_swbd_pso = cbind(dt.ent_swbd_pso, pathdev_new = predict(pdBCMod, dt.ent_swbd_pso$pathdev))

m0 = lm(pathdev_new ~ PSO, dt.ent_swbd_pso)
summary(m0)
# PSO           3.8528     1.6699   2.307   0.0229 *
# Adjusted R-squared:  0.03654
# F-statistic: 5.323 on 1 and 113 DF,  p-value: 0.02287
# Still significant
