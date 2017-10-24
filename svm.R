library(e1071)
library(data.table)

# Test
test = function(model, data, training, testset='test', initial=99999999) {
    if (testset == 'training') {
        s2 = subset(data, training & time < initial)
    } else {
        s2 = subset(data, !training & time < initial)
    }
    ## take mean for each dialogue
    dia = levels(as.factor(as.integer(s2$dialogue)))
    thedialnum = as.integer(s2$dialogue)
    predicted = c()
    target = c()
    for(i in dia) {
        p = predict(model, subset(s2, thedialnum==i))
        predicted = c(predicted, mean(p))
        target = c(target, mean(subset(s2, thedialnum==i)$score))
    }
    return (cor(predicted, target)^2)
}


# Leave-one-out crossvalidation
crossval = function(formula, data, testset='test', initial=99999999, folds=5, gamma=5, cv=NULL) {
    res = c()
    if (length(cv) < 1) {
        cv = sample(0:(folds-1), length(levels(data$dial)), replace=TRUE)
    }
    ## /* cv is a vector as long as the list of available dialogues.  For each dialogue it
    ##    assigns a fold. */
    thedialnum = as.integer(data$dialogue)
    for (i in 0:(folds-1)) {
        training = (cv[thedialnum]!=i)   # & data$time<initial)
        s = svm(eval(formula),  data=data, subset=training, kernel="radial", gamma=gamma)
        res = c(res, test(s, data, training, testset, initial))
    }
    print(mean(res))
    return (mean(res))
}


# Read the original data from Reitter & Moore (2007)
d = read.table('data/maptask.performance.bal.data', header=TRUE)
x = d
x5 = subset(d, time < 300)
folds = 10
commoncv1 = sample(0:(folds-1), length(levels(x$dialogue)), replace=TRUE)
commoncv2 = sample(0:(folds-1), length(levels(x5$dialogue)), replace=TRUE)


# Combine with PSO feature and RP feature
dt.pso = readRDS('data/dt.pso.rds')
dt.rp = readRDS('data/dt.peakRP.rds')
dt.comb = dt.rp[dt.pso, nomatch=0]
setkey(dt.comb, observation)

d.new = data.table(d)
setkey(d.new, dialogue)
dt.comb2 = d.new[dt.comb, nomatch=0]

folds = 10
x.new = dt.comb2
commoncv3.new = sample(0:(folds-1), length(levels(x.new$dialogue)), replace=TRUE)


# Run cross-validation
# Table 4

# Row 5: R&M + PSO + RP
result5 = crossval(quote(score ~ word.reps * letter.reps * rule.reps * word.total * letter.total * time + PSO*time + time*(peakPSmean + peakPSmedian + peakPSmax)), x.new, 'test',  folds=10, gamma=1, cv=commoncv3.new)
# 0.2235511

# Row 6:
result6 = crossval(quote(score ~ word.reps * letter.reps * rule.reps * word.total * letter.total * time + PSO*time*(peakPSmean + peakPSmedian + peakPSmax)), x.new, 'test',  folds=10, cv=commoncv3.new)
# 0.2275495
