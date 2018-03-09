library("xts")
load("../Qprepare/prepareddataSC.RData")

# first row of train only contains zeros! train = unscaled_train
train = scaledtrain

# Format data into POSIXct
train[, 1] = as.POSIXct(train[, 1])
dates = train[, 1]

# probably unnecessary!!!
end_date = as.POSIXct("2015-01-01 01:00:00 CET")
last_known_data = which(dates == end_date) - 1
origin = "1970-01-01 00:00:00"

x_raw = xts(train[, -1], order.by = train[, 1])
# Only use data until december 2014 to select the model
x.sample = x_raw["/2014-12-01"]
y.sample = (x.sample$`S&P500`)

#TODO do some tests to check if we can apply pca
#i copied this 

#How many Principal Components need to be retained? - Parallel-Analysis of Horn

# 2.1) Matrix-Plot
p = solve(cor(x.sample, use = "complete.obs"))
matplot(p, show.legend = T, axes = F)
# Result: The diagonal elements of the matrix lie above the non-diagonal elements. This
# indicates suitability of dataset for PC/FA.

# 2.2) Bartlett-Test of Sphericity

# Result: P-Value is way below 0.05. The null hypothesis of the correlation matrix being the
# identity matrix can be rejected; the dataset is suitable.

# 2.3) What correlations might drive our principal comonent analysis?
pairs(dat_pca1, cex = 0.5, upper.panel = NULL)
# Interpretation: The plot implies correlations b/w most of the variables - exception: GDP
# growth rate.

# 2.4) Main indicator for suitability: KMO/MSA
KMO(dat_pca1)
# MSA for GDP-Growth-Rate is just below the 0.5 threshold (0.46). In a first attempt I keep
# it. After having conducted the first PCA, I repeat an alternative PCA w/o GDP growth rate.

pca = function(x) {
    #compute eigen values to create orthogonal matrices U and V
    xTx  = t(x) %*% x
    xxT  = x %*% t(x)
    eig1 = eigen(xTx)
    eig2 = eigen(xxT)
    V    = as.matrix(eig1$vectors)
    U    = as.matrix(eig2$vectors)
    
    dimnames(V) = list(colnames(x), paste0("PC", seq_len(ncol(V))))
    #compute sigma values and normalize
    stdev = sqrt(eig2$values)/sqrt(max(1, nrow(x) - 1))
    #compute USigma
    x     = x %*% V
    res   = list(sdev = stdev, rotation = V, x = x)
    res
}

# Extract PCAs
pca_data = pca(x.sample)
screeplot(pca_data, main = deparse(substitute(pca_data)),type = "l")

diff_index = xts(pca_data$x[, 1:5], order.by = dates[1:273])
save(diff_index, file = "diff_index.RData")
save(x.sample, file = "x.sample.RData")
save(y.sample, file = "y.sample.RData")
