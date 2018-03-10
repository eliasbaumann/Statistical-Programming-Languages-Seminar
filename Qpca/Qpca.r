library("xts")
load("../Qprepare/prepareddataSC.RData")

# first row of train only contains zeros! 
train = scaledtrain

# Format data into POSIXct
train[, 1] = as.POSIXct(train[, 1])
dates = train[, 1]

x_raw = xts(train[, -1], order.by = train[, 1])
# Only use data until december 2014 to select the model
x.sample = x_raw["/2014-12-01"]
y.sample = (x.sample$`S&P500`)

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
