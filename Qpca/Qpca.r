library("xts")
load("../Qprepare/prepareddataSC.RData")

train = scaledtrain

# Format data into POSIXct
train[, 1] = as.POSIXct(train[, 1])
dates      = train[, 1]

x_raw = xts(train[, -1], order.by = train[, 1])
# Only use data until december 2014 to select the model Later data will be used for evaluation
x.sample = x_raw["/2014-12-01"]
y.sample = (x.sample$`S&P500`)

# Custom implementation of R's prcomp to calculate principal components using SVD 
# Input: Matrix containing time series data values 
# Output: Matrix x containing principal components 
#         Vector sdev containing standard deviation for PCs 
#         Matrix rotation containing rotations
pca = function(x) {
    # compute eigen values to create orthogonal matrices U and V
    xTx  = t(x) %*% x
    xxT  = x %*% t(x)
    eig1 = eigen(xTx)
    eig2 = eigen(xxT)
    V    = as.matrix(eig1$vectors)
    U    = as.matrix(eig2$vectors)
    
    dimnames(V) = list(colnames(x), paste0("PC", seq_len(ncol(V))))
    # compute sigma values and normalize
    stdev = sqrt(eig2$values)/sqrt(max(1, nrow(x) - 1))
    
    # compute USigma
    x   = x %*% V
    res = list(sdev = stdev, rotation = V, x = x)
    res
}

# Extract PCs
pca_data = pca(x.sample)

# Plot scree plot to check for elbow for principal component selection
pdf(file = "PCAplot.pdf", width = 8, height = 6, paper = "special")
screeplot(pca_data, main = deparse(substitute(pca_data)), type = "l")
dev.off()

# Save data
diff_index = xts(pca_data$x[, 1:4], order.by = dates[1:273])
save(diff_index, file = "diff_index.RData")
save(x.sample, file = "x.sample.RData")
save(y.sample, file = "y.sample.RData")
