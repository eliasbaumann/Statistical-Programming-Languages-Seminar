library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)
library(data.table)
library(xtable)

load("../Qload/data.RData")
# check for NA (missing) values
any(is.na(train))

# Stock & Watson do not treat missing values. This project does not need to as all data is
# complete.

# Compare selected series and plot them
tmp    = train[, c("date", "INDPRO", "S&P500", "GDAXI", "MANEMP")]
melted = melt(tmp, measure.vars = colnames(tmp[-1]))


unpreparedplot = ggplot(data = melted, aes(x = date, y = value, colour = variable, linetype = variable)) + 
    geom_line(size = 1) + ggtitle("Unprocessed") + scale_y_log10() + theme(panel.background = element_rect(fill = "transparent", 
    colour = "transparent"))

pdf(file = "InitialDataPlot.pdf", width = 12, height = 6, paper = "special")
unpreparedplot
dev.off()
# store plot without legend for later
unpreparedplot = unpreparedplot + theme(legend.position = "none")

# check for outliers visually
pdf(file = "box.pdf", width = 10)
boxplot(train[, -1], varwidth = TRUE)
dev.off()
# since this is macroeconomic data and all outliers are explainable by events or legislations
# (which can be treated as events as well). The paper this work is based on removes all
# observations exceeding 10 times the interquartile range from the median


# Augmented Dickey Fuller Test

# First get values to test against:
criticalValues = qnorm(c(0.01, 0.05, 0.1)/2)

# Augmented Dickey Fuller Test function 
# Input: Vector x containing data from a time series 
# Output: Boolean True or False depending on the result of the test
dickey = function(x) {
    # get change values
    dy  = diff(train[, x])
    lag = 1
    dyt = embed(dy, lag + 1)[, 1]
    
    # get lagged values
    yt_1 = train[, x][(lag + 1):length(dy)]
    # deltaYt-1
    dyt_1 = embed(dy, lag + 1)[, 2]
    
    # apply linear model to find out if regression coefficient is 1
    res = summary(lm(dyt ~ yt_1 - 1 + dyt_1))
    # can we reject that regression coefficient is 1?
    res$coefficients[1, 3] < criticalValues[3]
}

# apply augmented dickey fuller test to all series
seriesDetails$dickeyres = sapply(colnames(train[, -1]), dickey)

# Define which preprocessing step should be done for the time series This is based on the appendix
# of the paper and on the authors choices of what to apply. As it is not clear to why some of the
# preprocessing steps were applied.  we use specific keywords to find series that are treated in a
# certain way.

seriesDetails$preparation = case_when(seriesDetails$units %in% c("3-Month Annualized Percent Change", 
    "Percent", "Percent Change at Annual Rate", "Percent of Capacity") ~ "Nothing", grepl("Nonsupervisory Employees: Manufacturing", 
    seriesDetails$title) ~ "Nothing", grepl("Trade Balance", seriesDetails$title) ~ "First Difference", 
    grepl("Consumer Price", seriesDetails$title) ~ "Second Difference of Logarithm", grepl("Hourly Earnings", 
        seriesDetails$title) ~ "Second Difference of Logarithm", grepl("Index", seriesDetails$units) ~ 
        "First Difference of Logarithm", grepl("Thousands", seriesDetails$units) ~ "First Difference of Logarithm", 
    seriesDetails$units %in% "Millions of Dollars" ~ "First Difference of Logarithm", grepl("Personal Consumption", 
        seriesDetails$title) ~ "First Difference of Logarithm", TRUE ~ "Second Difference of Logarithm")

# print the table for latex paper
xtable(seriesDetails)

# create lookup
dt = as.data.table(seriesDetails)
setindex(dt, "id")

# function for first differencing of logarithm 
# Input: Vector x containing data from a time series
# Ouput: Vector containing first difference of logarithm of the series
firstDifferenceLog = function(x) {
    tmp = diff(log(x), lag = 1)
    tmp[-1]
}
# function for second differencing of logarithm 
# Input: Vector x containing data from a time series
# Ouput: Vector containing second difference of logarithm of the series
secondDifferenceLog = function(x) {
    tmp = diff(log(x), lag = 1, differences = 2)
    tmp
}

# use a lookup to figure out what function to apply to data this function applies the
# preprocessing depending on the series details lookup table 
# Input:  Data frame data contains all  time series data 
#         Data.Table lookup contains a lookup table with values for every series id
# Output: Data frame with preprocessed information
prepare = function(data, lookup) {
    tmp = data[, -1]
    
    res = sapply(colnames(tmp), function(x) if (lookup[id %in% x]$preparation == "First Difference of Logarithm") {
        firstDifferenceLog(tmp[, x])
    } else {
        if (lookup[id %in% x]$preparation == "Second Difference of Logarithm") {
            secondDifferenceLog(tmp[, x])
        } else {
            if (lookup[id %in% x]$preparation == "First Difference") {
                diff(tmp[, x], lag = 1)[-1]
            } else {
                tmp[, x][-c(1, 2)]
            }
            
        }
    })
    data = data[-c(1, 2), ]
    data[, -1] = res
    data
}

# apply the preprocessing function
trainprepared = prepare(train, dt)

# check if there is any time series that fails the dickey fuller test
any(sapply(colnames(trainprepared[, -1]), dickey) == TRUE)

# plot results
tmp     = trainprepared[, c("date", "INDPRO", "S&P500", "GDAXI", "MANEMP")]
melted2 = melt(tmp, measure.vars = colnames(tmp[-1]))

differencedloggedplot = ggplot(data = melted2, aes(x = date, y = value, colour = variable, linetype = variable)) + 
    geom_smooth(fill = NA, span = 0.1) + ggtitle("After logarithm and differencing") + theme(panel.background = element_rect(fill = "transparent", 
    colour = "transparent"), legend.position = "none")

# center and scale
scaledtrain = trainprepared

# Function to subtract the mean from a vector done separately to avoid doing it inline 
# Input:  Vector x containing numeric values 
# Output: Vector containing numeric values with mean subtracted
subtractMean = function(x) {
    x - mean(x)
}

# Calculates the standard deviation
# Input:  Vector x containing numeric values
# Output: Standard deviation (adjusted with n-1 instead of n)
std = function(x) {
    sqrt(sum((x - mean(x))^2)/((length(x) - 1)))
}

# calculates the root mean square 
# Input: Vector x containing numeric values 
# Output: Root mean square (adjusted with n-1 instead of n)
rms = function(x) {
    sqrt(sum(x^2)/(length(x) - 1))
}

# custom scale & center function based on the functionality of R's base package scale function It
# only works correctly when applying it to data column wise Applying it to a matrix will result in
# incorrect results 
# Input:  Vector x containing numeric values 
#         Boolean center decides whether the data is centered
#         Boolean scale decides wether the data is scaled 
# Output: Vector processed depending on center and scale
scaleCustom = function(x, center = T, scale = T) {
    if (center == T) {
        x = subtractMean(x)
        if (scale == T) {
            x/std(x)
        } else {
            x
        }
    } else {
        if (scale == T) {
            x/rms(x)
        } else {
            x
        }
    }
}

# Apply the custom scale function to all columns of the dataset
scaledtrain[, -1] = apply(trainprepared[, -1], 2, scaleCustom)

# save data
save(scaledtrain, file = "prepareddataSC.RData")

# create a final plot to see differences between all three plots
tmp     = scaledtrain[, c("date", "INDPRO", "S&P500", "GDAXI", "MANEMP")]
melted3 = melt(tmp, measure.vars = colnames(tmp[-1]))

scaledplot = ggplot(data = melted3, aes(x = date, y = value, colour = variable, linetype = variable)) + 
    geom_smooth(fill = NA, span = 0.1) + ggtitle("After centering and scaling") + theme(panel.background = element_rect(fill = "transparent", 
    colour = "transparent"))



pdf(file = "comparisonAfterPrep.pdf", width = 12, height = 6, paper = "special")
grid.arrange(grobs = list(unpreparedplot, differencedloggedplot, scaledplot), widths = c(2.1, 1, 1), 
    layout_matrix = rbind(c(1, 2, 2), c(3, 3, 3)))
dev.off()
