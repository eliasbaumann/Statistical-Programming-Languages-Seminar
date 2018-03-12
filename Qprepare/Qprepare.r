library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)
library(data.table)
library(xtable)

load("../Qload/data.RData")
# check for NA values
any(is.na(train))

#Stock & Watson do not treat missing values. This project does not need to as all data is complete.

# compare some data
tmp    = train[, c("date", "INDPRO", "S&P500", "GDAXI", "MANEMP")]
melted = melt(tmp, measure.vars = colnames(tmp[-1]))


unpreparedplot = ggplot(data = melted, aes(x = date, y = value, colour = variable, linetype = variable)) + geom_line(size = 1) + 
    ggtitle("Unprocessed") + scale_y_log10() + theme(panel.background = element_rect(fill = "transparent", colour = "transparent"))

pdf(file = "InitialDataPlot.pdf", width = 12, height = 6, paper = "special")
unpreparedplot
dev.off()

unpreparedplot = unpreparedplot + theme(legend.position = "none")

# check for outliers visually
boxplot(train[, -1], varwidth = TRUE)
pdf(file = "box.pdf", width = 10)
boxplot(train[, -1], varwidth = TRUE)
dev.off()
# since this is macroeconomic data and all outliers are explainable by events or legislations (which can be
# treated as events as well). The paper this work is based on removes all observations exceeding 10 times the
# interquartile range from the median

# Augmented Dickey Fuller Test

# First get values to test against:
criticalValues = qnorm(c(0.01, 0.05, 0.1)/2)

# Augmented Dickey Fuller Test function. 
# x: input data (a single time series)
dickey = function(x) {
    # get change values
    dy  = diff(train[, x])
    lag = 1
    # dyt
    dyt = embed(dy, lag + 1)[, 1]
    
    # get lagged values
    yt_1  = train[, x][(lag + 1):length(dy)]
    # deltaYt-1
    dyt_1 = embed(dy, lag+1)[, 2]
    
    # apply linear model to find out if regression coefficient is 1
    res = summary(lm(dyt ~ yt_1 - 1 + dyt_1))
    # can we reject that regression coefficient is 1?
    res$coefficients[1, 3] < criticalValues[3]
}

# apply augmented dickey fuller test to all series
seriesDetails$dickeyres = sapply(colnames(train[, -1]), dickey)

# Define which preprocessing step should be done for the time series This is based on the appendix of the paper
# and on the authors choices of what to apply

seriesDetails$preparation = case_when(
    seriesDetails$units %in% c("3-Month Annualized Percent Change", "Percent", 
    "Percent Change at Annual Rate", "Percent of Capacity") ~ "Nothing", 
    grepl("Nonsupervisory Employees: Manufacturing", seriesDetails$title) ~ "Nothing", 
    grepl("Trade Balance", seriesDetails$title) ~ "First Difference",
    grepl("Consumer Price", seriesDetails$title) ~ "Second Difference of Logarithm", 
    grepl("Hourly Earnings", seriesDetails$title) ~ "Second Difference of Logarithm", 
    grepl("Index", seriesDetails$units) ~ "First Difference of Logarithm", 
    grepl("Thousands", seriesDetails$units) ~ "First Difference of Logarithm",
    seriesDetails$units %in% "Millions of Dollars" ~ "First Difference of Logarithm", 
    grepl("Personal Consumption", seriesDetails$title) ~ "First Difference of Logarithm",
    TRUE ~ "Second Difference of Logarithm"
    )

#print the table for latex paper
xtable(seriesDetails)

# create lookup
dt = as.data.table(seriesDetails)
setindex(dt, "id")

# function for first differencing
firstDifferenceLog = function(x) {
    tmp = diff(log(x), lag = 1)
    tmp[-1]
}
# function for second differencing
secondDifferenceLog = function(x) {
    tmp = diff(log(x), lag = 1, differences = 2)
    tmp
}

# use lookup to figure out what function to apply to data
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
    data       = data[-c(1, 2), ]
    data[, -1] = res
    data
}

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
scaledtrain       = trainprepared
scaledtrain[, -1] = sapply(trainprepared[, -1], scale)

# save data 
save(scaledtrain, file = "prepareddataSC.RData")

tmp     = scaledtrain[, c("date", "INDPRO", "S&P500", "GDAXI", "MANEMP")]
melted3 = melt(tmp, measure.vars = colnames(tmp[-1]))

# create a final plot to see differences between all three plots
scaledplot = ggplot(data = melted3, aes(x = date, y = value, colour = variable, linetype = variable)) + geom_smooth(fill = NA, 
    span = 0.1) + ggtitle("After centering and scaling") + theme(panel.background = element_rect(fill = "transparent", 
    colour = "transparent"))



pdf(file = "comparisonAfterPrep.pdf", width = 12, height = 6, paper = "special")
grid.arrange(grobs = list(unpreparedplot, differencedloggedplot, scaledplot), widths = c(2.1, 1, 1), layout_matrix = rbind(c(1, 
    2, 2), c(3, 3, 3)))
dev.off()
