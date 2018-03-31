library("xts")
load("../Qprepare/prepareddataSC.RData")
load("../Qmodel_select/model_sel.RData")
load("../Qmodel_select/selected_models.RData")
source("../Qpca/Qpca.R")

origin          = "1970-01-01 00:00:00"
dates           = as.POSIXct(scaledtrain[,1])
x_raw           = xts(scaledtrain[,-1], order.by = dates)
predictions     = data.frame(date = "", y_hat_DI = "", y_hat_DI_AR = "", y_hat_AR = "")
end_date        = as.POSIXct("2015-01-01 01:00:00 CET")
last_known_data = which(dates == end_date)-1
m               = length(selected_models)
forecast_dates  = dates[dates >= "2015-01-01" &  dates  <= "2016-12-01"]

prep_dat = function(t,h){
  # constructs input data for the step wise model fitting and prediction
  # Input : integer t holds the month that is objective to forecasting, t=0 => 01-2015
  #         integer h forecasting horizon
  # Output: xts dat holding the diffusion indexes 1:5 and y.sample
  x        = x_raw[1:(last_known_data + t - h),] 
  y.sample = x$`S&P500`
  pca_data = pca(x)
  diff_index = xts(pca_data$x[,1:5], order.by = dates[1:(last_known_data + t - h)] )
  dat = cbind(diff_index, y.sample)
  return(dat)
}

update_predict = function(i,t){
  # updates the i'th model of list selected models using data dat and then forecasts the t'th month
  # Input : integer i specifies the model which is object to forecasting
  #         integer t holds the month that is objective to forecasting, t=0 => 01-2015
  # Output: numeric prediction holding the point prediction of the model
  diff = model_sel$diff[i]
  h = model_sel$h[i] - 1 
  p = model_sel$p[i]
  q = model_sel$q[i]
  
  dat   = prep_dat(t,h)
  model = update(selected_models[[i]], data = dat)
  
  dat        = prep_dat(t,h)
  prediction = tail(predict(model, newdata = dat),1)
  return(prediction)
}

# res holds all combinations of models i and forecasting objective months t
res = expand.grid(i = c(1:m),t = c(0:length(forecast_dates)))
# Applies the update predict function on res
res$predictions = apply(res,1, function(x) update_predict(x['i'], x['t']))

# Use dplyr::inner_join without loading the package, because dplyr masks xts::lag no matter in what sequence the packages are loaded
# join the information about the models stored in model_sel with the predictions in res
joined_predictions = dplyr::inner_join(model_sel, res)

# put each models prediction in a column of a data.frame and name the columns accordingly
forecasts = data.frame(sapply(c(1:m),function(i) joined_predictions[joined_predictions$i == i,"predictions"]))
colnames(forecasts) = row.names(model_sel)
forecasts$y = as.numeric(x_raw$`S&P500`["2015-01-01/2016-12-01"])
forecasts$date = forecast_dates

plot_models = function(DI, DI_AR, AR){
  # plots all three models in comparison to the realized data y
  # Input : numeric DI, DI_AR, AR holding the forecasts from each of the models
  # Output: plot
  plot(forecasts$date, forecasts$y, type = "b",pch=19, xlab = "Month", ylab = "y-value" )
  points(forecasts$date, DI, type = "b",pch=19, col ="red")
  points(forecasts$date, DI_AR, type = "b",pch=19, col ="orange")
  points(forecasts$date, AR, type = "b",pch=19, col ="green")
  legend( x="bottomright", 
          legend=c("y","y_hat_DI", "y_hat_DI_AR", "y_hat_AR"),
          col=c("black","red", "orange", "green"), lwd=1, lty=c(1,1,2,1)
          , pch=19, cex = 0.8)
}
#{jpeg('PCAplot.jpg')
#dev.off()}
plot_models(forecasts$DI_h1, forecasts$DI_AR_h1, forecasts$AR_h1)
plot_models(forecasts$DI_h3, forecasts$DI_AR_h3, forecasts$AR_h3)
plot_models(forecasts$DI_h6, forecasts$DI_AR_h6, forecasts$AR_h6)
plot_models(forecasts$DI_h12, forecasts$DI_AR_h12, forecasts$AR_h12)

save(forecasts, file="forecasts.RData")

