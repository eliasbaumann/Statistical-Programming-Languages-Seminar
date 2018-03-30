load("../Qforecasting/forecasts.RData")
m_h1  = data.frame(DI = forecasts$DI_h1, DI_AR  = forecasts$DI_AR_h1, AR  = forecasts$AR_h1, h = 1)
m_h3  = data.frame(DI = forecasts$DI_h3, DI_AR  = forecasts$DI_AR_h3, AR  = forecasts$AR_h3, h = 3)
m_h6  = data.frame(DI = forecasts$DI_h6, DI_AR  = forecasts$DI_AR_h6, AR  = forecasts$AR_h6, h = 6)
m_h12 = data.frame(DI = forecasts$DI_h12, DI_AR = forecasts$DI_AR_h12, AR = forecasts$AR_h12, h = 12)

# Calculates the Root Mean Square Error for each of the models
RMSE.fun    = function(y_hat){sqrt(sum((forecasts$y - y_hat)^2)/length(y_hat))}
RMSE        = lapply(list(m_h1, m_h3, m_h6, m_h12), function(x) apply(x[,-4],2,RMSE.fun))
names(RMSE) = c("h1", "h3", "h6", "h12")
RMSE

# calculates the relativ mean square error compared to the AR benchmark
rel_rmse.fun        = function(m){
  return(list(DI    = m["DI"]/m["AR"], 
              DI_AR = m["DI_AR"]/m["AR"] ))
}

rel_rmse        = lapply(RMSE[], rel_rmse.fun)
names(rel_rmse) = c("h1", "h3", "h6", "h12")
rel_rmse

dm.test   = function(y_hat, benchmark, h){
  # compute forecasting errors
  e1      = (y_hat - forecasts$y)^2
  e2      = (benchmark - forecasts$y)^2
  # compute loss differential
  d       = e1 - e2
  res     = lm(d ~ 1)
  n       = length(e1)
  d.cov   = acf(d, na.action = na.omit, lag.max = h - 1, type = "covariance", plot = FALSE)$acf[, , 1]
  # compute 
  dv      = sum(c(d.cov[1], 2 * d.cov[-1])) / length(d)
  t.value = res$coefficients/sqrt(dv)
  p.value = (1-pt(abs(t.value) , df = n - 1))*2
  return(list(t.value = t.value,p.value = p.value))}

dm.results = lapply(list(m_h1, m_h3, m_h6, m_h12), function(m) list(DI = dm.test(m$DI,m$AR, h = m$h[1]), DI_AR = dm.test(m$DI_AR,m$AR, h = m$h[1])))
dm.results
