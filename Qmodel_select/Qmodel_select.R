# Model selection
# Selects models based on the Bayesian Information Criterion (BIC), for each of the desired model specifications
library("xts")
load("../Qpca/diff_index.RData")
load("../Qpca/y.sample.RData")

dat = cbind(diff_index, y.sample)
# data frame that holds all possible model parameters
model_select = expand.grid(diff = c(0:4), q = c(1:8), p = c(0:8), h = c(1,3,6,12))

model_ = function(diff, q, p, h){
  # Fits model according to input specification
  # Input : Integer diff, the number of diffusion indexes to be used
  #         Integer q, the lag length of the diffusion index term
  #         Integer p, the lag length of the AR term
  #         Integer h, the forecasting horizon
  # Output: list holding 1. numeric bic and 2. lm model object model
  h = h - 1
  
  if(diff == 0 & p != 0 & q == 1){
    model = lm(dat$S.P500 ~ lag(dat$S.P500, k=c(1:+p)+h),
               na.action = na.omit)
  } else if(p == 0){
    model = lm(dat$S.P500 ~ lag(dat[,1:diff],k=c(1:q)+h),
               na.action = na.omit)
  } else if(diff == 0 & (p == 0 | q != 1)){
    return(NA)
  } else{
    model = lm(dat$S.P500 ~ lag(dat[,1:diff],k=c(1:q)+h) + 
                 lag(dat$S.P500, k=c(1:p)+h),
               na.action = na.omit)
  }
  bic = BIC(model)
  return(list(bic, model))
}

# Applies the model_ function on all possible parameter combinations stored in model_select
res = apply(model_select, 1, function(x) model_(x['diff'],x['q'],x['p'],x['h']))
# extracts all BICs from res
model_select$BIC = sapply(res, '[[', 1)

extract_model = function(models){
  # Extracts the best models for each model family based on BIC
  # Input : data.frame x containing all models from a certain family
  # Output: data.frame out containing the rows of x with the best models based on BIC
  models_h = list(models[models$h == 1,],models[models$h == 3,],models[models$h == 6,],models[models$h == 12,])
  res     = lapply(models_h, function(x){x[which(x$BIC == min(x$BIC, na.rm = T)),]})
  out     = data.frame(rbind(res[[1]], res[[2]],res[[3]],res[[4]]))
  return(out)
}

# selects the optimal DI model based on BIC
DI_models       = model_select[model_select$p == 0 & model_select$diff > 0,]
DI_model_select = extract_model(DI_models)

# selects the optimal DI_AR model based on BIC
DI_AR_models       = model_select[model_select$diff > 0 & model_select$p > 0,]
DI_AR_model_select = extract_model(DI_AR_models)

# selects the optimal AR model based on BIC
AR_models       = model_select[model_select$diff == 0 & model_select$p > 0, ]
AR_model_select = extract_model(AR_models)

# stores all selected model specifications in data frame
model_sel = rbind(DI_model_select, DI_AR_model_select, AR_model_select)

# Extracts all choosen models from list of models and associated BICs
selected_models      = lapply(as.integer(row.names(model_sel)), function(x){res[[x]][[2]]})
row.names(model_sel) = c("DI_h1", "DI_h3", "DI_h6", "DI_h12",
                         "DI_AR_h1", "DI_AR_h3", "DI_AR_h6", "DI_AR_h12",
                         "AR_h1", "AR_h3", "AR_h6", "AR_h12")


save(model_sel, file = "model_sel.RData")
save(selected_models, file = "selected_models.RData")