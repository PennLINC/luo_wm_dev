library(mgcv)
library(gratia)
library(tidyverse)
library(dplyr)

######################################
# FIT GAM SMOOTH (FOR TRACT PROFILES)
######################################
## Function to fit a GAM (nodewise_measure ~ s(smooth_var, k = knots, fx = set_fx) + covariates)) 
## per each node for each tract and save out statistics and derivative-based characteristics
gam.fit.smooth <- function(gam.data, tract_node, smooth_var, covariates, knots, set_fx = FALSE){
  # Fit the GAM
  gam.data$sex <- as.factor(gam.data$sex)
  modelformula <- as.formula(sprintf("%s ~ s(%s, k = %s, fx = %s) + %s", tract_node, smooth_var, knots, set_fx, covariates))  # 3 knots, fx = T should term be unpenalized  
  gam.model <- gam(modelformula, method = "REML", data = gam.data)
  gam.results <- summary(gam.model)
  
  # GAM derivatives
  ## Get derivatives of the smooth function using the gratia derivatives function; gratia estimates derivatives from GAM smooths via finite differences
  derv <- derivatives(gam.model, term = sprintf('s(%s)',smooth_var), interval = "simultaneous", unconditional = F) #derivative at 200 indices of smooth_var with a simultaneous CI
  ## Identify derivative significance window (basically finding where there is significant change in a node)
  derv <- derv %>%
    mutate(sig = !(0 >lower & 0 < upper)) #add "sig" column (TRUE/FALSE) to derv. 
    # Derivative is sig if the lower CI is not < 0 while the upper CI is > 0 (i.e., in the CI does not include 0).
  derv$sig_deriv = derv$derivative*derv$sig #add "sig_deriv column that has all the significant derivative values, 
    #but all non-significant derivatives are set to 0. Aka, mask out non-significant deriv points by the derv$sig TRUE/FALSE column
    
  # GAM statistics
  ## Get the F value for the smooth term and the GAM-based significance of the term
  ## (F-tests on smooth terms (rather than for the full model) are 
  ## joint tests for equality to zero for all of the coefficients making up a single spline term)
  gam.smooth.F <- gam.results$s.table[3]
  gam.smooth.pvalue <- gam.results$s.table[4]
  
  # Calculate the magnitude and significance of the **smooth term** effect by comparing full and reduced models
  ## Compare a full model GAM (with the smooth term) to a nested, reduced model (with covariates only)
  nullmodel <- as.formula(sprintf("%s ~ %s", tract_node, covariates)) #no smooth term
  gam.nullmodel <- gam(nullmodel, method = "REML", data = gam.data)
  gam.nullmodel.results <- summary(gam.nullmodel)
  
  ## Full versus reduced model anova p-value
  anova.smooth.pvalue <- anova.gam(gam.nullmodel,gam.model,test='F')$`Pr(>F)`[2] 
  
  ## Full versus reduced model: delta R.sq (adj)
  ### effect size
  adjRsq <- abs(gam.results$r.sq - gam.nullmodel.results$r.sq) 
  ### effect direction
  linearmodel <- as.formula(sprintf("%s ~ %s + %s", tract_node, smooth_var, covariates))
  lm.model.t <- summary(lm(linearmodel, data=gam.data))$coefficients[2,3] #t-value for smooth_var
  if(lm.model.t < 0){ #if the linear model t-value for smooth_var is less than 0, make the delta adj R.sq negative
    adjRsq <- adjRsq*-1}
  
  ## Full versus reduced model: direction-dependent partial R squared
  ### effect size
  sse.model <- sum((gam.model$y - gam.model$fitted.values)^2)
  sse.nullmodel <- sum((gam.nullmodel$y - gam.nullmodel$fitted.values)^2)
  partialRsq <- (sse.nullmodel - sse.model)/sse.nullmodel
  ### effect direction
  mean.derivative <- mean(derv$derivative)
  if(mean.derivative < 0){ #if the average derivative is less than 0, make the effect size estimate negative
    partialRsq <- partialRsq*-1}
  
  # Derivative-based temporal characteristics
  ## Age of maximal developmental change
  if(sum(derv$sig) > 0){ 
    derv$abs_sig_deriv = round(abs(derv$sig_deriv),5) #absolute value significant derivatives
    maxval <- max(derv$abs_sig_deriv) #find the largest derivative
    window.peak.change <- derv$data[derv$abs_sig_deriv == maxval] #identify the age(s) at which the derivative is greatest in absolute magnitude
    peak.change <- mean(window.peak.change)} #identify the age of peak developmental change
  if(sum(derv$sig) == 0){ 
    peak.change <- NA}  
  
  ## Age of decrease offset: age of maturation if DWI measure decreases with age (e.g. MD)
  if(sum(derv$sig) > 0){ 
    decreasing.range <- derv$data[derv$sig_deriv < 0] #identify all ages with a significant negative derivative 
    # (i.e., smooth_var indices where y is decreasing)
    if(length(decreasing.range) > 0){
      last.decrease <- max(decreasing.range) #find oldest age with a significant negative derivative
      if(last.decrease == derv$data[length(derv$data)]) #if the last age of significant decrease is the oldest age in the dataset
        decrease.offset <- last.decrease
        #decrease.offset <- 0 # the node never matures if the latest significant decrease occurs at oldest age. Not using this approach since we want to include immature nodes in sensitivity analyses.
      if(last.decrease != derv$data[length(derv$data)]){  # if age of last significant derivative is NOT the oldest age,
        decrease.offset.row <- which(derv$data == last.decrease) + 1 #use above to find the first age when the derivative is not significant
        decrease.offset <- derv$data[decrease.offset.row]}
    }
    if(length(decreasing.range) == 0) # if there are no ages with a significant negative derivative, then set decrease.offset to NA
      decrease.offset <- NA}
  if(sum(derv$sig) == 0){ 
    decrease.offset <- NA}  
  
  ## Age of increase offset: age of maturation if DWI measure increases with age (e.g. FA)
  if(sum(derv$sig) > 0){ 
    increasing.range <- derv$data[derv$sig_deriv > 0] #identify all ages with a significant positive derivative (i.e., smooth_var indices where y is increasing)
    if(length(increasing.range) > 0){
      last.increase <- max(increasing.range) #find oldest age with a significant positive derivative
      if(last.increase == derv$data[length(derv$data)]) #if the last age of significant increase is the oldest in the dataset
        increase.offset <- last.increase
      if(last.increase != derv$data[length(derv$data)]){ 
        increase.offset.row <- which(derv$data == last.increase) + 1 #use above to find the first age when the derivative is not significant
        increase.offset <- derv$data[increase.offset.row]}
    }
    if(length(increasing.range) == 0)
      increase.offset <- NA}
  if(sum(derv$sig) == 0){ 
    increase.offset <- NA}  
  
  ## Age of last change
  if(sum(derv$sig) > 0){ 
    change.offset <- max(derv$data[derv$sig==T])} #find last age in the smooth where derivative is significant
  if(sum(derv$sig) == 0){ 
    change.offset <- NA}  
  
  ## Age of first developmental slowing
  maxval <- max(derv$abs_sig_deriv) # find the largest derivative
  ### find the first age where the derivative is less than the maximum derivative
  if (sum(derv$sig) > 0) {
    slowing.range <- derv$data[abs(derv$abs_sig_deriv) < maxval]
    if (length(slowing.range) > 0) {
      slowing.onset <- min(slowing.range) # find the first age of developmental slowing
    } else {
      slowing.onset <- NA
    }
  } else {
    slowing.onset <- NA
  }
  
  # Model Fit
  model_AIC <- AIC(gam.model)
  edf <- k.check(gam.model)[2]
  k_index <- k.check(gam.model)[3]
  pval_basischeck <- k.check(gam.model)[4]
  
  # compile results
  gam.smooth.results <- data.frame(tract_node = as.character(tract_node), 
                                   GAM.smooth.Fvalue = as.numeric(gam.smooth.F), 
                                   GAM.smooth.pvalue = as.numeric(gam.smooth.pvalue), 
                                   GAM.smooth.AdjRsq = as.numeric(adjRsq),
                                   GAM.smooth.partialR2 = as.numeric(partialRsq), 
                                   Anova.smooth.pvalue = as.numeric(anova.smooth.pvalue), 
                                   smooth.peak.change = as.numeric(peak.change), 
                                   smooth.decrease.offset = as.numeric(decrease.offset),
                                   smooth.increase.offset = as.numeric(increase.offset), 
                                   smooth.last.change = as.numeric(change.offset),
                                   smooth.slowing.onset = as.numeric(slowing.onset),
                                   model_AIC = as.numeric(model_AIC), 
                                   edf = as.numeric(edf), 
                                   k_index = as.numeric(k_index), 
                                   pval_basischeck = as.numeric(pval_basischeck))
  return(gam.smooth.results)
}


######################################
# FIT GAM SMOOTH (FOR T1w/T2w Cortical data)
######################################
## Function to fit a GAM (region ~ s(smooth_var, k = knots, fx = set_fx) + covariates)) 
## save out statistics and derivative-based characteristics
gam.fit.smooth.cortex <- function(gam.data, region, smooth_var, covariates, knots, set_fx = FALSE){
  # Fit the GAM
  print(region)
  modelformula <- as.formula(sprintf("%s ~ s(%s, k = %s, fx = %s) + %s", region, smooth_var, knots, set_fx, covariates))  # 3 knots, fx = T should term be unpenalized  
  gam.model <- gam(modelformula, method = "REML", data = gam.data)
  gam.results <- summary(gam.model)
  
  # GAM derivatives
  ## Get derivatives of the smooth function using the gratia derivatives function; gratia estimates derivatives from GAM smooths via finite differences
  derv <- derivatives(gam.model, term = sprintf('s(%s)',smooth_var), interval = "simultaneous", unconditional = F) #derivative at 200 indices of smooth_var with a simultaneous CI
  ## Identify derivative significance window (basically finding where there is significant change in a node)
  derv <- derv %>%
    mutate(sig = !(0 >lower & 0 < upper)) #add "sig" column (TRUE/FALSE) to derv. 
  # Derivative is sig if the lower CI is not < 0 while the upper CI is > 0 (i.e., in the CI does not include 0).
  derv$sig_deriv = derv$derivative*derv$sig #add "sig_deriv column that has all the significant derivative values, 
  #but all non-significant derivatives are set to 0. Aka, mask out non-significant deriv points by the derv$sig TRUE/FALSE column
  
  # GAM statistics
  ## Get the F value for the smooth term and the GAM-based significance of the term
  ## (F-tests on smooth terms (rather than for the full model) are 
  ## joint tests for equality to zero for all of the coefficients making up a single spline term)
  gam.smooth.F <- gam.results$s.table[3]
  gam.smooth.pvalue <- gam.results$s.table[4]
  
  # Calculate the magnitude and significance of the **smooth term** effect by comparing full and reduced models
  ## Compare a full model GAM (with the smooth term) to a nested, reduced model (with covariates only)
  nullmodel <- as.formula(sprintf("%s ~ %s", region, covariates)) #no smooth term
  gam.nullmodel <- gam(nullmodel, method = "REML", data = gam.data)
  gam.nullmodel.results <- summary(gam.nullmodel)
  
  ## Full versus reduced model anova p-value
  anova.smooth.pvalue <- anova.gam(gam.nullmodel,gam.model,test='F')$`Pr(>F)`[2] 
  
  ## Full versus reduced model: delta R.sq (adj)
  ### effect size
  adjRsq <- abs(gam.results$r.sq - gam.nullmodel.results$r.sq) 
  ### effect direction
  linearmodel <- as.formula(sprintf("%s ~ %s + %s", region, smooth_var, covariates))
  lm.model.t <- summary(lm(linearmodel, data=gam.data))$coefficients[2,3] #t-value for smooth_var
  if(lm.model.t < 0){ #if the linear model t-value for smooth_var is less than 0, make the delta adj R.sq negative
    adjRsq <- adjRsq*-1}
  
  ## Full versus reduced model: direction-dependent partial R squared
  ### effect size
  sse.model <- sum((gam.model$y - gam.model$fitted.values)^2)
  sse.nullmodel <- sum((gam.nullmodel$y - gam.nullmodel$fitted.values)^2)
  partialRsq <- (sse.nullmodel - sse.model)/sse.nullmodel
  ### effect direction
  mean.derivative <- mean(derv$derivative)
  if(mean.derivative < 0){ #if the average derivative is less than 0, make the effect size estimate negative
    partialRsq <- partialRsq*-1}
  
  # Derivative-based temporal characteristics
  ## Age of decrease offset: age of maturation if DWI measure decreases with age (e.g. MD)
  if(sum(derv$sig) > 0){ 
    decreasing.range <- derv$data[derv$sig_deriv < 0] #identify all ages with a significant negative derivative 
    # (i.e., smooth_var indices where y is decreasing)
    if(length(decreasing.range) > 0){
      last.decrease <- max(decreasing.range) #find oldest age with a significant negative derivative
      if(last.decrease == derv$data[length(derv$data)]) #if the last age of significant decrease is the oldest age in the dataset
        # decrease.offset <- last.decrease
        decrease.offset <- 0 # the node never matures if the latest significant decrease occurs at oldest age. Code as 0 for later logistic regression analysis
      if(last.decrease != derv$data[length(derv$data)]){  # if age of last significant derivative is NOT the oldest age,
        decrease.offset.row <- which(derv$data == last.decrease) + 1 #use above to find the first age when the derivative is not significant
        decrease.offset <- derv$data[decrease.offset.row]}
    }
    if(length(decreasing.range) == 0) # if there are no ages with a significant negative derivative, then set decrease.offset to NA
      decrease.offset <- NA}
  if(sum(derv$sig) == 0){ 
    decrease.offset <- NA}  
  
  ## Age of increase offset: age of maturation if DWI measure increases with age (e.g. FA)
  if(sum(derv$sig) > 0){ 
    increasing.range <- derv$data[derv$sig_deriv > 0] #identify all ages with a significant positive derivative (i.e., smooth_var indices where y is increasing)
    if(length(increasing.range) > 0){
      last.increase <- max(increasing.range) #find oldest age with a significant positive derivative
      if(last.increase == derv$data[length(derv$data)]) #if the last age of significant increase is the oldest in the dataset
        increase.offset <- last.increase
      if(last.increase != derv$data[length(derv$data)]){ 
        increase.offset.row <- which(derv$data == last.increase) + 1 #use above to find the first age when the derivative is not significant
        increase.offset <- derv$data[increase.offset.row]}
    }
    if(length(increasing.range) == 0)
      increase.offset <- NA}
  if(sum(derv$sig) == 0){ 
    increase.offset <- NA}  
  
  ## Age of last change
  if(sum(derv$sig) > 0){ 
    change.offset <- max(derv$data[derv$sig==T])} #find last age in the smooth where derivative is significant
  if(sum(derv$sig) == 0){ 
    change.offset <- NA}  

  
  # Model Fit
  model_AIC <- AIC(gam.model)
  edf <- k.check(gam.model)[2]
  k_index <- k.check(gam.model)[3]
  pval_basischeck <- k.check(gam.model)[4]
  
  # compile results
  gam.smooth.results <- data.frame(region = as.character(region), 
                                   GAM.smooth.Fvalue = as.numeric(gam.smooth.F), 
                                   GAM.smooth.pvalue = as.numeric(gam.smooth.pvalue), 
                                   GAM.smooth.AdjRsq = as.numeric(adjRsq),
                                   GAM.smooth.partialR2 = as.numeric(partialRsq), 
                                   Anova.smooth.pvalue = as.numeric(anova.smooth.pvalue), 
                                  
                                   smooth.decrease.offset = as.numeric(decrease.offset),
                                   smooth.increase.offset = as.numeric(increase.offset), 
                                   smooth.last.change = as.numeric(change.offset),
                                  
                                   model_AIC = as.numeric(model_AIC), 
                                   edf = as.numeric(edf), 
                                   k_index = as.numeric(k_index), 
                                   pval_basischeck = as.numeric(pval_basischeck))
  return(gam.smooth.results)
}



##################
# DERIVATIVES  
##################
# note: can be used for showing where significant developmental change is happening (probably everywhere), for differences in derivative magnitude, 
# and for computing annualized rate of change  
# see graham's code: https://github.com/gbaum/hcpd_myelin/blob/main/07_brms_regional_posterior_deriv_analysis.R#L237C3-L238C53
# posterior_annualized_roc <- fit_deriv_auc$param_posterior/age_range
# mean_post_roc[i] <- mean(posterior_annualized_roc)

##Function to compute smooth derivatives for a main GAM model and for individual draws from the simulated posterior distribution
gam.derivatives <- function(gam.data, tract_node, smooth_var, covariates, knots, set_fx = FALSE, draws, increments, return_posterior_derivatives = TRUE){
  
  #Set parameters
  npd <- as.numeric(draws) #number of draws from the posterior distribution; number of posterior derivative sets estimated
  np <- as.numeric(increments) #number of smooth_var increments to get derivatives at
  EPS <- 1e-07 #finite differences
  UNCONDITIONAL <- FALSE #should we account for uncertainty when estimating smoothness parameters?
  
  # Fit the GAM
  gam.data$sex <- as.factor(gam.data$sex)
  modelformula <- as.formula(sprintf("%s ~ s(%s, k = %s, fx = %s) + %s", tract_node, smooth_var, knots, set_fx, covariates))  # 3 knots, fx = T should term be unpenalized  
  gam.model <- gam(modelformula, method = "REML", data = gam.data)
  gam.results <- summary(gam.model)
  
  #Extract gam input data
  df <- gam.model$model #extract the data used to build the gam, i.e., a df of y + predictor values 
  
  #Create a prediction data frame, used to estimate (posterior) model coefficients
  thisPred <- data.frame(init = rep(0,np)) 
  
  theseVars <- attr(gam.model$terms,"term.labels") #gam model predictors (smooth_var + covariates)
  varClasses <- attr(gam.model$terms,"dataClasses") #classes of the model predictors and y measure
  thisResp <- as.character(gam.model$terms[[2]]) #the measure to fit for each posterior draw
  for (v in c(1:length(theseVars))) { #fill the prediction df with data 
    thisVar <- theseVars[[v]]
    thisClass <- varClasses[thisVar]
    if (thisVar == smooth_var) { 
      thisPred[,smooth_var] = seq(8, max(df[,smooth_var],na.rm = T), length.out = np) #generate a range of np data points, from minimum of smooth term to maximum of smooth term
    } else {
      switch (thisClass,
              "numeric" = {thisPred[,thisVar] = median(df[,thisVar])}, #make predictions based on median value
              "factor" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]}, #make predictions based on first level of factor 
              "ordered" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]} #make predictions based on first level of ordinal variable
      )
    }
  }
  pred <- thisPred %>% select(-init) #prediction df
  pred2 <- pred #second prediction df
  pred2[,smooth_var] <- pred[,smooth_var] + EPS #finite differences
  
  #Estimate smooth derivatives
  derivs <- derivatives(gam.model, term = sprintf('s(%s)',smooth_var), interval = "simultaneous", unconditional = UNCONDITIONAL, data = pred) #derivative at 200 indices of smooth_var with a simultaneous CI
  derivs.fulldf <- derivs %>% select(data, derivative, se, lower, upper)
  derivs.fulldf <- derivs.fulldf %>% mutate(significant = !(0 > lower & 0 < upper))
  derivs.fulldf$significant.derivative = derivs.fulldf$derivative*derivs.fulldf$significant
  colnames(derivs.fulldf) <- c(sprintf("%s", smooth_var), "derivative", "se", "lower", "upper", "significant", "significant.derivative")
  
  #Estimate posterior smooth derivatives from simulated GAM posterior distribution
  if(return_posterior_derivatives == TRUE){
    Vb <- vcov(gam.model, unconditional = UNCONDITIONAL) #variance-covariance matrix for all the fitted model parameters (intercept, covariates, and splines)
    sims <- MASS::mvrnorm(npd, mu = coef(gam.model), Sigma = Vb) #simulate model parameters (coefficents) from the posterior distribution of the smooth based on actual model coefficients and covariance
    X0 <- predict(gam.model, newdata = pred, type = "lpmatrix") #get matrix of linear predictors for pred
    X1 <- predict(gam.model, newdata = pred2, type = "lpmatrix") #get matrix of linear predictors for pred2
    Xp <- (X1 - X0) / EPS 
    posterior.derivs <- Xp %*% t(sims) #Xp * simulated model coefficients = simulated derivatives. Each column of posterior.derivs contains derivatives for a different draw from the simulated posterior distribution
    posterior.derivs <- as.data.frame(posterior.derivs)
    colnames(posterior.derivs) <- sprintf("draw%s",seq(from = 1, to = npd)) #label the draws
    posterior.derivs <- cbind(as.numeric(pred[,smooth_var]), posterior.derivs) #add smooth_var increments from pred df to first column
    colnames(posterior.derivs)[1] <- sprintf("%s", smooth_var) #label the smooth_var column
    posterior.derivs <- cbind(as.character(tract_node), posterior.derivs) #add tract_node label to first column
    colnames(posterior.derivs)[1] <- "label" #label the column
    posterior.derivs.long <- posterior.derivs %>% pivot_longer(contains("draw"), names_to = "draw",values_to = "posterior.derivative")
  } #np*npd rows, 3 columns (smooth_var, draw, posterior.derivative)
  
  if(return_posterior_derivatives == FALSE)
    return(derivs.fulldf)
  if(return_posterior_derivatives == TRUE)
    return(posterior.derivs.long)
}


########################################### 
# CALCULATE ZERO-CENTERED SMOOTH ESTIMATES 
########################################### 
## Function to estimate the zero-averaged gam smooth function 
gam.estimate.smooth <- function(gam.data, tract_node, smooth_var, covariates, knots, set_fx = FALSE, increments, age1, age2){
  # Fit the GAM
  gam.data$sex <- as.factor(gam.data$sex)
  modelformula <- as.formula(sprintf("%s ~ s(%s, k = %s, fx = %s) + %s", tract_node, smooth_var, knots, set_fx, covariates))  # 3 knots, fx = T should term be unpenalized  
  gam.model <- gam(modelformula, method = "REML", data = gam.data)
  gam.results <- summary(gam.model)
  
  # Extract gam input data
  df <- gam.model$model #extract the data used to build the gam, i.e., a df of y + predictor values 
  
  # Create a prediction data frame
  np <- increments #number of predictions to make; predict at np increments of smooth_var
  thisPred <- data.frame(init = rep(0,np)) #initiate a prediction df 
  
  theseVars <- attr(gam.model$terms,"term.labels") #gam model predictors (smooth_var + covariates)
  varClasses <- attr(gam.model$terms,"dataClasses") #classes of the model predictors and y measure
  thisResp <- as.character(gam.model$terms[[2]]) #the measure to predict
  for (v in c(1:length(theseVars))) { #fill the prediction df with data for predictions. These data will be used to predict the output measure (y) at np increments of the smooth_var, holding other model terms constant
    thisVar <- theseVars[[v]]
    thisClass <- varClasses[thisVar]
    if (thisVar == smooth_var) { 
      thisPred[,smooth_var] = seq(age1, age2, length.out = np) #generate a range of np data points, from minimum of smooth term to maximum of smooth term
    } else {
      switch (thisClass,
              "numeric" = {thisPred[,thisVar] = median(df[,thisVar])}, #make predictions based on median value
              "factor" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]}, #make predictions based on first level of factor 
              "ordered" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]} #make predictions based on first level of ordinal variable
      )
    }
  }
  pred <- thisPred %>% select(-init)
  
  # Estimate the smooth trajectory 
  estimated.smooth <- smooth_estimates(object = gam.model, data = pred)
  estimated.smooth <- estimated.smooth %>% select(age, est)
  
  return(estimated.smooth)
}




###################################### 
# PREDICT GAM SMOOTH FITTED VALUES
###################################### 
##Function to predict fitted values of a measure based on a fitted GAM smooth (measure ~ s(smooth_var, k = knots, fx = set_fx) + covariates)) and a prediction df
gam.smooth.predict <- function(gam.data, tract_node, smooth_var, covariates, knots, set_fx = FALSE, increments, age1, age2){
  
  # Fit the GAM
  gam.data$sex <- as.factor(gam.data$sex)
  modelformula <- as.formula(sprintf("%s ~ s(%s, k = %s, fx = %s) + %s", tract_node, smooth_var, knots, set_fx, covariates))  # 3 knots, fx = T should term be unpenalized  
  gam.model <- gam(modelformula, method = "REML", data = gam.data)
  gam.results <- summary(gam.model)

  #Extract gam input data
  df <- gam.model$model #extract the data used to build the gam, i.e., a df of y + predictor values 
  
  #Create a prediction data frame
  np <- increments #number of predictions to make; predict at np increments of smooth_var
  thisPred <- data.frame(init = rep(0,np)) #initiate a prediction df 
  
  theseVars <- attr(gam.model$terms,"term.labels") #gam model predictors (smooth_var + covariates)
  varClasses <- attr(gam.model$terms,"dataClasses") #classes of the model predictors and y measure
  thisResp <- as.character(gam.model$terms[[2]]) #the measure to predict
  for (v in c(1:length(theseVars))) { #fill the prediction df with data for predictions. These data will be used to predict the output measure (y) at np increments of the smooth_var, holding other model terms constant
    thisVar <- theseVars[[v]]
    thisClass <- varClasses[thisVar]
    if (thisVar == smooth_var) { 
      thisPred[,smooth_var] = seq(age1, age2, length.out = np) #generate a range of np data points, from minimum of smooth term to maximum of smooth term
    } else {
      switch (thisClass,
              "numeric" = {thisPred[,thisVar] = median(df[,thisVar])}, #make predictions based on median value
              "factor" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]}, #make predictions based on first level of factor 
              "ordered" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]} #make predictions based on first level of ordinal variable
      )
    }
  }
  pred <- thisPred %>% select(-init)
  
  #Generate predictions based on the gam model and predication data frame
  predicted.smooth <- fitted_values(object = gam.model, data = pred)
  predicted.smooth <- predicted.smooth %>% select(all_of(smooth_var), fitted, se, lower, upper)
  
  return(predicted.smooth)
}

######################################################################################
# PREDICT GAM SMOOTH FITTED VALUES FOR A SPECIFIED VALUE OF AN INTERACTING COVARIATE 
######################################################################################
# note: for environment or cognition, potentially
## Function to predict fitted values of a measure for a given value of a covariate, using a varying coefficients smooth-by-linear covariate interaction
gam.smooth.predict.covariateinteraction <- function(gam.data, tract_node, smooth_var, int_var, int_var.predict, covariates, knots, set_fx = FALSE, increments, age1, age2){
  # Fit the gam
  gam.data$sex <- as.factor(gam.data$sex)
  modelformula <- as.formula(sprintf("%1$s ~ s(%2$s, k=%3$s, fx=%4$s) + s(%2$s, by=%5$s, k=%3$s, fx=%4$s) + %6$s", tract_node, smooth_var, knots, set_fx, int_var, covariates))
  gam.model <- gam(modelformula, method = "REML", data=gam.data)
  gam.results <- summary(gam.model) 
  
  # Extract gam input data
  df <- gam.model$model #extract the data used to build the gam, i.e., a df of y + predictor values 
  
  # Create a prediction data frame
  np <- increments #number of predictions to make; predict at np increments of smooth_var
  thisPred <- data.frame(init = rep(0,np)) #initiate a prediction df 
  
  theseVars <- attr(gam.model$terms,"term.labels") #gam model predictors (smooth_var + covariates)
  varClasses <- attr(gam.model$terms,"dataClasses") #classes of the model predictors and y measure
  thisResp <- as.character(gam.model$terms[[2]]) #the measure to predict
  for (v in c(1:length(theseVars))) { #fill the prediction df with data for predictions. These data will be used to predict the output measure (y) at np increments of the smooth_var, holding other model terms constant
    thisVar <- theseVars[[v]]
    thisClass <- varClasses[thisVar]
    if (thisVar == smooth_var) { 
      thisPred[,smooth_var] = seq(age1, age2, length.out = np) #generate a range of np data points, from minimum of smooth term to maximum of smooth term
    } else {
      switch (thisClass,
              "numeric" = {thisPred[,thisVar] = median(df[,thisVar])}, #make predictions based on median value
              "factor" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]}, #make predictions based on first level of factor 
              "ordered" = {thisPred[,thisVar] = levels(df[,thisVar])[[1]]} #make predictions based on first level of ordinal variable
      )
    }
  }
  pred <- thisPred %>% select(-init)
  pred[,int_var] <- as.numeric(int_var.predict)
  
  # Generate fitted (predicted) values based on the gam model and prediction data frame
  predicted.smooth <- fitted_values(object = gam.model, data = pred)
  predicted.smooth$fitted.centered <- (predicted.smooth$fitted-gam.results$p.table[1,1]) #subtract the intercept from fitted values
  predicted.smooth <- predicted.smooth %>% select(all_of(smooth_var), fitted, se, lower, upper, fitted.centered)
  
  return(predicted.smooth)
}

# for sex - start here
## Function to predict fitted values of a measure for a given value of a covariate, using a varying coefficients smooth-by-factor covariate interaction
gam.smooth.predict.covariateinteraction.factor <- function(gam.data, tract_node, smooth_var, int_var, int_var.predict, covariates, knots, set_fx = FALSE, increments, age1, age2){
  # Ensure int_var is a factor if not already
  gam.data[[int_var]] <- as.factor(gam.data[[int_var]])
  
  # Fit the gam
  modelformula <- as.formula(sprintf("%1$s ~ s(%2$s, k=%3$s, fx=%4$s) + s(%2$s, by=%5$s, k=%3$s, fx=%4$s) + %6$s", 
                                     tract_node, smooth_var, knots, set_fx, int_var, covariates))
  gam.model <- gam(modelformula, method = "REML", data=gam.data)
  gam.results <- summary(gam.model)
  
  # Extract gam input data
  df <- gam.model$model
  
  # Create a prediction data frame
  np <- increments
  thisPred <- data.frame(init = rep(0, np))
  
  theseVars <- attr(gam.model$terms, "term.labels")
  varClasses <- attr(gam.model$terms, "dataClasses")
  
  for (v in c(1:length(theseVars))) {
    thisVar <- theseVars[v]
    thisClass <- varClasses[thisVar]
    if (thisVar == smooth_var) {
      thisPred[, smooth_var] = seq(age1, age2, length.out = np)
    } else {
      switch(thisClass,
             "numeric" = {thisPred[, thisVar] = median(df[, thisVar], na.rm = TRUE)},
             "factor" = {thisPred[, thisVar] = ifelse(thisVar == int_var, int_var.predict, levels(df[, thisVar])[[1]])},
             "ordered" = {thisPred[, thisVar] = levels(df[, thisVar])[[1]]}
      )
    }
  }
  
  pred <- thisPred %>% select(-init)
  pred[,int_var] <- int_var.predict
  
  # Generate fitted (predicted) values based on the gam model and prediction data frame
  predicted.smooth <- predict(gam.model, newdata = pred, type = "response")
  predicted.smooth <- data.frame(smooth_var = pred[[smooth_var]], fitted = predicted.smooth)
  return(predicted.smooth)
}

##################################### 
# FIT GAM FACTOR-SMOOTH INTERACTION
##################################### 
# note: for age by sex interaction
##Function to fit a GAM with a factor-smooth interaction and obtain statistics for the interaction term 
gam.factorsmooth.interaction <- function(gam.data, tract_node, smooth_var, int_var, covariates, knots, set_fx = FALSE){
  # Fit the gam
  modelformula <- as.formula(sprintf("%1$s ~ s(%2$s, k=%3$s, fx=%4$s) + s(%2$s, by=%5$s, k=%3$s, fx=%4$s) + %6$s", tract_node, smooth_var, knots, set_fx, int_var, covariates))
  gam.model <- gam(modelformula, method = "REML", data=gam.data)
  gam.results <- summary(gam.model)
  
  #GAM statistics
  #F value for the smooth term and GAM-based significance of the smooth term
  gam.int.F <- gam.results$s.table[2,3]
  gam.int.pvalue <- gam.results$s.table[2,4]
  
  interaction.stats <- data.frame(as.character(tract_node), as.numeric(gam.int.F), as.numeric(gam.int.pvalue))
  colnames(interaction.stats) <- c("tract_node", "GAM.int.Fvalue", "GAM.int.pvalue")
  return(interaction.stats)
}

############################################### 
# FIT GAM SMOOTH WITH A COVARIATE OF INTEREST
############################################### 
# note: for environment or cognition, potentially
##Function to fit a GAM (nodewise_measure ~ s(smooth_var, k = knots, fx = set_fx) + covariate of interest + control covariates)) and save out statistics for the first covariate
gam.fit.covariate <- function(gam.data, tract_node, smooth_var, covariate.interest, covariates.noninterest, knots, set_fx = FALSE){
  # Fit the gam
  gam.data$sex <- as.factor(gam.data$sex)
  modelformula <- as.formula(sprintf("%s ~ s(%s, k = %s, fx = %s) + %s + %s", tract_node, smooth_var, knots, set_fx, covariate.interest, covariates.noninterest))
  gam.model <- gam(modelformula, method = "REML", data=gam.data)
  gam.results <- summary(gam.model)
  
  #GAM statistics
  #t-value for the covariate of interest term and GAM-based significance of this term
  gam.cov.tvalue <- gam.results$p.table[2,3]
  #GAM based significance of the term
  gam.cov.pvalue <- gam.results$p.table[2,4]
  
  #Calculate the magnitude and significance of the covariate of interest effect by comparing full and reduced models
  ##Compare a full model GAM (with the covariate of interest) to a nested, reduced model (without covariate of interest)
  nullmodel <- as.formula(sprintf("%s ~ s(%s, k = %s, fx = %s) + %s", tract_node, smooth_var, knots, set_fx, covariates.noninterest))
  gam.nullmodel <- gam(nullmodel, method = "REML", data=gam.data)
  gam.nullmodel.results <- summary(gam.nullmodel)
  
  ##Full versus reduced model anova p-value
  anova.cov.pvalue <- anova.gam(gam.nullmodel,gam.model,test='F')$`Pr(>F)`[2]
  if(is.na(anova.cov.pvalue)){ #if residual deviance is exactly equal between full and reduced models and p=value = NA, set p = 1
    anova.cov.pvalue <- 1}
  
  ## Full versus reduced model: delta R.sq (adj)
  ### effect size
  adjRsq <- abs(gam.results$r.sq - gam.nullmodel.results$r.sq) 
  ### effect direction
  linearmodel <- as.formula(sprintf("%s ~ %s + %s + %s", tract_node, smooth_var, covariate.interest, covariates.noninterest))
  lm.model.t <- summary(lm(linearmodel, data=gam.data))$coefficients[2,3] #t-value for smooth_var
  if(lm.model.t < 0){ #if the linear model t-value for smooth_var is less than 0, make the delta adj R.sq negative
    adjRsq <- adjRsq*-1}
  
  ##Full versus reduced model direction-dependent partial R squared
  ### effect size
  sse.model <- sum((gam.model$y - gam.model$fitted.values)^2)
  sse.nullmodel <- sum((gam.nullmodel$y - gam.nullmodel$fitted.values)^2)
  partialRsq <- (sse.nullmodel - sse.model)/sse.nullmodel
  ### effect direction
  if(gam.cov.tvalue < 0){ #if the gam t-value for covariate of interest is less than 0, make the partialRsq negative
    partialRsq <- partialRsq*-1}
  
  results <- data.frame(tract_node = as.character(tract_node), 
                        GAM.cov.tvalue = as.numeric(gam.cov.tvalue), 
                        GAM.cov.pvalue = as.numeric(gam.cov.pvalue), 
                        ANOVA.cov.pvalue = as.numeric(anova.cov.pvalue), 
                        Effectsize.cov.AdjRsq = as.numeric(adjRsq), 
                        Effectsize.cov.partialRsq = as.numeric(partialRsq))
  return(results)
}