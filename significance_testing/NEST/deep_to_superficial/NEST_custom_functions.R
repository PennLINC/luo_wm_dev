


library(dplyr)
library(gratia) 
library(mgcv)
library(parallel)
library(rjson)
library(stringr)
library(tidyr)
library(NEST)

# this is a custom statFun NEST function for computing delta adjusted rsq
#' statFun.gam.deltaRsq() will be called by NEST_audrey() if NEST argument statFun=="gam.deltaRsq"
#'@param X n x p matrix of image measurements (n = number of subjects, p = number of image locations)
#'@param dat matrix including phenotype and any covariates. make sure all columns in dat are names
#'@param gam.formula user-specified formula for gam fit at each vertex (in each gam, outcome will be a different column of X and the right side of the formula should include variables included in dat matrix)
#'@param lm.formula user-specified formula for lm fit at each vertex (purpose is to get the sign of the delta adjusted R-squared)
#'@param y.in.gam character specifying how the phenotype variable can be identified in gam. if phenotype is included as a linear term in the gam formula, this could just be the variable name. if there's a smooth term for the phenotype, it may be something like s(y)
#'@param y.in.lm character specifying how the phenotype variable can be identified in lm. again, might just be the name of the variable, but could be different (e.g., if testing an interaction term)
#'@param y.permute specify names of columns in dat matrix that should be permuted when getNull = TRUE
#'@param n.cores for parallelization, number of cores to use (default is 1 / no parallelization)
#'@param seed optional to set seed
#'@param n.perm number of permutations to conduct for inference. default is 999 (i.e. minimum p-value 1/1000)
#'@param getNull whether to obtain null distribution vs. just get observed map of statistics. default will be TRUE inside NEST function, but the statFun function will then be recursively called to get null distribution and getNull will then switch to FALSE
#'@export
statFun.gam.deltaRsq = function(X, dat, gam.formula, lm.formula, y.in.gam, y.in.lm, y.permute = NULL, n.cores = 1, seed = NULL, n.perm = 999, getNull = TRUE){
  
  xyz = cbind(X.v = X[,1], # just use the first image location for building model.matrix (will end up refitting at every location though)
              dat)
  
  # initial gam (to get model matrix)
  gam.init = gam(formula = gam.formula, data = data.frame(xyz))
  gam.model.matrix = model.matrix(gam.init)
  
  # weird bug: model matrix is not full rank; remove duplicated column
  if (sum(duplicated(t(gam.model.matrix))) > 0){
    gam.model.matrix = gam.model.matrix[,-which(duplicated(t(gam.model.matrix)))]
  }
  
  columns.to.test = colnames(gam.model.matrix)[grepl(y.in.gam, colnames(gam.model.matrix), fixed = TRUE)]
  
  # pre-specify the permutations (i.e., do the same permutations of people across vertices)
  if (getNull == TRUE){
    if (is.null(y.permute)){
      message("need to specify which columns of `dat` should be permuted")
      return(NULL)
    }
    if (!is.null(seed)){set.seed(seed)}
    perm.ind = lapply(1:n.perm, FUN = function(k){
      sample(1:nrow(X), replace = FALSE)
    })
  }
  
  # Calculate delta adjusted R-squared (ΔR^2adj) for observed data:
  stat.obs = unlist(mclapply(1:ncol(X), FUN = function(v){
    xyz[,"X.v"] = X[,v]
    
    # Full model (with smooth term)
    gam.fullmodel = gam(formula = gam.formula, data = data.frame(xyz))
    gam.fullmodel.results = summary(gam.fullmodel)
    
    # Null model (without smooth term)
    nullmodel.formula = update(gam.formula, . ~ . - s(age, k = 3, fx = TRUE))  # Remove the smooth term
    gam.nullmodel = gam(formula = nullmodel.formula, data = data.frame(xyz))
    gam.nullmodel.results = summary(gam.nullmodel)
    
    # Calculate delta adjusted R-squared (ΔR^2adj)
    adjRsq = abs(gam.fullmodel.results$r.sq - gam.nullmodel.results$r.sq)
    
    # Determine the sign using the linear model
    lm.model = lm(lm.formula, data = data.frame(xyz))
    lm.model.t = summary(lm.model)$coefficients[2, 3]  # t-value for the first covariate (assumed to be the smooth term)
    
    if(lm.model.t < 0){
      adjRsq = adjRsq * -1
    }
    
    return(adjRsq)
  }, mc.cores = n.cores))
  
  # If getNull is TRUE, calculate the null distribution of ΔR^2adj
  if (getNull == TRUE){
    stat.null = lapply(1:n.perm, FUN = function(k){
      
      dat.k = dat
      dat.k[,y.permute] = dat.k[perm.ind[[k]], y.permute]
      
      statFun.gam.deltaRsq(X = X,
                           dat = dat.k,
                           gam.formula = gam.formula,
                           lm.formula = lm.formula,
                           y.in.gam = y.in.gam,
                           y.in.lm = y.in.lm, n.cores = n.cores, seed = seed,
                           n.perm = NULL,  # no permutations to do with getNull = FALSE
                           getNull = FALSE)
      
    })
    return(list(T.obs = stat.obs,
                T.null = stat.null))
  } else{
    return(stat.obs)
  }
}


#' Updated NEST function that accepts statFun.gam.deltaRsq
#'@param statFun Function name for the statistical function to be used (e.g., "lm", "gam.mvwald", "gam.deltaRsq").
#'@param args List of arguments required by the selected statFun function.
#'@param net.maps List of vectors (one for each network), where each vector contains 0s and 1s indicating network membership.
#'@param one.sided Logical. If TRUE, performs a one-sided test.
#'@param n.cores Number of cores to use for parallel processing.
#'@param seed Optional seed for reproducibility.
#'@param what.to.return Character vector indicating what outputs to return (e.g., "pval", "ES", "T.obs", "T.null").
#'@export
NEST_audrey = function(statFun, args, net.maps, one.sided = TRUE, n.cores = 1, seed = NULL, what.to.return = c("pval")){
  
  # Check input requirements
  if (!is.list(net.maps)){
    message("net.maps argument should be a list, of vectors (one for each network)")
    return(NULL)
  }
  
  if (ncol(args$X) != length(net.maps[[1]])){
    message("each network should be specified as a vector with length equal to number of columns of X.")
    return(NULL)
  }
  
  if (!identical(as.numeric(sort(unique(unlist(net.maps)))), as.numeric(c(0,1)))){ 
    message("net.maps should include only 0's and 1's (1 = in network/ROI; 0 = outside network/ROI)")
    return(NULL)
  }
  
  # Map brain-phenotype associations
  if (statFun == "lm"){
    required.args = c("X", "y")
    optional.args = c("Z", "type", "FL", "getNull","n.perm")
    
    args = checkArgs(args = args, required.args = required.args, optional.args = optional.args)
    if (!isFALSE(args)){
      statFun.out = statFun.lm(X = args$X,
                               y = args$y,
                               Z = args$Z,
                               type = args$type,
                               n.cores = n.cores,
                               seed = seed,
                               FL = args$FL,
                               n.perm = args$n.perm,
                               getNull = args$getNull)
    } else{
      message("fix args!")
      return(NULL)
    }
  }
  
  if (statFun == "gam.mvwald" || statFun == "gam.deltaRsq"){
    
    # Check args:
    required.args = c("X","dat","gam.formula","lm.formula","y.in.gam","y.in.lm")
    optional.args = c("n.perm")
    
    args = checkArgs(args = args, required.args = required.args, optional.args = optional.args)
    
    if (!isFALSE(args)){
      if (statFun == "gam.mvwald") {
        statFun.out = statFun.gam.mvwald(X = args$X,
                                         dat = args$dat,
                                         gam.formula = args$gam.formula,
                                         lm.formula = args$lm.formula,
                                         y.in.gam = args$y.in.gam,
                                         y.in.lm = args$y.in.lm,
                                         y.permute = args$y.permute,
                                         n.cores = n.cores, seed = seed,
                                         n.perm = args$n.perm,
                                         getNull = TRUE)
      } else if (statFun == "gam.deltaRsq") {
        statFun.out = statFun.gam.deltaRsq(X = args$X,
                                           dat = args$dat,
                                           gam.formula = args$gam.formula,
                                           lm.formula = args$lm.formula,
                                           y.in.gam = args$y.in.gam,
                                           y.in.lm = args$y.in.lm,
                                           y.permute = args$y.permute,
                                           n.cores = n.cores, seed = seed,
                                           n.perm = args$n.perm,
                                           getNull = TRUE)
      }
    } else{
      message("fix args!")
      return(NULL)
    }
  }
  
  ES.list = mclapply(net.maps, FUN = function(net.map){
    ES.obs = enrichScore(stat.map = statFun.out$T.obs,
                         net.map = net.map,
                         one.sided = one.sided,
                         save.detail = F)
    
    ES.null = lapply(1:args$n.perm, FUN = function(k){
      enrichScore(stat.map = statFun.out$T.null[[k]],
                  net.map = net.map,
                  one.sided = one.sided,
                  save.detail = F)
    })
    
    return(list(ES.obs = ES.obs,
                ES.null.dist = ES.null))
  }, mc.cores = n.cores)
  
  pval.list = mclapply(1:length(net.maps), FUN = function(net){
    pvalFun(obs = ES.list[[net]]$ES.obs, null.dist = ES.list[[net]]$ES.null.dist)
  })
  
  out = list()
  
  if ("everything" %in% what.to.return){
    out$pval = pval.list
    out$ES = ES.list
    out$T.obs = statFun.out$T.obs
    out$T.null = statFun.out$T.null
    return(out)
  }else{
    if ("pval" %in% what.to.return){ 
      out$pval = pval.list
    }
    
    if ("ES" %in% what.to.return){
      out$ES = ES.list
    }
    
    if ("ES.obs" %in% what.to.return){
      out$ES.obs = sapply(ES.list, FUN = function(x){x$ES.obs})
    }
    
    if ("T.obs" %in% what.to.return){
      out$T.obs = statFun.out$T.obs
    }
    
    if ("T.null" %in% what.to.return){
      out$T.null = statFun.out$T.null
    }
    return(out)
  }
}
