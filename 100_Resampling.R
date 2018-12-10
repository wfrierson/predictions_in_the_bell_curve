###
# The following custom functions perform resampling techniques via bootstrapping and the jackknife method
###

# Perform 1 bootstrap iteration of logistic regression
logit.bootstrap.oneModel <- function(DT, formula, set.seed.indicator = TRUE, set.seed.value) {
  if (set.seed.indicator) {
    set.seed(set.seed.value)
  }
  sampleIndices <- sample(1:nrow(DT), nrow(DT), replace = TRUE)
  DT.boot <- DT[sampleIndices]
  fit <- glm(formula = formula, data = DT.boot, family = binomial(link = "logit"))
  
  # Format output
  fit <- coef(summary(fit))
  results.boot <- as.data.table(fit)
  results.boot[, term := rownames(fit)]
  setnames(results.boot, old = c('Estimate', 'Std. Error', 'z value', 'Pr(>|z|)'), new = c('estimate', 'stdError', 'zValue', 'pValue'))
  
  # To view all glm diagnostics across all boot iterations, reshape them so each value is a new column (to make stacking results easier)
  output <- dcast(results.boot, ... ~ term, value.var = colnames(results.boot)[1:4])
  return(output)
}

# Score logit model in large batch
# Assumes:
# 1) coef.toScore is output of logit.bootstrap.oneModel()
# 2) formula.toScore is same formula used to produce coef.toScore
# 3) Assumes DT.toScore has fields needed to create a design matrix from formula.toScore
logit.score <- function(DT.toScore, formula.toScore = formula, coef.toScore) {
  # data.table::dcast uses alphabetical order on long-to-wide columns. 
  # The following ensures that the column order from model.matrix is consistent with bootstrapped/jackknifed glm coefficients
  
  # Build design matrix
  m <- model.matrix(formula.toScore, DT.toScore)
  
  # Handle case where a model variable is a categorical factor
  names.m <- colnames(m)
  names.coef <- colnames(coef.toScore)
  names.estimates <- gsub('estimate_','',grep('estimate_', names.coef, value = TRUE))
  
  # Variables in names.diff should represent the base level of categorical factors in the formula
  names.diff <- setdiff(names.m, names.estimates)
  
  if(length(names.diff) > 0) {
    # Drop base class of factor since model.matrix treats it as a constant 0 for logit models
    # This is needed because coef.toScore excludes the base level
    # There's probably a better way to do this...
    names.drop.ind <- which(names.m == names.diff)
    names.m.drop <- names.m[-names.drop.ind]
    featuresString <- paste0('estimate_', names.m.drop)
    
    DT.scores <- plogis(m[, -names.drop.ind] %*% t(coef.toScore[, mget(featuresString)]))
  } else {
    featuresString <- paste0('estimate_', names.m)
    
    DT.scores <- plogis(m %*% t(coef.toScore[, mget(featuresString)]))
  }
  
  numModels <- nrow(coef.toScore)
  responseString <- gsub('()','',formula.toScore[2])
  DT.scores <- as.data.table(cbind(DT.toScore[, .(actual = get(responseString))], DT.scores))
  setnames(DT.scores, paste0('V',1:numModels), paste0('iter',1:numModels))
  
  return(DT.scores)
}

## Bootstraped logistic regression, a wrapper for logit.bootstrap.oneModel
logit.bootstrap <- function(DT, iterations, formula, DT.holdout, set.seed.indicator = TRUE, scoreType = 'training', parallel = TRUE, cl, keepCluster = FALSE) {
  coef.boot <- if(parallel) {
    clusterCall(cl, function(x) library(data.table))
    clusterExport(cl, list('DT', 'formula', 'set.seed.indicator', 'logit.bootstrap.oneModel'), environment())
    
    rbindlist(parLapply(cl, 1:iterations, function(i) logit.bootstrap.oneModel(DT = DT, formula = formula, set.seed.indicator = set.seed.indicator, set.seed.value = i)))
  } else {
    rbindlist(lapply(1:iterations, function(i) logit.bootstrap.oneModel(DT = DT, formula = formula, set.seed.indicator = set.seed.indicator, set.seed.value = i)))
  }
  
  if(!keepCluster) {
    on.exit(stopCluster(cl))
  }
  
  output <- list(
    coef.boot
  )
  
  if(scoreType == 'both') {
    output[[2]] <- logit.score(DT.toScore = DT, formula.toScore = formula, coef.toScore = coef.boot)
    output[[3]] <- logit.score(DT.toScore = DT.holdout, formula.toScore = formula, coef.toScore = coef.boot)
    names(output) <- c('coefficients', 'modeled training', 'modeled holdout')
  } else if(scoreType == 'training') {
    output[[2]] <- logit.score(DT.toScore = DT, formula.toScore = formula, coef.toScore = coef.boot)
    names(output) <- c('coefficients', 'modeled training')
  } else if(scoreType == 'holdout') {
    output[[2]] <- logit.score(DT.toScore = DT.holdout, formula.toScore = formula, coef.toScore = coef.boot)
    names(output) <- c('coefficients', 'modeled holdout')
  }
  
  return(output)
}

# Perform 1 jackknife logistic regression estimation
logit.jackknife.oneModel <- function(DT, formula, omittedRecord) {
  DT.jack <- DT[!omittedRecord]
  fit <- glm(formula = formula, data = DT.jack, family = binomial(link = "logit"))
  
  # Format output
  fit <- coef(summary(fit))
  results.jack <- as.data.table(fit)
  results.jack[, term := rownames(fit)]
  setnames(results.jack, old = c('Estimate', 'Std. Error', 'z value', 'Pr(>|z|)'), new = c('estimate', 'stdError', 'zValue', 'pValue'))
  
  # To view all glm diagnostics across all boot iterations, reshape them so each value is a new column (to make stacking results easier)
  output <- dcast(results.jack, ... ~ term, value.var = colnames(results.jack)[1:4])
  return(output)
}

# Jackknifed logistic regression, a wrapper for logit.jackknife.oneModel
logit.jackknife <- function(DT, formula, DT.holdout, scoreType = 'training', parallel = TRUE, cl, keepCluster = FALSE) {
  n <- nrow(DT)
  
  coef.jack <- if(parallel) {
    clusterCall(cl, function(x) library(data.table))
    clusterExport(cl, list('DT', 'formula', 'logit.jackknife.oneModel'), environment())
    
    rbindlist(parLapply(cl, 1:n, function(i) logit.jackknife.oneModel(DT = DT, formula = formula, omittedRecord = i)))
  } else {
    rbindlist(lapply(1:n, function(i) logit.jackknife.oneModel(DT = DT, formula = formula, omittedRecord = i)))
  }
  
  if(!keepCluster) {
    on.exit(stopCluster(cl))
  }
  
  output <- list(
    coef.jack
  )
  
  if(scoreType == 'both') {
    output[[2]] <- logit.score(DT.toScore = DT, formula.toScore = formula, coef.toScore = coef.jack)
    output[[3]] <- logit.score(DT.toScore = DT.holdout, formula.toScore = formula, coef.toScore = coef.jack)
    names(output) <- c('coefficients', 'modeled training', 'modeled holdout')
  } else if(scoreType == 'training') {
    output[[2]] <- logit.score(DT.toScore = DT, formula.toScore = formula, coef.toScore = coef.jack)
    names(output) <- c('coefficients', 'modeled training')
  } else if(scoreType == 'holdout') {
    output[[2]] <- logit.score(DT.toScore = DT.holdout, formula.toScore = formula, coef.toScore = coef.jack)
    names(output) <- c('coefficients', 'modeled holdout')
  }
  
  return(output)
}