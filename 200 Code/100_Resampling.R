###
# The following custom functions perform resampling techniques via bootstrapping and the jackknife method
###

# Perform 1 bootstrap iteration of logistic regression
logit.bootstrap.oneModel <- function(
  DT.training
  , DT.holdout
  , formula
  , label
  , key = 'ID'
) {
  sampleIndices.training <- sample(1:nrow(DT.training), nrow(DT.training), replace = TRUE)
  DT.boot.training <- copy(DT.training[sampleIndices.training])
  fit <- glm(
    formula = formula
    , data = DT.boot.training
    , family = binomial(link = "logit")
  )
  
  # Store bootstrapped fitted values from training data
  fitted.boot.training <- data.table(
    bootIteration = label
    , DT.boot.training[, .(key = get(key), actual = get(as.character(formula)[2]))]
    , modeled_probability = fit[['fitted.values']]
  )
  
  setnames(
    fitted.boot.training
    , 'key'
    , key
  )
  
  # Store bootstrapped fitted values from holdout data
  sampleIndices.holdout <- sample(1:nrow(DT.holdout), nrow(DT.holdout), replace = TRUE)
  DT.boot.holdout <- copy(DT.holdout[sampleIndices.holdout])
  fitted.boot.holdout <- data.table(
    bootIteration = label
    , DT.boot.holdout[, .(key = get(key), actual = get(as.character(formula)[2]))]
    , modeled_probability = predict(fit, newdata = DT.boot.holdout, type = 'response')
  )
  setnames(
    fitted.boot.holdout
    , 'key'
    , key
  )
    
  # Format output
  fit <- coef(summary(fit))
  coef.boot <- as.data.table(fit)
  coef.boot[, term := rownames(fit)]
  setnames(
    coef.boot
    , old = c('Estimate', 'Std. Error', 'z value', 'Pr(>|z|)')
    , new = c('estimate', 'stdError', 'zValue', 'pValue')
  )
  
  # To view all glm diagnostics across all boot iterations, reshape them so each value is a new column (to make stacking results easier)
  coef.boot <- dcast(coef.boot, ... ~ term, value.var = colnames(coef.boot)[1:4])
  
  output <- list(
    'coef' = coef.boot
    , 'fitted.values.training' = fitted.boot.training
    , 'fitted.values.holdout' = fitted.boot.holdout
  )
  
  return(output)
}

# Score logit model in large batch
# Assumes:
# 1) coef.toScore is output of logit.bootstrap.oneModel()
# 2) formula.toScore is same formula used to produce coef.toScore
# 3) Assumes DT.toScore has fields needed to create a design matrix from formula.toScore
logit.score <- function(
  DT.toScore
  , formula.toScore
  , coef.toScore
  , key
  , jackknife.Ind = FALSE
) {
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
  
  numModels <- copy(nrow(coef.toScore))
  responseString <- gsub('()','',formula.toScore[2])
  DT.scores <- as.data.table(
    cbind(
      DT.toScore[, .(key = get((key)), actual = get(responseString))]
      , DT.scores
    )
  )
  setnames(
    DT.scores
    , c('key', paste0('V',1:numModels))
    , c(key, paste0('modeled_probability',1:numModels))
  )
  
  # If scoring a jackknife model, remove scored value for each omitted row in training data
  if(jackknife.Ind) {
    for(iter in as.integer(1:nrow(DT.toScore))){
      set(DT.scores, i = iter, j = iter + 2L, NA_real_)
    }
  }
  
  DT.scores <- data.table::melt(
    DT.scores
    , id.vars = c(key, 'actual')
    , measure.vars = patterns('modeled_probability')
    , variable.name = 'bootIteration'
    , value.name = 'modeled_probability'
    , variable.factor = FALSE
  )
  
  # data.table::frank is very fast
  # Using frank below assumes that the modeled probability columns are in the order of their boot strapped iteration
  # e.g., The 1st boot iteration comes from column modeled_probability1 and happens to be melted into the first numModels entries of DT.scores
  # This is consistent with the logit.* functions, so it seems fine to use. Could use gsub on 'modeled_probability', but is ~12x slower!
  DT.scores[, bootIteration := data.table::frank(bootIteration, ties.method = 'dense')]
  setkey(DT.scores, bootIteration)
  
  return(DT.scores)
}

## Bootstraped logistic regression, a wrapper for logit.bootstrap.oneModel
logit.bootstrap <- function(
  DT
  , iterations
  , formula
  , DT.holdout
  , key = 'ID'
  , parallel = TRUE
  , cl = NULL
  , keepCluster = FALSE
) {
  boot.output <- if(parallel) {
    clusterCall(cl, function(x) library(data.table))
    clusterExport(cl, list('DT', 'DT.holdout', 'formula', 'logit.bootstrap.oneModel'), environment())
    
    parLapply(
      cl
      , 1:iterations
      , function(i) logit.bootstrap.oneModel(
                      DT.training = DT
                      , DT.holdout = DT.holdout
                      , formula = formula
                      , label = i
                      , key = key
                    )
    )
  } else {
    lapply(
      1:iterations
      , function(i) logit.bootstrap.oneModel(
                      DT.training = DT
                      , DT.holdout = DT.holdout
                      , formula = formula
                      , label = i
                    )
    )
  }
  
  ## Extract components of boot.output and combine into new data.tables
  # Bootstrapped coefficients from probability model
  coef.boot <- rbindlist(lapply(boot.output, function(x) x[['coef']]))
  
  # Bootstrapped fitted probabilities from training data
  prob.boot.training <- rbindlist(lapply(boot.output, function(x) x[['fitted.values.training']]))
  
  setkey(prob.boot.training, bootIteration)
  
  # Bootstrapped fitted probabilities from training data
  # prob.boot.holdout <- do.call(cbind, lapply(boot.output, function(x) x[['fitted.values.holdout']]))
  prob.boot.holdout <- rbindlist(lapply(boot.output, function(x) x[['fitted.values.holdout']]))
  
  setkey(prob.boot.holdout, bootIteration)
  
  if(!keepCluster) {
    on.exit(stopCluster(cl))
  }
  
  output.names <- c(
    'coefficients'
    , 'modeled training'
    , 'modeled holdout'
    , 'modeled bootstrap training'
    , 'modeled bootstrap holdout'
  )
  output <- vector(mode="list", length = length(output.names))
  names(output) <- output.names
  output[['coefficients']] <- coef.boot[, -1]
  
  # The following scores the bootstrapped coefficients against the training and/or holdout datasets
  # Note: This is not proper bootstrapping, but rather a make-shift stability analysis
  output[['modeled training']] <- logit.score(
    DT.toScore = DT
    , formula.toScore = formula
    , coef.toScore = coef.boot
    , key = key
  )
  output[['modeled holdout']] <- logit.score(
    DT.toScore = DT.holdout
    , formula.toScore = formula
    , coef.toScore = coef.boot
    , key = key
  )
  
  output[['modeled bootstrap training']] <- prob.boot.training
  output[['modeled bootstrap holdout']] <- prob.boot.holdout
  
  return(output)
}

# Perform 1 jackknife logistic regression estimation
logit.jackknife.oneModel <- function(
  DT
  , formula
  , omittedRecord
  , key = 'ID'
) {
  DT.jack <- DT[!omittedRecord]
  fit <- glm(
    formula = formula
    , data = DT.jack
    , family = binomial(link = "logit")
  )
  
  # Store bootstrapped fitted values
  fitted.jack <- data.table(
    bootIteration = omittedRecord
    , DT.jack[, .(key = get(key), actual = get(as.character(formula)[2]))]
    , modeled_probability = fit[['fitted.values']]
  )
  
  setnames(
    fitted.jack
    , 'key'
    , key
  )
  
  # Format output
  fit <- coef(summary(fit))
  coef.jack <- as.data.table(fit)
  coef.jack[, term := rownames(fit)]
  setnames(
    coef.jack
    , old = c('Estimate', 'Std. Error', 'z value', 'Pr(>|z|)')
    , new = c('estimate', 'stdError', 'zValue', 'pValue')
  )
  
  # To view all glm diagnostics across all boot iterations, reshape them so each value is a new column (to make stacking results easier)
  coef.jack <- dcast(coef.jack, ... ~ term, value.var = colnames(coef.jack)[1:4])
  
  output <- list(
    'coef' = coef.jack
    , 'fitted.values' = fitted.jack
  )
  
  return(output)
}

# Jackknifed logistic regression, a wrapper for logit.jackknife.oneModel
logit.jackknife <- function(
  DT
  , formula
  , DT.holdout
  , key = 'ID'
  , parallel = TRUE
  , cl
  , keepCluster = FALSE
) {
  n <- nrow(DT)
  
  jack.output <- if(parallel) {
    clusterCall(cl, function(x) library(data.table))
    clusterExport(cl, list('DT', 'formula', 'logit.jackknife.oneModel'), environment())
    
    parLapply(
      cl
      , 1:n
      , function(i) logit.jackknife.oneModel(
                      DT = DT
                      , formula = formula
                      , omittedRecord = i
                      , key = key
                    )
    )
  } else {
    lapply(
      1:n
      , function(i) logit.jackknife.oneModel(
                      DT = DT
                      , formula = formula
                      , omittedRecord = i
                      , key = key
                    )
    )
  }
  
  ## Extract components of boot.output and combine into new data.tables
  # Bootstrapped coefficients from probability model
  coef.jack <- rbindlist(lapply(jack.output, function(x) x[['coef']]))
  
  # Bootstrapped fitted probabilities
  prob.jack <- rbindlist(lapply(jack.output, function(x) x[['fitted.values']]))
  
  setkey(prob.jack, bootIteration)
  
  if(!keepCluster) {
    on.exit(stopCluster(cl))
  }
  
  output.names <- c(
    'coefficients'
    , 'modeled jackknife training'
  )
  output <- vector(mode="list", length = length(output.names))
  names(output) <- output.names
  # For some reason, a column filled with "." is inserted as the 1st column. Not sure why so removing it...
  output[['coefficients']] <- coef.jack[, -1]
  output[['modeled jackknife training']] <- prob.jack
  
  return(output)
}

calculateConfidenceIntervals.bootPercentile <- function(
  DT.boot.sample
  , DT.boot.original
  , DT.apparent
  , aggregateName = 'cutoff'
  , metricName = 'mcc'
  , bootLabelString = 'bootIteration'
  , alpha = 0.05
) {
  metricName.boot <- paste0('i.', metricName)
  DT.join <- DT.apparent[DT.boot.sample, on = aggregateName][!is.na(get(metricName)) & !is.na(get(metricName.boot))]
  
  DT.CI <- DT.join[
    , .(
      metric.original = mean(get(metricName), na.rm = TRUE)
      , metric.bootstrap = mean(get(metricName.boot), na.rm = TRUE)
      , counts = .N
      , lower = quantile(get(metricName.boot), alpha/2)
      , upper = quantile(get(metricName.boot), 1 - alpha/2)
    )
    , keyby = aggregateName
  ]
  
  DT.CI[, metric.bias := metric.bootstrap - metric.original]
  DT.CI[, metric.bias.corrected := 2 * metric.original - metric.bootstrap]
  
  newnames <- paste0(metricName, c('', '.bootstrap', '.bias', '.bias.corrected'))
  setnames(
    DT.CI
    , c('metric.original', 'metric.bootstrap', 'metric.bias', 'metric.bias.corrected')
    , newnames
  )
  
  DT.optimism <- calculateBiasCorrection.optimism(
    DT.boot.sample = DT.boot.sample
    , DT.boot.original = DT.boot.original
    , DT.apparent = DT.apparent
    , aggregateName = aggregateName
    , metricName = metricName
    , bootLabelString = bootLabelString
  )
  
  DT.CI <- DT.CI[DT.optimism, on = aggregateName]
  
  return(DT.CI)
} 

calculateConfidenceIntervals.BCa <- function(
  DT.boot.sample
  , DT.boot.original
  , DT.jackknife
  , DT.apparent
  , aggregateName = 'cutoff'
  , metricName = 'mcc'
  , bootLabelString = 'bootIteration'
  , alpha = 0.05
) {
  # The following is adapted from John Fox's primer on bootstrap regression and accelerated bias-corrected percentile intervals found here:
  # http://statweb.stanford.edu/~tibs/sta305files/FoxOnBootingRegInR.pdf
  
  # 0) Create string for joined metricName name
  metricName.boot <- paste0('i.', metricName)
  
  # 1) Get jackknife mean of metricName by aggregate
  DT.jackknife.means <- DT.jackknife[
    , .(
      metric.mean = mean(get(metricName), na.rm = TRUE)
    )
    , keyby = aggregateName
  ]
  
  # 2) Calculate correction factor a, empirical skewness of metricName in original data via jackknife resampling
  DT.correction.a <- DT.jackknife.means[DT.jackknife, on = aggregateName][!is.na(metric.mean) & !is.na(get(metricName))][
    , .(
      a = sum((metric.mean - get(metricName))^3, na.rm = TRUE) / (6 * sum((metric.mean - get(metricName))^2, na.rm = TRUE)^1.5)
      , metric.var = var(get(metricName), na.rm = TRUE)
    )
    , keyby = aggregateName
  ][
    # Special case where jackknife estimates are all equal, i.e., 0 variance. Replace a with 0 in this case.
    , a := ifelse(metric.var == 0, 0, a)
  ][!is.na(a)]
  
  # 3) Calculate correction factor z, accounting for distribution asymmetry
  # Note: get(metricName.boot) represents the bootstrapped metricName values. 
  # get(metricName) represents the sample metricName values, i.e., from the original data
  DT.join <- DT.apparent[DT.boot.sample, on = aggregateName][!is.na(get(metricName)) & !is.na(get(metricName.boot))]
  
  DT.correction.z <- DT.join[
    , .(
      z.pct = sum(ifelse(get(metricName.boot) <= get(metricName), 1, 0), na.rm = TRUE) / (.N + 1)
      , counts = .N
      , metric.mean = mean(get(metricName.boot), na.rm = TRUE)
    )
    , keyby = aggregateName
  ][
    , z := ifelse(metric.mean == 1L, 0, qnorm(z.pct))
  ]
  
  # 4) Combine correction factors a and z to calculate bootstrap indices for lower and upper bounds of confidence interval
  DT.bounds <- DT.correction.z[DT.correction.a][
    , z.crit := qnorm(1 - alpha/2)
  ][
    , `:=` (
        z.a1 = z + (z - z.crit) / (1 - a * (z - z.crit))
        , z.a2 = z + (z + z.crit) / (1 - a * (z + z.crit))
    )
  ][
    , `:=` (
      a1.ind = floor(pnorm(z.a1) * counts)
      , a2.ind = floor(pnorm(z.a2) * counts)
    )
  ][
    , `:=` (
      lb.ind = ifelse(a1.ind > counts, counts, ifelse(a1.ind < 1, 1, a1.ind))
      , ub.ind = ifelse(a2.ind > counts, counts, ifelse(a2.ind < 1, 1, a2.ind))
    )
  ]
  
  # 5) Get bootstrap estimates from indices in prior step
  DT.boot.bounds <- DT.bounds[DT.join
                              , .(
                                aggregate = get(aggregateName), lb.ind, ub.ind
                                , metric.original = get(metricName)
                                , metric.bootstrap = get(metricName.boot)
                                , metric.mean
                                , counts
                              )
                              , nomatch = 0]
  setnames(DT.boot.bounds, 'aggregate', aggregateName)
  setorderv(DT.boot.bounds, c(aggregateName, 'metric.bootstrap'))
  
  # Select bootstrap estimates of performanceMetric using indices for each aggregate value
  # Use more clunky way via rbindlist to account for banded x-values that have equal indices, e.g., when all bootstrapped values are equal to a special value
  DT.original.bounds <- rbindlist(list(
    DT.boot.bounds[, .SD[unique(lb.ind)], by = aggregateName]
    , DT.boot.bounds[, .SD[unique(ub.ind)], by = aggregateName]
  ))
  
  # Add a trivial field for use in reshaping DT.original.bounds
  DT.original.bounds[, bound := c('lower', 'upper') , by = aggregateName]
  
  # Transform from long to wide to have fields for each bound of performanceMetric
  dcast.formula <- paste0(aggregateName, ' + metric.original + metric.mean + counts ~ bound')
  DT.BCa.CI <- data.table::dcast(
    DT.original.bounds[, -c('lb.ind', 'ub.ind')]
    , formula = dcast.formula
    , value.var = 'metric.bootstrap'
  )
  
  # 6) Calculate bootstrap bvias
  DT.BCa.CI[, metric.bias := metric.mean - metric.original]
  DT.BCa.CI[, metric.bias.corrected := 2 * metric.original - metric.mean]
  
  # 7) Relabel columns to preserve descriptive data
  newnames <- paste0(metricName, c('', '.bootstrap', '.bias', '.bias.corrected'))
  setnames(
    DT.BCa.CI
    , c('metric.original', 'metric.mean', 'metric.bias', 'metric.bias.corrected')
    , newnames
  )
  
  DT.optimism <- calculateBiasCorrection.optimism(
    DT.boot.sample = DT.boot.sample
    , DT.boot.original = DT.boot.original
    , DT.apparent = DT.apparent
    , aggregateName = aggregateName
    , metricName = metricName
    , bootLabelString = bootLabelString
  )
  
  DT.BCa.CI <- DT.BCa.CI[DT.optimism, on = aggregateName]
  
  return(DT.BCa.CI)
}

calculateConfidenceIntervals.probability <- function(
  DT.apparent
  , DT.probabilities.boot.sample
  , DT.probabilities.boot.original
  , DT.probabilities.jackknife = NA
  , xString
  , yString
  , key = 'ID'
  , bandingPrecision = 10
  , alpha = 0.05
  , methodCI = 'BCa'
) {
  DT.apparent <- copy(DT.apparent)
  yString_modeled <- paste0(yString, '_modeled')
  xString_banded <- paste0(xString, '_banded')
  DT.apparent[, (xString_banded) := factor(floor(get(xString) * bandingPrecision) / bandingPrecision)]
  
  DT.probabilities.boot.sample <- DT.apparent[DT.probabilities.boot.sample, on = key]
  
  DT.probabilities.boot.original <- DT.apparent[DT.probabilities.boot.original, on = key]
  
  DT.apparent.agg <- DT.apparent[
    , .(
      response = mean(get(yString), na.rm = TRUE)
      , modeled_probability = mean(get(yString_modeled), na.rm = TRUE)
    )
    , keyby = xString_banded
  ]
  
  if(methodCI == 'BCa') {
    DT.probabilities.jackknife <- DT.apparent[DT.probabilities.jackknife, on = key]
    
    DT.bootCI <- calculateConfidenceIntervals.BCa(
      DT.boot.sample = DT.probabilities.boot.sample
      , DT.boot.original = DT.probabilities.boot.original
      , DT.apparent = DT.apparent.agg
      , DT.jackknife = DT.probabilities.jackknife
      , aggregateName = xString_banded
      , metricName = 'modeled_probability'
      , alpha = alpha
    )
  } else if(methodCI == 'bootPercentile') {
    DT.bootCI <- calculateConfidenceIntervals.bootPercentile(
      DT.boot.sample = DT.probabilities.boot.sample
      , DT.boot.original = DT.probabilities.boot.original
      , DT.apparent = DT.apparent.agg
      , aggregateName = xString_banded
      , metricName = 'modeled_probability'
      , alpha = alpha
    )
  }
  
  return(DT.bootCI)
}

calculateBiasCorrection.optimism <- function(
  DT.boot.sample
  , DT.boot.original
  , DT.apparent
  , aggregateName = 'cutoff'
  , metricName = 'mcc'
  , bootLabelString = 'bootIteration'
) {
  metricName.boot <- paste0('i.', metricName)
  
  DT.boot.original <- DT.boot.original[
    , .(
      metric.original = mean(get(metricName))
    )
    , by = c(bootLabelString, aggregateName)
  ]
  
  DT.boot.sample <- DT.boot.sample[
    , .(
      metric.sample = mean(get(metricName))
    )
    , by = c(bootLabelString, aggregateName)
  ]
  
  DT.BC <- DT.boot.original[
    DT.boot.sample
    , on = c(bootLabelString, aggregateName)
  ][
    , .(
      aggregate = get(aggregateName)
      , metric_difference = metric.sample - metric.original
      , metric.sample
    )
  ][!is.na(metric_difference)][
    , .(
      optimism = mean(metric_difference, na.rm = TRUE)
      , metric.mean.bootstrap = mean(metric.sample, na.rm = TRUE)
      , bootIterations = .N
    )
    , keyby = aggregate
  ]
  setnames(
    DT.BC
    , 'aggregate'
    , aggregateName
  )
  
  DT.BC <- DT.BC[
    DT.apparent
    , on = aggregateName
  ][
    , .(
      aggregate = get(aggregateName)
      , metric.optimism = optimism
      , metric.optimism.corrected = get(metricName) - optimism
    )
  ]
  
  setnames(
    DT.BC
    , c('aggregate', 'metric.optimism', 'metric.optimism.corrected')
    , c(aggregateName, paste0(metricName, c('.optimism', '.optimism.corrected')))
  )
  setkeyv(DT.BC, aggregateName)
  
  return(DT.BC)
}