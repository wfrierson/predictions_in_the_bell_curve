###
# The following custom functions create classifications from modeled probabilities and then quantify their performance
###

calculateMCC.bootPackage <- function(DT, indices, formula, cutoff) {
  DT.copy <- copy(DT)
  response.name <- as.character(formula)[2]
  
  DT.boot <- DT[indices]
  fit <- glm(
    data = DT.boot
    , formula = formula
    , family = binomial(link = "logit")
    , model = FALSE
    , y = FALSE
  )
  
  DT.boot[, modeled := fit[['fitted.values']]]
  DT.boot[, classification := as.integer(modeled > cutoff)]
  
  mcc <- DT.boot[, cor(get(response.name), classification)]
  
  return(mcc)
}

# GIven the inputs of a confusion matrix, calculate Matthew's correlation coefficient.
calculateMCC <- function(TN, FN, FP, TP) {
  output <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(output)
}

calculateSensitivity <- function(FN, TP) {
  output <- TP / (TP + FN)
  return(output)
}

calculateSpecificity <- function(TN, FP) {
  output <- TN / (TN + FP)
  return(output)
}

calculateMCC.boot <- function(
  DT.probabilities
  , probabilityString
  , actualString = 'actual'
  , bootLabelString = ''
  , numCutoffs = 5e2
  , singleCutoff = NA
) {
  cutoff.seq <- if(is.na(singleCutoff)) {
    seq(0,1, length.out = numCutoffs)
  } else {
    singleCutoff
  }
  
  mcc.cutoff.names <- paste0('mcc_cutoff_',1:numCutoffs)
  names(cutoff.seq) <- mcc.cutoff.names
  
  # Don't calculate MCC if cutoff > max(probability) or cutoff < min(probability) (which would give classifications all equal to 0/1, variance would then be zero, and correlation can't be calculated)
  # Would still run trying to do the calculation. Save time by omitting these cases.
  if(bootLabelString == '') {
    DT.mcc <- DT.probabilities[
      , lapply(cutoff.seq, function(cutoff) ifelse(
        cutoff > max(get(probabilityString), na.rm = TRUE) | cutoff < min(get(probabilityString), na.rm = TRUE)
        , NA_real_
        , suppressWarnings(cor(get(actualString), as.integer(get(probabilityString) > cutoff), use = 'pairwise.complete.obs'))
        ))
    ]
    
    DT.mcc <- data.table::melt(
      DT.mcc
      , measure.vars = mcc.cutoff.names
      , value.name = 'mcc'
      , variable.name = 'cutoff'
    )
    
    DT.mcc[, 'bootIteration' := 'single']
  } else {
    DT.mcc <- DT.probabilities[
      , lapply(cutoff.seq, function(cutoff) ifelse(
        cutoff > max(get(probabilityString), na.rm = TRUE) | cutoff < min(get(probabilityString), na.rm = TRUE)
        , NA_real_
        , suppressWarnings(cor(get(actualString), as.integer(get(probabilityString) > cutoff), use = 'pairwise.complete.obs'))
      ))
      , by = .(bootIteration = get(bootLabelString))
    ]
    
    DT.mcc <- data.table::melt(
      DT.mcc
      , id.vars = 'bootIteration'
      , measure.vars = mcc.cutoff.names
      , value.name = 'mcc'
      , variable.name = 'cutoff'
      , variable.factor = FALSE
    )
    setnames(DT.mcc, 'bootIteration', bootLabelString)
  }
  
  DT.mcc[, cutoff := cutoff.seq[as.integer(gsub('mcc_cutoff_','',cutoff))]]
  
  return(DT.mcc[!is.na(mcc)])
}

calculateMCC.boot.parallel <- function(
  DT.probabilities
  , probabilityString
  , actualString = 'actual'
  , bootLabelString = ''
  , numCutoffs = 5e2
  , singleCutoff = NA
  , cl
  , numCores = 8
) {
  bootIterations <- nrow(DT.probabilities[, .N, by = bootLabelString])
  parallelIterations <- bootIterations / numCores / 10
  
  clusterCall(cl, function(x) library(data.table))
  clusterExport(cl, c('calculateMCC.boot', 'probabilityString', 'actualString', 'bootLabelString', 'numCutoffs', 'singleCutoff'), environment())
  
  DT.probabilities[, parallelGrouping := floor(get(bootLabelString) / parallelIterations)]
  DT.mcc <- rbindlist(parLapply(cl, split(DT.probabilities, by = 'parallelGrouping')
                                , function(DT) 
                                  calculateMCC.boot(
                                    DT.probabilities = DT
                                    , probabilityString = probabilityString
                                    , actualString = actualString
                                    , bootLabelString = bootLabelString
                                    , numCutoffs = numCutoffs
                                    , singleCutoff = singleCutoff
                                  )
  ))
  stopCluster(cl)
  DT.probabilities[, parallelGrouping := NULL]
  
  return(DT.mcc)
}

# For each probability cutoff, build a confusion matrix using an observed binary target and a column of binary classifications (derived from applying the probability threshold to a column of modeled probabilities). Additional performance metrics calculated for confusion matrix include Cohen's kappa and Matthew's Correlation Coefficient
confusionMatrix.singleModel <- function(
  DT.predictions
  , predictionString
  , actualString = 'actual'
  , numCutoffs = 5e2
  , singleCutoff = NA
  , label
  , onlyCalcMCC = TRUE
) {
  predictor.cutoff.names <- if(is.na(singleCutoff)) {
    paste0('predictor.cutoff',1:numCutoffs)
  } else {
    'predictor.cutoff1'
  }
  
  cutoff.seq <- if(is.na(singleCutoff)) {
    seq(0,1, length.out = numCutoffs)
  } else {
    singleCutoff
  }
  
  # Create numCutoffs-many columns thresholded by the given cutoff and append to DT.predictions
  DT.predictions[, (predictor.cutoff.names) := lapply(cutoff.seq, function(cutoff) as.integer(get(predictionString) > cutoff))]
  
  # If only calculating MCC, use fact that MCC = cor(actual_target, modeled_classification).
  # No need to calculate confusion matrix. Much faster!
  if(onlyCalcMCC) {
    # Create data.table output that has the same schema when creating a confusion matrix
    output <- data.table(
      cutoff = cutoff.seq
      , FN = NA
      , FP = NA
      , TN = NA
      , TP = NA
      , sensitivity = NA
      , specificity = NA
      # Directly calculate MCC by taking correlation between actual target and modeled classifications
      , mcc = as.vector(t(
          DT.predictions[, lapply(.SD, function(modeled_classification) suppressWarnings(cor(get(actualString), modeled_classification, use = 'pairwise.complete.obs'))), .SDcols = predictor.cutoff.names]
      ))
      , OA = NA
      , EA = NA
      , kappa = NA
      , label = (label)
    )
    
    return(output)
  }
  
  # Create unioned confusion matrices across all numCutoffs-many columns
  DT.predictions.agg <- DT.predictions[
    , c(
      sapply(.SD, function(x) list(pred1 = sum(ifelse(x == 1, 1, 0), na.rm = TRUE)))
      , sapply(.SD, function(x) list(pred0 = sum(ifelse(x == 0, 1, 0), na.rm = TRUE)))
    )
    , by = .(actual = get(actualString))
    , .SDcols = predictor.cutoff.names
  ]
  
  # Melt confusion matrices along the actual target values, in order to derive relevant columns
  DT.predictions.agg <- melt(DT.predictions.agg, id.vars = 'actual', measure.vars = colnames(DT.predictions.agg)[-1])
  DT.predictions.agg[
    , `:=` (
      # Extract the cutoff iteration number for documentation
      cutoffIndex = as.integer(gsub('cutoff','',gsub("^[^.]*\\.|\\.[^.]*$", '', variable)))
      
      # Multiplying by 2 because target is binary
      , predicted = c(rep(1, length(predictor.cutoff.names) * 2), rep(0, length(predictor.cutoff.names) * 2))
    )
  ][
    , `:=` (
      # Lookup cutoff value given index
      cutoff = cutoff.seq[cutoffIndex]
      , variable = NULL
      
      # Build variable used to create confusion matrix cells
      , typePrep = actual*10 + predicted
    )
  ]
  
  # Define the type of confusion matrix cell, e.g., TP, TN, FP, FN
  DT.predictions.agg[
    , type := ifelse(
      typePrep == 0
      , 'TN'
      , ifelse(
        typePrep == 11
        , 'TP'
        , ifelse(
          typePrep == 1
          , 'FP'
          , ifelse(
            typePrep == 10
            , 'FN'
            , 'err'
          )
        )
      )
    )
  ]
  
  # Remove temporary column
  DT.predictions.agg[, typePrep := NULL]
  
  # Dcast the type values to new columns: TP, TN, FP, FN.
  DT.predictions.agg <- dcast(DT.predictions.agg, formula = cutoff ~ type, value.var = 'value')
  
  # Calculate confusion matrix statistics given TP, TN, FP, FN.
  DT.predictions.agg[
    , `:=` (
      sensitivity = calculateSensitivity(FN, TP)
      , specificity = calculateSpecificity(TN, FP)
      , mcc = calculateMCC(TN, FN, FP, TP)
      , OA = (TP + TN) / nrow(DT.predictions)
      , EA = ((TP + FP) * (TP + FN) + (TN + FP) * (TN + FN)) / nrow(DT.predictions)^2
    )
  ][, kappa := (OA - EA) / (1 - EA)]
  
  # Add an arbitrary label to distinguish this output from other boot-strapped iterations
  DT.predictions.agg[, label := (label)]
  
  return(DT.predictions.agg)
}

# Wrapper of confusionMatrix.singleModel to extend to multiple models and in-parallel
# Note: This function really should be executed with parallel processing, otherwise it will take a long time!
confusionMatrix.multipleModels <- function(
  DT.predictions
  , actualString = 'actual'
  , numCutoffs = 5e2
  , parallel = TRUE
  , cl
  , keepCluster = FALSE
) {
  bootIterations <- length(grep('modeled_probability', colnames(DT.predictions), value = TRUE))
  
  DT.performance <- if(parallel) {
    clusterCall(cl, function(x) library(data.table))
    clusterExport(cl, c('DT.predictions', 'confusionMatrix.singleModel', 'calculateSensitivity', 'calculateSpecificity', 'calculateMCC'), environment())
    
    rbindlist(parLapply(cl, split(1:bootIterations, ceiling(1:bootIterations/(bootIterations/10))), function(x) 
      DT.predictions[, rbindlist(lapply(x, function(pred) 
        # Note: A separated data.table is created here to avoid altering the DT.predictions input of confusionMatrix.multipleModels,
        #       since confusionMatrix.singleModel writes new columns to its own DT.predictions input.
        confusionMatrix.singleModel(DT.predictions = data.table(actual = get(actualString), prediction = get(paste0('modeled_probability',pred))), predictionString = 'prediction', actualString = 'actual', numCutoffs = numCutoffs, label = pred)
      ))
      ]
    ))
  } else {
    rbindlist(lapply(split(1:bootIterations, ceiling(1:bootIterations/(bootIterations/10))), function(x) 
      DT.predictions[, rbindlist(lapply(x, function(pred) 
        confusionMatrix.singleModel(DT.predictions = data.table(actual = get(actualString), prediction = get(paste0('modeled_probability',pred))), predictionString = 'prediction', actualString = 'actual', numCutoffs = numCutoffs, label = pred)
      ))
      ]
    ))
  }
  
  if(!keepCluster) {
    on.exit(stopCluster(cl))
  }
  
  return(DT.performance)
}

calculateConfidenceIntervals.classification <- function(
  DT.apparent
  , DT.classifications.boot.sample
  , DT.classifications.boot.original
  , DT.classifications.jackknife = NA
  , xString
  , yString
  , bootLabelString = 'bootIteration'
  , key = 'ID'
  , bandingPrecision = 10
  , alpha = 0.05
  , methodCI = 'BCa'
) {
  DT.apparent <- copy(DT.apparent)
  yString_class <- paste0(yString, '_classification')
  xString_banded <- paste0(xString, '_banded')
  DT.apparent[, (xString_banded) := factor(floor(get(xString) * bandingPrecision) / bandingPrecision)]
  
  DT.classifications.boot.sample <- DT.apparent[DT.classifications.boot.sample, on = key][
    , .(
      classification = mean(classification, na.rm = TRUE)
    )
    , by = c(bootLabelString, xString_banded)
  ]
  DT.classifications.boot.original <- DT.apparent[DT.classifications.boot.original, on = key][
    , .(
      classification = mean(classification, na.rm = TRUE)
    )
    , by = c(bootLabelString, xString_banded)
  ]
  
  DT.apparent.agg <- DT.apparent[
    , .(
      response = mean(get(yString), na.rm = TRUE)
      , classification = mean(get(yString_class), na.rm = TRUE)
    )
    , by = xString_banded
  ]
  
  if(methodCI == 'BCa') {
    DT.classifications.jackknife <- DT.apparent[DT.classifications.jackknife, on = key][
      , .(
        classification = mean(classification, na.rm = TRUE)
      )
      , by = c(bootLabelString, xString_banded)
    ]
    
    DT.bootCI <- calculateConfidenceIntervals.BCa(
      DT.boot.sample = DT.classifications.boot.sample
      , DT.boot.original = DT.classifications.boot.original
      , DT.apparent = DT.apparent.agg
      , DT.jackknife = DT.classifications.jackknife
      , aggregateName = xString_banded
      , metricName = 'classification'
      , alpha = alpha
    )
    
  } else if(methodCI == 'bootPercentile') {
    DT.bootCI <- calculateConfidenceIntervals.bootPercentile(
      DT.boot.sample = DT.classifications.boot.sample
      , DT.boot.original = DT.classifications.boot.original
      , DT.apparent = DT.apparent.agg
      , aggregateName = xString_banded
      , metricName = 'classification'
      , alpha = alpha
    )
  }
  
  # Ensure bias & optimism corrected values are between 0 & 1
  DT.bootCI[
    , `:=` (
      classification.bias.corrected = ifelse(classification.bias.corrected > 1, 1, ifelse(classification.bias.corrected < 0, 0, classification.bias.corrected))
      , classification.optimism.corrected = ifelse(classification.optimism.corrected > 1, 1, ifelse(classification.optimism.corrected < 0, 0, classification.optimism.corrected))
    )
  ]
  
  return(DT.bootCI)
}

calculateConfidenceIntervals.classification.fromProbabilities <- function(
  DT.apparent
  , DT.probabilities.boot.sample
  , DT.probabilities.boot.original
  , DT.probabilities.jackknife = NA
  , xString
  , yString
  , probabilityString = 'modeled_probability'
  , bootLabelString = 'bootIteration'
  , cutoff
  , key = 'ID'
  , bandingPrecision = 10
  , alpha = 0.05
  , methodCI = 'BCa'
) {
  DT.apparent <- copy(DT.apparent)
  yString_class <- paste0(yString, "_classification")
  yString_modeled <- paste0(yString, '_modeled')
  xString_banded <- paste0(xString, '_banded')
  
  DT.apparent[, (xString_banded) := factor(floor(get(xString) * bandingPrecision) / bandingPrecision)]
  DT.apparent[, (yString_class) := as.integer(get(yString_modeled) > cutoff)]
  
  DT.probabilities.boot.sample[, classification := as.integer(get(probabilityString) > cutoff)]
  DT.probabilities.boot.original[, classification := as.integer(get(probabilityString) > cutoff)]
  
  if(methodCI == 'BCa') {
    DT.probabilities.jackknife[, classification := as.integer(get(probabilityString) > cutoff)]
    
    DT.bootCI <- calculateConfidenceIntervals.classification(
      DT.apparent = DT.apparent
      , DT.classifications.boot.sample = DT.probabilities.boot.sample
      , DT.classifications.boot.original = DT.probabilities.boot.original
      , DT.classifications.jackknife = DT.probabilities.jackknife
      , xString = xString
      , yString = yString
      , key = key
      , bandingPrecision = bandingPrecision
      , alpha = alpha
      , methodCI = methodCI
    )
    
    DT.probabilities.jackknife[, classification := NULL]
    
  } else if(methodCI == 'bootPercentile') {
    DT.bootCI <- calculateConfidenceIntervals.classification(
      DT.apparent = DT.apparent
      , DT.classifications.boot.sample = DT.probabilities.boot.sample
      , DT.classifications.boot.original = DT.probabilities.boot.original
      , xString = xString
      , yString = yString
      , key = key
      , bandingPrecision = bandingPrecision
      , alpha = alpha
      , methodCI = methodCI
    )
  }
  
  DT.probabilities.boot.sample[, classification := NULL]
  DT.probabilities.boot.original[, classification := NULL]
  
  return(DT.bootCI)
}