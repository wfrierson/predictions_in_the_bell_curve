###
# The following custom functions create classifications from modeled probabilities and then quantify their performance
###

# For each probability cutoff, build a confusion matrix using an observed binary target and a column of binary classifications (derived from applying the probability threshold to a column of modeled probabilities). Additional performance metrics calculated for confusion matrix include Cohen's kappa and Matthew's Correlation Coefficient
confusionMatrix.singleModel <- function(DT.predictions, predictionString, actualString = 'actual', numCutoffs = 5e2, singleCutoff = NA, label) {
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
  
  # Create numCutoffs-many columns thresholded by the given cutoff
  DT.predictions[, (predictor.cutoff.names) := lapply(cutoff.seq, function(cutoff) as.numeric(get(predictionString) > cutoff))]
  
  # Create unioned confusion matrices across all numCutoffs-many columns
  DT.predictions.agg <- DT.predictions[
    , c(
      sapply(.SD, function(x) list(pred1 = sum(ifelse(x == 1, 1, 0))))
      , sapply(.SD, function(x) list(pred0 = sum(ifelse(x == 0, 1, 0))))
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
      sensitivity = TP / (TP + FN)
      , specificity = TN / (TN + FP)
      , mcc = (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
      , OA = (TP + TN) / nrow(DT.predictions)
      , EA = ((TP + FP) * (TP + FN) + (TN + FP) * (TN + FN)) / nrow(DT.predictions)^2
    )
    ][, kappa := (OA - EA) / (1 - EA)]
  
  # Add an arbitrary label to distinguish this output from other boot-strapped iterations
  DT.predictions.agg[, label := (label)]
  
  # Remove trivial ROC points & return
  # return(DT.predictions.agg[OA != EA])
  return(DT.predictions.agg)
}

# Wrapper of confusionMatrix.singleModel to extend to multiple models
# Note: This function really should be executed with parallel processing, otherwise it will take a long time!
confusionMatrix.multipleModels <- function(DT.predictions, actualString = 'actual', numCutoffs = 5e2, parallel = TRUE, cl, keepCluster = FALSE) {
  bootIterations <- ncol(DT.predictions) - 1
  
  DT.performance <- if(parallel) {
    clusterCall(cl, function(x) library(data.table))
    clusterExport(cl, c('DT.predictions', 'confusionMatrix.singleModel'), environment())
    
    rbindlist(parLapply(cl, split(1:bootIterations, ceiling(1:bootIterations/(bootIterations/10))), function(x) 
      DT.predictions[, rbindlist(lapply(x, function(pred) 
        confusionMatrix.singleModel(DT.predictions = data.table(actual = get(actualString), prediction = get(paste0('iter',pred))), predictionString = 'prediction', actualString = 'actual', numCutoffs = numCutoffs, label = pred)
      ))
      ]
    ))
  } else {
    rbindlist(lapply(split(1:bootIterations, ceiling(1:bootIterations/(bootIterations/10))), function(x) 
      DT.predictions[, rbindlist(lapply(x, function(pred) 
        confusionMatrix.singleModel(DT.predictions = data.table(actual = get(actualString), prediction = get(paste0('iter',pred))), predictionString = 'prediction', actualString = 'actual', numCutoffs = numCutoffs, label = pred)
      ))
      ]
    ))
  }
  
  if(!keepCluster) {
    on.exit(stopCluster(cl))
  }
  
  return(DT.performance)
}