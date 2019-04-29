###
# The following custom functions create various visualizations to understand the predictive performance of binary classifications
###

lightBlue <- 'royalblue'

# Average the response (via yString) and the predicted probabilities over a discretized version of the input xString (via bandingPrecision)
# This function assumes that DT has already been scored with the original, non-bootstrapped logistic regression model, i.e., response.modeled exists
plotFrequencyVsProbability <- function(
  DT.factors
  , xString
  , yString
  , response.label
  , bandingPrecision = 10
  , output.format = 'plot_grid'
) {
  # Make explicit copy to avoid writing by reference to original object
  # This assumes nrow(DT.factors) is reasonably small, e.g., few thousand rows
  DT.factors <- copy(DT.factors)
  yString_modeled <- paste0(yString, '_modeled')
  
  xString_banded <- paste0(xString, '_banded')
  DT.factors[, (xString_banded) := factor(floor(get(xString) * bandingPrecision) / bandingPrecision)]
  
  DT.plot <- DT.factors[
    , .(
      response = mean(get(yString), na.rm = TRUE)
      , modeled_probability = mean(get(yString_modeled), na.rm = TRUE)
      , counts = .N
    )
    , by = xString_banded
  ]
  names.plot <- c('Observed Response', 'Probability Model (Original Sample)')
  setnames(DT.plot, c('response', 'modeled_probability'), names.plot)
  
  DT.melt <- melt(
    DT.plot
    , id.vars = xString_banded
    , measure.vars = names.plot
    , value.name = 'response'
    , variable.name = 'Response_Type'
  )
  
  plot.a2e <- ggplot(
    DT.melt[Response_Type != 'Observed Response']
    , aes_string(x = xString_banded, y = 'response', color = 'Response_Type', group = 'Response_Type')
  ) + 
    geom_line() + 
    geom_point(data = DT.melt[Response_Type == 'Observed Response'], aes(y = response, group = Response_Type)) + 
    scale_color_manual(values = c('black', lightBlue)) +
    theme_bw() +
    guides(
      color = guide_legend(
        title = 'Response Type:'
        , override.aes = list(
          linetype = c("blank", "solid")
          , shape = c(16, NA)
        )
      )
    ) +
    ylab(paste0(response.label, ' (field: ',yString, ')')) +
    theme(
      axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
      , legend.position="bottom"
    )
  
  plot.volume <- ggplot(
    DT.plot
    , aes_string(
      x = xString_banded
      , y = 'counts'
    )
  ) + 
    geom_bar(stat = 'identity') + 
    theme_void()
  
  if(output.format == 'plot_grid') {
    output <- cowplot::plot_grid(plot.volume, plot.a2e, nrow = 2, rel_heights = c(0.15, 0.85), align = 'v')
  } else if(output.format == 'list') {
    output <- list(
      'volume' = plot.volume
      ,'a2e' = plot.a2e
    )
  }
  
  return(output)
}

# Compare observed response with the bootstrap average of modeled probability and BCa percentile intervals for modeled probability by a given (discretized) factor
plotFrequencyVsProbability.bootCI <- function(
  DT.factors
  , DT.probabilities.boot = NULL
  , DT.probabilities.jackknife = NULL
  , DT.bootCI = NULL
  , xString
  , yString
  , key = 'ID'
  , bandingPrecision = 10
  , alpha = 0.05
  , response.label
  , methodCI = 'BCa'
  , output.format = 'plot_grid'
) {
  DT.factors <- copy(DT.factors)
  yString_modeled <- paste0(yString, '_modeled')
  xString_banded <- paste0(xString, '_banded')
  
  DT.factors[, (xString_banded) := factor(floor(get(xString) * bandingPrecision) / bandingPrecision)]
  
  if(is.null(DT.bootCI)) {
    # Calculate bias-corrected accelerated bootstrapped 95% confidence intervals for yString by xString
    DT.bootCI <- calculateConfidenceIntervals.probability(
      DT.factors = DT.factors
      , DT.probabilities.boot = DT.probabilities.boot
      , DT.probabilities.jackknife = DT.probabilities.jackknife
      , xString = xString
      , yString = yString
      , key = key
      , bandingPrecision = bandingPrecision
      , methodCI = methodCI
    )
  } else {
    # These are assumed to be small enough to copy without a material memory impact
    DT.bootCI <- copy(DT.bootCI)
  }
   
  DT.original <- DT.factors[
    , .(
      response = mean(get(yString), na.rm = TRUE)
      , modeled_probability = mean(get(yString_modeled), na.rm = TRUE)
      , counts = .N
    )
    , by = xString_banded
  ]
  
  DT.plot <- DT.bootCI[DT.original[, mget(c(xString_banded, 'response'))], on = xString_banded]
  
  names.plot <- c('Observed Response', 'Probability Model (Original Sample)')
  setnames(DT.plot, c('response', 'modeled_probability'), names.plot)
  
  DT.plot.melt <- melt(
    DT.plot
    , id.vars = c(xString_banded, 'lower', 'upper')
    , measure.vars = names.plot
    , value.name = 'response'
    , variable.name = 'Response_Type'
  )
  
  plot.a2e.bootCI <- ggplot(
    DT.plot.melt[Response_Type != names.plot[1]]
    , aes_string(
      x = xString_banded
      , y = 'response'
      , color = 'Response_Type'
      , group = 'Response_Type'
    )
  ) + 
    geom_line() + 
    geom_ribbon(
      data = DT.plot.melt[Response_Type != names.plot[1]]
      , aes(
        ymin = lower
        , ymax = upper
        , fill = Response_Type
        , group = Response_Type
      )
      , alpha = 0.2
      , linetype = 0
      , show.legend = FALSE
    ) +
    geom_point(
      data = DT.plot.melt[Response_Type == names.plot[1]]
      , aes(
        y = response
        , color = Response_Type
        , group = Response_Type
      )
    ) + 
    theme_bw() +
    guides(
      color = guide_legend(
        title = 'Response Type:'
        , override.aes = list(
          linetype = c("blank", "solid")
          , shape = c(16, NA)
        )
      )
    ) +
    scale_color_manual(values = c('black', lightBlue)) +
    scale_fill_manual(values = lightBlue) +
    ylab(paste0(response.label, ' (field: ',yString, ')')) +
    theme(
      axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
      , legend.position="bottom"
    )
  
  plot.volume <- ggplot(
    DT.plot
    , aes_string(
      x = xString_banded
      , y = 'counts'
    )
  ) + 
    geom_bar(stat = 'identity') + 
    theme_void()
  
  if(output.format == 'plot_grid') {
    output <- cowplot::plot_grid(plot.volume, plot.a2e.bootCI, nrow = 2, rel_heights = c(0.15, 0.85), align = 'v')
  } else if(output.format == 'list') {
    output <- list(
      'volume' = plot.volume
      ,'a2e' = plot.a2e.bootCI
    )
  }
  
  return(output)
}

plotFrequencyVsClassification.bootCI <- function(
  DT.factors
  , DT.probabilities.boot = NULL
  , DT.probabilities.jackknife = NULL
  , DT.classification.bootCI = NULL
  , DT.probabilities.bootCI = NULL
  , xString
  , yString
  , cutoff
  , key = 'ID'
  , bandingPrecision = 10
  , alpha = 0.05
  , response.label
  , methodCI = 'BCa'
  , output.format = 'plot_grid'
) {
  DT.factors <- copy(DT.factors)
  yString_class <- paste0(yString, "_class")
  yString_modeled <- paste0(yString, '_modeled')
  xString_banded <- paste0(xString, '_banded')

  DT.factors[, (xString_banded) := factor(floor(get(xString) * bandingPrecision) / bandingPrecision)]
  DT.factors[, (yString_class) := as.integer(get(yString_modeled) > cutoff)]
  
  if(is.null(DT.classification.bootCI)) {
    # Calculate bias-corrected accelerated bootstrapped 95% confidence intervals for yString by xString
    DT.classification.bootCI <- calculateConfidenceIntervals.classification.fromProbabilities(
      DT.factors = DT.factors
      , DT.probabilities.boot = DT.probabilities.boot
      , DT.probabilities.jackknife = DT.probabilities.jackknife
      , xString = xString
      , yString = yString
      , cutoff = cutoff
      , key = key
      , bandingPrecision = bandingPrecision
      , alpha = alpha
      , methodCI = methodCI
    )
  } else {
    # These are assumed to be small enough to copy without a material memory impact
    DT.classification.bootCI <- copy(DT.classification.bootCI)
  }
  setnames(DT.classification.bootCI, c('lower', 'upper'), c('lower.class', 'upper.class'))
  
  if(is.null(DT.probabilities.bootCI)) {
    # Calculate bias-corrected accelerated bootstrapped 95% confidence intervals for yString by xString
    DT.probabilities.bootCI <- calculateConfidenceIntervals.probability(
      DT.factors = DT.factors
      , DT.probabilities.boot = DT.probabilities.boot
      , DT.probabilities.jackknife = DT.probabilities.jackknife
      , xString = xString
      , yString = yString
      , key = key
      , bandingPrecision = bandingPrecision
      , methodCI = methodCI
    )
  } else {
    # These are assumed to be small enough to copy without a material memory impact
    DT.probabilities.bootCI <- copy(DT.probabilities.bootCI)
  }
  setnames(DT.probabilities.bootCI, c('lower', 'upper'), c('lower.prob', 'upper.prob'))
  
  DT.original <- DT.factors[
    , .(
      response = mean(get(yString), na.rm = TRUE)
      , classification = mean(get(yString_class), na.rm = TRUE)
      , counts = .N
    )
    , by = xString_banded
  ]
  
  DT.plot <- DT.classification.bootCI[DT.probabilities.bootCI, , on = xString_banded][DT.original[, mget(c(xString_banded, 'response'))], on = xString_banded]
  
  cutoff.label <- floor(cutoff*1e4)/1e4
  names.plot <- c(
    'Observed Response'
    , 'Probability Model (Original Sample)'
    , paste0('Modeled Classification (Original Sample, Cutoff = ', cutoff.label, ')')
  )
  setnames(DT.plot, c('response', 'modeled_probability', 'classification'), names.plot)
  
  DT.plot.melt <- melt(
    DT.plot
    , id.vars = c(xString_banded, 'lower.class', 'upper.class', 'lower.prob', 'upper.prob')
    , measure.vars = names.plot
    , value.name = 'response'
    , variable.name = 'Response_Type'
  )
  
  plot.classification <- ggplot(
    DT.plot.melt[Response_Type != names.plot[1]]
    , aes_string(
      x = xString_banded
      , y = 'response'
      , color = 'Response_Type'
      , group = 'Response_Type'
    )
  ) + 
    geom_line() + 
    geom_ribbon(
      data = DT.plot.melt[Response_Type == names.plot[2]]
      , aes(
        ymin = lower.prob
        , ymax = upper.prob
        , fill = Response_Type
        , group = Response_Type
      )
      , alpha = 0.2
      , linetype = 0
      , show.legend = FALSE
    ) +
    geom_ribbon(
      data = DT.plot.melt[Response_Type == names.plot[3]]
      , aes(
        ymin = lower.class
        , ymax = upper.class
        , fill = Response_Type
        , group = Response_Type
      )
      , alpha = 0.2
      , linetype = 0
      , show.legend = FALSE
    ) +
    geom_point(
      data = DT.plot.melt[Response_Type == names.plot[1]]
      , aes(
        y = response
        , color = Response_Type
        , group = Response_Type
      )
    ) + 
    theme_bw() +
    scale_fill_manual(values = c(lightBlue, 'grey')) +
    guides(
      color = guide_legend(
        title = 'Response Type:'
        , override.aes = list(
            linetype = c("solid", "blank", "solid")
            , shape = c(NA, 16, NA)
        )
        , nrow = 2
        , byrow = TRUE
      )
    ) +
    scale_color_manual(
      values = c(lightBlue, 'black', 'grey')
    ) +
    ylab(paste0(response.label, ' (field: ',yString, ')')) +
    theme(
      axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
      , legend.position="bottom"
    )
  
  plot.volume <- ggplot(
    DT.original
    , aes_string(
      x = xString_banded
      , y = 'counts'
    )
  ) + 
    geom_bar(stat = 'identity') + 
    theme_void()
  
  if(output.format == 'plot_grid') {
    output <- cowplot::plot_grid(plot.volume, plot.classification, nrow = 2, rel_heights = c(0.15, 0.85), align = 'v')
  } else if(output.format == 'list') {
    output <- list(
      'volume' = plot.volume
      ,'classification' = plot.classification
    )
  }
  
  return(output)
}

# Plot distribution of given performance metric
plotPerformanceMetric.hist <- function(
  DT.performance
  , performanceMetric = 'mcc'
  , bins = 100
) {
  xlabel <- switch(performanceMetric, 'mcc' = "Matthew's Correlation Coefficient", 'kappa' = "Cohen's Kappa")  
  
  p <- ggplot(
    DT.performance
    , aes_string(x = performanceMetric)
  ) + 
    geom_histogram(bins = bins) +
    theme_bw() +
    xlab(xlabel) +
    ggtitle(paste('Distribution of', xlabel, 'when cutoff = ',floor(DT.performance[, mean(cutoff)] * 1e4) / 1e4)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16)
      , axis.text=element_text(size = 16)
      , axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
      , legend.position="bottom"
    )
  
  return(p)
}

# Plot bootstrapped distribution of performanceMetric by cutoff within DT.performance and include mean of performanceMetric
plotPerformanceMetric.cutoff.mean <- function(
  DT.performance
  , performanceMetric = 'mcc'
  , bins = 50
) {
  ylabel <- switch(performanceMetric, 'mcc' = "Matthew's Correlation Coefficient", 'kappa' = "Cohen's Kappa") 
  
  p <- ggplot(
    DT.performance
    , aes_string(x = 'cutoff', y = performanceMetric)
  ) + 
    stat_bin_hex(bins = bins) + 
    geom_line(
      data = DT.performance[
        , .(
          performanceMetric.mean = mean(get(performanceMetric), na.rm = TRUE)
        )
        , keyby = .(cutoff = floor(cutoff*100)/100)
      ]
      , aes(
        x = cutoff
        , y = performanceMetric.mean
        , color = factor(paste0('mean ', performanceMetric))
      )
    ) +
    scale_colour_manual(values = 'orange') +
    guides(color = 'none') +
    theme_bw() +
    ylab(ylabel) +
    theme(
      axis.text=element_text(size = 16)
      , axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
      , legend.position="bottom"
    )
  
  return(p)
}

# Function to plot the following aestheticcs across a grid of cutoff values:
# i) Solid line representing the performance metric using the original (non-bootstrapped) data
# ii) Ribbon around representing the bias-corrected accelerated confidence interval of the population's true performance metric
# iii) Dashed line representing the mean of bootstrapped performance metrics
# iv) A dashed vertical line representing the cutoff with optimal performance metric using the dashed bootstrapped lined
#
# The confidence interval can be created on-the-fly using DT.performance.boot, DT.performance.original, DT.performance.jackknife.
# Alternatively, pre-computed confidence intervals can be inputted via DT.bootCI
plotPerformanceMetric.cutoff.bootCI <- function(
  DT.performance.boot = NULL
  , DT.performance.original = NULL
  , DT.performance.jackknife = NULL
  , DT.bootCI = NULL
  , performanceMetric = 'mcc'
  , performanceMetric.boot = 'mcc.bootstrap'
  , cutoff.name = 'cutoff'
  , lowerCI.name = 'lower'
  , upperCI.name = 'upper'
  , alpha = 0.05
  , methodCI = 'BCa'
  , cutoff.optimal = NULL
  , cutoff.optimal.label = NULL
  , output.format = 'plot_grid'
) {
  # The following is adapted from John Fox's primer on bootstrap regression and accelerated bias-corrected percentile intervals found here:
  # http://statweb.stanford.edu/~tibs/sta305files/FoxOnBootingRegInR.pdf
  
  # In case this plotting function is called after the column names of DT.CI have been changed, 
  # remove the renamed components to ensure ylabel is created correctly
  performanceMetric.label <- gsub('.sample.*', '', performanceMetric)
  ylabel <- switch(performanceMetric.label, 'mcc' = "Matthew's Correlation Coefficient", 'kappa' = "Cohen's Kappa") 
  
  if(is.null(DT.bootCI)) {
    # Calculate bias-corrected accelerated bootstrapped 95% confidence intervals for performanceMetric by cutoff
    if(methodCI == 'BCa') {
      DT.bootCI <- calculateConfidenceIntervals.BCa(
        DT.boot = DT.performance.boot
        , DT.original = DT.performance.original
        , DT.jackknife = DT.performance.jackknife
        , aggregateName = cutoff.name
        , metricName = performanceMetric
        , alpha = alpha
      )
    } else if(methodCI == 'bootPercentile') {
      DT.bootCI <- calculateConfidenceIntervals.bootPercentile(
        DT.boot = DT.performance.boot
        , DT.original = DT.performance.original
        , aggregateName = cutoff.name
        , metricName = performanceMetric
        , alpha = alpha
      )
    }
    
  } else {
    # These are assumed to be small enough to copy without a material memory impact
    DT.bootCI <- copy(DT.bootCI)
  }
  
  names.plot <- c('Original Sample', 'Bootstrapped Average')
  setnames(DT.bootCI, c(performanceMetric, performanceMetric.boot), names.plot)
  
  if(is.null(cutoff.optimal)) {
    cutoff.optimal <- DT.bootCI[order(-get(names.plot[2]))][1L, get(cutoff.name)]
  }
  
  if(is.null(cutoff.optimal.label)) {
    cutoff.optimal.label <- 'Selected Cutoff from Bootstrapped Average'
  }
  
  # Melt the plot data
  DT.plot <- data.table::melt(
    DT.bootCI
    , id.vars = c(cutoff.name, lowerCI.name, upperCI.name)
    , measure.vars = names.plot
    , value.name = 'response'
    , variable.name = 'Response_Type'
  )
  
  # Plot
  plot.metric <- ggplot(
    DT.plot[Response_Type == names.plot[1]]
    , aes_string(
      x = cutoff.name
      , y = 'response'
      , color = 'Response_Type'
      , group = 'Response_Type'
    )
  ) + 
    geom_ribbon(
      aes_string(
        ymin = lowerCI.name
        , ymax = upperCI.name
      )
      , alpha = 0.2
      , color = NA
      , fill = lightBlue
      , show.legend = FALSE
    ) + 
    geom_line() + 
    # Include mean of performanceMetric by cutoff as a dashed line to help see the overall trend
    geom_line(
      data = DT.plot[Response_Type != names.plot[1]]
      , linetype = 2
    ) +
    theme_bw() +
    scale_color_manual(
      values = c(rep(lightBlue, 2), 'black')
    ) +
    guides(
      color = guide_legend(
        title = 'Response Type:'
        , override.aes = list(
            linetype = c("dashed", "solid", "dashed")
            , size = c(1, 1, 1.25)
        )
        , nrow = 2
        , byrow = TRUE
      )
    ) +
    xlim(c(0, 1)) +
    ylab(ylabel) +
    xlab('Cutoff') +
    theme(
      axis.text=element_text(size = 16)
      , axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
      , legend.position="bottom"
      , legend.key.height = unit(2, 'mm')
      , legend.key.width = unit(4, 'line')
    ) +
    geom_vline(aes(
      xintercept = cutoff.optimal
      , color = cutoff.optimal.label
    )
      , linetype = 2
      , size = 1.5
      , show.legend = FALSE
    )
  
  plot.volume <- ggplot(
    DT.bootCI
    , aes_string(
      x = cutoff.name
      , y = 'counts'
    )
  ) + 
    geom_bar(stat = 'identity') + 
    xlim(c(0, 1)) +
    theme_void()
  
  if(output.format == 'plot_grid') {
    output <- cowplot::plot_grid(plot.volume, plot.metric, nrow = 2, rel_heights = c(0.15, 0.85), align = 'v')
  } else if(output.format == 'list') {
    output <- list(
      'volume' = plot.volume
      ,'metric' = plot.metric
    )
  }
  
  return(output)
}

# Build receiver operating characteristic plot using bootstrapped classification data 
plotROC <- function(DT.performance) {
  ## Build needed data.tables
  # Table representing reference line
  DT.roc <- unique(DT.performance[
    , .(
      FPR = 1 - specificity
      , TPR = 1 - specificity
      , TPR.ub = 1 - specificity
      , TPR.lb = 1 - specificity
      , cutoff
      , Group = factor('Reference line')
      , alpha = 1
      , type = factor('dashed', levels = c('solid', 'dashed'))
    )
  ])
  
  # Table representing the mean ROC data from bootstrapped classification data 
  # Note: Bootstrapped quantiles are used, which are not as reliable as the BCa confidence intervals
  # To be discussed in the report, BCa intervals aren't needed here since the optimal cutoff value derived from ROC tends to be very poor.
  # Hence, the conclusions of the report are unchanged if BCa intervals are used here.
  DT.mean <- DT.performance[
    , .(
      TPR = mean(sensitivity, na.rm = TRUE)
      , cutoff = mean(cutoff)
      , TPR.ub = quantile(sensitivity, 1 - 0.05/2, na.rm = TRUE)
      , TPR.lb = quantile(sensitivity, 0.05/2, na.rm = TRUE)
      
    )
    , keyby = .(FPR = floor(-specificity*100)/100 + 1)
  ][
    ,.(
      FPR
      , TPR
      , TPR.ub
      , TPR.lb
      , cutoff
      , Group = factor('Mean of bootstrap models')
      , alpha = 1
      , type = factor('solid', levels = c('solid', 'dashed'))
    )
  ]
  
  # Calculate optimal cutoff using on ROC data and store corresponding sensitivity and specificity
  cutoff.roc <- DT.mean[
    , .(
      FPR
      , TPR
      , cutoff
      , distFromCorner = sqrt((TPR - 1)^2 + (FPR)^2)
      , Group = factor('Optimal ROC Cutoff')
      , type = factor('solid', levels = c('solid', 'dashed'))
      , alpha = 1
    )
  ][order(distFromCorner)][1L]
  
  p <- ggplot(
    rbindlist(list(
      DT.roc
      , DT.mean
    ))
    , aes(
      x = FPR
      , y = TPR
      , alpha = alpha
      , group = Group
      , color = Group
      , linetype = type
    )
  ) + 
    geom_line() +
    geom_ribbon(
      data = DT.mean
      , aes(
        ymin = TPR.lb
        , ymax = TPR.ub
      )
      , color = 'grey'
      , alpha = 0.2
    ) +
    guides(
      alpha = 'none'
      , linetype = 'none'
      , color = guide_legend(
        title = 'Response Type:'
        , override.aes = list(
          linetype = c("dashed", "solid")
        )
      )
    ) +
    xlim(c(-0.1, 1.1)) +
    theme_bw() +
    scale_color_manual(values = c(
      'Bootstrap models' = 'grey',
      'Reference line' = 'black',
      'Mean of bootstrap models' = 'red'
    )) +
    geom_point(data = cutoff.roc, aes(x = FPR, y = TPR, group = Group), color = 'black', size = 5, shape = 1) + 
    geom_text(data = cutoff.roc, x = -Inf, y = Inf, color = 'black', label = paste0('Optimal cutoff by maximizing sensitivity and specificity (circled) = ', floor(cutoff.roc$cutoff * 1e3)/1e3), hjust = -0.01, vjust = 1.3) + 
    xlab('False positive rate') +
    ylab('True positive rate') +
    theme(
      axis.text=element_text(size = 16)
      , axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
      , legend.position="bottom"
    )
  
  return(p)
}

plotMultipleClassifications <- function(
  DT.factors
  , DT.predictions
  , DT.performance
  , xString
  , yString
  , cutoffs
  , key = 'ID'
  , performanceMetric = 'mcc'
  , bandingPrecision = 10
  , response.label
) {
  ### ASSUMPTIONS ###
  # 1) DT.performance comes from confusionMatrix.*
  # 2) DT.predictions comes from logit.bootstrap[['modeled *']]
  # 3) DT.predictions and DT.factors have the same row order (which should be true if the prior assumption is true)
  
  ### SETUP ###
  # Store names of bootstrapped probabilities
  colnames.predictions <- grep('modeled_probability', copy(colnames(DT.predictions)), value = TRUE)
  bootIterations <- length(colnames.predictions)
  
  # Append DT.factors with DT.predictions Assumes order is the same!
  xyString <- c(xString, yString)
  
  DT <- DT.factors[DT.predictions, on = key][, mget(c(xyString, colnames.predictions))]
  
  # Create banded x-field for use in plotting
  # Assumes field in xString is continuous and with meaingful information for decimal values!
  # This is true for the factors under consideration in this analysis.
  xString_banded <- paste0(xString, '_banded')
  DT[, (xString_banded) := factor(floor(get(xString) * bandingPrecision) / bandingPrecision)]
  
  ### CUTOFFS ###
  # Calculate absolute difference between DT.performance$cutoff and each element of cutoffs
  DT.cutoffs <- DT.performance[, lapply(unname(cutoffs), function(x) abs(cutoff - x))]
  
  # Find indices of DT.cutoffs that are closest to the values in cutoffs
  cutoffs.ind <- unlist(lapply(seq_along(cutoffs), function(ind) DT.cutoffs[, .I[which.min(get(paste0('V',ind)))]]))
  
  # Use closest cutoff values in DT.performance to prevent recomputing expensive calculations
  cutoffs.closest <- DT.performance[cutoffs.ind]$cutoff
  
  # Aggregate performance metrics from DT.performance
  DT.performance.agg <- DT.performance[
    cutoff %in% cutoffs.closest
    , .(
        mcc = mean(mcc, na.rm = TRUE)
        , kappa = mean(kappa, na.rm = TRUE)
      )
    , keyby = cutoff
  ]
  
  # Append original cutoff values to create a lookup table
  DT.performance.agg <- cbind(DT.performance.agg, cutoff.original = cutoffs)
  setkey(DT.performance.agg, cutoff.original)
  
  ### CREATE CLASSIFICATIONS ###
  # Restate cutoffs to nearest % for clearer column names
  cutoffs <- as.integer(cutoffs * 1e4)/1e4
  
  # Create vector to store names of bootstrapped classifications for each cutoff
  colnames.classifications <- unlist(lapply(cutoffs, function(cutoff) paste0(colnames.predictions, '_class_', cutoff)))
  
  # Create data.frame with all pairwise combinations of predicted probability columns and cutoff values
  cases <- expand.grid(colnames.predictions, cutoffs.closest, stringsAsFactors = TRUE)
  
  # Convert rows of data.frame to lists for easier use in lapply
  cases <- split(cases, seq(nrow(cases)))
  
  # Create new classication columns: 1 for each pair of predicted probabilities and cutoff value
  DT[, (colnames.classifications) := lapply(cases, function(case) as.integer(get(as.character(case[[1]])) > case[[2]]))]
  
  ### MELT CLASSIFICATION AND PROBABILITY COLUMNS ###
  # Rename classifications by removing their distinct bootstrap iteration number, since we are distinguishing by cutoff instead
  # This intentionally creates duplicate column names which will later be melted together
  colnames.classifications.new <- gsub('bootIter[[:digit:]]+_class_', '', colnames.classifications)
  setnames(DT, colnames.classifications, colnames.classifications.new)
  
  # Add copies of modeled probabilities with a similar naming setup, to make it easier to average them all together
  colnames.predictions.new <- rep('probabilities', length(colnames.predictions))
  setnames(DT, colnames.predictions, colnames.predictions.new)
  
  # Combine the duplicate classification and probability columns
  # For classification columns, e.g., instead of nrow(DT)-many rows with length(colnames.predictions)xlength(cutoffs)-many classifications columns, 
  # combine these into nrow(DT)xlength(colnames.predictions)-many rows with length(cutoffs)-many classifications columns
  # The key functionality here is patterns() which uses a regular expression to find duplicate column names
  colnames.melt <- c(yString, 'probabilities', cutoffs)
  DT.melt <-  melt(DT
                  , id.vars = xString_banded
                  , measure.vars = patterns(colnames.melt)
                  , value.name = colnames.melt
              )
  
  # Melt DT.melt again so that all classification and probability values are in 1 column called 'value'
  DT.melt <-  melt(
                  DT.melt
                  , id.vars = xString_banded
                  , measure.vars = colnames.melt
                  , na.rm = TRUE
                  , variable.name = 'group'
                  , value.name = 'value'
              )
  
  # Create dataset used for plotting by calculating mean value across different groups, i.e., mean classification or mean probability
  DT.plot <- DT.melt[ 
    , .(
      frequency = mean(value)
      , counts = .N
    )
    , by = mget(colnames(DT.melt)[1:2])
  ]
  
  # Currently, data.table does not appear to allow using a variable string to define an ad-hoc aggregation value in a group-by
  # Doing this manually to preserve the original yString label
  yString_average <- paste0(yString,'_average')
  setnames(DT.plot, 'frequency', yString_average)
  
  # Append selected performance metric to use in coloring
  # Note: This will produce a warning when yString contains letters
  DT.plot <- cbind(
    DT.plot
    , DT.performance.agg[.(suppressWarnings(as.numeric(as.character(DT.plot$group)))), get(performanceMetric)]
  )
  
  # Rename performance metric
  setnames(DT.plot, 'V2', performanceMetric)
  
  # Format performanceMetric values for clearer plotting
  performanceMetric.levels <- DT.performance.agg[, as.integer(get(performanceMetric) * 100) / 100]
  
  # Create factor 'group.plot' to better document the plot itself by giving descriptive labels for each group
  group.plot.levels <- c('Observed Response', 'Modeled Probabilities', paste0('(Cutoff = ',cutoffs, ', ', performanceMetric , ' = ', performanceMetric.levels, ')'))
  DT.plot[, group.plot := factor(group.plot.levels[group], levels = group.plot.levels)]
  
  # Create color palette with length equal to number of cutoffs
  color.palette <- RColorBrewer::brewer.pal(length(cutoffs), 'Dark2')
  color.palette <- c('black', 'grey', color.palette)
  names(color.palette) <- group.plot.levels
  
  # Create ggplot objects
  plot.a2e <- ggplot(
    DT.plot[group != (yString)]
    , aes_string(
      x = xString_banded
      , y = yString_average
      , group = 'group.plot'
      , color = 'group.plot'
    )
  ) +
    geom_line() +
    geom_point(
      data = DT.plot[group == (yString)]
      , aes_string(
        x = xString_banded
        , y = yString_average
      )
    ) +
    # Use grey to plot bootstrapped probabilities and black for the observed response points. Otherwise, use color.palette for the bootstrapped classifications
    scale_color_manual(values = color.palette) +
    theme_bw() +
    ylab(paste0(response.label, ' (field: ',yString, ')')) +
    guides(color = guide_legend(title = 'Response Type:')) + 
    theme(
      axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
      , legend.position="bottom"
    )
  
  return(plot.a2e)
}