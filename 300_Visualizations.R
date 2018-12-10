###
# The following custom functions create various visualizations to understand the predictive performance of binary classifications
###

# Average the response (via yString) and the predicted probabilities over a discretized version of the input xString (via bandingPrecision)
# This function assumes that DT has already been scored with the original, non-bootstrapped logistic regression model, i.e., response.modeled exists
plotFrequencyVsProbability <- function(DT.factors, xString, yString, response.label, bandingPrecision = 10) {
  # Make explicit copy to avoid writing by reference to original object
  # This assumes nrow(DT.factors) is reasonably small, e.g., few thousand rows
  DT.factors <- copy(DT.factors)
  
  xString_banded <- paste0(xString, '_banded')
  DT.factors[, (xString_banded) := factor(floor(get(xString) * bandingPrecision) / bandingPrecision)]
  
  DT.plot <- DT.factors[
    , .(
      mean(get(yString), na.rm = TRUE)
      , mean(get(paste0(yString,"_modeled")), na.rm = TRUE)
    )
    , by = xString_banded
  ]
  names.plot <- c('Observed', 'Probability Model')
  setnames(DT.plot, 2:3, names.plot)
  
  response.type <- 'Response_Type'
  DT.melt <- melt(
    DT.plot
    , id.vars = xString_banded
    , measure.vars = names.plot
    , value.name = 'response'
    , variable.name = response.type
  )
  
  p <- ggplot(
    DT.melt[get(response.type) != 'Observed']
    , aes_string(x = xString_banded, y = 'response', color = response.type, group = response.type)
  ) + 
    geom_line() + 
    geom_point(data = DT.melt[get(response.type) == 'Observed'], aes_string(x = xString_banded, y = 'response', group = response.type)) + 
    scale_color_manual(values = c('black', '#00BFC4')) +
    theme_bw() +
    guides(linetype = 'none') +
    ylab(paste0(response.label, ' (field: ',yString, ')')) +
    theme(
      axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
    )
  
  return(p)
}

# Plot distribution of given performance metric
plotPerformanceMetric.hist <- function(DT.performance, performanceMetric = 'mcc', bins = 100) {
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
    )
  
  return(p)
}

# Plot bootstrapped distribution of performanceMetric by cutoff within DT.performance and include mean of performanceMetric
plotPerformanceMetric.cutoff.mean <- function(DT.performance, performanceMetric = 'mcc', bins = 50) {
  ylabel <- switch(performanceMetric, 'mcc' = "Matthew's Correlation Coefficient", 'kappa' = "Cohen's Kappa") 
  
  p <- ggplot(
    DT.performance
    , aes_string(x = 'cutoff', y = performanceMetric)
  ) + 
    stat_bin_hex(bins = bins) + 
    geom_line(
      data = DT.performance[, .(performanceMetric.mean = mean(get(performanceMetric), na.rm = TRUE)), keyby = .(cutoff = floor(cutoff*100)/100)]
      , aes(x = cutoff, y = performanceMetric.mean, color = factor(paste0('mean ', performanceMetric)))
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
    )
  
  return(p)
}

plotPerformanceMetric.cutoff.bootCI <- function(DT.performance.boot, DT.performance.original, DT.performance.jackknife, performanceMetric = 'mcc', bins = 50, alpha = 0.05) {
  # The following is adapted from John Fox's primer on bootstrap regression and accelerated bias-corrected percentile intervals found here:
  # http://statweb.stanford.edu/~tibs/sta305files/FoxOnBootingRegInR.pdf
  
  ylabel <- switch(performanceMetric, 'mcc' = "Matthew's Correlation Coefficient", 'kappa' = "Cohen's Kappa") 
  
  # 0) Create string for joined performanceMetric name
  performanceMetric.join <- paste0('i.', performanceMetric)
  
  # 1) Set cutoff as key
  setkey(DT.performance.jackknife, cutoff)
  setkey(DT.performance.original, cutoff)
  
  # 2) Get jackknife mean of performanceMetric by cutoff
  DT.jackknife.means <- DT.performance.jackknife[
    , .(
      performanceMetric.mean = mean(get(performanceMetric), na.rm = TRUE)
    )
    , keyby = cutoff
  ]
  
  # 3) Calculate correction factor a, empirical skewness of performanceMetric in original data via jackknife resampling
  DT.correction.a <- DT.jackknife.means[DT.performance.jackknife][!is.na(performanceMetric.mean) & !is.na(get(performanceMetric))][
    , .(
      a = sum(get(performanceMetric) - performanceMetric.mean)^3 / (6 * sum(get(performanceMetric) - performanceMetric.mean^2)^1.5)
    )
    , keyby = cutoff
  ][!is.na(a)]
  
  # 4) Calculate correction factor z, accounting for distribution asymmetry
  # Note: get(performanceMetric.join) represents the bootstrapped performanceMetric values. 
  # get(performanceMetric) represents the sample performanceMetric values, i.e., from the original data
  DT.correction.z <- DT.performance.original[DT.performance.boot][
    !is.na(get(performanceMetric)) & !is.na(get(performanceMetric.join))
    , .(
      z.pct = sum(ifelse(get(performanceMetric.join) <= get(performanceMetric), 1, 0)) / (.N + 1)
      , counts = .N
    )
    , keyby = cutoff
  ][
    , .(
      cutoff
      , counts
      , z = qnorm(z.pct)
    )
  ]
  
  # 5) Combine correction factors a and z to calculate bootstrap indices for lower and upper bounds of confidence interval
  DT.bounds <- DT.correction.z[
    DT.correction.a
    , .(
      cutoff, z, a, counts
      , z.crit = qnorm(1 - alpha/2)
    )
  ][
    , .(
      cutoff, z, a, counts
      , z.a1 = z + (z - z.crit) / (1 - a * (z - z.crit))
      , z.a2 = z + (z + z.crit) / (1 - a * (z + z.crit))
    )
  ][
    , .(
      cutoff, z, a, counts, z.a1, z.a2
      , a1.ind = floor(pnorm(z.a1) * counts)
      , a2.ind = floor(pnorm(z.a2) * counts)
    )
  ][ 
    , .(
      cutoff, z, a, counts, z.a1, z.a2
      , lb.ind = ifelse(a1.ind > counts, counts, ifelse(a1.ind < 1, 1, a1.ind))
      , ub.ind = ifelse(a2.ind > counts, counts, ifelse(a2.ind < 1, 1, a2.ind))
    )
  ]
  
  # 6) Get bootstrap estimates from indices in prior step
  DT.boot.bounds <- DT.bounds[DT.performance.original[DT.performance.boot][!is.na(get(performanceMetric)) & !is.na(get(performanceMetric.join))]
                              , .(
                                cutoff, lb.ind, ub.ind
                                , performanceMetric.original = get(performanceMetric)
                                , performanceMetric.boot = get(performanceMetric.join)
                              )
                              , nomatch = 0]
  setorder(DT.boot.bounds,  cutoff, performanceMetric.boot)
  
  # Select bootstrap estimates of performanceMetric using indices for each cutoff
  DT.original.bounds <- DT.boot.bounds[, .SD[unique(c(lb.ind, ub.ind))], by = cutoff]
  
  # Add a trivial field for use in reshaping DT.original.bounds
  DT.original.bounds[, bound := c('lower', 'upper') , by = cutoff]
  
  # Transform from long to wide to have fields for each bound of performanceMetric
  DT.plot.CI <- data.table::dcast(DT.original.bounds[, -c('lb.ind', 'ub.ind')], cutoff + performanceMetric.original ~ bound, value.var = 'performanceMetric.boot')
  
  # 7) Relabel columns for easier plotting
  setnames(DT.plot.CI, 'performanceMetric.original', performanceMetric)
  
  # Plot
  p <- ggplot(
    DT.performance.boot
    , aes_string(x = 'cutoff', y = performanceMetric)
  ) + 
    stat_bin_hex(bins = bins) +
    geom_line(data = DT.plot.CI, color = c('orange' = 'orange')) + 
    geom_ribbon(data = DT.plot.CI, aes_string(x = 'cutoff', ymin = 'lower', ymax = 'upper'), fill = 'orange', alpha = 0.2) + 
    # Include mean of performanceMetric by cutoff as a dashed line to help see the overall trend
    geom_line(
      data = DT.performance.boot[, .(performanceMetric.mean = mean(get(performanceMetric), na.rm = TRUE)), keyby = .(cutoff = floor(cutoff*100)/100)]
      , aes(x = cutoff, y = performanceMetric.mean)
      , color = 'orange'
      , linetype = 2
    ) +
    theme_bw() +
    ylab(ylabel) +
    theme(
      axis.text=element_text(size = 16)
      , axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
    )
  
  return(p)
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
      # , DT.boot
      , DT.mean
    ))
    , aes(x = FPR, y = TPR, alpha = alpha, group = Group, color = Group, linetype = type)
  ) + 
    geom_line() +
    geom_ribbon(data = DT.mean, aes(ymin = TPR.lb, ymax = TPR.ub), color = 'grey', alpha = 0.2) +
    guides(alpha = 'none', linetype = 'none') +
    xlim(c(-0.1, 1.1)) +
    theme_bw() +
    scale_color_manual(values = c(
      'Bootstrap models' = 'grey',
      'Reference line' = 'black',
      'Mean of bootstrap models' = 'red'
    )) +
    geom_point(data = cutoff.roc, aes(x = FPR, y = TPR, group = Group), color = 'black', size = 5, shape = 1) + 
    geom_text(data = cutoff.roc, x = -Inf, y = Inf, color = 'black', label = paste0('Optimal cutoff by maximizing sensitivity and specificity (circled) = ', floor(cutoff.roc$cutoff * 1e3)/1e3), hjust = -0.01, vjust = 1.1) + 
    xlab('False positive rate') +
    ylab('True positive rate') +
    theme(
      axis.text=element_text(size = 16)
      , axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
    )
  
  return(p)
}

# Plot predicted classification averages for a given cutoff and include bootstrapped confidence intervals via a normal approximation
plotClassificationCI <- function(DT.factors, DT.predictions, xString, yString, cutoff, bandingPrecision = 10, alpha = 0.05, response.label) {
  colnames.predictions <- copy(colnames(DT.predictions)[-1])
  colnames.classifications <- paste0(colnames.predictions, '_class')
  
  # Append DT.model with DT.predictions. Assumes order is the same!
  xyString <- c(xString, yString)
  DT <- cbind(DT.factors[, mget(xyString)], DT.predictions)
  
  # This produces a warning but I don't understand why...
  DT[, (colnames.classifications) := lapply(colnames.predictions, function(prediction) as.integer(get(prediction) > cutoff))]
  
  # Assumes field in xString is continuous and with meaingful values between [-1, +1]!
  # This is true for the factors under consideration in this analysis
  xString_banded <- paste0(xString, '_banded')
  DT[, (xString_banded) := factor(floor(get(xString) * bandingPrecision) / bandingPrecision)]
  
  # Melt classifications across banded values
  DT.melt <- melt(DT, id.vars = xString_banded, measure.vars = c(yString, colnames.classifications), variable.name = 'Response_Type', value.name = 'classification')
  
  # Note: This can eat up RAM, but preserves the response variable name. The factors could be cast to integers, but then labels are lost.
  DT.melt[Response_Type != (yString), Response_Type := factor(paste('cutoff = ',floor(cutoff*1e4)/1e4))]
  
  z <- qnorm(1 - alpha/2)
  DT.plot <- DT.melt[ 
    , .(
      classification_mean = mean(classification)
      , classification_sd = sd(classification)
      , counts = .N
    )
    , by = mget(colnames(DT.melt)[1:2])
  ][
    , `:=` (
      classification_lb = classification_mean - z * classification_sd / sqrt(counts)
      , classification_ub = classification_mean + z * classification_sd / sqrt(counts)
    )
  ]
  
  p <- ggplot(
    DT.plot
    , aes_string(
      x = xString_banded
      , y = 'classification_mean'
      , ymin = 'classification_lb'
      , ymax = 'classification_ub'
      , group = 'Response_Type'
      , color = 'Response_Type'
    )
  ) + 
    geom_ribbon(alpha = 0.2, linetype = 0) +
    geom_point() + 
    geom_line() + 
    theme_bw() +
    scale_color_manual(values = c('black', '#00BFC4')) +
    ylab(paste0(response.label, ' (field: ',yString, ')')) +
    theme(
      axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
    )
  
  return(p)
}

plotMultipleClassifications <- function(DT.factors, DT.predictions, DT.performance, xString, yString, cutoffs, performanceMetric = 'mcc', bandingPrecision = 10, response.label) {
  ### ASSUMPTIONS ###
  # 1) DT.performance comes from confusionMatrix.*
  # 2) DT.predictions comes from logit.bootstrap[['modeled *']]
  # 3) DT.predictions and DT.factors have the same row order (which should be true if the prior assumption is true)
  
  ### SETUP ###
  # Store names of bootstrapped probabilities
  colnames.predictions <- copy(colnames(DT.predictions)[-1])
  bootIterations <- length(colnames.predictions)
  
  # Append DT.factors with DT.predictions Assumes order is the same!
  xyString <- c(xString, yString)
  
  DT <- cbind(DT.factors[, mget(xyString)], DT.predictions)
  
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
  DT.performance.agg <- DT.performance[cutoff %in% cutoffs.closest, .(mcc = mean(mcc, na.rm = TRUE), kappa = mean(kappa, na.rm = TRUE)), keyby = cutoff]
  
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
  DT.melt <- melt(DT, id.vars = xString_banded, measure.vars = patterns(colnames.melt), value.name = colnames.melt)
  
  # Melt DT.melt again so that all classification and probability values are in 1 column called 'value'
  DT.melt <- melt(DT.melt, id.vars = xString_banded, measure.vars = colnames.melt, na.rm = TRUE, variable.name = 'group')
  
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
  group.plot.levels <- c('observed response', 'modeled probabilities', paste0('(cutoff = ',cutoffs, ', ', performanceMetric , ' = ', performanceMetric.levels, ')'))
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
    geom_point(data = DT.plot[group == (yString)], aes_string(x = xString_banded, y = yString_average)) +
    # Use grey to plot bootstrapped probabilities and black for the observed response points. Otherwise, use color.palette for the bootstrapped classifications
    scale_color_manual(values = color.palette) +
    theme_bw() +
    ylab(paste0(response.label, ' (field: ',yString, ')')) +
    theme(
      axis.title=element_text(size = 16)
      , legend.text=element_text(size = 16)
      , legend.title=element_text(size = 16)
    )
  
  # To show which factor levels of xString_banded have data (and so would effect the performance metric), plot bar chart of record counts using original data
  # For clarity, remove all other features for this plot
  plot.volume <- ggplot(DT.plot[group == (yString)], aes_string(x = xString_banded, y = 'counts')) + geom_bar(stat = 'identity') + theme_void()
  
  # NOTE: Commenting the following section out because Rstudio has a bug with plotly when the session is refreshed. 
  # Reverting to ggplot2 objects instead, which can later be transformed into plotly objects in a single session.
  # See here for more detalis: https://github.com/ropensci/plotly/issues/1204
  
  ## Convert to plotly object to become interactive
  # plot.a2e.plotly <- ggplotly(plot.a2e) %>% layout(legend = list(x = 0.84, y = 0.78))
  # plot.volume.plotly <- ggplotly(plot.volume) %>% layout(xaxis = list(showgrid = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE))
  
  # #Combine plots
  # p <- subplot(plot.volume.plotly, plot.a2e.plotly, nrows = 2, heights = c(0.2, 0.8), shareX = TRUE, titleY = TRUE)
  
  ## Remove new columns to leave DT.predictions unchanged
  # colnames.delte.ind <- (bootIterations+5):ncol(DT.predictions)
  # DT.predictions[, (colnames.delte.ind) := NULL]
  
  # return(p)
  
  output <- list('plot.a2e' = plot.a2e, 'plot.volume' = plot.volume)
  return(output)
}