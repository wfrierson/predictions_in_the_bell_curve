###
# The following code is launched by executing "source('200 Code/TBC_Bootstrap.R', encoding = 'UTF-8', echo=FALSE)", assuming a working directory with a "200 Code" folder.
# When launched, 
#     - each of the 24 TBC logistic regression models is reproduced
#     - run through a bootstrapping method
#     - evaluated via Matthew's Correlation Coefficient
#     - visualized
#     - saved
#     - the memory is cleared, and then
#     - the R session is restarted to ensure each model is built from scratch
#
# NOTE 1: THIS CODE IS INTENTIONALLY RECURSIVE. It's stop condition checks if there are no remaining TBC models (of the 24) to analyze, as logged by a text file in the folder to save the model output.
#
# NOTE 2: When 10K bootstrap iterations are used on all 24 models, the entire recursive batch process:
#     - Produces 941 files in the "100 Data" folder, 6.76 GBs in total
#     - Takes 4 - 5 hours
###

## Source core functionality and batch parameters
# All custom functions come from here:
source('200 Code/000_Misc.R')
source('200 Code/100_Resampling.R')
source('200 Code/200_Classification.R')
source('200 Code/300_Visualizations.R')

# The batch parameters to reproduce TBC models are stored in the tbc.models data.table built here:
source('200 Code/400_Model_Definitions.R')

# save location
save.folder <- '100 Data'

# log file location
log.file <- paste0(save.folder, '/log.txt')

# Use log to track complete batch runs
if(!file.exists(log.file)) {
  write('', file = log.file)
  current.model <- tbc.models[1L]
} else {
  completed.models <- read.table(file = log.file, blank.lines.skip = TRUE, stringsAsFactors = FALSE)$V1
  remaining.models <- tbc.models[!(label %in% completed.models)]
  
  if(nrow(remaining.models) == 0) {
    stop('No models remaining')
  }
  
  current.model <- remaining.models[1L]
}

# Load relevant parameters to run bootstrap analysis
label.model <- current.model$label
filter.model.training <- current.model$filter.training
filter.model.holdout <- current.model$filter.holdout
factors.model <- current.model$factors
formula.model <- as.formula(current.model$formula)
response.label.model <- current.model$target.description
bootIterations <- 1e4
parallel.ind <- TRUE
numCores <- 8

# Import data headers
# Note: Some particular settings are needed given how Rasmusen created this file
DT.headers <- data.table::fread('http://www.rasmusen.org/xpacioli/bellcurve/nation.hdr', sep = '\t', stringsAsFactors = FALSE, header = FALSE, skip = 4, fill = TRUE, select = 1:2)

# Ensure headers are all in one column
DT.headers[, header := ifelse(V1 == '', V2, V1)]

# Import the NATION.txt TBC data
DT.raw <- data.table::fread('http://www.rasmusen.org/xpacioli/bellcurve/NATION.TXT', stringsAsFactors = TRUE, header = FALSE, na.strings = '¥', col.names = DT.headers$header)

# Derive some target variables for downstream use
# Note: I derived these to reproduce certain TBC models. TBC authors may have used a different setup, but the model parameters are essentially the same.
DT.raw[, GEDvHSGr_Ind := ifelse(GEDvHSGr == 'GED', 1, 0)]
DT.raw[, LTHSvHS_Ind := ifelse(LTHSvHS == 'LTHS', 1, 0)]
DT.raw[, WedBy30_Ind := ifelse(WedBy30 == '1 Yes', 1, 0)]
DT.raw$Adult14S <- relevel(DT.raw$Adult14S, ref = 'UnmarMom')

# Create training data for bootstrapped logistic regression
DT.model <- na.omit(DT.raw[eval(parse(text = filter.model.training)), mget(eval(parse(text = factors.model)))])
DT.model.holdout <- na.omit(DT.raw[eval(parse(text = filter.model.holdout)), mget(eval(parse(text = factors.model)))])

# For convenience, get accessible strings for factors and response
# Note: tbc.models was built so that the response is always the last variable of factors.model
response <- tail(colnames(DT.model), n = 1)
response.modeled <- paste0(response,"_modeled")
factors <- head(colnames(DT.model), n = -1)

# Store mean response for use in report
response.mean <- DT.model[, mean(get(response))]

# Build a single logistic regression model with DT.model and formula.model
glm.model <- glm(
  data = DT.model
  , formula = formula.model
  , family = binomial(link = "logit")
)

# Append modeled probabilities to DT.model
DT.model[, (response.modeled) := glm.model$fitted.values]

# Score DT.model.holdout
DT.model.holdout[, (response.modeled) := predict(glm.model, newdata = DT.model.holdout, type = 'response')]

# Calculate confusion matrices and calculate predictive performance metrics for original model
DT.performance.model <- confusionMatrix.singleModel(
  DT.predictions = DT.model[, mget(c(response, response.modeled))]
  , predictionString = response.modeled
  , actualString = response
  , numCutoffs = 5e2
  , singleCutoff = NA
  , label = 'original'
)

DT.performance.model.holdout <- confusionMatrix.singleModel(
  DT.predictions = DT.model.holdout[, mget(c(response, response.modeled))]
  , predictionString = response.modeled
  , actualString = response
  , numCutoffs = 5e2
  , singleCutoff = NA
  , label = 'original'
)

# Run jackknife procedure
start_time <- Sys.time()
cat(paste0('\nStarting ', label.model, ' jackknife logistic regression...'))
cl <- snow::makeCluster(numCores, type = 'SOCK')
jackknife.model <- logit.jackknife(
  DT = DT.model
  , DT.holdout = DT.model.holdout
  , formula = formula.model
  , scoreType = 'both'
  , parallel = parallel.ind
  , cl = cl
  , keepCluster = FALSE
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

## Build confusion matrices and calculate predictive performance metrics for jackknife data
cat(paste0('\nBuilding ', label.model, ' confusion matrices for jackknife training data...'))
start_time <- Sys.time()
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.performance.jackknife.training <- confusionMatrix.multipleModels(
  DT.predictions = jackknife.model[['modeled training']]
  , actualString = 'actual'
  , numCutoffs = 5e2
  , parallel = parallel.ind
  , cl = cl
  , keepCluster = FALSE
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

cat(paste0('\nBuilding ', label.model, ' confusion matrices for jackknife holdout data...'))
start_time <- Sys.time()
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.performance.jackknife.holdout <- confusionMatrix.multipleModels(
  DT.predictions = jackknife.model[['modeled holdout']]
  , actualString = 'actual'
  , numCutoffs = 5e2
  , parallel = parallel.ind
  , cl = cl
  , keepCluster = FALSE
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

## Run bootstrap procedure
start_time <- Sys.time()
cat(paste0('\nStarting ', label.model, ' bootstrapped logistic regression...'))
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.bootstrap <- logit.bootstrap(
  DT = DT.model
  , DT.holdout = DT.model.holdout
  , iterations = bootIterations
  , formula = formula.model
  , set.seed.indicator = TRUE
  , scoreType = 'both'
  , parallel = parallel.ind
  , cl = cl
  , keepCluster = FALSE
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

## Build confusion matrices and calculate predictive performance metrics
cat(paste0('\nBuilding ', label.model, ' confusion matrices for training data...'))
start_time <- Sys.time()
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.performance.training <- confusionMatrix.multipleModels(
  DT.predictions = DT.bootstrap[['modeled training']]
  , actualString = 'actual'
  , numCutoffs = 5e2
  , parallel = parallel.ind
  , cl = cl
  , keepCluster = FALSE
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

cat(paste0('\nBuilding ', label.model, ' confusion matrices for holdout data...'))
start_time <- Sys.time()
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.performance.holdout <- confusionMatrix.multipleModels(
  DT.predictions = DT.bootstrap[['modeled holdout']]
  , actualString = 'actual'
  , numCutoffs = 5e2
  , parallel = parallel.ind
  , cl = cl
  , keepCluster = FALSE
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

# Find 'interesting' cutoffs:
# 1) Optimal cutoff via equaly-weighted ROC curve, i.e., cutoff where ROC curve is closest to the specificity = 1 & sensitivity = 1
cutoff.roc <- DT.performance.training[
  , .(
    TPR = mean(sensitivity, na.rm = TRUE)
    , cutoff = mean(cutoff)
  )
  , keyby = .(FPR = floor(-specificity*200)/200 + 1)
][
  ,.(
    FPR
    , TPR
    , cutoff
  )
][
  , .(
    cutoff
    , distFromCorner = sqrt((TPR - 1)^2 + (FPR)^2)
  )
][order(distFromCorner)][1L, cutoff]

# 2) Cutoff implied by modeled probability when zAFQT89 = -2, i.e., 2 standard deviations below the mean
cutoff.2stdev <- DT.model[
  , .(
    prob.model = mean(get(response.modeled), na.rm = TRUE)
  )
  , by = .(zAFQT89_banded = floor(zAFQT89 * 100) / 100)
][
  , .(
    zAFQT89_banded
    , zero = abs(2 + zAFQT89_banded)
    , prob.model
  )
][order(zero)][1L, prob.model]

# 3) Cutoff where the mean of bootstrapped Matthew's Correlation coefficient is largest within DT.performance
DT.mcc.training <- DT.performance.training[
  , .(
    mcc = mean(mcc, na.rm = TRUE)
  )
  , keyby = .(cutoff = floor(cutoff*100)/100)
][order(-mcc)][1L]

cutoff.mcc <- DT.mcc.training[, cutoff]
mcc.optimal.training <- DT.mcc.training[, mcc]
mcc.optimal.holdout <- DT.performance.holdout[
  , .(
    mcc = mean(mcc, na.rm = TRUE)
  )
  , keyby = .(cutoff = floor(cutoff*100)/100)
][order(-mcc)][1L, mcc]

# Organize cutoffs
cutoffs <- c('cutoff.roc' = cutoff.roc, 'cutoff.2stdev' = cutoff.2stdev, 'cutoff.mcc' = cutoff.mcc)

## Build visualizations
cat(paste0('\nBuilding ', label.model, ' visualizations...'))
plots.a2e <- lapply(factors, function(x.label)
  plotFrequencyVsProbability(
    DT.factors = DT.model
    , xString = x.label
    , yString = response
    , bandingPrecision = 10
    , response.label = response.label.model
  )
)
names(plots.a2e) <- factors

# Build histograms of MCC estimates for selected cutoffs
adj.const <- c(-0.005, 0.005)
plots.hist.cutoffs.training <- lapply(cutoffs, function(cutoff.select)
  plotPerformanceMetric.hist(
    DT.performance = DT.performance.training[cutoff %between% (adj.const + cutoff.select)]
    , performanceMetric = 'mcc'
    , bins = 100
  )
)
names(plots.hist.cutoffs.training) <- names(cutoffs)

plots.hist.cutoffs.holdout <- lapply(cutoffs, function(cutoff.select)
  plotPerformanceMetric.hist(
    DT.performance = DT.performance.holdout[cutoff %between% (adj.const + cutoff.select)]
    , performanceMetric = 'mcc'
    , bins = 100
  )
)
names(plots.hist.cutoffs.holdout) <- names(cutoffs)

# Build plots of bootstrapped distribution of MCC estimates
plot.dist.mcc.training <- plotPerformanceMetric.cutoff.mean(DT.performance.training, performanceMetric = 'mcc', bins = 100)
plot.dist.mcc.holdout <- plotPerformanceMetric.cutoff.mean(DT.performance.holdout, performanceMetric = 'mcc', bins = 100)

# Build plots of MCC distributions with accelerated bias-corrected bootstrapped confidence intervals
plot.dist.mcc.training.bootCI <- plotPerformanceMetric.cutoff.bootCI(
  DT.performance.boot = DT.performance.training
  , DT.performance.original = DT.performance.model
  , DT.performance.jackknife = DT.performance.jackknife.training
  , performanceMetric = 'mcc'
  , bins = 100
  , alpha = 0.05
)

plot.dist.mcc.holdout.bootCI <- plotPerformanceMetric.cutoff.bootCI(
  DT.performance.boot = DT.performance.holdout
  , DT.performance.original = DT.performance.model.holdout
  , DT.performance.jackknife = DT.performance.jackknife.holdout
  , performanceMetric = 'mcc'
  , bins = 100
  , alpha = 0.05
)

# Build plots for ROC curves
plot.roc.training <- plotROC(DT.performance = DT.performance.training)
plot.roc.holdout <- plotROC(DT.performance = DT.performance.holdout)

# Build plots comparing average response and average bootstrapped classifications for selected cutoffs
plots.class.cutoffs.training <- lapply(cutoffs, function(cutoff)
  plotClassificationCI(
    DT.factors = DT.model
    , DT.predictions = DT.bootstrap[['modeled training']]
    , xString = 'zAFQT89'
    , yString = response
    , cutoff = cutoff
    , bandingPrecision = 10
    , alpha = 0.05
    , response.label = response.label.model
  )
)
names(plots.class.cutoffs.training) <- names(cutoffs)

plots.class.cutoffs.holdout <- lapply(cutoffs, function(cutoff)
  plotClassificationCI(
    DT.factors = DT.model.holdout
    , DT.predictions = DT.bootstrap[['modeled holdout']]
    , xString = 'zAFQT89'
    , yString = response
    , cutoff = cutoff
    , bandingPrecision = 10
    , alpha = 0.05
    , response.label = response.label.model
  )
)
names(plots.class.cutoffs.holdout) <- names(cutoffs)

# Add multiclassfication
plots.classFormat.mcc.training <- plotMultipleClassifications(
   DT.factors = DT.model
   , DT.predictions = DT.bootstrap[['modeled training']]
   , DT.performance = DT.performance.training
   , xString = 'zAFQT89'
   , yString = response
   , cutoffs = cutoffs['cutoff.mcc']
   , performanceMetric = 'mcc'
   , bandingPrecision = 10
   , response.label = response.label.model
)

plots.classFormat.training <- plotMultipleClassifications(
  DT.factors = DT.model
  , DT.predictions = DT.bootstrap[['modeled training']]
  , DT.performance = DT.performance.training
  , xString = 'zAFQT89'
  , yString = response
  , cutoffs = cutoffs
  , performanceMetric = 'mcc'
  , bandingPrecision = 10
  , response.label = response.label.model
)

plots.classFormat.mcc.holdout <- plotMultipleClassifications(
  DT.factors = DT.model.holdout
  , DT.predictions = DT.bootstrap[['modeled holdout']]
  , DT.performance = DT.performance.holdout
  , xString = 'zAFQT89'
  , yString = response
  , cutoffs = cutoffs['cutoff.mcc']
  , performanceMetric = 'mcc'
  , bandingPrecision = 10
  , response.label = response.label.model
)

plots.classFormat.holdout <- plotMultipleClassifications(
  DT.factors = DT.model.holdout
  , DT.predictions = DT.bootstrap[['modeled holdout']]
  , DT.performance = DT.performance.holdout
  , xString = 'zAFQT89'
  , yString = response
  , cutoffs = cutoffs
  , performanceMetric = 'mcc'
  , bandingPrecision = 10
  , response.label = response.label.model
)

# Package some stuff to save
inputs <- list('model parameters' = current.model, 'bootIterations' =  bootIterations, 'parallel.ind' = parallel.ind, 'numCores' = numCores, 'DT.model' = DT.model)
outputs <- list('response.mean' = response.mean, 'mcc.optimal.training' = mcc.optimal.training, 'mcc.optimal.holdout' = mcc.optimal.holdout, 'cutoffs' = cutoffs, 'glm.model' = glm.model)

cat('\nSaving...')
save.list <- list(
  'inputs'
  , 'outputs'
  , 'DT.bootstrap'
  , 'DT.performance.training'
  , 'DT.performance.holdout'
  , 'DT.performance.model'
  , 'DT.performance.model.holdout'
  , 'DT.performance.jackknife.training'
  , 'DT.performance.jackknife.holdout'
  , 'jackknife.model'
)

# Save RDS files
lapply(save.list, function(thing) {
  save.path <- paste0(save.folder,'/',label.model,'_',thing,'.rds')
  saveRDS(object = get(thing), file = save.path)
})

# Plots to save
save.listPlots <- list(
  'plots.a2e'
  ,'plots.hist.cutoffs.training'
  ,'plots.hist.cutoffs.holdout'
  ,'plots.class.cutoffs.training'
  ,'plots.class.cutoffs.holdout'
  ,'plots.classFormat.mcc.training'
  ,'plots.classFormat.training'
  ,'plots.classFormat.mcc.holdout'
  ,'plots.classFormat.holdout'
)
save.plots <- list(
  'plot.dist.mcc.training'
  ,'plot.dist.mcc.holdout'
  ,'plot.roc.training'
  ,'plot.roc.holdout'
  ,'plot.dist.mcc.training.bootCI'
  ,'plot.dist.mcc.holdout.bootCI'
)

# Save individual plots
lapply(save.plots, function(plot.toSave) {
  save.path <- paste0(save.folder,'/',label.model,'_',plot.toSave,'.svg')
  ggsave(filename = save.path, plot = get(plot.toSave), width = 18, height = 8)
})

# Save lists of plots
lapply(save.listPlots, function(listPlots.toSave) {
  plot.names <- names(get(listPlots.toSave))
  lapply(seq_along(get(listPlots.toSave)), function(plot.ind) {
    plot.name <- plot.names[plot.ind]
    save.path <- paste0(save.folder,'/',label.model,'_',listPlots.toSave,'_',plot.name,'.svg')
    ggsave(filename = save.path, plot = get(listPlots.toSave)[[plot.ind]], width = 18, height = 8)
  })
})

# Update log file
write(label.model, file = log.file, append = TRUE)

# Clear workspace
rm(list=ls())

# Refresh session & relaunch this script
rstudioapi::restartSession(command = "source('200 Code/TBC_Bootstrap.R', encoding = 'UTF-8', echo=FALSE)")