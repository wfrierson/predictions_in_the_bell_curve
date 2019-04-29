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
# NOTE 1: THIS CODE IS INTENTIONALLY RECURSIVE. It's stopping condition checks if there are no remaining TBC models (of the 24) to analyze, as logged by a text file in the folder to save the model output.
#
# NOTE 2: When 10K bootstrap iterations are used on all 24 models, the entire recursive batch process:
#     - Produces 193 files in the "100 Data" folder, 6.56 MBs in total
#     - Takes 4 hours
###

################################################################
# Source core functionality and batch parameters
################################################################
# All custom functions come from here:
source('200 Code/000_Misc.R')
source('200 Code/100_Resampling.R')
source('200 Code/200_Classification.R')
source('200 Code/300_Visualizations.R')

# The batch parameters to reproduce TBC models are stored in the tbc.models data.table built here:
source('200 Code/400_Model_Definitions.R')

################################################################
# Initialize Current Model
################################################################
# save location
save.folder <- '100 Data'

# log file location
log.file <- paste0(save.folder, '/log.txt')

# Use log to track complete batch runs
if(!file.exists(log.file)) {
  write('', file = log.file)
  current.model <- tbc.models[1L]
} else {
  completed.models <- read.table(file = log.file, blank.lines.skip = TRUE, stringsAsFactors = FALSE, sep = ',')$V1
  remaining.models <- tbc.models[!(label %in% completed.models)]
  
  if(nrow(remaining.models) == 0) {
    stop('No models remaining')
  }
  
  current.model <- remaining.models[1L]
}

bootIterations <- 1e4
parallel.ind <- TRUE
numCores <- 8

################################################################
# Import data
################################################################
# Import data headers
# Note: Some particular settings are needed given how the authors created this file
DT.headers <- data.table::fread(
  'http://www.rasmusen.org/xpacioli/bellcurve/nation.hdr'
  , sep = '\t'
  , stringsAsFactors = FALSE
  , header = FALSE
  , skip = 4
  , fill = TRUE
  , select = 1:2
)

# Ensure headers are all in one column
DT.headers[, header := ifelse(V1 == '', V2, V1)]

# Import the NATION.txt TBC data
DT.raw <- data.table::fread(
  'http://www.rasmusen.org/xpacioli/bellcurve/NATION.TXT'
  , stringsAsFactors = TRUE
  , header = FALSE
  # This file (oddly) uses ¥ as a NULL value...
  , na.strings = '¥'
  , col.names = DT.headers$header
)

################################################################
# Derive some target variables for downstream use
################################################################
# Note: I derived these to reproduce certain TBC models. TBC authors may have used a different setup, but the model parameters are essentially the same.
DT.raw[
  , `:=` (
    GEDvHSGr_Ind = ifelse(GEDvHSGr == 'GED', 1, 0)
    , LTHSvHS_Ind = ifelse(LTHSvHS == 'LTHS', 1, 0)
    , WedBy30_Ind = ifelse(WedBy30 == '1 Yes', 1, 0)
  )
]
DT.raw$Adult14S <- relevel(DT.raw$Adult14S, ref = 'UnmarMom')

################################################################
# Create training and holdout data.tables
################################################################
# Derive some target variables for downstream use
# Create training data for bootstrapped logistic regression by evaluating text
DT.model.training <- na.omit(DT.raw[
  eval(parse(text = current.model[['filter.training']]))
  , mget(
    c('ID', eval(parse(text = current.model[['factors']])))
  )
])

DT.model.holdout <- na.omit(DT.raw[
  eval(parse(text = current.model[['filter.holdout']]))
  , mget(
    c('ID', eval(parse(text = current.model[['factors']])))
  )
])

################################################################
# Store current model attributes for downstream labeling
################################################################
# For convenience, get accessible strings for factors and response
# Note: tbc.models was built so that the response is always the last variable of current.model[['factors']]
factors <- head(colnames(DT.model.training), n = -1)[-1]

# Store mean response for use in report
response.mean.training <- DT.model.training[
  , mean(
    get(current.model[['response']])
  )
]
response.mean.holdout <- DT.model.holdout[
  , mean(
    get(current.model[['response']])
  )
]

start_time.initial <- Sys.time()
################################################################
# Build logistic regression model using original sample
################################################################
# Build a single logistic regression model with DT.model.training and current.model[['formula']]
glm.model <- glm(
  data = DT.model.training
  , formula = current.model[['formula']][[1]]
  , family = binomial(link = "logit")
  , model = FALSE
  , y = FALSE
)
glm.model$data <- NULL

# Append modeled probabilities and linear predictors to DT.model.training
DT.model.training[, (current.model[['response.modeled']]) := glm.model[['fitted.values']]]
DT.model.training[, (current.model[['response.linpred']]) := glm.model[['linear.predictors']]]

# Score DT.model.holdout
DT.model.holdout[, (current.model[['response.modeled']]) := predict(glm.model, newdata = DT.model.holdout, type = 'response')]
DT.model.holdout[, (current.model[['response.linpred']]) := predict(glm.model, newdata = DT.model.holdout, type = 'link')]

################################################################
# Calculate confusion matrices and calculate predictive 
# performance metrics for original sample
################################################################
DT.mcc.model.training <- calculateMCC.boot(
  DT.probabilities =  DT.model.training[
                        , mget(
                          c(current.model[['response']], current.model[['response.modeled']])
                        )
                      ]
  , probabilityString = current.model[['response.modeled']]
  , actualString = current.model[['response']]
  , bootLabelString = ''
  , numCutoffs = 5e2
  , singleCutoff = NA
)

DT.mcc.model.holdout <- calculateMCC.boot(
  DT.probabilities =  DT.model.holdout[
    , mget(
      c(current.model[['response']], current.model[['response.modeled']])
    )
    ]
  , probabilityString = current.model[['response.modeled']]
  , actualString = current.model[['response']]
  , bootLabelString = ''
  , numCutoffs = 5e2
)

################################################################
# Run jackknife procedure
################################################################
start_time <- Sys.time()
cat(paste0('\nStarting ', current.model[['label']], ' jackknife logistic regression...'))
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.jackknife <- logit.jackknife(
  DT = DT.model.training
  , DT.holdout = DT.model.holdout
  , key = 'ID'
  , formula = current.model[['formula']][[1]]
  , parallel = parallel.ind
  , cl = cl
  , keepCluster = FALSE
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

################################################################
# Build confusion matrices and calculate predictive 
# performance metrics for jackknife data
################################################################
cat(paste0('\nCalculating ', current.model[['label']], ' MCC for jackknife output (jackknife training samples)...'))
start_time <- Sys.time()
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.mcc.jackknife.training <- calculateMCC.boot.parallel(
  DT.probabilities = DT.jackknife[['modeled jackknife training']]
  , probabilityString = 'modeled_probability'
  , actualString = 'actual'
  , bootLabelString = 'bootIteration'
  , numCutoffs = 5e2
  , cl = cl
  , numCores = numCores
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

################################################################
## Run bootstrap procedure
################################################################
start_time <- Sys.time()
cat(paste0('\nStarting ', current.model[['label']], ' bootstrapped logistic regression...'))
cl <- snow::makeCluster(numCores, type = 'SOCK')
clusterSetupRNG(cl, seed = 123)
DT.bootstrap <- logit.bootstrap(
  DT = DT.model.training
  , DT.holdout = DT.model.holdout
  , key = 'ID'
  , iterations = bootIterations
  , formula = current.model[['formula']][[1]]
  , parallel = parallel.ind
  , cl = cl
  , keepCluster = FALSE
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

################################################################
# Calculate average and standard deviation of bootstrapped 
# coefficients
################################################################
coef.bootstrap <- calculateCoefficientSummary(DT.coef = DT.bootstrap[['coefficients']])
names(coef.bootstrap) <- paste0(names(coef.bootstrap),'.bootstrap')

coef.jackknife <- calculateCoefficientSummary(DT.coef = DT.jackknife[['coefficients']])
names(coef.jackknife) <- paste0(names(coef.jackknife),'.jackknife')

################################################################
# Build confusion matrices and calculate predictive 
# performance metrics for bootstrapped data
################################################################
cat(paste0('\nCalculating ', current.model[['label']], ' MCC for bootstrapped output (training)...'))
start_time <- Sys.time()
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.mcc.bootstrap.modeled.training <- calculateMCC.boot.parallel(
  DT.probabilities = DT.bootstrap[['modeled training']]
  , probabilityString = 'modeled_probability'
  , actualString = 'actual'
  , bootLabelString = 'bootIteration'
  , numCutoffs = 5e2
  , cl = cl
  , numCores = numCores
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

cat(paste0('\nCalculating ', current.model[['label']], ' MCC for bootstrapped output (holdout)...'))
start_time <- Sys.time()
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.mcc.bootstrap.modeled.holdout <- calculateMCC.boot.parallel(
  DT.probabilities = DT.bootstrap[['modeled holdout']]
  , probabilityString = 'modeled_probability'
  , actualString = 'actual'
  , bootLabelString = 'bootIteration'
  , numCutoffs = 5e2
  , cl = cl
  , numCores = numCores
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

cat(paste0('\nCalculating ', current.model[['label']], ' MCC for bootstrapped output (bootstrapped training samples)...'))
start_time <- Sys.time()
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.mcc.bootstrap.training <- calculateMCC.boot.parallel(
  DT.probabilities = DT.bootstrap[['modeled bootstrap training']]
  , probabilityString = 'modeled_probability'
  , actualString = 'actual'
  , bootLabelString = 'bootIteration'
  , numCutoffs = 5e2
  , cl = cl
  , numCores = numCores
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

cat(paste0('\nCalculating ', current.model[['label']], ' MCC for bootstrapped output (bootstrapped holdout samples)...'))
start_time <- Sys.time()
cl <- snow::makeCluster(numCores, type = 'SOCK')
DT.mcc.bootstrap.holdout <- calculateMCC.boot.parallel(
  DT.probabilities = DT.bootstrap[['modeled bootstrap holdout']]
  , probabilityString = 'modeled_probability'
  , actualString = 'actual'
  , bootLabelString = 'bootIteration'
  , numCutoffs = 5e2
  , cl = cl
  , numCores = numCores
)
end_time <- Sys.time()
displayRuntime(start_time, end_time)

################################################################
# Find cutoff where the mean of bootstrapped Matthew's Correlation coefficient 
# is largest within DT.mcc.bootCI.training
################################################################
DT.mcc.bootCI.training <- calculateConfidenceIntervals.BCa(
  DT.boot.sample = DT.mcc.bootstrap.training
  , DT.boot.original = DT.mcc.bootstrap.modeled.training
  , DT.jackknife = DT.mcc.jackknife.training
  , DT.apparent = DT.mcc.model.training
  , aggregateName = 'cutoff'
  , metricName = 'mcc'
  , bootLabelString = 'bootIteration'
  , alpha = 0.05
)

cutoff.mcc <- DT.mcc.bootCI.training[order(-mcc.bootstrap)][1L, cutoff]

DT.mcc.bootCI.holdout <- calculateConfidenceIntervals.bootPercentile(
  DT.boot.sample = DT.mcc.bootstrap.holdout
  , DT.boot.original = DT.mcc.bootstrap.modeled.holdout
  , DT.apparent = DT.mcc.model.holdout
  , aggregateName = 'cutoff'
  , metricName = 'mcc'
  , bootLabelString = 'bootIteration'
  , alpha = 0.05
)
################################################################
# Calculate tables of observed response frequency, average 
# of bootstrapped modeled probability, and BCa 95% confidence 
# intervals
################################################################
DTs.probability.bootCI.training <- lapply(factors, function(x.label) 
  calculateConfidenceIntervals.probability(
    DT.apparent = DT.model.training
    , DT.probabilities.boot.sample = DT.bootstrap[['modeled bootstrap training']]
    , DT.probabilities.boot.original = DT.bootstrap[['modeled training']]
    , DT.probabilities.jackknife = DT.jackknife[['modeled jackknife training']]
    , xString = x.label
    , yString = current.model[['response']]
    , key = 'ID'
    , alpha = 0.05
    , bandingPrecision = 10
    , methodCI = 'BCa'
  )
)
names(DTs.probability.bootCI.training) <- factors

DTs.probability.bootCI.holdout <- lapply(factors, function(x.label) 
  calculateConfidenceIntervals.probability(
    DT.apparent = DT.model.holdout
    , DT.probabilities.boot.sample = DT.bootstrap[['modeled bootstrap holdout']]
    , DT.probabilities.boot.original = DT.bootstrap[['modeled holdout']]
    , xString = x.label
    , yString = current.model[['response']]
    , key = 'ID'
    , bandingPrecision = 10
    , methodCI = 'bootPercentile'
  )
)
names(DTs.probability.bootCI.holdout) <- factors

################################################################
# Calculate tables of observed response frequency, average 
# of bootstrapped modeled probability, and BCa 95% confidence 
# intervals
################################################################
DTs.mcc.classification.bootCI.training <- lapply(factors, function(x.label) 
  calculateConfidenceIntervals.classification.fromProbabilities(
    DT.apparent = DT.model.training
    , DT.probabilities.boot.sample = DT.bootstrap[['modeled bootstrap training']]
    , DT.probabilities.boot.original = DT.bootstrap[['modeled training']]
    , DT.probabilities.jackknife = DT.jackknife[['modeled jackknife training']]
    , xString = x.label
    , yString = current.model[['response']]
    , cutoff = cutoff.mcc
    , key = 'ID'
    , bandingPrecision = 10
    , alpha = 0.05
    , methodCI = 'BCa'
  )
)
names(DTs.mcc.classification.bootCI.training) <- factors

DTs.mcc.classification.bootCI.holdout <- lapply(factors, function(x.label) 
  calculateConfidenceIntervals.classification.fromProbabilities(
    DT.apparent = DT.model.holdout
    , DT.probabilities.boot.sample = DT.bootstrap[['modeled bootstrap holdout']]
    , DT.probabilities.boot.original = DT.bootstrap[['modeled holdout']]
    , xString = x.label
    , yString = current.model[['response']]
    , cutoff = cutoff.mcc
    , key = 'ID'
    , bandingPrecision = 10
    , alpha = 0.05
    , methodCI = 'bootPercentile'
  )
)
names(DTs.mcc.classification.bootCI.holdout) <- factors

################################################################
# Calculate optimal MCC value and confidence interval
# using the boot package as a reference
################################################################
optimal.mcc.boot.training.bootPackage <- boot(
  data = DT.model.training
  , statistic = calculateMCC.bootPackage
  , R = bootIterations
  , formula = current.model[['formula']][[1]]
  , cutoff = cutoff.mcc
)

optimal.mcc.bootCI.training.bootPackage <- boot.ci(optimal.mcc.boot.training.bootPackage, type = 'bca')

DT.optimal.mcc.bootCI.training.bootPackage <- data.table(
  optimal.mcc.bootPackage = optimal.mcc.bootCI.training.bootPackage$t0
  , lower.mcc.bootPackage = as.numeric(optimal.mcc.bootCI.training.bootPackage$bca[,4])
  , upper.mcc.bootPackage = as.numeric(optimal.mcc.bootCI.training.bootPackage$bca[,5])
)

################################################################
# Save data
################################################################
cat('\nSaving...')

# Package some stuff to save
inputs <- list(
  'model_parameters' = current.model
  , 'bootIterations' =  bootIterations
  , 'parallel.ind' = parallel.ind
  , 'numCores' = numCores
  , 'DT.model.training' = DT.model.training
  , 'DT.model.holdout' = DT.model.holdout
)

# Rename fields of DT.mcc.bootCI.* for clarity when they are later imported for the report via the outputs data.table
names.DT.mcc.bootCI <- c('cutoff', 'mcc', 'mcc.bootstrap', 'lower', 'upper', 'mcc.bias', 'mcc.optimism', 'mcc.optimism.corrected')
names.DT.mcc.bootCI.training <- c('cutoff.mcc.training', 'mcc.apparent.training', 'mcc.bootstrap.training', 'mcc.bootstrap.lower.BCa.training', 'mcc.bootstrap.upper.BCa.training', 'mcc.bias.training', 'mcc.optimism.training', 'mcc.optimism.corrected.training')
names.DT.mcc.bootCI.holdout <- c('cutoff.mcc.holdout', 'mcc.apparent.holdout', 'mcc.bootstrap.holdout', 'mcc.bootstrap.lower.bootPercentile.holdout', 'mcc.bootstrap.upper.bootPercentile.holdout', 'mcc.bias.holdout', 'mcc.optimism.holdout', 'mcc.optimism.corrected.holdout')
setnames(
  DT.mcc.bootCI.training
  , names.DT.mcc.bootCI
  , names.DT.mcc.bootCI.training
)
setnames(
  DT.mcc.bootCI.holdout
  , names.DT.mcc.bootCI
  , names.DT.mcc.bootCI.holdout
)

# Store various model outputs as a data.table
# In the report, these can be easily combined via data.table::rbindlist(list(.))
outputs <- data.table(
  current.model
  , nobs.training = nrow(DT.model.training)
  , nobs.holdout = nrow(DT.model.holdout)
  , response.mean.training = response.mean.training
  , response.mean.holdout = response.mean.holdout
  , t(coef.bootstrap)
  , t(coef.jackknife)
  # Store rows of DT.mcc.bootCI.* with largest values of mcc.bootstrap.* 
  # These will be the reported MCC values in the report
  , DT.mcc.bootCI.training[order(-mcc.bootstrap.training)][1L]
  , DT.mcc.bootCI.holdout[order(-mcc.bootstrap.holdout)][1L]
  , DT.optimal.mcc.bootCI.training.bootPackage
  , glm.model = list(list(glm.model))
)

# Change DT.mcc.bootCI.* names back to plot these with more readable function calls
setnames(
  DT.mcc.bootCI.training
  , names.DT.mcc.bootCI.training
  , names.DT.mcc.bootCI
)
setnames(
  DT.mcc.bootCI.holdout
  , names.DT.mcc.bootCI.holdout
  , names.DT.mcc.bootCI
)

save.list <- list(
  'inputs'
  , 'outputs'
  , 'DTs.probability.bootCI.training'
  , 'DTs.probability.bootCI.holdout'
  , 'DTs.mcc.classification.bootCI.training'
  , 'DTs.mcc.classification.bootCI.holdout'
  , 'DT.mcc.bootCI.training'
  , 'DT.mcc.bootCI.holdout'
)

# Save RDS files
lapply(save.list, function(thing) {
  save.path <- paste0(save.folder,'/',current.model[['label']],'_',thing,'.rds')
  saveRDS(object = get(thing), file = save.path)
})

end_time.final <- Sys.time()
cat(paste0('\nTotal runtime for ', current.model[['label']], 'model'))
displayRuntime(start_time.initial, end_time.final)

################################################################
# Finish current model and launch next batch run
################################################################
total_runtime <- floor(difftime(end_time.final, start_time.initial, units = 'secs') * 10) / 10

# Update log file
write(paste0(as.character(current.model[['label']]),', total runtime = ', total_runtime), file = log.file, append = TRUE)

# Clear workspace
rm(list=ls())

# Refresh session & relaunch this script
rstudioapi::restartSession(command = "source('200 Code/TBC_Bootstrap.R', encoding = 'UTF-8', echo=FALSE)")