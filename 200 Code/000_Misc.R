###
# This code attaches needed packages and defines a convenience function to report timing
###

library(data.table)     # Used to store and transform large datasets efficiently
library(ggplot2)        # Used to create visualizations
library(cowplot)        # Used to arrange ggplot2 visualizations
library(snow)           # Used to certain methods in parallel. Note: I know snow is antiquated, but it's the only parallel processing package that has worked for me!
library(boot)           # Used to compare optimal MCC values
library(plotly)         # Used to interact with visualizations
library(knitr)          # Used to produce the markdown document
library(stargazer)      # Used to format logistic regression output
library(kableExtra)     # Used to produce nicer looking tables in markdown
library(RColorBrewer)   # Used to give more interesting color pallete in report

## Misc
displayRuntime <- function(start_time, end_time) {
  cat(paste0('\nRuntime: ', floor(difftime(end_time, start_time, units = 'secs') * 10) / 10, 's\n'))
}

calculateCoefficientSummary <- function(DT.coef) {
  coef.names <- grep('estimate_', colnames(DT.coef), value = TRUE)
  DT.coef.summary <- DT.coef[
    , unlist(lapply(.SD
                    , function(col) list(
                      mean = mean(col, na.rm = TRUE)
                      , sd =  sd(col, na.rm = TRUE)
                    )
    ))
    , .SDcols = coef.names
  ]
  
  # Reorder coef.bootstrap to have means first and SDs second
  DT.coef.summary <- DT.coef.summary[
    c(
      # Means
      seq(1, length(coef.names) * 2, 2)
      
      # Standard deviations
      , seq(2, length(coef.names) * 2, 2)
    )
  ]
  
  return(DT.coef.summary)
}