###
# This code attaches needed packages and defines a convenience function to report timing
###

library(data.table)     # Used to store and transform large datasets
library(ggplot2)        # Used to create visualizations
library(plotly)         # Used to make ggplot2 visualizations interactive
library(snow)           # Used to certain methods in parallel. Note: I know snow is antiquated, but it's the only parallel processing package that has worked for me!

## Misc
displayRuntime <- function(start_time, end_time) {
  cat(paste0('\nRuntime: ', floor(difftime(end_time, start_time, units = 'secs') * 10) / 10, 's\n'))
}