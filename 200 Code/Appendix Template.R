```{r header{{model}}}
cat(paste0('## ', tbc.models[label == {{model}}, labelAppendix]))
```

## Poverty - Highschool

```{r appendix_model2_a2e, fig.show = 'hold'}
model <- 'poverty.hs'

# plots[['poverty']][['plots.a2e.training_zAFQT89']]
plotFrequencyVsProbability(
  DT.factors = inputs[[model]][['DT.model.training']]
  , xString = 'zAFQT89'
  , yString = outputs[label == model, response]
  , bandingPrecision = 10
  , response.label = outputs[label == model, target.descriptions.formatted]
  , output.format = 'plot_grid'
)

# plots[['poverty']][['plots.a2e.training_zSES']]
plotFrequencyVsProbability(
  DT.factors = inputs[[model]][['DT.model.training']]
  , xString = 'zSES'
  , yString = outputs[label == model, response]
  , bandingPrecision = 10
  , response.label = outputs[label == model, target.descriptions.formatted]
  , output.format = 'plot_grid'
)

# plots[['poverty']][['plots.a2e.training_zAge']]
plotFrequencyVsProbability(
  DT.factors = inputs[[model]][['DT.model.training']]
  , xString = 'zAge'
  , yString = outputs[label == model, response]
  , bandingPrecision = 10
  , response.label = outputs[label == model, target.descriptions.formatted]
  , output.format = 'plot_grid'
)

```


```{r appendix_model2_summary, results = 'asis'}
stargazer(outputs[label == (model), glm.model][[1]], type = 'html')
```

```{r appendix_model2, fig.show = 'hold'}
# model <- 'poverty.hs'
# summary(outputs[label == (model), glm.model][[1]])

# plots[[model]][['plots.a2e.training_zAFQT89']]
# plots[[model]][['plots.a2e.training_zSES']]
# plots[[model]][['plots.a2e.training_zAge']]

# cat(paste0('The optimal MCC value using training data = ',outputs[label == (model), mcc.optimal.training][[1]]))

kable(data.table(
  Dataset = c('Training', 'Holdout')
  , `Optimal MCC` = c(
    outputs[label == (model), mcc.bootstrap.training][[1]]
    , outputs[label == (model), mcc.bootstrap.holdout][[1]]
  )
)) %>% kable_styling(bootstrap_options = "striped", full_width = F)

cat(paste0('The cutoff value associated with the optimal MCC value from training data = ',outputs[label == (model), cutoff.mcc.training]))

# plots[['poverty']][['plot.dist.mcc.bootCI.training']]
plotPerformanceMetric.cutoff.bootCI(
  DT.bootCI = DT.mcc.bootCI.training[label == model]
  , performanceMetric = 'mcc'
  , performanceMetric.boot = 'mcc.bootstrap'
  , cutoff.name = 'cutoff'
  , lowerCI.name = 'lower'
  , upperCI.name = 'upper'
)

# plots[['poverty']][['plot.dist.mcc.bootCI.holdout']]
plotPerformanceMetric.cutoff.bootCI(
  DT.bootCI = DT.mcc.bootCI.holdout[label == model]
  , performanceMetric = 'mcc'
  , performanceMetric.boot = 'mcc.bootstrap'
  , cutoff.name = 'cutoff'
  , lowerCI.name = 'lower'
  , upperCI.name = 'upper'
)

# plots[['poverty']][['plot.classFormat.mcc.training']]
plotFrequencyVsClassification.bootCI(
  DT.factors = inputs[[model]][['DT.model.training']]
  , DT.classification.bootCI = DTs.mcc.classification.bootCI.training[[model]][['zAFQT89']]
  , DT.probabilities.bootCI = DTs.probability.bootCI.training[[model]][['zAFQT89']]
  , xString = 'zAFQT89'
  , yString = outputs[label == model, response]
  , cutoff = outputs[label == model, cutoff.mcc.training]
  , key = 'ID'
  , bandingPrecision = 10
  , alpha = 0.05
  , response.label = outputs[label == model, target.descriptions.formatted]
)

# plots[['poverty']][['plot.classFormat.mcc.holdout']]
plotFrequencyVsClassification.bootCI(
  DT.factors = inputs[[model]][['DT.model.holdout']]
  , DT.classification.bootCI = DTs.mcc.classification.bootCI.holdout[[model]][['zAFQT89']]
  , DT.probabilities.bootCI = DTs.probability.bootCI.holdout[[model]][['zAFQT89']]
  , xString = 'zAFQT89'
  , yString = outputs[label == model, response]
  , cutoff = outputs[label == model, cutoff.mcc.training]
  , key = 'ID'
  , bandingPrecision = 10
  , alpha = 0.05
  , response.label = outputs[label == model, target.descriptions.formatted]
)
```