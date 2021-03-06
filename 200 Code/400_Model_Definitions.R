###
# The following parameters are used to define different batch runs in reproducing logistic regression models from The Bell Curve
#    Herrnstein and Murray. (1994). The Bell Curve. New York: The Free Press. 1st paperback edition 1996. ISBN-13: 978-0-684-82429
###

# Short labels to distinguish among the various models
tbc.labels <- c(
  'poverty'
  ,'poverty.hs'
  ,'dropout'
  ,'dropout_interaction'
  ,'get_ged'
  ,'get_bachelors'
  ,'oolf4wks'
  ,'oolf4wks.hs'
  ,'oolf4wks.col'
  ,'unemployed4wks'
  ,'unemployed4wks.hs'
  ,'unemployed4wks.col'
  ,'ever_married30'
  ,'ever_married30.hs'
  ,'ever_married30.col'
  ,'divorced_in5yrs'
  ,'divorced_in5yrs.hs'
  ,'divorced_in5yrs.col'
  ,'divorced_in5yrs_parents'
  ,'surveyed_in_jail'
  ,'surveyed_in_jail.hs'
  ,'middle_class_values'
  ,'middle_class_values.hs'
  ,'middle_class_values.col'
)

# Lightly formatted labels to be used in automated appendix
tbc.labelsAppendix <- c(
  'Poverty'
  ,'Poverty (Highschool Subset)'
  ,'Dropout'
  ,'Dropout (With Interaction)'
  ,'Get GED'
  ,'Get Bachelors'
  ,'Out of the Labor Force'
  ,'Out of the Labor Force (Highschool Subset)'
  ,'Out of the Labor Force (College Subset)'
  ,'Unemployed'
  ,'Unemployed (Highschool Subset)'
  ,'Unemployed (College Subset)'
  ,'Ever Married'
  ,'Ever Married (Highschool Subset)'
  ,'Ever Married (College Subset)'
  ,'Divorced in 1st 5 Years of Marriage'
  ,'Divorced in 1st 5 Years of Marriage (Highschool Subset)'
  ,'Divorced in 1st 5 Years of Marriage (College Subset)'
  ,'Divorced in 1st 5 Years of Marriage (With Parents Factors)'
  ,'Surveyed in Jail'
  ,'Surveyed in Jail (Highschool Subset)'
  ,shQuote('Middle Class Values')
  ,paste0(shQuote('Middle Class Values'), ' (Highschool Subset)')
  ,paste0(shQuote('Middle Class Values'), ' (College Subset)')
)

# Strings representing filters/subsets needed to reproduce training data for each model
tbc.filters.training <- c(
  "Race4 == 'White' & Sample == 'XSection' & EmpSchl == 1"
  ,"Race4 == 'White' & Sample == 'XSection' & EmpSchl == 1 & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'XSection' & LTHSvGED != 'GED' & !is.na(AllvBA)"
  ,"Race4 == 'White' & Sample == 'XSection' & LTHSvGED != 'GED' & !is.na(AllvBA)"
  ,"Race4 == 'White' & Sample == 'XSection' & LTHSvGED != 'LTHS' & !is.na(AllvBA)"
  ,"Race4 == 'White' & Sample == 'XSection' & !(BA_Atta == 'InSchl/NoD' & AllvBA == 0 & is.na(AllvBA))"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Emp568) & Sex == 'Male'"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Emp568) & Sex == 'Male' & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Emp568) & Sex == 'Male' & EdSample == 'College'"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Emp568) & Sex == 'Male' & !is.na(UnempSam)"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Emp568) & Sex == 'Male' & !is.na(UnempSam) & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Emp568) & Sex == 'Male' & !is.na(UnempSam) & EdSample == 'College'"
  ,"Race4 == 'White' & Sample == 'XSection' & IntAge90 >= 30 & !is.na(EverWed)"
  ,"Race4 == 'White' & Sample == 'XSection' & IntAge90 >= 30 & !is.na(EverWed) & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'XSection' & IntAge90 >= 30 & !is.na(EverWed) & EdSample == 'College'"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Div5Yrs)"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Div5Yrs) & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Div5Yrs) & EdSample == 'College'"
  ,"Race4 == 'White' & Sample == 'XSection' & !is.na(Div5Yrs) & Adult14S != ''"
  ,"Race4 == 'White' & Sample == 'XSection' & Sex == 'Male'"
  ,"Race4 == 'White' & Sample == 'XSection' & Sex == 'Male' & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'XSection'"
  ,"Race4 == 'White' & Sample == 'XSection' & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'XSection' & EdSample == 'College'"
)

# Strings representing filters/subsets needed to reproduce holdout data for each model
tbc.filters.holdout <- c(
  "Race4 == 'White' & Sample == 'Supplement' & EmpSchl == 1"
  ,"Race4 == 'White' & Sample == 'Supplement' & EmpSchl == 1 & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'Supplement' & LTHSvGED != 'GED' & !is.na(AllvBA)"
  ,"Race4 == 'White' & Sample == 'Supplement' & LTHSvGED != 'GED' & !is.na(AllvBA)"
  ,"Race4 == 'White' & Sample == 'Supplement' & LTHSvGED != 'LTHS' & !is.na(AllvBA)"
  ,"Race4 == 'White' & Sample == 'Supplement' & !(BA_Atta == 'InSchl/NoD' & AllvBA == 0 & is.na(AllvBA))"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Emp568) & Sex == 'Male'"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Emp568) & Sex == 'Male' & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Emp568) & Sex == 'Male' & EdSample == 'College'"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Emp568) & Sex == 'Male' & !is.na(UnempSam)"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Emp568) & Sex == 'Male' & !is.na(UnempSam) & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Emp568) & Sex == 'Male' & !is.na(UnempSam) & EdSample == 'College'"
  ,"Race4 == 'White' & Sample == 'Supplement' & IntAge90 >= 30 & !is.na(EverWed)"
  ,"Race4 == 'White' & Sample == 'Supplement' & IntAge90 >= 30 & !is.na(EverWed) & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'Supplement' & IntAge90 >= 30 & !is.na(EverWed) & EdSample == 'College'"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Div5Yrs)"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Div5Yrs) & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Div5Yrs) & EdSample == 'College'"
  ,"Race4 == 'White' & Sample == 'Supplement' & !is.na(Div5Yrs) & Adult14S != ''"
  ,"Race4 == 'White' & Sample == 'Supplement' & Sex == 'Male'"
  ,"Race4 == 'White' & Sample == 'Supplement' & Sex == 'Male' & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'Supplement'"
  ,"Race4 == 'White' & Sample == 'Supplement' & EdSample == 'HS'"
  ,"Race4 == 'White' & Sample == 'Supplement' & EdSample == 'College'"
)

# Strings representing columns needed to reproduce each model
tbc.factors <- c(
  "c('zAFQT89', 'zSES', 'zAge', 'Pov89')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'Pov89')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'LTHSvHS_Ind')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'LTHSvHS_Ind')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'GEDvHSGr_Ind')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'AllvBA')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MoOLF89')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MoOLF89')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MoOLF89')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MoUnemp8')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MoUnemp8')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MoUnemp8')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'WedBy30_Ind')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'WedBy30_Ind')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'WedBy30_Ind')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MarDate', 'Div5Yrs')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MarDate', 'Div5Yrs')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MarDate', 'Div5Yrs')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'Adult14S', 'Div5Yrs')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'Jail')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'Jail')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MCV_Inde')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MCV_Inde')"
  ,"c('zAFQT89', 'zSES', 'zAge', 'MCV_Inde')"
)

# Strings representing binary targets used to reproduce each model
tbc.response <- c(
  'Pov89'
  ,'Pov89'
  ,'LTHSvHS_Ind'
  ,'LTHSvHS_Ind'
  ,'GEDvHSGr_Ind'
  ,'AllvBA'
  ,'MoOLF89'
  ,'MoOLF89'
  ,'MoOLF89'
  ,'MoUnemp8'
  ,'MoUnemp8'
  ,'MoUnemp8'
  ,'WedBy30_Ind'
  ,'WedBy30_Ind'
  ,'WedBy30_Ind'
  ,'Div5Yrs'
  ,'Div5Yrs'
  ,'Div5Yrs'
  ,'Div5Yrs'
  ,'Jail'
  ,'Jail'
  ,'MCV_Inde'
  ,'MCV_Inde'
  ,'MCV_Inde'
)

tbc.response.modeled <- paste0(tbc.response,'_modeled')
tbc.response.linpred <- paste0(tbc.response,'_linearpredictor')

# Strings representing formulae needed to reproduce each model
tbc.formulae <- c(
  "Pov89 ~ zAFQT89 + zSES + zAge"
  ,"Pov89 ~ zAFQT89 + zSES + zAge"
  ,"LTHSvHS_Ind ~ zAFQT89 + zSES + zAge"
  ,"LTHSvHS_Ind ~ zAFQT89 + zSES + zAge + zAFQT89:zSES"
  ,"GEDvHSGr_Ind ~ zAFQT89 + zSES + zAge"
  ,"AllvBA ~ zAFQT89 + zSES + zAge"
  ,"MoOLF89 ~ zAFQT89 + zSES + zAge"
  ,"MoOLF89 ~ zAFQT89 + zSES + zAge"
  ,"MoOLF89 ~ zAFQT89 + zSES + zAge"
  ,"MoUnemp8 ~ zAFQT89 + zSES + zAge"
  ,"MoUnemp8 ~ zAFQT89 + zSES + zAge"
  ,"MoUnemp8 ~ zAFQT89 + zSES + zAge"
  ,"WedBy30_Ind ~ zAFQT89 + zSES + zAge"
  ,"WedBy30_Ind ~ zAFQT89 + zSES + zAge"
  ,"WedBy30_Ind ~ zAFQT89 + zSES + zAge"
  ,"Div5Yrs ~ zAFQT89 + zSES + zAge + MarDate"
  ,"Div5Yrs ~ zAFQT89 + zSES + zAge + MarDate"
  ,"Div5Yrs ~ zAFQT89 + zSES + zAge + MarDate"
  ,"Div5Yrs ~ zAFQT89 + zSES + zAge + Adult14S"
  ,"Jail ~ zAFQT89 + zSES + zAge"
  ,"Jail ~ zAFQT89 + zSES + zAge"
  ,"MCV_Inde ~ zAFQT89 + zSES + zAge"
  ,"MCV_Inde ~ zAFQT89 + zSES + zAge"
  ,"MCV_Inde ~ zAFQT89 + zSES + zAge"
)

tbc.formulae <- unlist(lapply(tbc.formulae, function(x) as.formula(x, env = .GlobalEnv)))

# These are the page numbers in The Bell Curve (ISBN-13: 978-0-684-82429) with corresponding
# logistic regression output for each model
# With the exception of the models below, all model parameters were reproduced to at least 4 decimal places
# Partial reproduction:
#      Model: get_bachelors
#      Comment: The parameter values are very close, but not within 4 decimal places. 
#               I suspect the book's description of data filter is incomplete, since the row counts differ slightly. 
#               That said, I think this reproduction is good enough to include in this analysis.
# 
#      Model: divorced_in5yrs_parents
#      Comment: The parameter levels for the Adult14S field are different than in the book. However, all other parameters match.
#               I suspect this is a difference in how STATA and R handle categorical factors, and so 
#               the model is adequately reproduced.
tbc.appendix.pages <- c(
  620
  , 620
  , 620
  , 621
  , 622
  , 622
  , 623
  , 623
  , 624
  , 624
  , 624
  , 625
  , 625
  , 626
  , 626
  , 626
  , 627
  , 627
  , 627
  , 645
  , 645
  , 646
  , 646
  , 647
)

# Descriptions of each binary target as written in The Bell Curve
# These will be used to label plots
tbc.target.descriptions <- c(
  "Under the official poverty line in 1989"
  ,"Under the official poverty line in 1989"
  ,"Permanently dropped out of high school"
  ,"Permanently dropped out of high school"
  ,"Received a GED instead of a high school diploma"
  ,"Received a bachelor's degree"
  ,"Out of the labor force for four weeks or more in 1989"
  ,"Out of the labor force for four weeks or more in 1989"
  ,"Out of the labor force for four weeks or more in 1989"
  ,"Unemployed for four weeks or more in 1989"
  ,"Unemployed for four weeks or more in 1989"
  ,"Unemployed for four weeks or more in 1989"
  ,"Ever married before the age of 30"
  ,"Ever married before the age of 30"
  ,"Ever married before the age of 30"
  ,"Divorced within the first 5 years of marriage"
  ,"Divorced within the first 5 years of marriage"
  ,"Divorced within the first 5 years of marriage"
  ,"Divorced within the first 5 years of marriage"
  ,"Subjects interviewed in jail at least once from 1979 to 1990"
  ,"Subjects interviewed in jail at least once from 1979 to 1990"
  ,"Subjects scored 'yes' on the Middle Class Values Index"
  ,"Subjects scored 'yes' on the Middle Class Values Index"
  ,"Subjects scored 'yes' on the Middle Class Values Index"
)

tbc.target.descriptions.formatted <- paste('%', tbc.target.descriptions)

# Categories for each binary target. Used for the plot comparing optimal MCC values between training/holding data for each model
tbc.categories <- c(
  'Poverty'
  ,'Poverty'
  ,'Education'
  ,'Education'
  ,'Education'
  ,'Education'
  ,'Employment'
  ,'Employment'
  ,'Employment'
  ,'Employment'
  ,'Employment'
  ,'Employment'
  ,'Marriage'
  ,'Marriage'
  ,'Marriage'
  ,'Marriage'
  ,'Marriage'
  ,'Marriage'
  ,'Marriage'
  ,'Crime'
  ,'Crime'
  ,shQuote('Middle Class Values')
  ,shQuote('Middle Class Values')
  ,shQuote('Middle Class Values')
)

# Combine all prior character vectors in a data.table to use in defining each training data, holdout data, and model
tbc.models <- data.table(
  label = factor(tbc.labels, levels = tbc.labels)
  , labelAppendix = tbc.labelsAppendix
  , category = tbc.categories
  , target.description = tbc.target.descriptions
  , target.descriptions.formatted = tbc.target.descriptions.formatted
  , filter.training = tbc.filters.training
  , filter.holdout = tbc.filters.holdout
  , response = tbc.response
  , response.modeled = tbc.response.modeled
  , response.linpred = tbc.response.linpred
  , factors = tbc.factors
  , formula = tbc.formulae
  , appendix.page = tbc.appendix.pages
)