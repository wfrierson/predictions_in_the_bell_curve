This repo represents a final project for a data science master's course. As a result, none of it has been peer reviewed.

***
Overview
***

Despite its publication nearly 25 years ago, the controversial book The Bell Curve (TBC) has persisted in conversations about IQ. The authors, Richard Herrnstein and Charles Murray, claim that IQ strongly influences many significant life events -- like whether someone will enter poverty, drop out of school, get married, or even go to jail. Even more contentious, they extend the consequences of low IQ to demographics like ethnicity and nationality. As evidence, the authors build and discuss dozens of probability models using public data. 

Although TBC has been criticized extensively, these responses have mostly come from social psychologists and not statisticians. For this report, I intend to evaluate the statistical meaning of 24 of the nearly 3 dozen models via bootstrap regression. In doing so, I will illustrate the impact of two scientific philosophies, explanation and prediction, in the context of presenting statistical models to the public. I will describe why prediction is the only philosophy relevant to the claims in TBC and, finally, that the 24 probability models are inadequate in supporting the authors’ public policy recommendations. 

***
Methods
***
1. Specify a training dataset and formula consistent with a TBC model.

2. Fit 10,000 logistic regression models via bootstrapping (i.e., resampling the observed joint distribution with equal sizes)

3. Apply each bootstrapped model to the training data.

4. Map the modeled probabilities to a classification by selecting a cutoff value

5. Build a confusion matrix with the modeled classifications and actual target data.

6. Calculate various performance metrics for a binary target, in particular, MCC.

7. Repeat steps 4 - 6 for a large range of possible cutoff values (i.e., a linear grid of length 500 between 0 and 1).

8. Inspect the distribution of MCC across all cutoff values to find the maximum MCC value.

9. With the IQ factor on the x-axis, plot the average observed target and bootstrapped classifications for a set of cutoff values (including that which maximizes MCC). Use the plot to build intuition on the maximum MCC value.

10. Repeat all prior steps for each of the 24 training datasets and formulae.

11. Summarize the maximum MCC values across all reproduced TBC models and by training and holdout data.

These steps are implemented in R using the following scripts within this repository:

* *000_Misc.R*: Attaches needed packages and defines a convenience function to report timing

* *100_Resampling.R*: Custom functions to perform resampling techniques via bootstrapping and the jackknife method

* *200_Classification.R*: Custom functions to create classifications from modeled probabilities and then quantify their performance

* *300_Visualizations.R*: Custom functions to create various visualizations to understand the predictive performance of binary classifications

* *400_Model_Definitions.R*: Parameters used to define and reproduce logistic regression models from The Bell Curve

* *TBC_Bootstrap.R*: Recursive code that executes a batch run for each TBC probability model. **This script is the workhorse of the entire project**. See its code for details.

**Note**: When 10,000 bootstrap iterations are used on all 24 models in the prior script, the entire recursive batch process:

- Produces 941 files in the "100 Data" folder, 6.76 GBs in total
- Takes 4 - 5 hours

***
References
***

1. Herrnstein and Murray. (1994). The Bell Curve. New York: The Free Press.

2. Bureau of Labor Statistics, U.S. Department of Labor. National Longitudinal Survey of Youth 1979 cohort, 1979-2012 (rounds 1-25). Produced and distributed by the Center for Human Resource Research, The Ohio State University. Columbus, OH: 2014.

3. prepared for the U.S. Department of Labor by Center for Human Resource Research, The Ohio State University. (2001). NLSY79 users' guide : a guide to the 1979-2000 National Longitudinal Survey of Youth data. Columbus, Ohio :Center for Human Resource Research, Ohio State University.

4. Heckman, J. (1995). Lessons from The Bell Curve. Journal of Political Economy, 103(5), 1091–1120.

5. Krenz, C. (2007, August 8). Anatomy of an Analysis. Retrieved from http://www.claudiax.net/bell.html.

6. Shmueli, G. (2010). To Explain or To Predict? Statistical Science, 25(3), 289–310.

7. Miele, F. (1995). For Whom The Bell Curve Tolls. Skeptic, Volume 3, #3, 34 – 41.

8. Boughorbel S, Jarray F, El-Anbari M (2017) Optimal classifier for imbalanced data using Matthews Correlation Coefficient metric. PLoS ONE 12(6).

9. Mukaka MM. Statistics corner: A guide to appropriate use of correlation coefficient in medical research. Malawi Med J. 2012;24(3):69-71.

10. Herrnstein and Murray (1994). Nation.txt, Nation.hdr, and 2TBC_Documentation.ascii. Retrieved from http://www.rasmusen.org/xpacioli/bellcurve.
