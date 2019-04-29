This repo started as a final project for a data science master's course. None of it has been peer reviewed.

***
Overview
***

Despite its publication nearly 25 years ago, the controversial book The Bell Curve (TBC)[^1] has persisted in conversations about IQ. The authors, Richard Herrnstein and Charles Murray, claim that IQ strongly influences many significant life events -- like whether someone will enter poverty, drop out of school, get married, or even go to jail. Even more contentious, they extend the consequences of low IQ to demographics like ethnicity and nationality. As evidence, the authors build and discuss dozens of probability models using public data.

Although TBC has been criticized extensively, these responses have generally come from social psychologists and not statisticians. For this report, I intend to evaluate the statistical meaning of 24 of the nearly 3 dozen models via bootstrap optimism on Matthew's correlation coefficient. In doing so, I will illustrate the impact of two scientific philosophies, explanation and prediction, in the context of presenting statistical models to the public. I will describe why prediction is the only philosophy relevant to the claims in TBC and, finally, that the 24 probability models are inadequate in supporting the authors’ public policy recommendations. 

***
Methods
***
For 10,000 bootstrap iterations and each TBC model:

1. Define two bootstrap samples:
    
    * Bootstrapped training data
    
    * Bootstrapped holdout data

2. Build a bootstraped probability model using training data

3. With this model, score the:
    
    * Bootstrapped training sample
    
    * Bootstrapped holdout sample
    
    * Original training sample
    
    * Original holdout sample

4. For each scored data set, build 500 classifications using cutoff probabilities in a linear grid across (0, 1)

5. For each classifier, calculate Matthew's Correlation Coefficient

6. For each cutoff value, calculate: 
    
    * The optimism for MCC
    
    * Bias-corrected accelerated (BCa) 95% confidence intervals for training data
    
    * Bootstrapped percentile 95% confidence intervals for holdout data

7. Lastly, for each model, find the cutoff value with the largest mean bootstrapped MCC value, i.e., the "optimal cutoff".

These steps are implemented in R using the following scripts within this repository:

* *000_Misc.R*: Attaches needed packages and defines convenience functions

* *100_Resampling.R*: Custom functions to perform resampling techniques via bootstrapping and the jackknife method

* *200_Classification.R*: Custom functions to create classifications from modeled probabilities and then quantify their performance

* *300_Visualizations.R*: Custom functions to create various visualizations to understand the predictive performance of binary classifications

* *400_Model_Definitions.R*: Parameters used to define and reproduce logistic regression models from The Bell Curve

* *TBC_Bootstrap.R*: Recursive code that executes a batch run for each TBC probability model. **This script is the workhorse of the entire project**. See its code for details.

**Note**: When 10,000 bootstrap iterations are used on all 24 models in the prior script (with parallel computing), the entire recursive batch process:

- Produces 193 files in the "100 Data" folder, 6.56 MBs in total
- Takes 4 - 5 hours

***
References
***

1) Herrnstein and Murray. (1994). The Bell Curve. New York: The Free Press. 1st paperback edition 1996. ISBN-13: 978-0-684-82429

2) Bureau of Labor Statistics, U.S. Department of Labor. National Longitudinal Survey of Youth 1979 cohort, 1979-2012 (rounds 1-25). Produced and distributed by the Center for Human Resource Research, The Ohio State University. Columbus, OH: 2014.

3) Prepared for the U.S. Department of Labor by Center for Human Resource Research, The Ohio State University. (2001). NLSY79 users' guide : a guide to the 1979-2000 National Longitudinal Survey of Youth data. Columbus, Ohio :Center for Human Resource Research, Ohio State University.

4) Heckman, J. (1995). Lessons from The Bell Curve. Journal of Political Economy, 103(5), 1091–1120.

5) Krenz, C. (2007, August 8). Anatomy of an Analysis. Retrieved from http://www.claudiax.net/bell.html.

6) Shmueli, G. (2010). To Explain or To Predict? Statistical Science, 25(3), 289–310.

7) Miele, F. (1995). For Whom The Bell Curve Tolls. Skeptic, Volume 3, #3, 34 – 41.

8) Mukaka MM. Statistics corner: A guide to appropriate use of correlation coefficient in medical research. Malawi Med J. 2012;24(3):69-71.

9) Boughorbel S, Jarray F, El-Anbari M (2017) Optimal classifier for imbalanced data using Matthews Correlation Coefficient metric. PLoS ONE 12(6).

10) Efron B. (1983). Estimating the error rate of a prediction rule: improvement on cross-validation. Journal of the American Statistical Association, 78:316-331.

11) Harrell, F. E., Lee, K. L., & Mark, D. B. (1996). Tutorial in Biostatistics: Multivariable prognostic models. Statistics in Medicine, 15:361-387.

12) Herrnstein and Murray (1994). Nation.txt, Nation.hdr, and 2TBC_Documentation.ascii. Retrieved from http://www.rasmusen.org/xpacioli/bellcurve.

13) Bradley Efron (1987) Better Bootstrap Confidence Intervals, Journal of the American Statistical Association, 82:397, 171-185, DOI: 10.1080/01621459.1987.10478410.