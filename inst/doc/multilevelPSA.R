## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(digits = 2)

## ----echo=FALSE, out.width='90%', fig.align='center', fig.cap='Annotated multilevel PSA assessment plot. This plot compares private schools (x- axis) against public schools (y-axis) for North America from the Programme of International Student Assessment.'----
knitr::include_graphics('AnnotatedCircPlot.png')

## ----setup, message=FALSE-----------------------------------------------------
library(multilevelPSA)
library(party)

## ----load-data, message=FALSE-------------------------------------------------
data(pisana)
data(pisa.psa.cols)
pisana$MathScore <- apply(pisana[,paste0('PV', 1:5, 'MATH')], 1, sum) / 5

## ----ctree, message=FALSE, results='hide'-------------------------------------
mlpsa <- mlpsa.ctree(pisana[,c('CNT', 'PUBPRIV', pisa.psa.cols)], 
                     formula = PUBPRIV ~ ., 
                     level2 = 'CNT')
mlpsa.df <- getStrata(mlpsa, pisana, level2 = 'CNT')

## ----logistic-regressions, warning=FALSE, results='hide'----------------------
mlpsa.lr <- mlpsa.logistic(pisana[,c('CNT', 'PUBPRIV', pisa.psa.cols)], 
                           formula = PUBPRIV ~ ., 
                           level2 = 'CNT')
mlpsa.lr.df <- getPropensityScores(mlpsa.lr, nStrata = 5)

## ----logistic-regression-results----------------------------------------------
head(mlpsa.lr.df)

## ----covariate-balance, warning=FALSE, fig.cap='Multilevel PSA balance plot for PISA. The eﬀect sizes (standardized mean diﬀerences) for each covariate are provided before PSA adjustment (blue triangles) and after PSA adjustment (red circles).', fig.width=6, fig.height=8, out.width='100%'----
cv.bal <- covariate.balance(covariates = pisana[,pisa.psa.cols],
                            treatment = pisana$PUBPRIV,
                            level2 = pisana$CNT,
                            strata = mlpsa.df$strata)
head(as.data.frame(cv.bal))
plot(cv.bal)

## -----------------------------------------------------------------------------
results.psa.math <- mlpsa(response = mlpsa.df$MathScore,
                          treatment = mlpsa.df$PUBPRIV,
                          strata = mlpsa.df$strata,
                          level2 = mlpsa.df$CNT)

## -----------------------------------------------------------------------------
summary(results.psa.math)

## ----psa-circ-plot, fig.cap='Multilevel PSA assessment plot for PISA. The main panel provides the adjusted mean for private (x-axis) and public (y-axis) for each country. The left and lower panels provide the mean for each stratum for the public and private students, respectively. The overall adjusted mean diﬀerence is represented by the dashed blue line and the 95% confidence interval by the dashed green lines. There is a statistically significant diﬀerence between private and public school student performance as evidenced by the confidence interval not spanning zero (i.e. not crossing the unit line y=x.', fig.width=6, fig.height=6, out.width='100%'----
plot(results.psa.math)

## ----difference-plot, fig.cap='Multilevel PSA diﬀerence plot for PISA. Each blue dot corresponds to the eﬀect size (standardized mean diﬀerence) for each country. The vertical blue line corresponds to the overall eﬀect size for all countries. The green lines correspond to the 95% confidence intervals. The dashed green lines Bonferroni-Sidak (c.f. Abdi, 2007) adjusted confidence intervals. The size of each dot is proportional to the sample size within each country.', fig.width=6, fig.height=2, out.width='100%'----
mlpsa.difference.plot(results.psa.math, 
                      sd = mean(mlpsa.df$MathScore, na.rm=TRUE))

