---
title: "Experiment 2: PowerAnalysis"
author: "Hannah"
date: "4/7/2021"
output: 
  html_document:
    keep_md: true
---

## Sources
1. Green & MacLeod's 2016 paper, titled, "SIMR: an R package for power analysis of generalized linear mixed models by simulation"
2. Dr. Humburg's post on conducting power analyses using "simr" (https://humburg.github.io/Power-Analysis/simr_power_analysis.html)

## Data Generation
### Create the Dataframe
#### In this section, we create the base dataframe with corner angles and indexes of difficulty for our population.

```r
# first, create our variables
angle <- c(45, 90, 135) # three different corner angles
index <- c(1.807, 1.17, 0.874) # three indexes of difficulty
ss <- 99999 # create a sample size for our population
angle2 <- rep(angle, each = ss/3) # repeat instruction methods 
index2 <- rep(rep(index, each = ss/9), 3) # repeat index of difficulty

# concatenate into a dataframe
df_pop <- data.frame(corner_angle=factor(angle2), indx_dff=index2)
```

### Estimate Cornering Times
#### In this section, we create the estimated cornering times. We will do this using Pastel et al.'s (2007) fitted model (plus some variability). These will serve as the cornering times for the 90-degree corners, as this model was developed using 90-degree corners.
#### We will then do the same for the different corner angles. We will increase and decrease cornering times by 20% for the 135-degree and 45-degree corner angles, respectively. We do this because it is expected that cornering time will be affected by corner angle. 


```r
# define error term to induce some variability in our estimates 
error_term <- rnorm(ss/9, mean=0, sd = 0.7) 

# use Pastel et al.'s (2007) fitted model to get estimated cornering times for each index of difficulty & corner angle

# 45 degrees
Y1 <- (0.975 + .787*1.807 + error_term)*1.2
Y2 <- (0.975 + .787*1.17 + error_term)*1.2
Y3 <- (0.975 + .787*0.874 + error_term)*1.2

# 90 degrees
Y4 <- (0.975 + .787*1.807 + error_term)
Y5 <- (0.975 + .787*1.17 + error_term)
Y6 <- (0.975 + .787*0.874 + error_term)

# 135 degrees
Y7 <- (0.975 + .787*1.807 + error_term)*.8
Y8 <- (0.975 + .787*1.17 + error_term)*.8
Y9 <- (0.975 + .787*0.874 + error_term)*.8

# finally, we append to the population dataframe
df_pop$ct <- c(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9)

# then, we create a sample of this population
pid <- factor(1:5) # five subjects
PID <- rep(pid, each = 9) # repeat participant ID 9 times each
ANGLE <- rep(rep(angle, each = 3), 5) # repeat instruction method 3 times each and do that five times
ID <- rep(index, 15) # repeat index of difficulty values 15 times

# concatenate into a dataframe
df_sample <- data.frame(PID=PID, ANGLE=factor(ANGLE), ID=ID)

# get cornering times from our population
df_sample$ct <- matrix(replicate(5, c(sample(Y1, 1, replace = T), sample(Y2, 1, replace = T), 
                                      sample(Y3, 1, replace = T), sample(Y4, 1, replace = T), 
                                      sample(Y5, 1, replace = T), sample(Y6, 1, replace = T),
                                      sample(Y7, 1, replace = T), sample(Y8, 1, replace = T),
                                      sample(Y9, 1, replace = T))), ncol=1)

# view dataframe
head(df_sample, n = 12)
```

```
##    PID ANGLE    ID        ct
## 1    1    45 1.807 4.2889665
## 2    1    45 1.170 2.4713125
## 3    1    45 0.874 1.6056548
## 4    1    90 1.807 1.7516443
## 5    1    90 1.170 0.8842682
## 6    1    90 0.874 1.8026378
## 7    1   135 1.807 0.8179061
## 8    1   135 1.170 1.3499737
## 9    1   135 0.874 1.6076043
## 10   2    45 1.807 3.1279091
## 11   2    45 1.170 3.4672728
## 12   2    45 0.874 2.8909066
```

## Power Analysis
### Simluate Baseline Model
#### In this section, we use the simr package to estimate power

```r
# load simr package 
library(simr)
```

```
## Warning: package 'simr' was built under R version 4.0.3
```

```
## Loading required package: lme4
```

```
## Loading required package: Matrix
```

```
## Registered S3 methods overwritten by 'car':
##   method                          from
##   influence.merMod                lme4
##   cooks.distance.influence.merMod lme4
##   dfbeta.influence.merMod         lme4
##   dfbetas.influence.merMod        lme4
```

```
## 
## Attaching package: 'simr'
```

```
## The following object is masked from 'package:lme4':
## 
##     getData
```

```r
# set simr progress to FALSE (if you want to see the progress, this can be excluded)
simrOptions(progress=FALSE)

# Now, to get what we believe the TRUE effects to be, we fit a linear model to the population data
fit <- lm(ct ~ corner_angle*indx_dff, data = df_pop)

# view model summary
summary(fit)
```

```
## 
## Call:
## lm(formula = ct ~ corner_angle * indx_dff, data = df_pop)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.2427 -0.4578  0.0048  0.4556  2.9246 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               1.17942    0.01324   89.08   <2e-16 ***
## corner_angle90           -0.19657    0.01872  -10.50   <2e-16 ***
## corner_angle135          -0.39314    0.01872  -21.00   <2e-16 ***
## indx_dff                  0.94440    0.00987   95.68   <2e-16 ***
## corner_angle90:indx_dff  -0.15740    0.01396  -11.28   <2e-16 ***
## corner_angle135:indx_dff -0.31480    0.01396  -22.55   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7015 on 99993 degrees of freedom
## Multiple R-squared:  0.2913,	Adjusted R-squared:  0.2913 
## F-statistic:  8222 on 5 and 99993 DF,  p-value: < 2.2e-16
```

```r
# Now, we set our model parameters - extracted from the linear model we just fit 
fixed <- c(summary(fit)$coef[1], summary(fit)$coef[2], summary(fit)$coef[3],
           summary(fit)$coef[4], summary(fit)$coef[5], summary(fit)$coef[6])
rand <- 0.1 # assign random effect intercept variance
res <- 0.5 # assign residual variance

# fit the baseline simulation model using our sample dataframe
model <- makeLmer(y ~ ANGLE*ID + (1|PID), fixef = fixed, VarCorr = rand, sigma = res, data = df_sample)

# run simulation & compare to model without the interaction term
sim <- powerSim(model, nsim=100, test = fcompare(y~ANGLE+ID))
```

```
## boundary (singular) fit: see ?isSingular
```

```
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
```

```r
# view power
sim
```

```
## Power for model comparison, (95% confidence interval):
##       12.00% ( 6.36, 20.02)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ANGLE + ID + [re]
## 
## Based on 100 simulations, (2 warnings, 0 errors)
## alpha = 0.05, nrow = 45
## 
## Time elapsed: 0 h 0 m 12 s
```


```r
# First, let's extend the number of participants to 15 instead of 5
model2 <- extend(model, along = 'PID', n=15)

# run the simulation
sim2 <- powerSim(model2, nsim = 500, test = fcompare(y~ANGLE+ID))
```

```
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
```

```r
# View power
sim2
```

```
## Power for model comparison, (95% confidence interval):
##       20.20% (16.77, 23.99)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ANGLE + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 135
## 
## Time elapsed: 0 h 0 m 56 s
```

```r
# Not quite. Let's also extend the number of trials for each index of difficulty for each subject 
model3 <- extend(model2, within = 'PID+ANGLE+ID', n=20)

# run the simulation 
sim3 <- powerSim(model3, nsim=500, test = fcompare(y~ANGLE+ID))

# view power
sim3
```

```
## Power for model comparison, (95% confidence interval):
##       99.60% (98.56, 99.95)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ANGLE + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 2700
## 
## Time elapsed: 0 h 1 m 21 s
```

```r
# fit power curve so we can visualize how power changes as the model is extended 
pc <- powerCurve(model3, test = fcompare(y~ANGLE+ID), within = 'PID+ANGLE+ID')

# plot the curve
plot(pc, xlab='Number_of_Observations')
```

![](Exp2_PowerAnalysis_files/figure-html/extend baseline model-1.png)<!-- -->

```r
# print values
print(pc)
```

```
## Power for model comparison, (95% confidence interval),
## by number of observations within PID+ANGLE+ID:
##       3: 44.40% (41.29, 47.54) - 405 rows
##       5: 63.80% (60.73, 66.78) - 675 rows
##       7: 80.30% (77.70, 82.72) - 945 rows
##       9: 89.60% (87.54, 91.42) - 1215 rows
##      11: 94.00% (92.34, 95.39) - 1485 rows
##      12: 95.50% (94.02, 96.70) - 1620 rows
##      14: 97.60% (96.45, 98.46) - 1890 rows
##      16: 98.60% (97.66, 99.23) - 2160 rows
##      18: 99.40% (98.70, 99.78) - 2430 rows
##      20: 100.0% (99.63, 100.0) - 2700 rows
## 
## Time elapsed: 0 h 16 m 37 s
```

```r
# ok, we need ~9 observations within pid, instruction method, and index of difficulty; let's look specifically at this number of observations 
model4 <- extend(model2, within = 'PID+ANGLE+ID', n=9)

# power for interaction term
sim4 <- powerSim(model4, nsim=500, test = fcompare(y~ANGLE+ID))
sim4
```

```
## Power for model comparison, (95% confidence interval):
##       88.20% (85.04, 90.89)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ANGLE + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 1215
## 
## Time elapsed: 0 h 1 m 2 s
```

```r
# power for main effect of index of difficulty
sim5 <- powerSim(model4, nsim=500, test = fcompare(y~ANGLE))
sim5
```

```
## Power for model comparison, (95% confidence interval):
##       100.0% (99.26, 100.0)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ANGLE + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 1215
## 
## Time elapsed: 0 h 1 m 3 s
```

```r
# power for main effect of angle
sim6 <- powerSim(model4, nsim=500, test = fcompare(y~ID))
sim6
```

```
## Power for model comparison, (95% confidence interval):
##       100.0% (99.26, 100.0)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 1215
## 
## Time elapsed: 0 h 1 m 2 s
```
