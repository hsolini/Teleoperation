---
title: 'Experiment 1: Power Analysis'
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
#### In this section, we create the base dataframe with instruction methods and indexes of difficulty for our population.

```r
knitr::opts_chunk$set(message = FALSE)

# first, create our variables
instr <- c('accuracy', 'speed') # two instruction methods
index <- c(1.807, 1.17, 0.874) # three indexes of difficulty
ss <- 100002 # create a sample size for our population
instr2 <- rep(instr, each = ss/2) # repeat instruction methods 
index2 <- rep(index, each = ss/6) # repeat index of difficulty

# concatenate all of that into a dataframe
df_pop <- data.frame(instr_meth=factor(instr2), indx_dff=index2)
```

### Estimate Cornering Times
#### In this section, we create the estimated cornering times. We will do this using Pastel et al.'s (2007) fitted model. For the accuracy condition, we will create a sample of probable values for each of the three indexes of difficulty. This is done by using the fitted model (plus some variability). 
#### We will then do the same for the speed condition. Except here, we will reduce cornering times by 10% - as we expect cornering time to decrease when subjects are told to focus only on speed. 


```r
# define error term to induce some variability in our estimates 
error_term <- rnorm(ss/6, mean=0, sd = 0.7) 

# use Pastel et al.'s (2007) fitted model to get estimated cornering times for each index of difficulty
Y1 <- 0.975 + .787*1.807 + error_term
Y2 <- 0.975 + .787*1.17 + error_term
Y3 <- 0.975 + .787*0.874 + error_term

# Then do the same for the speed condition (but reduce estimated cornering time by 10%)
Y4 <- (0.975 + .787*1.807 + error_term)*.9
Y5 <- (0.975 + .787*1.17 + error_term)*.9
Y6 <- (0.975 + .787*0.874 + error_term)*.9

# finally, we append to our df
df_pop$ct <- c(Y1, Y2, Y3, Y4, Y5, Y6)

# then, we create a sample of this population so we can assess power
pid <- factor(1:10) # ten subjects
PID <- rep(pid, each = 3) # repeat participant ID 3 times each
IM <- rep(rep(instr, each = 3), 5) # repeat instruction method 3 times each and do that five times
ID <- rep(index, 10) # repeat index of difficulty values 10 times

# Now, we concatenate all of that into a dataframe
df_sample <- data.frame(PID=PID, IM=factor(IM), ID=ID)

# get cornering times from our population
df_sample$ct <- matrix(replicate(5, c(sample(Y1, 1, replace = T), sample(Y2, 1, replace = T), 
                                      sample(Y3, 1, replace = T), sample(Y4, 1, replace = T), 
                                      sample(Y5, 1, replace = T), sample(Y6, 1, replace = T))), ncol=1)
# view our sample data
head(df_sample, n=10)
```

```
##    PID       IM    ID        ct
## 1    1 accuracy 1.807 2.3335767
## 2    1 accuracy 1.170 1.2531979
## 3    1 accuracy 0.874 0.8411663
## 4    2    speed 1.807 2.1278983
## 5    2    speed 1.170 1.8180868
## 6    2    speed 0.874 0.7245497
## 7    3 accuracy 1.807 2.0764792
## 8    3 accuracy 1.170 1.6658515
## 9    3 accuracy 0.874 3.0031103
## 10   4    speed 1.807 0.9718931
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

```r
# set simr progress to FALSE (if you want to see the progress, this can be excluded)
simrOptions(progress=FALSE)

# Now, to get what we believe the TRUE effects to be, we fit a linear model to the population data
fit <- lm(ct ~ instr_meth*indx_dff, data = df_pop)

# view model summary
summary(fit)
```

```
## 
## Call:
## lm(formula = ct ~ instr_meth * indx_dff, data = df_pop)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.61047 -0.44048 -0.00219  0.44247  2.67011 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               0.977404   0.010218  95.659  < 2e-16 ***
## instr_methspeed          -0.097740   0.014450  -6.764 1.35e-11 ***
## indx_dff                  0.787000   0.007617 103.320  < 2e-16 ***
## instr_methspeed:indx_dff -0.078700   0.010772  -7.306 2.78e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6631 on 99998 degrees of freedom
## Multiple R-squared:  0.1774,	Adjusted R-squared:  0.1774 
## F-statistic:  7189 on 3 and 99998 DF,  p-value: < 2.2e-16
```

```r
# Now, we set our model parameters - extracted from the linear model we just fit 
fixed <- c(summary(fit)$coef[1], summary(fit)$coef[2], summary(fit)$coef[3], summary(fit)$coef[4])
rand <- 0.1 # assign random effect intercept variance
res <- 0.2 # assign residual variance

# fit the baseline simulation model using our sample dataframe
model <- makeLmer(y ~ IM*ID + (1|PID), fixef = fixed, VarCorr = rand, sigma = res, data = df_sample)

# run simulation & compare to model without the interaction term
sim <- powerSim(model, nsim=100, test = fcompare(y~IM+ID))

# view power
sim
```

```
## Power for model comparison, (95% confidence interval):
##       10.00% ( 4.90, 17.62)
## 
## Test: Likelihood ratio
##       Comparison to y ~ IM + ID + [re]
## 
## Based on 100 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 30
## 
## Time elapsed: 0 h 0 m 11 s
```

### Simulate Again
#### Above, our power was quite low. In this next section, therefore, we vary the number of subjects and trials to determine the total number of observations needed for appropriate power. 

```r
# First, let's extend the number of participants to 20 instead of 10
model2 <- extend(model, along = 'PID', n=20)

# run the simulation
sim2 <- powerSim(model2, nsim = 500, test = fcompare(y~IM+ID))

# View power
sim2
```

```
## Power for model comparison, (95% confidence interval):
##        9.60% ( 7.16, 12.53)
## 
## Test: Likelihood ratio
##       Comparison to y ~ IM + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 60
## 
## Time elapsed: 0 h 0 m 53 s
```

```r
# Not quite. Let's also extend the number of trials for each index of difficulty for each subject 
model3 <- extend(model2, within = 'PID+IM+ID', n=40)

# run the simulation 
sim3 <- powerSim(model3, nsim=500, test = fcompare(y~IM+ID))

# view power
sim3
```

```
## Power for model comparison, (95% confidence interval):
##       98.00% (96.35, 99.04)
## 
## Test: Likelihood ratio
##       Comparison to y ~ IM + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 2400
## 
## Time elapsed: 0 h 1 m 17 s
```

```r
# fit power curve so we can visualize how power changes as the model is extended 
pc <- powerCurve(model3, test = fcompare(y~IM+ID), within = 'PID+IM+ID')

# plot the curve
plot(pc, xlab = 'Number_of_Observations')
```

![](Exp1_PowerAnalysis_files/figure-html/extend baseline model-1.png)<!-- -->

```r
# print values
print(pc)
```

```
## Power for model comparison, (95% confidence interval),
## by number of observations within PID+IM+ID:
##       3: 18.80% (16.42, 21.36) - 180 rows
##       7: 36.60% (33.61, 39.67) - 420 rows
##      11: 51.20% (48.05, 54.34) - 660 rows
##      15: 64.20% (61.14, 67.18) - 900 rows
##      19: 76.10% (73.33, 78.71) - 1140 rows
##      24: 85.20% (82.85, 87.34) - 1440 rows
##      28: 89.90% (87.86, 91.70) - 1680 rows
##      32: 93.80% (92.12, 95.21) - 1920 rows
##      36: 95.10% (93.57, 96.35) - 2160 rows
##      40: 96.90% (95.63, 97.88) - 2400 rows
## 
## Time elapsed: 0 h 15 m 42 s
```

```r
# ok, we need ~24 observations within pid, instruction method, and index of difficulty; let's look specifically at this number of observations 
model4 <- extend(model2, within = 'PID+IM+ID', n=24)

# power for interaction term
sim4 <- powerSim(model4, nsim=500, test = fcompare(y~IM+ID))
sim4
```

```
## Power for model comparison, (95% confidence interval):
##       79.60% (75.80, 83.05)
## 
## Test: Likelihood ratio
##       Comparison to y ~ IM + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 1440
## 
## Time elapsed: 0 h 1 m 7 s
```

```r
# power for main effect of index of difficulty
sim5 <- powerSim(model4, nsim=500, test = fcompare(y~IM))
sim5
```

```
## Power for model comparison, (95% confidence interval):
##       100.0% (99.26, 100.0)
## 
## Test: Likelihood ratio
##       Comparison to y ~ IM + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 1440
## 
## Time elapsed: 0 h 1 m 5 s
```

```r
# power for main effect of instruction method
sim6 <- powerSim(model4, nsim=500, test = fcompare(y~ID))
sim6
```

```
## Power for model comparison, (95% confidence interval):
##       83.80% (80.27, 86.92)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 1440
## 
## Time elapsed: 0 h 1 m 5 s
```
