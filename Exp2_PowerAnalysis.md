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
index <- c(0.874, 0.7, 0.585, 0.503) # four indexes of difficulty
ss <- 999996 # create a sample size for our population
angle2 <- rep(angle, each = ss/3) # repeat instruction methods 
index2 <- rep(rep(index, each = ss/12), 3) # repeat index of difficulty

# concatenate into a dataframe 
df_pop <- data.frame(corner_angle=factor(angle2), indx_dff=index2)
```

### Estimate Cornering Times
#### In this section, we create the estimated cornering times. We will do this using Pastel et al.'s (2007) fitted model (plus some variability). These will serve as the cornering times for the 90-degree corners, as this model was developed using 90-degree corners.
#### We will then do the same for the different corner angles. We will increase and decrease cornering times by 20% for the 135-degree and 45-degree corner angles, respectively. We do this because it is expected that cornering time will be affected by corner angle. 


```r
# define error term to induce some variability in our estimates 
error_term <- rnorm(ss/12, mean=0, sd = 0.7) 

# use Pastel et al.'s (2007) fitted model to get estimated cornering times for each index of difficulty & corner angle

# 45 degrees
Y1 <- (0.975 + .787*0.874 + error_term)*1.2
Y2 <- (0.975 + .787*0.7 + error_term)*1.2
Y3 <- (0.975 + .787*0.585 + error_term)*1.2
Y4 <- (0.975 + .787*0.503 + error_term)*1.2

# 90 degrees
Y5 <- (0.975 + .787*0.874 + error_term)
Y6 <- (0.975 + .787*0.7 + error_term)
Y7 <- (0.975 + .787*0.585 + error_term)
Y8 <- (0.975 + .787*0.503 + error_term)

# 135 degrees
Y9 <- (0.975 + .787*0.874 + error_term)*.8
Y10 <- (0.975 + .787*0.7 + error_term)*.8
Y11 <- (0.975 + .787*0.585 + error_term)*.8
Y12 <- (0.975 + .787*0.503 + error_term)*.8

# finally, we append to the population dataframe
df_pop$ct <- c(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12)

# then, we create a sample of this population
pid <- factor(1:5) # five subjects
PID <- rep(pid, each = 12) # repeat participant ID 15 times each
ANGLE <- rep(rep(angle, each = 4), 5) # repeat each angle 5 times and do that 5 times
ID <- rep(index, 15) # repeat index of difficulty values 15 times

# concatenate into a dataframe
df_sample <- data.frame(PID=PID, ANGLE=factor(ANGLE), ID=ID)

# get cornering times from our population
df_sample$ct <- matrix(replicate(5, c(sample(Y1, 1, replace = T), sample(Y2, 1, replace = T), 
                                      sample(Y3, 1, replace = T), sample(Y4, 1, replace = T), 
                                      sample(Y5, 1, replace = T), sample(Y6, 1, replace = T),
                                      sample(Y7, 1, replace = T), sample(Y8, 1, replace = T),
                                      sample(Y9, 1, replace = T), sample(Y10, 1, replace = T),
                                      sample(Y11, 1, replace = T), sample(Y12, 1, replace = T))), ncol=1)

# view dataframe
head(df_sample, n = 12)
```

```
##    PID ANGLE    ID         ct
## 1    1    45 0.874 2.24967910
## 2    1    45 0.700 2.10686662
## 3    1    45 0.585 1.48603257
## 4    1    45 0.503 1.28669829
## 5    1    90 0.874 1.24721615
## 6    1    90 0.700 2.47470157
## 7    1    90 0.585 2.12831138
## 8    1    90 0.503 0.07213496
## 9    1   135 0.874 0.89148315
## 10   1   135 0.700 1.31352141
## 11   1   135 0.585 1.83804649
## 12   1   135 0.503 1.78153004
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
# set simr progress to FALSE
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
## -3.4812 -0.4578 -0.0010  0.4611  3.4328 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               1.173349   0.005981  196.18   <2e-16 ***
## corner_angle90           -0.195558   0.008458  -23.12   <2e-16 ***
## corner_angle135          -0.391116   0.008458  -46.24   <2e-16 ***
## indx_dff                  0.944400   0.008797  107.36   <2e-16 ***
## corner_angle90:indx_dff  -0.157400   0.012440  -12.65   <2e-16 ***
## corner_angle135:indx_dff -0.314800   0.012440  -25.30   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7071 on 999990 degrees of freedom
## Multiple R-squared:  0.1266,	Adjusted R-squared:  0.1265 
## F-statistic: 2.898e+04 on 5 and 999990 DF,  p-value: < 2.2e-16
```

```r
# Now, we set our model parameters - extracted from the linear model we just fit 
fixed <- c(summary(fit)$coef[1], summary(fit)$coef[2], summary(fit)$coef[3],
           summary(fit)$coef[4], summary(fit)$coef[5], summary(fit)$coef[6])
rand <- 0.1 # assign random effect intercept variance
res <- 0.3 # assign residual variance

# fit the baseline simulation model using our sample dataframe
model <- makeLmer(y ~ ANGLE*ID + (1|PID), fixef = fixed, VarCorr = rand, sigma = res, data = df_sample)

# run simulation & compare to model without the interaction term
sim <- powerSim(model, nsim=500, test = fcompare(y~ANGLE+ID))
```

```
## boundary (singular) fit: see ?isSingular
```

```
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
##        7.80% ( 5.61, 10.51)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ANGLE + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 60
## 
## Time elapsed: 0 h 1 m 7 s
```


```r
# First, let's extend the number of participants to 42 instead of 5
model2 <- extend(model, along = 'PID', n=42)

# run the simulation
sim2 <- powerSim(model2, nsim=500, test = fcompare(y~ANGLE+ID))

# View power
sim2
```

```
## Power for model comparison, (95% confidence interval):
##       24.00% (20.32, 27.99)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ANGLE + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 504
## 
## Time elapsed: 0 h 1 m 18 s
```

```r
# Let's also extend the number of trials for each index of difficulty for each subject 
model3 <- extend(model2, within = 'PID+ANGLE+ID', n=10)

# run the simulation 
sim3 <- powerSim(model3, nsim=500, test = fcompare(y~ANGLE+ID))

# view power
sim3
```

```
## Power for model comparison, (95% confidence interval):
##       97.60% (95.85, 98.75)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ANGLE + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 5040
## 
## Time elapsed: 0 h 2 m 20 s
```

```r
# fit power curve so we can visualize how power changes as the model is extended 
pc <- powerCurve(model3, test = fcompare(y~ANGLE+ID), within = 'PID+ANGLE+ID')

# plot the curve
plot(pc, xlab='Number of Observations per Subject, Index of Difficulty, & Corner Angle')
```

![](Exp2_PowerAnalysis_files/figure-html/extend baseline model-1.png)<!-- -->

```r
# print values
print(pc)
```

```
## Power for model comparison, (95% confidence interval),
## by number of observations within PID+ANGLE+ID:
##       3: 54.50% (51.35, 57.62) - 1512 rows
##       4: 67.80% (64.80, 70.69) - 2016 rows
##       5: 78.00% (75.30, 80.53) - 2520 rows
##       6: 83.20% (80.74, 85.47) - 3024 rows
##       7: 88.30% (86.14, 90.23) - 3528 rows
##       8: 92.10% (90.25, 93.70) - 4032 rows
##       9: 95.20% (93.69, 96.44) - 4536 rows
##      10: 97.30% (96.10, 98.21) - 5040 rows
## 
## Time elapsed: 0 h 24 m 24 s
```

```r
# ok, we need ~6 observations within pid, instruction method, and index of difficulty; let's look specifically at this number of observations 
model4 <- extend(model2, within = 'PID+ANGLE+ID', n=6)

# power for interaction term
sim4 <- powerSim(model4, nsim=500, test = fcompare(y~ANGLE+ID))
sim4
```

```
## Power for model comparison, (95% confidence interval):
##       83.60% (80.06, 86.74)
## 
## Test: Likelihood ratio
##       Comparison to y ~ ANGLE + ID + [re]
## 
## Based on 500 simulations, (0 warnings, 0 errors)
## alpha = 0.05, nrow = 3024
## 
## Time elapsed: 0 h 2 m 2 s
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
## alpha = 0.05, nrow = 3024
## 
## Time elapsed: 0 h 2 m 2 s
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
## alpha = 0.05, nrow = 3024
## 
## Time elapsed: 0 h 2 m 26 s
```

