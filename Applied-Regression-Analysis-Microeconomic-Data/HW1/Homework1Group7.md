# Homework 1 - Group 7

### Problem 1
#### Q1: Describe the incidence of treatment by experimental status. Is there substantial crossover? Are the other observable variables balanced between treatment and control groups? How would you test this?
  - Incidence_Treat = 64.86%; Incidence_Control = 34.38%
  - There is subtantial crossover in both groups:
    Crossover_Treat = 35.14%; Crossover_Control = 34.38%; Crossover_total = 34.89%
  - We compared means and standard deviations across the two groups, and differences are quite marginal. We also draw qqplots of Age and pre12ern variables, to compare the distributions across the two groups. Although there is an outlier of pre12ern in the treatment group, the quantiles locate quite near the 45 degree line. **Therefore, we think that the two groups are generally balanced**

|     | afdc_ra           | badenglh           | emp               | fdst_ra           | ged               | kids              | kidsud4           | longafdc          | married           | minor02           | minor03           | minor04            | neveradc          | neverful           | neverwrk          | nodegree          | single            | numinhh          | schlhgst         | agesq            | pre12ern         | pre12wrk          | bifid            | age              | caltime          | site             |
|-----|-------------------|--------------------|-------------------|-------------------|-------------------|-------------------|-------------------|-------------------|-------------------|-------------------|-------------------|--------------------|-------------------|--------------------|-------------------|-------------------|-------------------|------------------|------------------|------------------|------------------|-------------------|------------------|------------------|------------------|------------------|
| m_t | 0.455503512880562 | 0.0482191780821918 | 0.546442151004889 | 0.554433221099888 | 0.228260869565217 | 0.809551543389633 | 0.3451536643026   | 0.326519337016575 | 0.216232227488152 | 0.33623030961434  | 0.131450298750679 | 0.0363932645301467 | 0.398365122615804 | 0.0744160782183596 | 0.186854970124932 | 0.489253393665158 | 0.331161137440758 | 3.4404432132964  | 11.3617372182518 | 1231.93264530147 | 2865.20733788396 | 0.299265605875153 | 134247.452471483 | 34.3590439978273 | 26.4149918522542 | 8.6040195545899  |
| m_c | 0.480246913580247 | 0.0509049773755656 | 0.505039193729003 | 0.580941446613088 | 0.224076281287247 | 0.802902055622733 | 0.31358024691358  | 0.373725934314836 | 0.216867469879518 | 0.341545352743561 | 0.128779395296753 | 0.0246360582306831 | 0.38876404494382  | 0.0907054871220605 | 0.198208286674132 | 0.503488372093023 | 0.385542168674699 | 3.37643020594966 | 11.288           | 1257.63717805151 | 2558.93298059965 | 0.295454545454545 | 134164.860022396 | 34.5834266517357 | 26.5890257558791 | 8.60582306830907 |
| s_t | 0.498161980194573 | 0.214287774220195  | 0.497973718143295 | 0.49716770949112  | 0.419831727571147 | 0.392769256959299 | 0.475558907415065 | 0.469069313543691 | 0.411796443869607 | 0.472547121154349 | 0.337983974811721 | 0.187317521750126  | 0.489694837078362 | 0.262518112505641  | 0.389900970061507 | 0.500025927160062 | 0.470770360964521 | 1.80398174493184 | 1.71833715229916 | 770.07389808015  | 3633.40508328563 | 0.458076544334807 | 19425.9383429578 | 9.26252594661144 | 4.25101945732597 | 4.82657556249635 |
| s_c | 0.499918350091369 | 0.219928115554692  | 0.500254782202867 | 0.493688472684762 | 0.417221258925955 | 0.398047649249749 | 0.464234576488391 | 0.484066347371633 | 0.412360083989877 | 0.474493673275987 | 0.335143315741462 | 0.15510016551921   | 0.487743640423195 | 0.28735077145452   | 0.398873319711774 | 0.500278775466685 | 0.48701660147628  | 1.66465667878962 | 1.67612935389366 | 841.591740364842 | 3341.59832989055 | 0.456535123559829 | 19612.3700325372 | 9.81402370789192 | 4.27372990762483 | 4.7563092061623  |

![Age](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/Age_Comparison.png?raw=true)
![Earning](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/earning_comparison.png?raw=true)

```
setwd("D:\\Econometrics\\Program Evaluation\\Assignments\\HW1")
library(tidyverse)
df = read.csv('JTPA.csv', header = TRUE)
attach(df)

a = table(expstat, treated)
incidence_t = a[2,2]/sum(a[2,])
incidence_c = a[1,2]/sum(a[1,])
crossover_t = 1 - incidence_t
crossover_c = incidence_c
crossover_tot = 1 - sum(diag(a))/sum(a)
group_t = filter(df, expstat==1)
group_c = filter(df, expstat==0)
group_t_other = select(group_t, -c('expstat', 'treated'))
group_c_other = select(group_c, -c('expstat', 'treated'))
m_t = apply(group_t_other, 2, mean, na.rm = TRUE)
m_c = apply(group_c_other, 2, mean, na.rm = TRUE)
s_t = apply(group_t_other, 2, sd, na.rm = TRUE)
s_c = apply(group_c_other, 2, sd, na.rm = TRUE)
compare = rbind(m_t, m_c, s_t, s_c)
write.csv(compare, 'compare.csv')
# qqplots
age_t = group_t['age'][,1]; age_c = group_t['age'][,1]
pre4ern_t = group_t['pre12ern'][,1]; pre4ern_c = group_c['pre12ern'][,1]
qqplot(age_t, age_c, main = "Age Comparison")
qqplot(pre4ern_t, pre4ern_c, main = 'Earnings Comparison')
```

#### Q2: Estimate the Intent To Treat (ITT) parameter without covariates.
*Prediction Model*:

- ![e1](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/eqn1.png?raw=true)

Coefficients:

|             | Estimate | Std. Error | t-value | p-value    |
|-------------|----------|------------|---------|------------|
| (Intercept) | 0.50504  | 0.01669    | 30.262  | <2e-16 *** |
| expstat     | 0.04140  | 0.02034    | 2.036   | 0.0419 *   |

```
itt = lm(emp ~ expstat, data=df)
summary(itt)
```

#### Q3:Estimate the impact of treatment on the compliers (the Bloom estimator) without covariates.
Calculating the bloom estimator is essentially equal to calculating the LATE within the compliers. Our strategy is to extract the subset of compliers from the original dataset, and apply regression to estimate the treatment effect and standard error.

*Prediction Model*: 

- ![e2](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/eqn2.png?raw=true)

Coefficients:

|             | Estimate | Std. Error | t-value | p-value    |
|-------------|----------|------------|---------|------------|
| (Intercept) | 0.51024  | 0.02056    | 24.823  | <2e-16 *** |
| treated     | 0.05258  | 0.02510    | 2.095   | 0.0363 *   |

```
df_complier = filter(df, (expstat==1 & treated==1)|(expstat==0 & treated==0))
bloom = lm(emp ~ treated, data = df_complier)
summary(bloom)
```

#### Q4: The data contain numerous predetermined covariates. Re-estimate the ITT using these covariates. Re-estimate the Bloom estimator using these covariates. What happens to the standard errors of these estimates?

Since there are a plethora of covariates can serve the potential regressors, I deliberately select the following variables to re-execute the estimations:

- afdc_ra, badenglh, ged, kids, kidsud4, married, minor02, minor03, minor04, neverful, neverwrk, nodegree, numinhh, schlhgst, agesq, pre12ern, pre12wrk, age

With these additional variables

*Prediction Models*:

In the whole sample (AlwaysTakers + Compliers + NeverTakers, assuming for monotonicity):
- ![e3](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/eqn3.png?raw=true)

Coefficients after screening out insignificant variables:

|             | Estimate   | Std. Error | t-value | p-value      |
|-------------|------------|------------|---------|--------------|
| (intercept) | 4.906e-01  | 2.309e-01  | 2.125   | 0.033799 *   |
| expstat     | 4.586e-02  | 2.782e-02  | 1.648   | 0.099548 .   |
| afdc_ra     | -6.757e-02 | 2.937e-02  | -2.301  | 0.021542 *   |
| ged         | 6.250e-02  | 3.234e-02  | 1.933   | 0.053452 .   |
| kidsud4     | -1.000e-01 | 3.215e-02  | -3.112  | 0.001900 **  |
| numinhh     | 1.558e-02  | 8.820e-03  | 1.766   | 0.077545 .   |
| schlhgst    | 3.115e-02  | 8.446e-03  | 3.689   | 0.000234 *** |
| agesq       | 2.572e-04  | 1.430e-04  | 1.799   | 0.072240 .   |
| pre12ern    | 1.217e-05  | 4.467e-06  | 2.725   | 0.006512 **  |
| pre12wrk    | 9.792e-02  | 3.342e-02  | 2.930   | 0.003446 **  |
| age         | -1.998e-02 | 1.116e-02  | -1.791  | 0.073585 .   |

Within compliers:
- ![e4](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/eqn4.png?raw=true)

Coefficients after screening out insignificant variables:

|             | Estimate   |Std. Error  |t-value  | p-value      | 
|-------------|------------|------------|---------|--------------|
|(Intercept)  | 0.38545    |0.09173     |4.202    | 2.82e-05 *** |
|treated      |0.05901     |0.02796     |2.111    | 0.03497 *    |
|afdc_ra      |-0.11740    |0.02828     |-4.151   | 3.52e-05 *** |
|kidsud4      | -0.05897   | 0.02829    |-2.085   | 0.03728 *    |
|minor03      |-0.10866    |0.03957     |-2.746   | 0.00611 **   |
|schlhgst     |0.01585     |0.00767     |2.066    | 0.03905 *    |
|pre12wrk     |0.15555     |0.02988     |5.207    | 2.22e-07 *** |

Now, let us compare the results with Q2 and Q3:

- for ITT estimators: `0.04140 (0.02034) vs 0.04586 (0.02782)` The estimator and its estimated variance both increased slightly.

- for Bloom estimator: `0.05258 (0.02510) vs 0.05901 (0.02796)` The estimator and its estimated variance both increased slightly.

##### Q5: How might the procedure in question 4 be abused?

With other covariates included in the regression, problems of endogeneity might br introduced when some variables are correlated with unobserved variables in the error term. In this case, procedure in question 4 will biase the estimation of ITT and Bloom Estimand. With endogenous covariates included, we simply can't reach a clean estimate of the intended coefficients.

### Problem 2

##### Q1: 
For each individual, we can model their income in different states (with a terminal degree from college or high school):

- ![e5](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/eqn5.png?raw=true)

Their latent variable *D<sub>i</sub>* would take value 1 if the
individual chooses to attend college and 0 otherwise. We simulate 1000 observations according to this setup:

```
set.seed(1)
highSchool = rnorm(1000, 40000, 10000)
college = rnorm(1000, 50000, 20000)
df1 = data.frame(highSchool, college)
df1 = mutate(df1, decision = highSchool<college)
perc1 = sum(df1$decision)/1000
perc1
```

Based on our simulation, **66.1%** of the individuals decide to attend college.

##### Q2:

The naive estimator is the **mean difference** between college students' observed earnings and observed earnings of high school students.

```
df1 = mutate(df1, earn_obs = highSchool*(1-decision) + college*decision)
naive2 = mean(df1$earn_obs[df1$decision]) - mean(df1$earn_obs[!df1$decision])
naive2
```

Our naive estimator is **15013.89**

##### Q3:

Averagr treatment effect is the weighted average of ATT and ATN: 
- ATE = Pr(D<sub>i</sub> = 1)ATT + Pr(D<sub>i</sub> = 0)ATN

```
col = df1[df1$decision,]
high = df1[!df1$decision,]
ate = perc1 * mean(col$college - col$highSchool) + (1-perc1)*mean(high$college - high$highSchool)
ate
```

Our estimated ATE is **9791.243**
ATE estimation is lower than the naive. This is because the naive estimator measures the difference in earnings between those who actually went to college (their earnings are higher than otherwise as high school students) and those who didn’t. College students’ earnings are marked with variance big enough to generate ‘outliers’ who did earn a lot compared to high school students.

##### Q4:
ATT is the average treatment effct among college graduates, whereas ATN is of high school graduates.

```
att = mean(col$college - col$highSchool)
atn = mean(high$college - high$highSchool)
att
atn
```

Our estimated ATT is **22505.03**, ATN is **-14998.77**

Therofore, forcing those who only end up with high school degree would in fact decrease their earnings.

##### Q5:
Individuals' earnings are governed by the following distribution:
- ![e7](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/eqn5.png?raw=true)

- ![e8](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/eqn7.png?raw=true)

```
set.seed(5)
Sigma = matrix(c(10000^2,0.6*10000*20000,0.6*10000*20000,20000^2),2,2)
library(MASS)
vcts = mvrnorm(n = 1000, c(40000, 50000), Sigma)
highSchool2 = vcts[,1]
college2 = vcts[,2]
df2 = data.frame(highSchool2, college2)
df2 = mutate(df2, decision = highSchool2<college2)
perc2 = sum(df2$decision)/1000
perc2
df2 = mutate(df2, earn_obs = highSchool2*(1-decision) + college2*decision)
naive2 = mean(df2$earn_obs[df2$decision]) - mean(df2$earn_obs[!df2$decision])
naive2
col2 = df2[df2$decision,]
high2 = df2[!df2$decision,]
ate2 = perc2 * mean(col2$college2 - col2$highSchool2) + (1-perc2)*mean(high2$college2 - high2$highSchool2)
ate2
```
In this simulation, **74.5%** of the individuals choose to attend college.

The estimators:
- naive estimator: **20146.53**
- ATE: **10932.86**

##### Q6:

Individuals' earnings are governed by the following distribution:
- ![e10](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/eqn5.png?raw=true)

- ![e11](https://github.com/KenChenCompEcon/Applied-Regrassion-Analysis/blob/master/HW1/eqn9.png?raw=true)

```
set.seed(6)
Sigma1 = matrix(c(10000^2,-0.5*10000*20000,-0.5*10000*20000,20000^2),2,2)
vcts1 = mvrnorm(n = 1000, c(40000, 50000), Sigma1)
highSchool3 = vcts1[,1]
college3 = vcts1[,2]
df3 = data.frame(highSchool3, college3)
df3 = mutate(df3, decision = highSchool3<college3)
perc3 = sum(df3$decision)/1000
perc3
df3 = mutate(df3, earn_obs = highSchool3*(1-decision) + college3*decision)
naive3 = mean(df3$earn_obs[df3$decision]) - mean(df3$earn_obs[!df3$decision])
naive3
col3 = df3[df3$decision,]
high3 = df3[!df3$decision,]
ate3 = perc3 * mean(col3$college3 - col3$highSchool3) + (1-perc3)*mean(high3$college3 - high3$highSchool3)
ate3
```
In this simulation, **63.3%** of the individuals choose to attend college.

The estimators:
- naive estimator: **12541.83**
- ATE: **9353.764**

The naive estimator decreased compared to the results in question 5. The reason should intuitive: 
- In question 4, each individual's earning as a high school student or a college student is positively correlated, which means those whose choose to attend college would also earn well as high school students. In this case, it is the least productive individuals that choose to attend only high school. Their earnings are very polarized.
- In question 5, each individual's earning as a high school student or a college student is negaively correlated. Those who do well as high school students would not be those who would earn much as college students. The same is true for college students. The two groups both do well on their own, and that's why the naive estimator is of less magnitude.

The ATE should stay same, around 10000.



