setwd("D:\\Econometrics\\Program Evaluation\\Assignments\\HW1")
library(tidyverse)
df = read.csv('JTPA.csv', header = TRUE)
attach(df)

# Problem 1
# Q1
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

#Q2
itt = lm(emp ~ expstat, data=df)
summary(itt)

#Q3
itt_coe = 0.04140296
bloom_coe = itt_coe/(incidence_t - incidence_c)
bloom_coe
bloom_sd = 0.02034/(incidence_t - incidence_c)
bloom_sd

#Q4
# Re-estimate the coefficients using all the covariates
itt1 = lm(emp ~ expstat +., data = df)
summary(itt1)
bloom1_coe = 0.04594/(incidence_t - incidence_c)
bloom1_sd = 3.081e-02/(incidence_t - incidence_c)
bloom1_coe
bloom1_sd

# Problem 2
# Q1
highSchool = rnorm(1000, 40000, 10000)
college = rnorm(1000, 50000, 20000)
df1 = data.frame(highSchool, college)
df1 = mutate(df1, decision = highSchool<college)
perc1 = sum(df1$decision)/1000
perc1

# Q2
df1 = mutate(df1, earn_obs = highSchool*(1-decision) + college*decision)
naive2 = mean(df1$earn_obs[df1$decision]) - mean(df1$earn_obs[!df1$decision])
naive2

# Q3
col = df1[df1$decision,]
high = df1[!df1$decision,]
ate = perc1 * mean(col$college - col$highSchool) + (1-perc1)*mean(high$college - high$highSchool)
ate

# Q4
att = mean(col$college - col$highSchool)
atn = mean(high$college - high$highSchool)
att
atn

# Q5
highSchool2 = rnorm(1000, 40000, 10000)
college2 = 50000 + 1.2*(highSchool2-40000) + sqrt(4-1.44)*rnorm(1000, 0, 10000)
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

# Q6
highSchool3 = rnorm(1000, 40000, 10000)
college3 = 50000 - (highSchool3-40000) + sqrt(3)*rnorm(1000, 0, 10000)
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