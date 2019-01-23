clear all

* import the dataset
import delimited "D:\Econometrics\Program Evaluation\HW\Applied-Regrassion-Analysis\HW2\usa_00002.csv"
set more off

// Preprocess the data
// split the race
gen race_white = (race==1)
gen race_black = (race==2)
gen race_other = (race==3 | race==7 | race==8| race==9)
gen race_asian = (race==4 | race==5 | race==6)
// split the hispanic status
gen hisp = (hispan!=0)
// split the Met2013
gen msa = (met2013!=0)
label variable msa "1 for those living in MSA"
// split the education
gen edu_blhigh = (educd<30)
gen edu_smhigh = (educd>=30 & educd<=61)
gen edu_ged = (educd==64)
gen edu_high = (educd==63)
gen edu_smcollege = (educd==65 | educd==71)
gen edu_asso = (educd==81)
gen edu_bachelor = (educd==101)
gen edu_master = (educd==114)
gen edu_prof = (educd==115)
gen edu_doctoral = (educd==116)
// variable if she has a child
gen has_child = (nchild > 0)

* Question 1
reg has_child age edu_* race_* hisp msa, r
predict pre_ols
scalar rmse_ols = e(rmse)
sum pre_ols

* Question 2
logit has_child age edu_* race_* hisp msa, vce(robust)
predict pre_logit
sum pre_logit
corr pre_ols pre_logit
// calculate root mean squared error
gen sse_logit = (has_child - pre_logit) * (has_child - pre_logit)
sum sse_logit
scalar rmse_logit = sqrt(r(sum)/_N) //adjust degree of freedom

* Question 3
egen group = group(age edu_* race_* hisp msa)
quietly tab group, gen(satu)
set matsize 2000
// with invariant cells included
quietly reg has_child satu*, r noconstant
predict pre_full 
count if pre_full==1
count if pre_full==0

* Question 4
// generate all second order terms
quietly gen age2 = age*age
foreach v of varlist race_white-edu_doctoral{
	foreach n of varlist `v'-edu_doctoral{
			quietly gen inter_`v'`n' = `v'*`n'
	}
}
foreach v of varlist race_white-edu_doctoral{
	quietly gen inter_age`v' = age*`v'
	quietly drop inter_`v'`v'
}

quietly logit has_child age age2 edu_* race_* hisp msa inter_*
predict pre_order2
corr pre_*
gen sse_order2 = (has_child - pre_order2) * (has_child - pre_order2)
quietly sum sse_order2
scalar rmse_order2 = sqrt(r(sum)/_N) //adjust degree of freedom

* Question 5
gen idx = floor(5*runiform() + 1)
forvalues i=1/5{
	quietly count if idx==`i'
	scalar obs`i' = r(N)
}
matrix input rmse_ols_oos = (0, 0, 0, 0, 0)
forvalues i=1/5{
	quietly reg has_child age edu_* race_* hisp msa if idx!= `i'
	predict pre_ols`i'
	gen sse_ols`i' = (has_child - pre_ols`i') * (has_child - pre_ols`i')
	quietly sum sse_ols`i' if idx==`i'
	mat rmse_ols_oos[1,`i'] = sqrt(r(sum)/obs`i') //adjust degree of freedom
}
mat list rmse_ols_oos
drop idx
// crossfold reg has_child age edu_* race_* hisp msa
gen idx = floor(5*runiform() + 1)
forvalues i=1/5{
	quietly count if idx==`i'
	scalar obs`i' = r(N)
}
matrix input rmse_logit_oos = (0, 0, 0, 0, 0)
forvalues i=1/5{
	quietly logit has_child age edu_* race_* hisp msa if idx!= `i'
	predict pre_logit`i'
	gen sse_logit`i' = (has_child - pre_logit`i') * (has_child - pre_logit`i')
	quietly sum sse_logit`i' if idx==`i'
	mat rmse_logit_oos[1,`i'] = sqrt(r(sum)/obs`i') //adjust degree of freedom
}
mat list rmse_logit_oos
drop idx
// crossfold logit has_child age edu_* race_* hisp msa [weight=perwt], eweight(perwt)
set seed 10 // we selected this seed because some randomization fail to give a converged model
gen idx = floor(5*runiform() + 1)
forvalues i=1/5{
	quietly count if idx==`i'
	scalar obs`i' = r(N)
}
matrix input rmse_order2_oos = (0, 0, 0, 0, 0)
forvalues i=1/5{
	quietly logit has_child age edu_* race_* hisp msa inter_* if idx!= `i'
	predict pre_order2`i'
	gen sse_order2`i' = (has_child - pre_order2`i') * (has_child - pre_order2`i')
	quietly sum sse_order2`i' if idx==`i'
	mat rmse_order2_oos[1,`i'] = sqrt(r(sum)/obs`i') //adjust degree of freedom
}
mat list rmse_order2_oos
//crossfold reg has_child age age2 edu_* race_* hisp msa inter_* [weight=perwt], eweight(perwt)

