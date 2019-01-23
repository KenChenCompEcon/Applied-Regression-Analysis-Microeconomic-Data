clear all

* import the dataset
import delimited "D:\Econometrics\Program Evaluation\HW\Applied-Regrassion-Analysis\HW3\usa_00002.csv"
set more off

// Preprocess the data
// split the race
gen race_white = (race==1)
gen race_black = (race == 2)
gen race_asian = (race==4 | race==5 | race==6)
keep if race_white==1
drop race_*
// split the hispanic status and keep with the non-hispanic population
gen hisp = (hispan!=0)
keep if hispan==0
drop hisp
// split the Met2013
gen msa = (met2013!=0)
label variable msa "1 for those living in MSA"
// split the education
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

// calculate propensity score
gen age2 = age*age
foreach v of varlist msa-edu_doctoral{
	foreach n of varlist `v'-edu_doctoral{
			gen inter_`v'`n' = `v'*`n'
	}
}
foreach v of varlist msa-edu_doctoral{
	gen inter_age`v' = age*`v'
	drop inter_`v'`v'
}
logit has_child age age2 edu_* msa inter_* [pweight=perwt]
predict pre_order2
// Q1

// Q2
forvalues i=1/_N{
	mat input mat`i' = (1, 1, 1, 1, 1)
}

// Q3
// construct IPW weight
gen IPW_att = (has_child==1)*perwt + (has_child==0)*perwt*pre_order2/(1-pre_order2)
reg uhrswork has_child [pweight=IPW_att],r 
reg urhswork has_child age age2 edu_* msa inter_* [pweight=IPW_att]
reg incearn has_child [pweight=IPW_att],r 
reg incwage has_child [pweight=IPW_att],r 

// Q4

// Q5
gen IPW_atn = (has_child==1)*perwt*(1-pre_order2)/pre_order2 + (has_child==0)*perwt
reg uhrswork has_child [pweight=IPW_atn],r 
reg incearn has_child [pweight=IPW_atn],r 
reg incwage has_child [pweight=IPW_atn],r 

gen IPW_ate = (has_child==1)*perwt/pre_order2 + (has_child==0)*perwt/(1-pre_order2)
reg uhrswork has_child [pweight=IPW_ate],r 
reg incearn has_child [pweight=IPW_ate],r 
reg incwage has_child [pweight=IPW_ate],r 
reg incwage has_child age age2 edu_* msa inter_* [pweight=IPW_ate]
