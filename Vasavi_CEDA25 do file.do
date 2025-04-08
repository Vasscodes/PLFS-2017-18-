***NAME: VASAVI***
***CEDA25 STATA test***

cap log close
cls
clear all
eststo clear


cd "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\raw"


**Ques 1 (i)**


use level01_FV.dta, clear

//genrating within sub sample weights
gen weights = multiplier/100 if nss== nsc
replace weights = multiplier/200 if nss ~= nsc


//genrating a variable for weighted average consumption expenditure with within sub sample weights by states
egen avg_cons_exp = wtmean(household_cons_expenditure_month), by(state) weight(weights)

//sorting average consumption in descending order
gsort -avg_cons_exp

//plotting bar graph for the average consumption for each state 
twoway (bar avg_cons_exp state, barwidth(0.5)), xlabel(1(1)36, valuelabel angle(45) labsize(vsmall)) title("State-wise Avg. Consumption", size(medsmall)) ytitle("Average Household Consumption", size(small)) graphregion(color(white))

//storing it to export it to latex
graph export "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\graphs\average_consumption_expenditure.eps", replace 

save "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_weights", replace //saving the data with weights

*-------------------------------------------------------------

**Ques 1 (ii)**


use "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_weights", clear

//sorting the household consuption expenditure by month variable
sort household_cons_expenditure_month


//calculating the deciles of household consumption expenditure for households across India
xtile decile = household_cons_expenditure_month [weight=weights], n(10)

//summarising the minimum and maximum cutoffs for each decile in a detailed table
estpost tabstat household_cons_expenditure_month, by(decile) statistics(min max) columns(statistics)

esttab using "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\tex files\consumption_deciles.tex", cells("Deciles of avg_cons_exp min max") nomtitle nonumber //to export table output in latex file 

save "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_deciles.dta", replace //saving the existinf .dta file with deciles 
//-------------------------------------------------------------

**Ques 1 (iii)**

//using the individual-level data
use "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\raw/level02_FV.dta", clear 

//tabulating age to get more information about the variable
tab age

//keeping correspondants who lie in the age group 15-59
keep if age >= 15 & age <= 59

//sorting age in ascending order after eliminating unwanted correspondants
sort age


//tabulating sex variable to get more information 
tab sex

//keeping only 2 relevant sex groups male and female
keep if sex == 1 | sex == 2

/*generating a dummy variable employed for correspondants whose
personal status code is between 11 and 51 i.e. who are employed
in some relevant work*/
gen employed = pr_status_code >= 11 & pr_status_code <= 51

//weights for generating combined estimates
gen weights = multiplier/100 if nsc == nss
replace weights = multiplier/200 if nsc ~= nss

//for proportion of individuals employed segragating by sex 
mean employed [aw=weights], over(sex)

//storing the table in a matrix form 
matrix prop = r(table)'
matrix rownames prop = Male Female
matrix prop1 = prop[1..., 1] //to display only first column of the table

esttab matrix(prop1) using "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\tex files\proportion_employed.tex", replace title("Proportion of individuals Employed by Sex") varlabels(Male "Male" Female "Female") collabels("Proportion employed") nomtitles nonumber noobs //storing the table to export it to latex

eststo clear //clearing anything stored to store new estimation table 

/*Regressing employedon female dummy to test whether the employement proportion is
significantly different for males and females at 5% significance 
level*/
eststo: reg employed i.sex [aw=weights] //storing the regression table

esttab using "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\tex files\regression_prop.tex", replace label compress se mtitles("OLS 1") scalars(F) title(Table for question (iii)) r2 nogaps addnotes("OLS 1 checks for difference in proportion of female and male employment") //exporting the estimation table to latex
//------------------------------------------------------------------
 
**Ques 1 (iv)**

/*using the household-level data with deciles and keeping just 
common_id and deciles and saving in a new dta file*/
use "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_deciles.dta", clear 
keep common_id decile
save "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_commonid.dta", replace

* Merge with individual-level data using common_id
use "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\raw\level02_FV.dta", clear
merge m:1 common_id using "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_commonid.dta"

//to check if the desired columns are merged and dropping the estra variable
tab _merge
keep if _merge == 3
drop _merge

//tabulating to get more information about sex
tab sex

//keeping only females
keep if sex == 2

//generating employed dummy for principal status codes 11-51
gen employed = pr_status_code >= 11 & pr_status_code <= 51

//generating combined weights
gen weights = multiplier/100 if nsc == nss
replace weights = multiplier/200 if nsc ~= nss

//generating the proportion of female employment rate segragating based on household consumption deciles 
collapse (mean) employed [aw=weights], by(decile)


/*plotting graph for proportion of female employed by consumption deciles
where the scatter plots show the deciles while the line graphs show the employment rate*/
 twoway (scatter employed decile) (line employed decile), ytitle(Female Employment Rate) //y axis tittle name
 xtitle(consumption Deciles) //x axis title name
 title(Female Employment Rate (15-59) by Consumption Deciles) //graph title
 legend(order(1 "Deciles" 2 "mean(employed)")) //legend name
 
 graph export "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\graphs\female_employment_by_decile.eps", replace
 
 //------------------------------------------------------------------------------
 
 *Ques 1 (v)
 
 
use "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\raw\level02_FV.dta", clear

//generating employed dummy for principal status codes 11-51
gen employed = pr_status_code >= 11 & pr_status_code <= 51
 
//genrating total earnings by combining total earnings in a week for both activity 1 and 2 for all individuals
gen total_earning_regular_wage = earning_regular_wage_1 + earning_regular_wage_2
 
//generating the no of days worked by all individuals
gen total_days_worked = 0
foreach day in 1stday 2ndday 3rdday 4thday 5thday 6thday 7thday {
replace total_days_worked = total_days_worked + (tot_hrs_`day' > 0 & tot_hrs_`day' != .)
}

//generating daily wage rate for individuals with salaried employment with principal status code 31
gen salaried_daily_wage = .
replace salaried_daily_wage = total_earning_regular_wage / total_days_worked if pr_status_code == 31 & total_days_worked > 0

//generating daily wage rate for individuals with casual employement with principal status codes 41, 42 and 51
gen casual_daily_wage = .
replace casual_daily_wage = total_earning_regular_wage / total_days_worked if inlist(pr_status_code, 41, 42, 51) & total_days_worked > 0

//generating daily wage rate for self employed individuals with principal codes 11, 12 and 21
gen selfemp_daily_wage = .
replace selfemp_daily_wage = total_earning_regular_wage / total_days_worked if inlist(pr_status_code, 11, 12, 21) & total_days_worked > 0

//-------------------------------------------------------------------------------

*Ques 1 (vi)

//generating overall daily wage rate combining all 3 wage rates in Ques 1 (v)
gen overall_daily_wage = .
replace overall_daily_wage = salaried_daily_wage if !missing(salaried_daily_wage)
replace overall_daily_wage = casual_daily_wage if missing(overall_daily_wage) & !missing(casual_daily_wage)
replace overall_daily_wage = selfemp_daily_wage if missing(overall_daily_wage) & !missing(selfemp_daily_wage)
save "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level02_FV_wage.dta", replace

//using household level data with deciles and saving just common_id and social group variables in a new dataset
use "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_deciles.dta"
keep common_id social_group
save "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01FV_social_group.dta", replace 

//merging individual level data with the new dataset containg common_id and social group variables
use "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level02_FV_wage.dta", clear
merge m:1 common_id using "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01FV_social_group.dta"

//to check if all the columns are merged and dropping the extra variable
tab _merge
drop _merge

//generating weights for combines estimates
gen weights = multiplier/100 if nsc == nss
replace weights = multiplier/200 if nsc ~= nss

//plotting graph for average overall wage rate segragating by caste or social groups
graph bar (mean) overall_daily_wage [aw=weights], by(social_group)

graph export "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\graphs\daily_wage_by_social_group.eps", replace

save "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level02_FV_wage.dta", replace

//-------------------------------------------------------------------------------

*Ques 1 (vii)

//merging month of survey and date of survey from household level data to individual level dataset
use "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_deciles.dta", clear 
keep common_id month_survey survey_date districtcode
save "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_districtcode.dta", replace

//Merge with individual-level data using common_id
use "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level02_FV_wage.dta", clear
merge m:1 common_id using "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level01_FV_districtcode.dta"

//check if all the sataset is merged and drop _merge variable 
tab _merge
keep if _merge == 3
drop _merge

tab sex 

//keep only males and females 
keep if sex == 1 | sex == 2

//keeping only working age population i.e. individuals who are in 15-59 category
keep if age >= 15 & age <= 59

//generate a variable for log daily wages
gen log_daily_wage = log(overall_daily_wage) 

eststo clear


//storing the regression results to export to latex
eststo: reghdfe log_daily_wage i.sex i.education_level age [aw=weights], absorb(districtcode pr_nco_code month_survey)

//exporting the regression result to latex using tex file
esttab using "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\tex files\gender_gap_estimation.tex", replace label compress se mtitles("OLS 1") scalars(F) title(Table for question (vii)) r2 nogaps addnotes("OLS 1 checks for difference in log of daily wage rate between men and women") 

save "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level02_FV_wage.dta", replace
//--------------------------------------------------------------------------------------------------------------------


*Ques 1 (viii)

eststo clear //clearing to discard previously stored data 

sum salaried_daily_wage //tabulating salaried daily wage variable to look for outliers 

gen log_salaried_daily_wage = log(salaried_daily_wage) //taking the log of salaried wage for estimating regression

keep if age >= 15 & age <=59 //keeping individuals in working age group (15-59)

gen post_policy = (survey_date > 20180100) if survey_date < . //generating dummies for individuals surveyed after January 2018 assigning 1 for such individuals and 0 otherwise

//estimating regression while controlling for district, occupation and month level and also age and education and storing it
eststo: reghdfe log_salaried_daily_wage i.sex i.sex##c.post_policy post_policy i.education_level age [aw=weights], absorb(districtcode pr_nco_code month_survey)

//for presenting the rregression in a latex file 
esttab using "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\tex files\log_daily_wage_estimation.tex", replace label compress se mtitles("OLS 1") scalars(F) title(Table for question (viii)) r2 nogaps addnotes("OLS 1 checks whether the policy introduced has any change in gender gap in wages of salaried individuals aged 15-59") 

save "C:\Users\vasav\Desktop\Vasavi_test_CEDA25\PLFS17_18\data\used data\level02_FV_wage.dta", replace
//----------------------------------------------------------------------------------------------------------------------









