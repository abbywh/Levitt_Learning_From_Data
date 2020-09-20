/*******************************************************************************
*	File name:		pset1_cleaning_answers.do
*  	Author(s):		Joel Whittier, Anya Marchenko (Template)
*  	Written:       	12/30/2018
*
*	Inputs:			fatalities_data_1997-2017.csv, 
*	                marijuana_legalization_status.csv
*	Outputs:		blueprint_data_cleaned.dta
*	
*	Description: 	Cleans and analyzes the Marijuana data to answer questions for
*					Pset 2 of Levitt Data Class
*
*	1. The description should say what the goal of the Do-file is. 
*
*	2. Make your code legible, not just functional. 
*		a) Indent your loops, conditionals, preserve/restores, other code chunks
*		b) Line up your punctuation: braces ("{" and "}"), equal signs ("="), 
*		colons (":"), quote marks (`" "'), etc.
*		c) Use "///" to break up lines of code that extend beyond the line break
*		d) Make clear section headers
*		e) Comment on everything that is not inherently clear to a first-time 
*		reader! Comments should explain why not how. 
*
* 
*******************************************************************************/

 
**********************
// Preliminaries
**********************

version 15.0 	// tells future Stata versions that this .do file is written in 15.1
clear all 		// clears errything 
set more off 	// tells stata not to pause when "more" message displayed in console
ssc install estout, replace


**********************
// Import Data
**********************

cd "C:\Users\jdw23\Downloads\econ_213_pset_2" 
import delimited using marijuana_legalization_status.csv, clear
cd "C:\Users\jdw23\Downloads\econ_213_pset_2"


********************************************************************************
*********                      Question 2: Data in the Real World     **********
********************************************************************************

**********************
// First Graph
**********************

//We generate the number of legal medically
sort(medical_legalization_year)

generate number_legal_med=sum(medical_legalization_year!=0)
replace number_legal_med=1+ number_legal_med[_n-1] if _n>1 ///
	& number_legal_med[_n]!=0



	
//Now generate number of legal treatment
sort(treatment_legalization_year)

generate number_legal_treatment=sum(treatment_legalization_year!=0)
replace number_legal_treatment=1+ number_legal_treatment[_n-1] if _n>1 ///
	& number_legal_treatment[_n] !=0

//keep only the maximum per year
bysort medical_legalization_year: egen t = max(number_legal_med)
bysort treatment_legalization_year: egen f = max(number_legal_treatment)

sort(medical_legalization_year)

*twoway line t medical_legalization_year if ///
*	(medical_legalization_year!=0) || line f ///
*	treatment_legalization_year if (treatment_legalization_year!=0), connect(ascending)

	
**********************
// Second Graph
**********************
//Now we do some data fixing

cd "C:\Users\jdw23\Downloads\econ_213_pset_2" 
import delimited using fatalities_data_1997-2017.csv, clear
cd "C:\Users\jdw23\Downloads\econ_213_pset_2"


drop v1
rename (washington-rhode_island) (death#), addnumber

reshape long death, i(year) j(v1)

merge m:1 v1 using marijuana_legalization_status.dta

drop v1
drop _merge
sort(state_names)

generate med_now = (year >= medical_legalization_year & medical_legalization_year !=0)
generate treatment_now = (year >= treatment_legalization_year & treatment_legalization_year !=0)

preserve
collapse (sum) death, by(year control)

*twoway line death year if (control == "TRUE") || line death year ///
*	if (control =="FALSE"), saving(control)

collapse (sum) death, by(year)

*twoway line death year, saving(tot)

*gr combine control.gph tot.gph, ycommon
restore 
**********************
// Third Graph
**********************


table state_name if year+1 == treatment_legalization_year & state_name != "Vermont", c(mean death)

table state_name if year == treatment_legalization_year, c(mean death)

********************************************************************************
*********                      Question 3: Running A Regression     **********
********************************************************************************


//Model deaths = lagged_deaths + control_state+year_before_2014+year_after_2014
//+treatment_weight*treatment_legal+med_legal*weight

//generate the year before and after

gen before_2014 = year < 2014
gen after_2014 = year >=2014
gen treat_weight_now = (treatment_legalization_weights*treatment_now)
gen med_weight_now = (medical_legalization_weights*med_now)

gen control_d = control == "TRUE"

sort state year

by state: gen lagged_deaths = death[_n -1]

eststo: regress death lagged_deaths control_d after_2014 ///
 treat_weight_now ///
 med_weight_now if year!=1997
 
esttab using principal.tex, label nostar title(Principal Regression\label{tab2})
 ********************************************************************************
*********                      Question 4: Running A Regression     **********
********************************************************************************
eststo: regress death lagged_deaths control_d ///
 treat_weight_now ///
 med_weight_now if year!=1997

eststo: regress death lagged_deaths control_d after_2014 ///
 treat_weight_now ///
 med_weight_now if year!=1997
 
eststo: regress death lagged_deaths control_d after_2014 ///
 treat_weight_now ///
 med_weight_now if year!=1997 & control_d == 1

eststo: regress death lagged_deaths control_d after_2014 ///
 treat_weight_now ///
 med_weight_now if year!=1997 & control_d == 0

eststo: regress death control_d before_2014 after_2014 ///
 treat_weight_now ///
 med_weight_now if year!=1997 
 
esttab using sens.tex, label nostar title(Sensitivity table\label{tab3})
