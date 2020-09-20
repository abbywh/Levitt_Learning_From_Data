/*******************************************************************************
*	File name:		pset1_cleaning_answers.do
*  	Author(s):		Joel Whittier, Anya Marchenko (Template)
*  	Written:       	12/30/2018
*
*	Inputs:			student_data.csv
*	
*	Outputs:		blueprint_data_cleaned.dta
*	
*	Description: 	Cleans the fake raw student data to answer questions for
*					Pset 1 of Levitt Data Class
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

cd "C:\Users\jdw23\Downloads" 
import delimited using student_data.csv, clear
cd "C:\Users\jdw23\Downloads"


********************************************************************************
*********                      Question 0: Naive Reg                  **********
********************************************************************************

* Your cleaning and regression code here

						  

**********************
// Re-import
**********************
cd "C:\Users\jdw23\Downloads"
import delimited using student_data.csv, clear
cd "C:\Users\jdw23\Downloads"	  

//Destring the exam scores

destring exam_score_normalized lagged_exam_score_normalized, replace ignore("NA")


//Now we control for if they recieved the treatment and see if it increased
//their scores
			
eststo: regress exam_score_normalized lagged_exam_score_normalized if treated == "No"

eststo: regress exam_score_normalized lagged_exam_score_normalized if treated == "Yes"		

esttab using 0noc.tex, label nostar title(No Controls table\label{tab1})

eststo clear
//Control according to 0.b

generate free_lunch_d = (free_lunch == "Yes")

generate two_parents_d = (two_parent == "Yes")

eststo: regress exam_score_normalized lagged_exam_score_normalized free_lunch_d ///
        two_parents_d if treated == "No"
		
eststo: regress exam_score_normalized lagged_exam_score_normalized free_lunch_d ///
        two_parents_d if treated == "Yes"

esttab using 0c.tex, label nostar title(Controlled table\label{tab1})

eststo clear
********************************************************************************
*********                      Question 1:                            **********
********************************************************************************

* Your cleaning and regression code here

//Re-import
import delimited using student_data.csv, clear

//Add the cleaning we did in question 0 back
destring exam_score_normalized lagged_exam_score_normalized, replace ignore("NA")

//Clean according to steps outlined in doc
replace school = subinstr(school," ","",.)
replace school = subinstr(school,".","",.)

replace school = proper(school)



//generate dummies
generate exam_missing = missing(exam_score_normalized)

generate lagged_missing = missing(lagged_exam_score_normalized)

generate free_lunch_d = (free_lunch == "Yes")

generate two_parent_d = (two_parent == "Yes")

generate sex_D = (two_parent == "Yes")

//set missing to 0 for convenience 
replace exam_score_normalized = 0 if missing(exam_score_normalized)

replace lagged_exam_score_normalized = 0 if missing(lagged_exam_score_normalized)

//Apply filters
drop if (exam_missing == 1 & lagged_missing == 1)

drop if abs(exam_score_normalized > 3) | abs(lagged_exam_score_normalized > 3)

drop if abs(exam_score_normalized- lagged_exam_score_normalized) > 2.5


********************************************************************************
*********                      Question 2:                            **********
********************************************************************************
eststo: regress exam_score_normalized lagged_exam_score_normalized if treated == "No"
		
eststo: regress exam_score_normalized lagged_exam_score_normalized if treated == "Yes"

esttab using 2noc.tex, label nostar title(No Controls\label{tab1})

eststo clear

//Control according to 0.b

eststo: regress exam_score_normalized lagged_exam_score_normalized free_lunch_d ///
        two_parent_d exam_missing lagged_missing if treated == "No"
		
eststo: regress exam_score_normalized lagged_exam_score_normalized free_lunch_d ///
        two_parent_d exam_missing lagged_missing if treated == "Yes"

esttab using 2c.tex, label nostar title(Controlled\label{tab1})

eststo clear

********************************************************************************
*********                      Question 3:                            **********
********************************************************************************
//Let's start by checking the means of the column. Theoretically, they should 
//Average to 0 in both groups


tab treated, sum(lagged_exam_score_normalized)

sdtest lagged_exam_score_normalized, by(treated)
 
//Cenetered around 0 so lagged score seeems randomized
//Test to see if school, classroom were randomized

est: tabulate school treated, chi2 expected

esttab using expected.tex, label nostar title (School vs Treated Frequencies\label{tabl})

eststo clear 

tabulate classroom treated, chi2 expected 

tabulate sex treated, chi2 expected

tabulate race treated, chi2 expected
********************************************************************************
*********                      Question 4:                            **********
********************************************************************************

eststo: regress exam_score_normalized lagged_exam_score_normalized free_lunch_d ///
        two_parent_d exam_missing lagged_missing if treated == "No", ///
		vce(bootstrap, rep(120) seed(1) strata(school)) 
		
eststo: regress exam_score_normalized lagged_exam_score_normalized free_lunch_d ///
        two_parent_d exam_missing lagged_missing if treated == "Yes", ///
		vce(bootstrap, rep(120) seed(2) strata(school) )

esttab using 4c.tex, label nostar title(Controlled\label{tab1})

eststo clear


