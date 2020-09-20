/*******************************************************************************
*	
*   File name:		pset_3_random_forest.do
*  	Author(s):		Joel Whittier, Nagisa Tadjfar
*  	Written:       	2/26/2018
*
*	Inputs:			Airbnb Dataset
*	
*	Outputs:		Predicted "review_scores_rating" variables
*	
*	Description: 	Useful commands for cleaning data, running random forest
*					
*******************************************************************************/
**********************
// Very helpful resource:
// https://www.stata.com/meeting/canada18/slides/canada18_Zou.pdf
**********************
  
**********************
// Loading Data 
**********************

version 15.0 	// tells future Stata versions that this .do file is written in 15.1
clear all 		// clears errything 
set more off 	// tells stata not to pause when "more" message displayed in console
cap log close 	// closes any previous log files

import delimited "C:\Users\jdw23\Downloads\ps3data", clear
//keep id review_scores_rating review_scores_rating weekly_price sex age square_feet bed_type room_type indicator // keep variables you think would impact the outcome variable
rename indicator data_class // rename variables if helpful (1 = training data, 2 = testing data, 3 = testing data with higher prices)

//Missing dependent variable

drop if data_class ==1 & number_of_reviews==0

/* At this point you should check your data to see if everything makes sense and clean if necessary */

replace bathrooms = abs(bathrooms)
replace bedrooms = abs(bedrooms)
replace beds = abs(beds)

//create groups
egen bed_type_cat = group(bed_type) // create categorical variables if necessary
egen room_type_cat = group(room_type)
egen raceg = group(race)
egen ageg = group(age)
egen picg = group(host_has_profile_pic)
egen time = group(group_host_response_time)
egen year = group(first_review_year)
egen cancel = group(cancellation_policy)
gen zip_di = floor(zipcode/10000) // this tells us the generic region
egen zip_dig = group(zip_di)

replace time = time+1 if (first_review_month >=7 & time ==2)
replace time = time-1 if (first_review_month <7 & time ==2)


replace cancel = 5 if cancel ==2 | cancel==4 | cancel ==6 | cancel == 7
//Now lets make some new statistsics


gen white = raceg == 6
gen old = ageg == 2

gen bpr = beds/bedroom

gen nver = length(host_verifications) - length(subinstr(host_verifications, ",", "", .))
gen namen = length(amenities) - length(subinstr(amenities, ",", "", .))
gen rules = 1 if house_rules=="NA"
replace rules = 0 if missing(rules)
gen bad_acom = 1 if accommodates > 2*beds | accommodates > 3*bathrooms
replace bad_acom = 0 if missing(bad_acom)

gen bad_trans = 1 if missing(transit)
replace bad_trans = 0 if missing(bad_trans)

gen no_int = 1 if interaction == "NA"
replace no_int = 0 if missing(no_int)


gen no_sum = 1 if missing(summary)
replace no_sum=0 if missing(no_sum)

gen descrip =length(description) - length(subinstr(amenities, " ", "", .))
gen bigword = length(description)/descrip

replace descrip = log(descrip)
 
//drop missing dependent variables
drop if missing(review_scores_rating) & data_class == 1 // cannot have missing dependent variable in training data
sort data_class id
gen cavail = has_availability == "t" 

gen lprice =  log(price)
gen lnum = log(number_of_reviews)


gen num = 1 if number_of_reviews < 2
replace num = 0 if missing(num)

gen day = 1 if strpos(calendar_updated,"day") | strpos(calendar_updated, "Day")
replace day =0 if missing(day)
gen week= 1 if strpos(calendar_updated,"Week") | strpos(calendar_updated, "week")
replace week =0 if missing(week)
gen month= 1 if strpos(calendar_updated,"Month") | strpos(calendar_updated, "month")
replace month =0 if missing(month)

gen lextra_people = log(extra_people)

gen lhost_response_rate =  log(host_response_rate+1)

egen stateg = group(state)
replace stateg = 3 if stateg ==4
replace stateg = 7 if stateg ==6
replace stateg = 7 if stateg ==8
replace stateg = 7 if stateg ==9
replace stateg = 7 if stateg ==12


gen house = 1 if property_type=="House"
replace house = 0 if missing(house)

gen apt = 1 if property_type=="Apartment"
replace apt = 0 if missing(apt)

gen other = 1 if apt == 0 & house == 0
replace other = 0 if missing(other)

gen maxnights = log(maximum_nights+1)

gen has_space = 1 if !missing(space)
replace has_space = 0 if missing(space)

gen smoke = 1 if strpos(space,"smoke") | strpos(space,"Smoke") ///
| strpos(house_rules,"smoke") | strpos(house_rules,"Smoke") ///
| strpos(space,"SMOKE") | strpos(house_rules,"SMOKE") ///
| strpos(space,"smoking") | strpos(space,"Smoking") ///
| strpos(house_rules,"smoking") | strpos(house_rules,"Smoking") ///
| strpos(space,"SMOKING") | strpos(house_rules,"SMOKING") 
replace smoke = 0 if missing(smoke)

gen clean = 1 if strpos(description, "clean") | strpos(description, "Clean")

//security_deposit cleaning_fee has pic price

gen lbeds= log(beds+1)

gen bad_host = 1 if host_response_rate==0
replace bad_host = 0 if missing(bad_host)
**********************
// 1.1
**********************
//We want to star by classifying the serious and not so serious airbnbs

//some formula of acccomadates bedroom bathroom?
//availability?, guest included, extra people 
//require profile picture phone verification 
//Use NLP to detect real shitty listings

regress review_scores_rating old i.room_type_cat /// 
	i.cancel lhost_response_rate host_listings_count ///
	host_is_superhost availability_30 nver  ///
	bad_acom reviews_per_month descrip bad_trans white lprice ///
	guests_included instant_bookable ///
	i.stateg lbeds apt maxnights smoke
//Now lets check this model
//Obviously check in sample
//then check distribution

replace lnum = 0 if missing(lnum)
replace descrip=0 if missing(descrip)

//Oh shit boys this is scewed as hell
//replace rating = 100 - 2.718281828459^(rating)
**********************
// Let's validate some models, boys!
**********************


**********************
// Install the necessary random forest plug-ins
**********************

ssc install KFOLDCLASS, replace 
ssc install randomforest, replace
// Type "help randomforest" in your command line and click the link that says "install"

//Create test file 
preserve // preserve and restore allow you to manipulate your dataset and generate new datasets, graphs, run commands while being able to not actually change your dataset
keep if data_class == 2
replace review_scores_rating = 1 // Create a dummy number here, can be any # (just not missing). 
tempfile testing_data // tempfiles are useful for defining cleaned, subsetted, or otherwise altered versions of your main dataset and allows you to use them without loading in a new file
save `testing_data', replace // associate a local variable with your tempfile that can be called later




**********************
// 1.2
**********************


restore
predict OLS if data_class==2
drop if missing(review_scores_rating) & data_class == 1
sort data_class id
count if data_class == 1
local training_rows = r(N)
count if data_class == 1 | data_class == 2
local testing_rows = r(N)
display `testing_rows'

replace review_scores_rating = 1 if data_class == 2

//should figure out the dummy for this

//ok lets loop de loop this thing to find our number of iterations 


//FIX THE DATA RESTORE MISSINGS SORRY THIS IS JANK BUT SO IS MY LIFE
replace lnum = . if lnum==0
replace descrip= . if descrip==0


set seed 1234 // this can be any number but asssures any random choices made with random forest algorithm are consistent each time you run code

gen it = .
gen val_rmse = .
gen oob_err = .
local j = 0

/*
forvalues i = 50(50)400{
	local j = `j' + 1
	randomforest review_scores_rating price sex age bed_type_cat room_type_cat in 1/`training_rows' ///
		, type("reg") iter(`i') lsize(20)
	predict prf in `training_rows'/`testing_rows'
	replace it = `i' in `j' 
	replace val_rmse = e(RMSE) in `j' 
	replace oob_err = e(OOB_Error) in `j'
	drop prf
	}
*/

//Converges at 300 iterations

gen nvars = .
/*
forvalues i = 1(1)15{
	local j = `j' + 1
	randomforest review_scores_rating old room_type_cat /// 
		time cancel host_response_rate host_listings_count ///
		host_is_superhost availability_30 nver  ///
		bad_acom reviews_per_month descrip bad_trans white no_sum price ///
		guests_included extra_people cavail bed_type_cat namen rules cleaning_fee ///
		instant_bookable number_of_reviews in 1/`training_rows', type("reg") iter(300) numvars(`i')
	predict prf in `training_rows'/`testing_rows'
	replace nvars = `i' in `j' 
	replace val_rmse = e(RMSE) in `j' 
	replace oob_err = e(OOB_Error) in `j'
	drop prf
	} */
//I could loop this but guess and check is much faster 
//Stata in 2019 LUL

//ok lets loop de loop this thing to find our number of variables 


**********************
// Let's validate some models, boys!
**********************


randomforest review_scores_rating old room_type_cat /// 
	time cancel host_response_rate host_listings_count ///
	host_is_superhost availability_30 nver  ///
	bad_acom reviews_per_month bad_trans white no_sum lprice ///
	guests_included extra_people cavail bed_type_cat namen rules cleaning_fee ///
	instant_bookable lnum descrip bigword day week month bpr num lextra_people lhost_response_rate stateg lbeds apt maxnights smoke bad_host in 1/`training_rows', type("reg") iter(100) lsize(20) numvars(12) depth(16)
predict RF in 1/`testing_rows'




* To generate predicted values once you have trained your model on training data
keep if data_class==2
// generate predicted values, can call this new variable anything! 
order id OLS RF // rearranges columns in your dataset, makes "browsing" easier
br // notice a new column associated with predicted scores
preserve 
keep id OLS RF
export delimited "1_12110109_Whittier_Joel.csv", replace // export csv file with predictions 
restore









