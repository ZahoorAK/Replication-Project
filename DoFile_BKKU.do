**APA reference to the paper: Brune, L., Karlan, D., Kurdi, S., & Udry, C. (2022). Social protection amidst social upheaval: Examining the impact of a multi-faceted program for ultra-poor households in Yemen. Journal of Development Economics, 155, 102780.
**Link to published paper: https://www-sciencedirect-com.ezpaarse.univ-paris1.fr/science/article/pii/S0304387821001395
**Replication file prepared by: Zahoor Ahmad Khan
**Replicating: Table1, Table2 and Table 13A


***********************************************************************************
*Replication code
***********************************************************************************

**Open dataset

global dirdata "directory to data location on pc"

use ${dirdata}Dataset_BKKU, clear



***********************************************************************************
*Codebook
***********************************************************************************

**Output codebook: Variables used
codebook num_adults_bsl nb_children_bsl hhsize_bsl avg_age_bsl avg_edu_years_bsl age_head_bsl hh_head_over60_bsl gender_head_bsl hh_head_educ_bsl disabl_head_bsl asset_tot_value_end ctotalpc_end ctotalhh_end inc_total_end inc_LS_end inc_nonLS_end fs_index_end savings_index_end percep_econ_end hous_sindex_end debt_index_end endline_survey village, compact



***********************************************************************************
*Summary statistics
***********************************************************************************

**Replicate Table1: Endline survey response rate & baseline summary statistics

prog def yemenbalancetable /*Define the program "yemenbalancetable" that outputs summary statistics of demographics comparaison between control/treatment groups*/

syntax varlist [if/], 		/// Variables to be included in table 
	[filename(string)] 		/// File name (default is TableX)
	[foldername(string)]	/// Output folder name (default is Replication/Output)
	[title(string)] 		/// Table title 
	[footnote(string)] 		/// Footnote text 
	[winsorize] 			/// Winsorize variables 
	[balancevar(varlist)]	/// Balance variable (default is treatment)
	
// **************************************************************
// PREPARE TABLE INPUTS 	

// FOLDER NAME 
// 	Set default folder to Replication/Output if not specified 
if "`foldername'"=="" {
	loc foldername "${rep_output}" 
}

// FILE NAME 
// 	Set a default filename 
if "`filename'"=="" {
	loc filename "Table1"
}
if "`winsorize'"=="winsorize" {
	loc filename "`filename'_win"
}

// WINSORIZE 
//	Edit varlist to include winsorized variable names if "winsorize" selected 
if "`winsorize'"=="winsorize" {
    loc templist `varlist'
	loc varlist // Clear varlist 
	foreach var in `templist' {
		loc currvar = subinstr("`var'","_bsl","_win1_bsl",.) // Add "_win1" suffix 
		loc varlist `varlist' `currvar' // Add to varlist if "winsorize" is selected
	}
}

// BALANCE VARIABLE 
if "`balancevar'"=="eligible" {
	loc label1 "Eligible"
	loc label2 "Non-Eligible"
	loc label3 "p-Value: Non-Eligible vs Eligible"
}
if "`balancevar'"=="" {
    loc balancevar treatment
	loc label1 "Treatment"
	loc label2 "Control"
	loc label3 "p-Value: Control vs Treatment"
}

// IF-STATEMENT 
if "`if'"!="" {
    loc ifif "if `if'"
	loc andif "& `if'"
} 

// FOOTNOTES 
// 	Add footnotes about baseline values and additional controls
if "`winsorize'"=="winsorize" {
	loc footnote "`footnote' Variables winsorized at 1%."
}

// **************************************************************
// CREATE TABLE FRAMEWORK 

clear all
eststo clear
estimates drop _all

loc columns = 5 //Change number of columns

set obs 10
gen x = 1
gen y = 1

forval i = 1/`columns' {
	eststo col`i': qui reg x y
}

loc count = 1
loc countamt = `count' + 1

loc stats "" // Added scalars to be filled
loc varlabels "" // Labels for row vars to be filled

use ${dirdata}Dataset_BKKU, clear 

// **************************************************************
// FILL TABLE CELLS 

loc i = 0 // Start a counter 

foreach var in `varlist' {

	// 1. Full sample
	qui sum `var' `ifif'
		loc stat`count' = r(mean)
		loc sd = r(sd)
		loc stat`count' = string(`stat`count'',"%9.2f")	
		loc sd = " (" + string(`sd',"%9.2f") + ")"
		estadd loc stat`count' = "`stat`count''" + "`sd'" : col1
			
	// 2. Balance Variable 
	qui sum `var' if `balancevar' == 1 `andif'
		loc stat`count' = r(mean)
		loc sd = r(sd)
		loc stat`count' = string(`stat`count'',"%9.2f")	
		loc sd = " (" + string(`sd',"%9.2f") + ")"
		estadd loc stat`count' = "`stat`count''"  + "`sd'": col2
			

	// 3. Control
	qui sum `var' if `balancevar' == 0 `andif'
		loc stat`count' = r(mean)
		loc sd = r(sd)
		loc stat`count' = string(`stat`count'',"%9.2f")	
		loc sd = " (" + string(`sd',"%9.2f") + ")"
		estadd loc stat`count' = "`stat`count''"  + "`sd'": col3
			
	// 4. p-values (Comparison vs Balance Variable)
	qui reg `var' `balancevar' i.village `ifif', r 
		test `balancevar' = 0 
		local thisp = `r(p)'
		loc stat`count' = r(p)
		loc stat`count' = string(`stat`count'',"%9.2f")	
		estadd loc stat`count' = "`stat`count''" : col4
		
	// 5. Observations 
	qui sum `var' `ifif'
		loc stat`count' = r(N)
		loc stat`count' = string(`stat`count'',"%9.0f")	
		estadd loc stat`count' = "`stat`count''" : col5
	
	// Row Labels and locals update
	loc thisvarlabel: var lab `var'
	loc varlabels "`varlabels' "`thisvarlabel'" "
	loc stats "`stats' stat`count'"
	loc count = `count' + 1
}

// F-test for joint significance 
qui reg `balancevar' `varlist' i.village `ifif', r 
test `varlist'

local thisp = `r(p)'
loc stat`count' = r(p)
loc stat`count' = string(`stat`count'',"%9.2f")	
estadd loc stat`count' = "`stat`count''" : col1
local varlabels "`varlabels' "F-test for joint significance" "
loc stats "`stats' stat`count'"


//***********************************************************
// EXPORT TABLE 

cd "`foldername'" // Call output folder directory 

esttab col* using "`filename'.csv",  title("`title'") cells(none) ///
	mtitle( "Full Sample" "`label1'" "`label2'" ///
	"`label3'" ///
	"Total N") stats(`stats', labels(`varlabels')) ///
	note("`footnote'")  ///
	compress wrap lines nonum replace plain

eststo clear

end 
yemenbalancetable num_adults_bsl nb_children_bsl hhsize_bsl avg_age_bsl avg_edu_years_bsl age_head_bsl hh_head_over60_bsl gender_head_bsl hh_head_educ_bsl disabl_head_bsl ///
		if endline_survey==1, ///
		filename("Table1") title("Table 1: Balance Test of Treatment Assignment for Endline Households Only") /// 
		winsorize foldername("$rep_output")
		
		

************************************************************************************
*Results table
************************************************************************************
	
**Replicate Table2: Treatment effect on key welfare outcomes

prog def yemenstandardtable /*Define the program "yemenstandardtable" that outputs the effect of treatment on welfare outcomes*/
syntax varlist [if/], 	/// Variables to be included in table 
	[filename(string)] 		/// File name (default is TableX)
	[foldername(string)]	/// Output folder name (default is Replication/Output)
	[title(string)] 		/// Table title 
	[footnote(string)] 		/// Footnote text 
	[baselinevals] 			/// Add baseline values of variables, when applicable
	[winsorize] 			/// Winsorize variables 
	[addlcontrols(varlist)] /// Add list of additional controls 
	[fixedeffects(varlist)] /// Set fixed effects (default is village) 
	[stderrors(string)] 	/// Set standard errors (default is robust)
	[tot]					/// Run treatment on treated 
	[fancyipw]				/// Run with fancy inverse probability weighting 
	[simpleipw]				/// Run with simple inverse probability weighting 
	[balance(varlist)]		/// Add balance tests for each variable 
	
// **************************************************************
// PREPARE TABLE INPUTS 	

// FOLDER NAME 
// 	Set default folder to Replication/Output if not specified 
if "`foldername'"=="" {
	loc foldername "${rep_output}" 
}

// FILE NAME 
// 	Set a default filename 
if "`filename'"=="" {
	loc filename "TableX"
}
if "`winsorize'"=="winsorize" {
	loc filename "`filename'_win"
}
if "`baselinevals'"=="baselinevals" {
	loc filename "`filename'_blv"
}
if "`addlcontrols'"!="" {
	loc filename "`filename'_ctl"
}

// WINSORIZE 
//	Edit varlist to include winsorized variable names if "winsorize" selected 
loc templist `varlist'
loc varlist // Clear varlist 
foreach var in `templist' {
	loc currvar = subinstr("`var'","_end","",.) // Cut out "_end" suffix 
	if "`winsorize'"=="winsorize" {
		loc varlist `varlist' `currvar'_win1 // Add "_win1" suffix if "winsorize" is selected
	}
	if "`winsorize'"=="" {
		loc varlist `varlist' `currvar'
	}
}

// ADDITIONAL CONTROLS 
loc templist `addlcontrols'
loc addlcontrols 
foreach var in `templist' {
    loc currvar = subinstr("`var'","_bsl","_win1_bsl",.) // Cut out "_end" suffix 
	if "`winsorize'"=="winsorize" {
		loc addlcontrols `addlcontrols' `currvar' m_`currvar' // Add "_win1" suffix if "winsorize" is selected
	}
	if "`winsorize'"=="" {
		loc varlist `addlcontrols' `var' m_`var'
	}
}

// FIXED EFFECTS 
// 	Set fixed effects to village, if not specified 
if "`fixedeffects'"=="" {
	loc fixedeffects village 
}

// STANDARD ERRORS 
//	Default standard errors to robust 
if "`stderrors'"=="" {
	loc stderrors robust 
}

// IF-STATEMENT 
if "`if'"!="" {
    loc ifif "if `if'"
	loc andif "& `if'"
} 

// BALANCE TESTING 
if "`balance'"!="" {
    loc balancevar `balance'
	loc balance balance 
}

// FOOTNOTES 
// 	Add footnotes about baseline values and additional controls
if "`winsorize'"=="winsorize" {
	loc footnote "`footnote' Variables winsorized at 1%."
}
if "`baselinevals'"=="baselinevals" {
	loc footnote "`footnote' Controls for baseline value of dependent variable."
}
if "`addlcontrols'"!="" {
	loc footnote "`footnote' Controls for additional variables."
}

// **************************************************************
// CREATE TABLE FRAMEWORK 

// Clear estimates 
clear
eststo clear
estimates drop _all 

// Create blank table
set obs 10
gen x = 1
gen y = 1

// Count outcomes and create placeholder estimates 
loc columns: word count `varlist' 

forval i = 1/`columns' { 
	eststo col`i': qui reg x y
}

// Set table cell locals 
loc j = 1
loc treatcoef = `j'		// Treatment starred coefficient
loc ++j
loc treatse = `j'		// Treatment standard error 
loc ++j
loc contmean = `j'		// Control mean 
loc ++j
loc contsd = `j'		// Control standard deviation 
loc ++j
loc obs = `j'			// Observations 
loc ++j
loc blcontrols  = `j'	// Binary for baseline controls 
loc ++j
loc numzero = `j'		// Count with outcome == 0

// Balance testing
if "`balance'"=="balance" {
    loc ++j
	loc baltest = `j'		// Balance test 
	loc ++j
	loc balproxy = `j'		// Proxy Flag 
	loc balword1 "Balance Test p-Value"
	loc balword2 "Proxy for Balance"  
}

loc stats "" 			// Added scalars to be filled
loc col_titles "" 		// Labels for columns vars to be filled

use ${dirdata}Dataset_BKKU, clear


// **************************************************************
// REPLACE MISSINGS WITH ZEROS

// ADDITIONAL CONTROLS 
// 	Replace missings with zeroes 
foreach var in `addlcontrols' {
	replace `var' = 0 if missing(`var') // Replace to 0 if missing 
}

// **************************************************************
// FILL TABLE CELLS 
//	Table cells include coefficients, standard errors, p-values 

loc i = 1 // Start a counter 

foreach y_var in `varlist' { // Loop through table variables 
	
	//***********************************************************
	// BASELINE CONTROLS 
	// 	Replace missings with zeroes 
	// 	Add "missing" dummies to controls 

	// Check whether baseline value exists 
	cap confirm variable `y_var'_bsl
	
	// If baseline doesn't exist... 
	if _rc { 
		loc proxyflag = 1 
		loc created_var = 1					
		gen `y_var'_bsl = 1 			// Create stand-in baseline var
		gen m_`y_var'_bsl = 1 			// Create stand-in baseline dummy var
		estadd loc stat`blcontrols' = "No" : col`i' // Note that baseline controls not used
	}
	
	// If baseline exists... 
	else if !_rc {
		loc proxyflag = 0 
		loc created_var = 0
		replace `y_var'_bsl = 0 if missing(`y_var'_bsl) // Replace baseline to 0 if missing 	
		if ("`baselinevals'"=="baselinevals") estadd loc stat`blcontrols' = "Yes" : col`i' // Note baseline controls used 
		else estadd loc stat`blcontrols' = "No" : col`i' // Note baseline controls not used 
	}
	
	if "`baselinevals'"=="baselinevals" local blvals `y_var'_bsl m_`y_var'_bsl // Add to baseline variables local 
	else local blvals // Empty
	
	//***********************************************************
	// RUN REGRESSION 

	// TREATMENT ON TREATED (ToT)
	if "`tot'"=="tot" { 
		xi: ivreg `y_var'_end `blvals' `addlcontrols' i.`fixedeffects' (treat_received = treatment), r
		loc coef "treat_received"
		loc main "false"
	}
	
	// "SIMPLE" INVERSE PROBABILITY WEIGHTING (Simple IPW)
	if "`simpleipw'"=="simpleipw"{
		// Drop existing inverse probability weighting variable 
		cap drop `ipw_simple3' 
		tempvar ipw_simple3
		gen `ipw_simple3' = .x
		
		// For each group, grab counts 
		count if treatment==0 
		local N_c = r(N)			
		count if !mi(`y_var'_end) & treatment==0 
		local N_c_found = r(N)
		
		count if treatment==1 & eligible==1
		local N_t_e1ig = r(N)
		count if !mi(`y_var'_end) & treatment==1 & eligible==1
		local N_t_e1ig_found = r(N)
		
		count if treatment==1 & eligible==0
		local N_t_notElig = r(N)
		count if !mi(`y_var'_end) & treatment==1 & eligible==0
		local N_t_notElig_found = r(N)
				
		// Use counts to calculate IPW 
		replace `ipw_simple3' = `N_c'/`N_c_found' if treatment==0 
		replace `ipw_simple3' = `N_t_e1ig'/`N_t_e1ig_found' if treatment==1 & eligible==1
		replace `ipw_simple3' = `N_t_notElig'/`N_t_notElig_found' if treatment==1 & eligible==0
		
		// IPW regression 
		areg `y_var'_end treatment `blvals' `addlcontrols' `ifif' [pw=`ipw_simple3'], absorb(`fixedeffects') vce(`stderrors')
		loc coef "treatment"
		loc main "false"
	}
		
	
	// "FANCY" INVERSE PROBABILITY WEIGHTING (Fancy IPW)
	if "`fancyipw'"=="fancyipw" {
		// Drop existing inverse probability weighting variable 
		cap drop ipw_temp		
		cap drop pr_temp
		
		// Predict weight 
		logit surveyed `addlcontrols' // <--- here can sub in other ML but i suspect it won't matter
		
		predict pr_temp, pr
		generate double ipw_temp = surveyed/pr_temp	
		
		// IPW regression
		areg `y_var'_end treatment `blvals' `addlcontrols' `ifif' [pw=ipw_temp], absorb(`fixedeffects') vce(`stderrors')
		loc coef "treatment"
		loc main "false"
	}
	
	// MAIN / DEFAULT REGRESSION 
	else if "`main'"!="false" { 
		qui areg `y_var'_end treatment `blvals' `addlcontrols' `ifif', absorb(`fixedeffects') vce(`stderrors') 
		loc coef "treatment"
	}
	
	//***********************************************************
	// SAVE STATISTICS
		
	// Save results to matrix A for later use
	mat def A = r(table)
	
	// Add measure of % zero
	qui count if `y_var'_end == 0 & e(sample) == 1
	loc top = r(N)
	qui count if  e(sample) == 1
	loc bot = r(N)
	estadd loc stat`numzero' = string(`top'/`bot',"%9.2f"): col`i'

	// Add treatment coefficient and standard error	
	// Standard error
	loc se = el(A,rownumb(A,"se"),colnumb(A,"`coef'"))
	estadd loc stat`treatse' = "(" + string(`se',"%9.2f") + ")" : col`i' //Add standard error
		
	// P-value: to get stars
	local thisp = el(A,rownumb(A,"pvalue"),colnumb(A,"`coef'"))
	
		if `thisp' < 0.01 {
			loc bstar "***"
		}
		else if `thisp' < 0.05 {
			loc bstar "**"
		}
		else if `thisp' < 0.1 {
			loc bstar "*"
		}
		else {
			local bstar ""
		}
	// Assign to coefficient
	loc coefficient = el(A,rownumb(A,"b"),colnumb(A,"`coef'"))
	estadd loc stat`treatcoef' = string(`coefficient',"%9.2f") + "`bstar'": col`i'

	// Control group mean and SD
	qui sum `y_var'_end if treatment == 0  & e(sample) == 1
	estadd loc stat`contmean' = string(`r(mean)', "%9.2f"): col`i'
	estadd loc stat`contsd' = string(`r(sd)', "%9.2f") : col`i'
		
	// Total observations
	qui sum `y_var'_end if e(sample) == 1
	estadd loc stat`obs' = string(`r(N)', "%9.0f"): col`i'
	
	// Balance Testing 
	if "`balance'"=="balance" {		
		if `proxyflag' { // If it does not exist... 
			foreach wins in "" "_win1" {
				if "`y_var'"=="asset_tot_value`wins'" {
					loc proxyvar asset_index`wins'_bsl 
				}
				if "`y_var'"=="inc_LS`wins'" {
					loc proxyvar ls_hany`wins'_bsl  
				}
				if "`y_var'"=="inc_nonLS`wins'" {
					loc proxyvar swf_income`wins'_bsl
				}
				if "`y_var'"=="savings_index`wins'" {
					loc proxyvar savings`wins'_bsl
				}
			}
			if "`proxyvar'"=="" {
			    estadd loc stat`baltest' = "--": col`i'
				estadd loc stat`balproxy' = "Missing": col`i'
			}
			else if "`proxyvar'"!="" {
				// loc ballist `ballist' `proxyvar'
			    qui reg `proxyvar' `balancevar' i.village `ifif', r 
				test `balancevar' = 0 
				estadd loc stat`baltest' = string(`r(p)', "%9.2f"): col`i'
				estadd loc stat`balproxy' = "Yes": col`i'
				loc proxyvar 
			}
		}
		
		else if !`proxyflag' { // If baseline value exists, run balance test 
			// loc ballist `ballist' `y_var'_bsl
		    qui reg `y_var'_bsl `balancevar' i.village `ifif', r 
			test `balancevar' = 0 
			estadd loc stat`baltest' = string(`r(p)', "%9.2f"): col`i'
			estadd loc stat`balproxy' = "No": col`i'
		}
		
	}
	
	loc ++i
	mat drop A
	
/*
	// Drop baseline dummy if created [LB: why?]
	if `created_var' == 1 {
		drop `y_var'_bsl 
		drop m_`y_var'_bsl
	}
*/
	
	// Row labels and update locals
	loc thisvarlabel: variable label `y_var'_end // Extracts label from row var
	local col_titles "`col_titles' "`thisvarlabel'" "		
	
}
// end foreach y_var
	
// Set stats to include in table
forv i = 1/`j' {
	loc stats "`stats' stat`i' "
}
// end forv i

	
//***********************************************************
// EXPORT TABLE 

cd "`foldername'" // Call output folder directory 

esttab col* using "`filename'.csv", title("`title'") cells(none) ///
	nonum mtitle(`col_titles') stats(`stats',labels("Treatment" " " "Control Mean" "Control SD" "Observations (Total)"  ///
	"Controls for Baseline Values" "Proportion of Obs Equal Zero" "`balword1'" "`balword2'")) ///
	note("`footnote'") compress wrap replace

/*
// F-test for joint significance 
if "`balance'"=="balance" {
    qui reg `balancevar' `ballist' i.village `ifif', r 
	test `ballist'
}
*/
	
end 
yemenstandardtable asset_tot_value_end ctotalpc_end ctotalhh_end inc_total_end inc_LS_end inc_nonLS_end fs_index_end savings_index_end percep_econ_end hous_sindex_end debt_index_end /// 
		if endline_survey==1, ///
		filename("Table2") title("Table 2: Treatment Effects on Key Welfare Outcomes") /// 
		winsorize baselinevals foldername("$rep_output") /// 
		balance(treatment)
		
		
		
*************************************************************************************
*Robustness check table
*************************************************************************************
**Replicate Table13A: Robustness Inverse probability weighting of key outcomes
{
	// Set controls 
	loc ipwcontrols num_adults_bsl nb_children_bsl hhsize_bsl avg_age_bsl	avg_edu_years_bsl gender_head_bsl age_head_bsl hh_head_over60_bsl hh_head_educ_bsl disabl_head_bsl
	
	// Loop through "simple" and "fancy" IPW
	foreach type in simple fancy {
		// Set label 
		loc typelabel = substr(proper("`type'"), 1, 4) 

		yemenstandardtable asset_tot_value_end ctotalpc_end ctotalhh_end inc_total_end inc_LS_end inc_nonLS_end fs_index_end savings_index_end percep_econ_end hous_sindex_end debt_index_end, ///
			filename("ATable13_`typelabel'IPW") title("Appendix Table 13: Treatment Effects on Key Welfare Outcomes with Inverse Probability Weighting") /// 
			baselinevals winsorize `type'ipw /// 
			addlcontrols(`ipwcontrols') foldername("$rep_output")
	}
}
