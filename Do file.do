******DATA PREPARATION******
*a)
use endline.dta, clear

*b) 
*Converting "None" to 0.
replace totformalborrow_24 = subinstr(totformalborrow_24,"None","0",.)
replace totinformalborrow_24 = subinstr(totinformalborrow_24,"None","0",.)
replace hhinc = subinstr(hhinc,"None","0",.)

*Converting string values to numeric
destring totformalborrow_24, generate(totformalborrow_24_n) force
destring totinformalborrow_24, generate(totinformalborrow_24_n) force
destring hhinc, generate(hhinc_n) force

*c) 
*Generating a variable for percentile
pctile percent = hhinc_n, nq(10)

*Graphing the avg. household income over percentiles 
graph bar (mean) totformalborrow_24_n (mean) totinformalborrow_24_n, ytitle("Frequency")over(percent)

*d)  
*Top-coding for the said variables at 3 times the standard deviation.
winsor2 hhinc_n, replace cut(2.1, 99.7)
winsor2 totformalborrow_24_n, replace cut(2.1, 99.7)
winsor2 totinformalborrow_24_n, replace cut(2.1, 99.7)

*e) 
*Labelling the top-coded variables
label variable totinformalborrow_24_n "Top-coded informal borrow"
label variable totformalborrow_24_n "Top-coded formal borrow"
label variable hhinc_n "Top-coded hhinc"

*g)
*Generating a variable for total borrowing 
gen total_borrowing = totformalborrow_24_n + totinformalborrow_24_n
save endline_1, replace

*h) 
*Sorting treatment data by group_id 
use treatment_status.dta,clear
sort group_id
save treatment_status.dta, replace
 
*Sorting endline dataset (Master file) 
use endline_1.dta,clear
sort group_id

*Merging the treatment and endline datasets
merge m:1 group_id using treatment_status, gen(et)

*i) 
*Generating dummy variable for poverty line
gen belowpovertyline=1
replace belowpovertyline=0 if hhinc_n>(26.995*30)

save endline_1.dta, replace

*k) 
*Sorting baseline controls data
use baseline_controls.dta, clear
sort hhid group_id hhnomembers 
save baseline_controls, replace

*Sorting endline dataset (Master file) 
use endline_1.dta,clear
sort group_id hhid hhnomembers

*Merging the baseline controls and endline datasets
merge 1:m group_id hhid hhnomembers using baseline_controls, gen(eb)
save endline_1.dta, replace

*******ANALYSIS****** 

*b)
*iii) 
*T-tests (in tabular form)
asdoc ttest hhcaste_bc==treated, replace
asdoc ttest hhcaste_mbc==treated, rowappend
asdoc ttest hhcaste_sc_st==treated, rowappend
asdoc ttest hhreg_muslim==treated, rowappend
asdoc ttest hhreg_christian==treated, rowappend
asdoc ttest hhnomembers_below18==treated, rowappend

*c) 
*Reg Model 1: OLS with FE; SE clustered
xi: quietly reg hhinc_n treated i.pair_id, robust cluster(pair_id)

*d) 
*Reg Model 2: OLS with FE; Log-Level
gen log_hhinc_n = ln(hhinc_n)
xi: quietly reg log_hhinc_n treated i.pair_id, robust cluster(pair_id)

*e) 
*Minority variable
gen minority=1
replace minority=0 if hhcaste_fc==1
*Level of Education variable 
gen levelofeduc_hoh=1
replace levelofeduc_hoh=2 if noclasspassed_hoh==1
replace levelofeduc_hoh=3 if higheduc_hoh==1

*Reg Model 3: OLS with FE and controls
eststo: xi: quietly reg log_hhinc_n minority hhnomembers_above18 gender_hoh hhnomembers age_hoh levelofeduc_hoh treated i.pair_id, robust cluster(pair_id)

*b) 
*Reg Model 3 tabular form
esttab using reg_tables.doc, label se compress nostar brackets drop(_Ipair_id*) replace 

*f) 
*Generating quartile variable for household income
xtile quart = hhinc_n, nq(4)

*Graphing a bar chart for presentation
graph bar total_borrowing, over(treated, relabel(1 "Controlled" 2 "Treated")) over(quart) title("Analysis (f)", span) ytitle("Average Borrowed Amount (INR)") bargap(10) asy

