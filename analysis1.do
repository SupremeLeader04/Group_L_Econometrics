
***Original dataset from PSID
use "$data/data_final.dta", clear

replace educ_psid=. if educ_psid==0
replace age_psid=. if age_psid==0


replace Texp_group = 0 if year_start==.
replace Texp_group = 0 if strong==0


* Balanced event years if years in analysis: 1988 to 2015
gen Texp_group3 = .
replace Texp_group3 = Texp_group if Texp_group>=-2 & Texp_group<=6
replace Texp_group3 = -3 if Texp_group<=-3 & Texp_group!=. /* Group unbalanced edges. */
replace Texp_group3 = 7 if Texp_group>=7 & Texp_group!=. /* Group unbalanced edges. */

replace marital_status_psid=. if (marital_status_psid==9)
replace educ_psid=. if educ_psid==0


* Family outcomes 
gen hrs_j = psid_work_hours if relation_hh==10 | relation_hh==20
bysort family_id year: egen hours_fam = total(hrs_j)
drop hrs_j

gen inc_j = real_wage_last_year if relation_hh==10 | relation_hh==20
bysort family_id year: egen wage_fam = total(inc_j)
drop inc_j

forvalues j=1/18 {
gen ch_age_any`j'=year-ch_yrbir`j'
}


forvalues j=1/18 {
gen tag_any`j' = (ch_age`j'>=19 & ch_age`j'<=23)
}

* Child who is 19 to 23 at any point in the year (which means that last year they were 18 to 22)
gen child_1923_cah = ((tag_any1==1) | tag_any2==1 | tag_any3==1 | tag_any4==1 | tag_any5==1 | tag_any6==1 | tag_any7==1 /// 
|tag_any8==1 |tag_any9==1 |tag_any10==1 |tag_any11==1 |tag_any12==1 |tag_any13==1 |tag_any14==1 |tag_any15==1 |tag_any16==1 /// 
|tag_any17==1 |tag_any18==1)


* Ranges from 0 to 100
replace employed_psid= employed_psid*100

gen merit_mis = merit_spending_per_fte_underg
replace merit_mis=. if state=="South Carolina" & year==1999
replace merit_mis=. if state=="Nevada" & year==2003
replace merit_mis=. if state=="Tennessee" & year==2005
replace merit_mis=. if state=="New Mexico" & year==2003
replace merit_mis=. if state=="Georgia" & (year==1994 | year==1995)

sort person_id year
bysort person_id: egen colever_ch19_num = max(colever_ch19)

gen needsp = need_spending_per_fte_underg

gen post = (year>=year_start) if year_start!=.
replace post=0 if strong==0 | year_start==.

gen mersp = merit_spending_per_fte_underg

replace year=year-1
drop cpiu cpi

ssc install cpigen
cpigen
replace cpi=1.35 if year==2014
replace cpiu=1.35 if year==2014
replace cpi=cpi/1.36
replace cpiu=cpiu/1.36
replace year=year+1

* Family after-tax income in $2015 dollars
gen real_fam_inc=income_after_tax_transfers/cpiu


* Create an indicator for the year when have a child who is 15 to 20 years old
forvalues j=1/18 {
gen tag_any`j'_a1520 = (ch_age`j'>=15 & ch_age`j'<=20)
}

gen child_1520 = (tag_any1_a1520==1 | tag_any2_a1520==1 | tag_any3_a1520==1 | tag_any4_a1520==1 | tag_any5_a1520==1 | tag_any6_a1520==1 | /// 
tag_any7_a1520==1 |tag_any8_a1520==1 |tag_any9_a1520==1 |tag_any10_a1520==1 |tag_any11_a1520==1 |tag_any12_a1520==1 |tag_any13_a1520==1 | /// 
tag_any14_a1520==1 |tag_any15_a1520==1 |tag_any16_a1520==1 |tag_any17_a1520==1 |tag_any18_a1520==1)

* Create an indicator for the year when your first child is 18 years old
gen tag1_a18 = (ch_age1==18)
* Create an indicator for the year your first child is 17 years old
gen tag1_a17 = (ch_age1==17)
* Create an indicator for the year your first child is 16 years old
gen tag1_a16 = (ch_age1==16)

* Create 'faminc_a18' that has the family income of the family when the first child was 18, and if missing 17, and if missing 16
* This creates the same variable for all years within 'person_id'
gen junk = real_fam_inc if tag1_a18==1
bysort person_id: egen faminc_a18 = max(junk)
drop junk
gen junk = real_fam_inc if tag1_a17==1
bysort person_id: egen faminc_a17 = max(junk)
drop junk
gen junk = real_fam_inc if tag1_a16==1
bysort person_id: egen faminc_a16 = max(junk)
drop junk

* Create hours_a18' that has work hours of a woman when the first child was 18, and if missing 17, and if missing 16
* This creates the same variable for all years within 'person_id'
gen junk = psid_work_hours if tag1_a18==1
bysort person_id: egen hours_a18 = max(junk)
drop junk
gen junk = psid_work_hours if tag1_a17==1
bysort person_id: egen hours_a17 = max(junk)
drop junk
gen junk = psid_work_hours if tag1_a16==1
bysort person_id: egen hours_a16 = max(junk)
drop junk

* Create family income only in the year when the person has a first child who is age 18, 17, or 16
gen faminc1_a18 = real_fam_inc if tag1_a18==1
replace faminc1_a18 = real_fam_inc if tag1_a17==1 & faminc_a18==.
replace faminc1_a18 = real_fam_inc if tag1_a16==1 & faminc_a17==. & faminc_a18==.

* Create work hours only in the year when the person has a first child who is age 18, 17, or 16
gen hours1_a18 = psid_work_hours if tag1_a18==1
replace hours1_a18 = psid_work_hours if tag1_a17==1 & hours_a18==.
replace hours1_a18 = psid_work_hours if tag1_a16==1 & hours_a17==. & hours_a18==.

* Only count kids who are 19 to 23 year old who are living with you
gen cores_j = 1 if relation_hh==30 & age_psid>=19 & age_psid<=23
bysort family_id year: egen cores_ch = max(cores_j)
replace cores_ch=0 if cores_ch==.

* 3 percentiles of family income 
gen pinca7=.
gen pinca8=.
gen pinca9=.
set more off
foreach j of numlist 1985/1997 1999(2)2015 {
_pctile real_fam_inc [w=lon_weight] if year==`j' & child_1520==1 & sex_psid==2, p(33, 67)
replace pinca7 = 1 if faminc1_a18 <=r(r1) & faminc1_a18!=. & year==`j'
replace pinca8= 1 if faminc1_a18>r(r1) & faminc1_a18<=r(r2) & year==`j'
replace pinca9=1 if faminc1_a18>r(r2) & faminc1_a18!=. & year==`j'
}

forvalues j=7/9 {
bysort person_id: egen pincf_`j' = max(pinca`j')
}

* 3 percentiles of hours
gen phrsa7=.
gen phrsa8=.
gen phrsa9=.
set more off
foreach j of numlist 1985/1997 1999(2)2015 {
_pctile psid_work_hours [w=lon_weight] if year==`j' & child_1520==1 & sex_psid==2, p(33, 67)
replace phrsa7 = 1 if hours1_a18 <=r(r1) & hours1_a18!=. & year==`j'
replace phrsa8= 1 if hours1_a18>r(r1) & hours1_a18<=r(r2) & year==`j'
replace phrsa9=1 if hours1_a18>r(r2) & hours1_a18!=. & year==`j'
}

forvalues j=7/9 {
bysort person_id: egen phrsf_`j' = max(phrsa`j')
}

* 3 percentiles for hours (sample of employed women only)
gen phrse7=.
gen phrse8=.
gen phrse9=.
set more off
foreach j of numlist 1985/1997 1999(2)2015 {
_pctile psid_work_hours [w=lon_weight] if year==`j' & child_1520==1 & sex_psid==2 & employed_psid==100, p(33, 67)
replace phrse7 = 1 if hours1_a18 <=r(r1) & hours1_a18!=. & hours1_a18>=52 & hours1_a18!=. & year==`j'
replace phrse8= 1 if hours1_a18>r(r1) & hours1_a18<=r(r2) & hours1_a18>=52 & hours1_a18!=. & year==`j'
replace phrse9=1 if hours1_a18>r(r2) & hours1_a18!=. & hours1_a18>=52 & hours1_a18!=. & year==`j'
}

forvalues j=7/9 {
bysort person_id: egen phrsfe_`j' = max(phrse`j')
}


**This is the clean data we use in the paper
save "$data/data_analysis1.dta", replace

***Main Tables of the paper
use "$data/data_analysis1.dta", clear
gen pre_unb = Texp_group>=-12 & Texp_group<=-3
gen post_unb = Texp_group>=7 & Texp_group<=12
gen post_bal = Texp_group>=1 & Texp_group<=6 

gen post_2 = Texp_group>=1 & Texp_group<=2
gen post_unb2 = Texp_group>=3 & Texp_group<=12

gen post_b1 = (Texp_group>=1 & Texp_group<=2)
gen post_b2 = (Texp_group>=3 & Texp_group<=4)
gen post_b3 = (Texp_group>=5 & Texp_group<=6)

bysort person_id: egen junkcol = max(colever_ch19)
gen ch_young_2633 = ch_young_age>=26 & ch_young_age<=33

replace age_psid=. if age_psid==0

* Need this for Table A11
gen college_ind = .
replace college_ind = 1 if colever_ch19>=1 & colever_ch19<=5
replace college_ind = 0 if colever_ch19==0

foreach var of varlist white_psid educ_psid age_psid marital_status_psid numkids_psid head_hh_psid /// 
need_spending_per_fte_underg tuition_state_public4 tuition_state_public2 unemprate log_revenue_pp mn_wage /// 
democrat_gov PovertyRate AFDCTANFRecipients {
gen `var'_mis = (`var'==. | `var'==.m)
replace `var'=0 if (`var'==. | `var'==.m)
}

tab marital_status_psid, g(marital)

gen some_col = (educ_psid>=13 & educ_psid<=17)
replace some_col = . if educ_psid==0

gen needsp_mis = need_spending_per_fte_underg_mis



** Create Table 3 (panels A and B)
xi i.state i.age_psid i.year i.educ_psid
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours employed_psid {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table3.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "all") 
local r append
}
}

** Create Table 3 (panel C)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if employed_psid==100 & state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table3.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "employed") 
local r append
}
}

** Create Table 4 (panels A and B)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours employed_psid {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if colever_ch19>=1 & colever_ch19<=5 & state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)

outreg2 using "$output/table4.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "all") 
local r append
}
}

** Create Table 4 (panel C)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if employed_psid==100 & colever_ch19>=1 & colever_ch19<=5 & state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table4.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "employed") 
local r append
}
}

** Create Table 6 (Panel A and Panel B)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours employed_psid {
forvalues j=1/4 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & state!="Arkansas" & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 , cluster(state)
outreg2 using "$output/table6.xls", `r' nocons keep(mersp) /// 
addstat(Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Table 6 (Panel C)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=1/4 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & employed_psid==100 & colever_ch19>=1 & colever_ch19<=5 & state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
outreg2 using "$output/table6.xls", `r' nocons keep(mersp) /// 
addstat(Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Table 7 (columns 1 to 6) 
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=4/4 {

* Head of household

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & head_hh_psid==1 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table7.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "head") 
local r append

* Not head of household

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & head_hh_psid==0 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table7.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "not head") 
local r append

* Not college educated woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & some_col==0 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table7.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "no college") 
local r append


* College Educated woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & some_col==1 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table7.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "college") 
local r append

* Nonwhite woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & white_psid==0 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table7.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "non-white") 
local r append

* White woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & white_psid==1 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table7.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "white") 
local r append


}
}

** Create Table 7 (columns 7, 8 and 9)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=4/4 {
forvalues i=7/9 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & pincf_`i'==1, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table7.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) /// 
bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "pincf_`i'") 
local r append
}
}
}

* Create Table 7 (columns 10 and 11)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=4/4 {
* 1 child in college
ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & colever_ch19==1 & child_1923_cah==1 & state!="Arkansas" & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table7.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "1 kid") 
local r append
* 2+ kids in college at the same time
ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & colever_ch19>=2 & colever_ch19<=5 & child_1923_cah==1 & state!="Arkansas" & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table7.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'","2 kid") 
local r append
}
}


*** Figures in the main paper with coefficients in appendix tables
** Create Figure 1 (panel A) 
** Create Table A3 
char Texp_group3[omit]0
char age_psid[omit]40
xi i.state i.Texp_group3 i.age_psid i.year i.educ_psid

set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' _ITexp_grou* `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a3.xls", `r' nocons keep(_ITexp*) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Figure 1 (panel B)
** Create Table A4
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' _ITexp_grou* `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==1 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a4.xls", `r' nocons keep(_ITexp*) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Figure 3 (panel A)
** Create Table A6
char Texp_group3[omit]0
* char year[omit]1989
char age_psid[omit]40
xi i.state i.Texp_group3 i.age_psid i.year i.educ_psid

set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist employed_psid {
forvalues j=1/4 {
regress `var' _ITexp_grou* `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a6.xls", `r' nocons keep(_ITexp*) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Figure 3 (Panel B)
** Create Table A7
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours{
forvalues j=1/4 {
regress `var' _ITexp_grou* `X`j'' [w=lon_weight] if employed_psid==100 & state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a7.xls", `r' nocons keep(_ITexp*) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Figure 4 (Panel A)
** Create Table A12
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' _ITexp_grou* `X`j'' [w=lon_weight] if colever_ch19>=1 & colever_ch19<=5 & state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a12.xls", `r' nocons keep(_ITexp*) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Figure 4 (Panel B)
** Create Table A19
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' _ITexp_grou* `X`j'' [w=lon_weight] if colever_ch19==0 & state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a19.xls", `r' nocons keep(_ITexp*) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** APPENDIX TABLES AND FIGURES

** Create Table A2
gen my_samp = (sex_psid==2 & year>=1989 & year<=1992 & age_psid>=35 & age_psid<=64) & strong==1 & numkids_psid!=0 & (relation_hh==10 | relation_hh==20) & state!="Arkansas"
set more off
foreach var of varlist psid_work_hours employed_psid {
regress `var' year_start if my_samp==1 [w=lon_weight], cluster(state)
sum `var' [w=lon_weight] if e(sample)
outreg2 using "$output/table_a2.xls", `r' nocons addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'") 
}


** Create Table A5 (panel A)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_b1 post_b2 post_b3 `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
test post_b1=post_b2=post_b3
outreg2 using "$output/table_a5.xls", `r' nocons keep(post_b1 post_b2 post_b3) /// 
addstat(states, e(N_clust), pvaltest, r(p)) /// 
 bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "college-age") 
local r append
}
}

** Create Table A5 (panel B)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_b1 post_b2 post_b3 `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & colever_ch19>=1 & colever_ch19<=5, cluster(state)
local mv = r(mean)
test post_b1=post_b2=post_b3
outreg2 using "$output/table_a5.xls", `r' nocons keep(post_b1 post_b2 post_b3) /// 
addstat(states, e(N_clust), pvaltest, r(p)) /// 
bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "college-going") 
local r append
}
}

** Create Table A11 (panel A)
set more off
local X4 "_Istate* _Iyear* need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"

local r replace

foreach var of varlist college_ind educ_psid white_psid numkids_psid head_hh_psid age_psid{
regress `var' pre_unb post_unb post_bal `X4' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2  /// 
& educ_psid_mis!=1 & white_psid_mis!=1 & numkids_psid_mis!=1 & head_hh_psid_mis!=1 & age_psid_mis!=1 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a11.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "college-age") 
local r append
}

** Create Table A11 (panel B)
set more off
local X4 "_Istate* _Iyear* need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"

local r append
foreach var of varlist educ_psid white_psid numkids_psid head_hh_psid age_psid{
regress `var' pre_unb post_unb post_bal `X4' [w=lon_weight] if colever_ch19>=1 & colever_ch19<=5 & state!="Arkansas" /// 
& child_1923_cah==1 & sex_psid==2 & educ_psid_mis!=1 & white_psid_mis!=1 & numkids_psid_mis!=1 & head_hh_psid_mis!=1 & age_psid_mis!=1 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)

outreg2 using "$output/table_a11.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "college-going") 
local r append
}

** Create Table A15
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' pre_unb post_unb2 post_2  `X`j'' [w=lon_weight] if junkcol>=1 & junkcol<=5 & state!="Arkansas" & sex_psid==2 & ch_young_2633==1 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a15.xls", `r' nocons keep(post_2) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Table A16 (panel A)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"

local r replace

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if child_1923_cah==1 & state!="Arkansas" & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state) savefirst 
est restore _ivreg2_mersp
outreg2 using "$output/table_a16.xls", `r' nocons bracket keep(post) asterisk(se) ctitle("model`j'", "`e(cmdline)'", "college-age")
local r append

}
}

** Create Table A16 (Panel B)

rename mersp m_spend

set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"

local r append

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
ivreg2 `var' (m_spend=post) `X`j'' [w=lon_weight] if colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & state!="Arkansas" & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state) savefirst 
est restore _ivreg2_m_spend
outreg2 using "$output/table_a16.xls", `r' nocons bracket keep(post) asterisk(se) ctitle("model`j'", "`e(cmdline)'", "college-going")
local r append
}
}

rename m_spend mersp 


** Create Table A17 (Panels 1 and 2)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours employed_psid {
forvalues j=1/4 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if child_1923_cah==1 & state!="Arkansas" & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
weakivtest
local fweak `r(F_eff)'
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a17.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat), F-weak, `fweak') /// 
bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "all") 
local r append
}
}

** Create Table A17 (Panel 3)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours{
forvalues j=1/4 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if employed_psid==100 & state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a17.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "employed") 
local r append
}
}

** Create Table A18 (panel A) 
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if colever_ch19==0 & state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a18.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Table A18 (Panel B)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=1/4 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if colever_ch19==0 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & state!="Arkansas", cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a18.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "`e(cmd)'") 
local r append
}
}

** Create Table A20 (columns 1 to 6)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=4/4 {

* Head of household

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & head_hh_psid==1 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a20.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "head") 
local r append
* Not head of household

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & head_hh_psid==0 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a20.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "not head") 
local r append

* Not college educated woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & some_col==0 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a20.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "no college") 
local r append

* College Educated woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & some_col==1 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a20.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "college") 
local r append

* Nonwhite woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & white_psid==0 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a20.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "non-white") 
local r append


* White woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & white_psid==1 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a20.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "white") 
local r append
}
}


** Create Table A20 (columns 7, 8, and 9)
xi i.state i.year i.educ_psid i.age_psid
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=4/4 {
forvalues i=7/9 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & pincf_`i'==1, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a20.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) /// 
bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "model`i'", "IV", "pincf_`i'") 
local r append
}
}
}

** Create Table A21

set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist employed_psid {
forvalues j=4/4 {

* Head of household

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & head_hh_psid==1 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a21.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "head") 
local r append
* Not head of household

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & head_hh_psid==0 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a21.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "not head") 
local r append

* Not college educated woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & some_col==0 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a21.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "no college") 
local r append

* College Educated woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & some_col==1 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a21.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "college") 
local r append

* Nonwhite woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & white_psid==0 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a21.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "non-white") 
local r append


* White woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & white_psid==1 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a21.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "IV", "white") 
local r append
}
}

** Create Table A21 (columns 7, 8, and 9) 
xi i.state i.year i.educ_psid i.age_psid
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist employed_psid {
forvalues j=4/4 {
forvalues i=7/9 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & pincf_`i'==1, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a21.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) /// 
bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "model`i'", "IV", "pincf_`i'") 
local r append
}
}
}

** Create Table A22 (columns 1 to 6)

set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist employed_psid {
forvalues j=4/4 {

* Head of Household

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & head_hh_psid==1 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a22.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "head") 
local r append

* Not head of household

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & head_hh_psid==0 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a22.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "not head") 
local r append

* Not college educated woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & some_col==0 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a22.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "no college") 
local r append


* College Educated woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & some_col==1 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a22.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "college") 
local r append

* Nonwhite woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & white_psid==0 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a22.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "non-white") 
local r append

* White woman

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & white_psid==1 & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a22.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "white") 
local r append


}
}

** Create Table A22 (coumns 7, 8 and 9)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist employed_psid {
forvalues j=4/4 {
forvalues i=7/9 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & pincf_`i'==1, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_a22.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) /// 
bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "pincf_`i'") 
local r append
}
}
}

** Create Table A25 (panel A)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist cores_ch {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a25.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "college-age") 
local r append
}
}

* Create Table A25 (panel B)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist cores_ch {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & colever_ch19>=1 & colever_ch19<=5, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_a25.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "college-going") 
local r append
}
}

** Create Figure A2 (panel A)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=4/4 {
foreach x in "FLORIDA" "GEORGIA" "KENTUCKY" "LOUISIANA" "NEVADA" "NEW MEXICO" "SOUTH CAROLINA" "TENNESSEE" "WEST VIRGINIA" {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & state_psid!="`x'", cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/figure_a2.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "`x'") 
local r append
}
}
}

** Create Figure A2 (panel B)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=4/4 {
foreach x in "FLORIDA" "GEORGIA" "KENTUCKY" "LOUISIANA" "NEVADA" "NEW MEXICO" "SOUTH CAROLINA" "TENNESSEE" "WEST VIRGINIA" {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & state_psid!="`x'" ///
& colever_ch19>=1 & colever_ch19<=5, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/figure_a2.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "`x'") 
local r append
}
}
}

** Create Table C1 (columns 1, 2, and 3, panel A)
xi i.state i.year i.educ_psid i.age_psid
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours {
forvalues j=4/4 {
forvalues i=7/9 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & phrsf_`i'==1, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_c1.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) /// 
bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "phrsf_`i'", "college-age") 
local r append
}
}
}

** Create Table C1 (columns 1, 2, and 3, panel B)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours {
forvalues j=4/4 {
forvalues i=7/9 {

ivreg2 `var' (mersp=post) `X`j'' [w=lon_weight] if state!="Arkansas" & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & phrsf_`i'==1, cluster(state)
sum `var' [w=lon_weight] if strong==1 & e(sample) & Texp_group==0
outreg2 using "$output/table_c1.xls", `r' nocons keep(mersp) /// 
addstat(Dep var mean, r(mean), Kleibergen-Paap F-statistic, e(widstat)) /// 
bracket asterisk(se) ctitle("`e(depvar)'", "`e(cmdline)'", "phrsf_`i'", "college-going") 
local r append
}
}
}

** Create Table C2 (panel A)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist part_time_psid {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_c2.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "college-age") 
local r append
}
}

** Create Table C2 (panel B)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' need_spending_per_fte_underg_mis tuition_state_public4 tuition_state_public2 need_spending_per_fte_underg tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist part_time_psid {
forvalues j=1/4 {
regress `var' pre_unb post_unb post_bal `X`j'' [w=lon_weight] if state!="Arkansas" & child_1923_cah==1 & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64 & colever_ch19>=1 & colever_ch19<=5, cluster(state)
sum `var' [w=lon_weight] if Texp_group==0 & strong==1 & e(sample)
outreg2 using "$output/table_c2.xls", `r' nocons keep(post_bal) /// 
addstat(Dep var mean, r(mean)) bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'", "college-going") 
local r append
}
}



** Create Table D1 (Panel A1 and Panel A2).
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r replace

foreach var of varlist psid_work_hours employed_psid {
forvalues j=1/4 {

ivreg2 `var' (percent_tuition=post) `X`j'' [w=lon_weight] if child_1923_cah==1 & state!="Arkansas" & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1989 & year<=2013 & age_psid<=64, cluster(state)
weakivtest
local fweak `r(F_eff)'
outreg2 using "$output/table_d1.xls", `r' nocons keep(percent_tuition) /// 
addstat(Kleibergen-Paap F-statistic, e(widstat), F-weak, `fweak') /// 
bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

** Create Table D1 (Panels B1 and B2)
set more off
local X1 "_Istate* _Iyear*"
local X2 "`X1' white_psid _Ieduc* _Iage* marital1 marital3-marital6 numkids_psid head_hh_psid white_psid_mis educ_psid_mis age_psid_mis marital_status_psid_mis numkids_psid_mis head_hh_psid_mis"
local X3 "`X2' needsp_mis tuition_state_public4 tuition_state_public2 needsp tuition_state_public4_mis tuition_state_public2_mis"
local X4 "`X3' unemprate log_revenue_pp mn_wage democrat_gov PovertyRate AFDCTANFRecipients unemprate_mis log_revenue_pp_mis mn_wage_mis democrat_gov_mis PovertyRate_mis AFDCTANFRecipients_mis"
local r append

foreach var of varlist psid_work_hours employed_psid {
forvalues j=1/4 {

ivreg2 `var' (percent_tuition=post) `X`j'' [w=lon_weight] if state!="Arkansas" & colever_ch19>=1 & colever_ch19<=5 & child_1923_cah==1 /// 
& state!="Arkansas" & sex_psid==2 /// 
& (relation_hh==10 | relation_hh==20) & year>=1988 & year<=2015 & age_psid<=64, cluster(state)
outreg2 using "$output/table_d1.xls", `r' nocons keep(percent_tuition) /// 
addstat(Kleibergen-Paap F-statistic, e(widstat)) /// 
bracket asterisk(se) ctitle("model `j'", "`e(depvar)'", "`e(cmdline)'") 
local r append
}
}

