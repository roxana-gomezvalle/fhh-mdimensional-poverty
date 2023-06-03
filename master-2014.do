/*====================================================================
Project      : Revisiting the linkages between female headship and ///
               (multidimensional) poverty: The case of Nicaragua
Author       : Roxana Gómez-Valle
Creation Date: 14 May 2022
Output       : Final databse for analysis
====================================================================*/

/*====================================================================
                        0: Program set up
====================================================================*/
global pjdatabase "C:\Users\User\Documents\MPI - FHH\Database"
global dofiles   "C:\Users\User\Documents\MPI - FHH\Do-files"

set more off , perm
clear all
version 15.1

/*====================================================================
                        1: Executing external do-files
====================================================================*/
qui {
do "${dofiles}/emnv14-mpi.do"
}

/*====================================================================
                        2: Defining headship 
====================================================================*/
use "${pjdatabase}/emnv14_04_poblacion.dta", clear
rename *, lower
numlabel, add

*------------------------------2.1: Self-reported headship
*--------------------2.1.1: General self-reported headship
gen self_fhh = ((s2p4 == 1) & (s2p5 == 2))

*--------------------2.1.2: Self-reported headship classification
clonevar self_composition = self_fhh 

gen aux_partner = (s2p4 == 2)
bys i00: egen partner = max(aux_partner)
replace    self_composition = 2 if (((s2p7 < 3) & (partner == 0)) & (self_fhh == 1))
replace    self_composition = 3 if (((s2p7 < 3) & (partner == 1)) & (self_fhh == 1))
replace    self_composition = 4 if ((self_fhh == 0) & (s2p7 > 2))
lab define self_composition 0 "MHH - Co-resident" 1 "FHH - De jure" 2 "FHH - De facto" ///
    3 "FHH - Co-resident" 4 "MHH - Single", replace
lab values self_composition self_composition

*------------------------------2.2: Alternative definitions
*--------------------2.2.1: General demographic definition
gen men = (s2p2a > 17 & s2p5 == 1)
bys i00: egen aux_demographic = max(men)
gen        demographic = (aux_demographic == 0)
lab var    demographic "Demographic female headship"
lab define demographic 0 "MHH" 1 "FHH"
lab values demographic demographic

gen adult_earner = (s2p2a > 17) 
bys i00: egen n_ae = sum(adult_earner)

gen unemployed = (s5p1 == 2 & s5p2 == 11) if !missing(s5p1)
gen     adult_employed = adult_earner - unemployed
replace adult_employed = . if (adult_employed == -1)
bys i00: egen ae = sum(adult_employed)

*--------------------2.2.2: Demographic definition 
clonevar   demographic_composition = demographic
replace    demographic_composition = 2 if ((s2p7 < 3) & (partner == 0) ///
    & (demographic == 1))
replace    demographic_composition = 3 if ((demographic ==0) & (s2p7 > 2)) 
lab define demographic_composition 0 "MHH - Co-resident" 1 "FHH - De jure" ///
    2 "FHH - De facto" 3 "MHH - Single", replace
lab values demographic_composition demographic_composition

*--------------------2.2.3: Economic working head definition
note: Calculated as defined by Rosenhouse (1989)
*-------------2.2.3.1: Hours in first job (waged and self-employed)
recode  s5p16b (1 = 0.16666667) (2 = 1) (3 = 4.33333) (else = .), gen (aux_week)
gen freq1 = s5p16a * aux_week
replace s5p17 = . if (s5p17 == 998)

gen hours_primary = s5p17 * freq1

*-------------2.2.3.2: Hours in second job (waged and self-employed)
recode  s5p32b (1 = 0.1666666) (2 = 1) (3 = 4.33333) (else = .), gen (aux_week2)	
gen freq2 = s5p32a * aux_week2

gen hours_second = s5p33 * freq2
 
*-------------2.2.3.3: Total worked hours
egen    worked_hours = rsum(hours_primary hours_second) 
replace worked_hours = . if ((hours_primary == .) & (hours_second == .)) 

*-------------2.2.3.4: Defining headship working hours
bys i00: egen main_hours = max(worked_hours)
gen hh_female_w = (main_hours == worked_hours) & s2p5 == 2 
bys i00: egen fhh_work = max(hh_female_w)
lab var       fhh_work "Working head"
lab define    fhh_work 1 "Female-headed hh" 0 "Male-headed hh"
lab values    fhh_work fhh_work

*-------------2.2.3.5: Headship working hours with hh composition
clonevar   work_composition = fhh_work
replace    work_composition = 2 if (((s2p7 < 3) & (partner == 0)) & (fhh_work == 1))
replace    work_composition = 3 if (((s2p7 < 3) & (partner == 1)) & (fhh_work == 1))
replace    work_composition = 4 if ((fhh_work == 0) & (s2p7 > 2))
lab define work_composition 0 "MHH - Co-resident" 1 "FHH - De jure" 2 "FHH - De facto" ///
    3 "FHH - Co-resident" 4 "MHH - Single", replace
lab values work_composition work_composition

preserve

*--------------------2.2.4: Cash head
use "${pjdatabase}/emnv14-hh-income.dta", clear
numlabel, add

keep i00 hh_m_income
merge 1:m i00 using "${pjdatabase}/emnv14-hh-income-person.dta", gen (merge)
drop merge
keep i00 dominio4 i06 s2p00 s2p2a s2p2b s2p3 miembro s2p4 s2p5 s2p7 i_employment ///
    hh_employment hh_m_income

*----------------2.2.4.1: Major earner definition
gen share_earner = i_employment / hh_employment
bys i00: egen main_earner = max(share_earner)
gen hh_earner = (main_earner == share_earner) & s2p5 == 2 
bys i00: egen fhh_earner = max(hh_earner)
lab var       fhh_earner "Cash head: Major earner"
lab define    fhh_earner 1 "Female-head hh" 0 "Male-head hh"
lab values    fhh_earner fhh_earner

*----------------2.2.4.2: Major income contributor definition
gen share_contributor = i_employment / hh_m_income
bys i00: egen main_contributor = max(share_contributor)
gen hh_contributor = (main_contributor==share_contributor) & s2p5 == 2 
bys i00: egen fhh_contributor = max(hh_contributor)
lab var       fhh_contributor "Cash head: Major incomes contributor"
lab define    fhh_contributor 1 "Female-head hh" 0 "Male-head hh"
lab values    fhh_contributor fhh_contributor

egen     hh_tag = tag(i00)
keep if (hh_tag == 1)

keep i00 fhh_earner fhh_contributor 
save "${pjdatabase}/hh-cash-head.dta", replace

restore
merge m:1 i00 using "${pjdatabase}/hh-cash-head.dta", gen (merge)
keep if (s2p4 == 1)
drop merge

*----------------2.2.4.3: Major earner hh composition
clonevar   earner_composition = fhh_earner
replace    earner_composition = 2 if (((s2p7 < 3) & (partner == 0)) & (fhh_earner == 1))
replace    earner_composition = 3 if (((s2p7 < 3) & (partner ==1 )) & (fhh_earner == 1))
replace    earner_composition = 4 if ((fhh_earner == 0) & (s2p7 > 2)) 
lab define earner_composition 0 "MHH - Co-resident" 1 "FHH - De jure" 2 "FHH - De facto" ///
    3 "FHH - Co-resident" 4 "MHH - Single", replace
lab values earner_composition earner_composition

*----------------2.2.4.4: Major income contributor hh composition
clonevar   contributor_composition = fhh_contributor
replace    contributor_composition = 2 if (((s2p7 < 3) & (partner == 0)) ///
    & (fhh_contributor == 1))
replace    contributor_composition = 3 if (((s2p7 < 3) & (partner == 1)) ///
    & (fhh_contributor == 1))
replace    contributor_composition = 4 if ((fhh_contributor == 0) & (s2p7 > 2))

lab define contributor_composition 0 "MHH - Co-resident" 1 "FHH - De jure" ///
    2 "FHH - De facto" 3 "FHH - Co-resident" 4 "MHH - Single", replace
lab values contributor_composition contributor_composition

note: Major earner and major contributor definitions return the same results. ///
Therefore, these are analysed separately.

preserve
keep i00 dominio4 i06 s2p00 s2p2a s2p2b s2p3 miembro s2p4 s2p5 s2p6a s2p6b   ///
    s2p6c s2p7 s4p12a s5p1 s5p2 s5p3 peso2 peso3 self_fhh self_composition   ///
	demographic demographic_composition fhh_work work_composition fhh_earner ///
	fhh_contributor earner_composition contributor_composition 
save "${pjdatabase}/fhh-definition-14.dta", replace

/*====================================================================
                        3: Summary statistics
====================================================================*/
*--------------------3.1: Data merge
merge 1:1 i00 using "${pjdatabase}/emnv14-mpi.dta", gen (merge)
save "${pjdatabase}/emnv14-mpi-fhh.dta", replace

*--------------------3.2: Descriptives
tab1 self_composition demographic_composition work_composition earner_composition 

sum s2p2a if (self_composition == 3), d
tab s4p12a if (self_composition == 3)

clonevar employment = s5p1
replace employment = 1 if ((s5p2 != 11) & (s5p2 != .))
replace employment = 1 if (s5p3 == 1)
tab self_fhh employment, row
tab self_fhh fhh_work, col
tab self_fhh fhh_contributor, col
tab fhh_work s2p7, row
tab fhh_contributor s2p7, row

tab h_25p h_50p
tab1 ch_*
sum rel_cont_*

/*====================================================================
                        4: Household comparison
====================================================================*/
*--------------------4.1: General self-reported headship
local indicators m0_25p m0_50p 
foreach indicator of local indicators {
univar `indicator', by(self_fhh)
ranksum `indicator', by(self_fhh) porder
}

*--------------------4.2: Demographic
foreach indicator of local indicators {
univar `indicator', by(demographic)
ranksum `indicator', by(demographic) porder
}

*--------------------4.3: Economic working head
foreach indicator of local indicators {
univar `indicator', by(fhh_work)
ranksum `indicator', by(fhh_work) porder
}

*--------------------4.4: Cash head: major income contributor
foreach indicator of local indicators {
univar `indicator', by(fhh_contributor)
ranksum `indicator', by(fhh_contributor) porder
}

*--------------------4.5: Self-reported headship classification
foreach indicator of local indicators {
univar `indicator', by(self_composition)
conovertest `indicator', by (self_composition) ma (sidak) 
}

*--------------------4.6: Demographic definition classification
foreach indicator of local indicators {
univar `indicator', by(demographic_composition)
conovertest `indicator', by (demographic_composition) ma (sidak)
}

*--------------------4.7: Economic working headship classification with hh composition
foreach indicator of local indicators {
univar `indicator', by(work_composition)
conovertest `indicator', by (work_composition) ma (sidak)
}

*--------------------4.8: Cash head: major income contributor - composition
foreach indicator of local indicators {
univar `indicator', by(contributor_composition)
conovertest `indicator', by (contributor_composition) ma (sidak) 
}

exit 
*End of do-file

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
Notes:
1. Rosenhouse (1989). Identifying the Poor: Is “Headship” a Useful Concept? ///
(LSMS Working Paper Number 58). The World Bank.                             ///
http://documents.worldbank.org/curated/en/920711468765037968/pdf/multi0page.pdf



 







