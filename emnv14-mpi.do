/*====================================================================
Project:       Revisiting the linkages between female headship and ///
               (multidimensional) poverty: The case of Nicaragua
Author:        Roxana Gómez-Valle
Creation Date: 20 Apr 2022 
Output:        MPI-LA based on Santos et al. (2015)
====================================================================*/

/*====================================================================
                        0: Program set up
====================================================================*/
global pjdatabase "C:\Users\User\OneDrive\MPI - FHH\Database"
global dofiles   "C:\Users\User\OneDrive\MPI - FHH\Do-files"

set more off , perm
clear all
version 15.1

/*====================================================================
                        1: Housing dimension
====================================================================*/
use "${pjdatabase}/emnv14_02_datos_de_la_vivienda_y_el_hogar.dta", clear
rename *, lower
numlabel, add

*--------------------1.1: Housing materials indicators
*---1.1.1: Floor
tab s1p5, miss
recode s1p5 (1/4 = 0) (5 = 1) (else = .), gen(d_floor)
lab var d_floor "Floor materials"

*---1.1.2: Roof
tab s1p6, miss
recode s1p6 (1/4 = 0) (5/6 = 1) (else = .), gen(d_roof)
lab var d_roof "Roof materials"

*---1.1.3: Wall
tab s1p4, miss
recode s1p4 (1/12 = 0) (13/15 = 1) (else = .), gen(d_wall)
lab var d_wall "Wall materials"

*---1.1.4: Overall housing materials indicator
egen    hh_d_materials = anymatch(d_wall d_floor d_roof), v(1)
lab var hh_d_materials "Housing materials"
gen     aux_miss = ((d_wall == .) & (d_floor == .) & (d_roof == .))
replace hh_d_materials = . if aux_miss == 1 
preserve

*--------------------1.2: People per room indicator
use "${pjdatabase}/emnv14_04_poblacion.dta", clear
rename *, lower
numlabel, add

bysort i00: egen hh_members = count(i00) //Counting number of household members
egen hh_tag = tag(i00) 
keep if (hh_tag == 1)
keep i00 hh_tag hh_members
save "${pjdatabase}/emnv14-hh-members.dta", replace

restore //Generating overcrowding indicator with variables needed
merge 1:1 i00 using "${pjdatabase}/emnv14-hh-members.dta", keepusing(hh_members)
drop _merge

gen pproom = hh_members / s1p8 
gen     hh_d_overcrowding = (pproom >= 3) if !missing(s1p8)
lab var hh_d_overcrowding "Overcrowding"
tab     hh_d_overcrowding [aw=peso2]

*--------------------1.3: Housing tenure indicator
tab s1p11, miss

recode s1p11 (1/4 6 = 0) (5/7 = 1) (else =. ), gen(hh_d_tenure)
lab var   hh_d_tenure "Housing tenure"
tab s1p11 hh_d_tenure, miss

/*====================================================================
                        2: Basic services dimension
====================================================================*/
*--------------------2.1: Improved water indicator
recode s1p15 (1 = 0) (2/9 = 1) (else = .), gen(hh_d_water)
replace hh_d_water = 0 if (((s1p15 == 2) | (s1p15 == 3) | (s1p15 == 4)) & (i06 == 2))
lab var hh_d_water "Improved water" 

*--------------------2.2: Improved sanitation indicator
recode s1p18 (3 4 = 0) (1 2 5 6 = 1) (else = .), gen(hh_d_sanitation)

replace   hh_d_sanitation = 0 if ((s1p18 == 2) & (i06 == 2))
lab var   hh_d_sanitation "Improved sanitation"
tab       hh_d_san [aw=peso2]
tab s1p18 hh_d_sanitation if (i06 == 1), miss
tab s1p18 hh_d_sanitation if (i06 == 2), miss

*--------------------2.3: Energy indicator
*---2.3.1: Lighting
recode s1p21 (1/3 = 0) (4/7 9 = 1) (else = .), gen(d_lighting)

lab var   d_lighting "Lighting"
tab s1p21 d_lighting, miss

*---2.3.2: Cooking fuel
recode s1p25 (2 4 5 = 0) (1 3 = 1) (else = .), gen(d_fuel)

lab var   d_fuel "Cooking fuel"
tab s1p25 d_fuel, miss

*---2.3.3: Overall energy indicator 
egen    hh_d_energy = anymatch(d_lighting d_fuel), v(1)
lab var hh_d_energy "Energy indicator"
gen aux_miss2 = ((d_lighting == .) & (d_fuel == .))
replace hh_d_energy = . if (aux_miss2 == 1)
tab     hh_d_energy [aw=peso2]

keep i00 i06 dominio4 peso2 peso3 hh_d_materials hh_d_overcrowding hh_d_tenure ///
    hh_d_water hh_d_sanitation hh_d_energy
preserve

/*====================================================================
                        3: Living standard dimension
====================================================================*/
*--------------------3.1: Monetary resources indicator
qui {
    do "${dofiles}/hh-incomes.do"
}

restore 
merge 1:1 i00 using "${pjdatabase}/emnv14-hh-income.dta", keepusing(poverty_ge)
drop _merge
rename poverty_gen hh_d_income

tab hh_d_income [aw=peso2]
preserve

*--------------------3.2: Durable goods indicator
use "${pjdatabase}/emnv14_18_parte_d_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

gen  tv_bw = ((s7dcod == 2) & (s7p47 == 1)) //TV as asset - author's addition
bys i00: egen tv = max(tv_bw)
drop tv_bw
replace s7p47 = 1 if ((s7dcod == 3) & (tv == 1))

keep if ((s7dcod == 3) | (s7dcod == 4) | (s7dcod == 16) | (s7dcod == 22)) 
//Keeping relevant assets

gen     d_assets = (s7p47 == 2) //Generating asset indicator
replace d_assets = . if ((i06 == 1) & (s7dcod == 3))
replace d_assets = . if ((i06 == 2) & (s7dcod == 16))

byso i00: egen hh_d_assets = min(d_assets)
lab var        hh_d_assets "Durable goods"

egen     hh_tag = tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_d_assets
save "${pjdatabase}/emnv14-hh-assets.dta", replace

restore
merge 1:1 i00 using "${pjdatabase}/emnv14-hh-assets.dta", keepusing(hh_d_assets)
drop _merge

tab hh_d_assets [aw=peso2]
preserve

/*====================================================================
                        4: Education dimension
====================================================================*/
use "${pjdatabase}/emnv14_04_poblacion.dta", clear
rename *, lower
numlabel, add

*--------------------4.1: Children's school attendance indicator
keep if ((s2p2a >= 6) & (s2p2a <= 17)) //Restricting database to children

recode s4p15 (1 = 0) (else = 1), gen(d_attendance)
bys i00: egen hh_d_attendance = max(d_attendance)
lab var       hh_d_attendance "Children's School attendance"
replace       hh_d_attendance = 0 if missing(hh_d_attend) 
tab           hh_d_attendance [aw=peso2]

*--------------------4.2: Schooling gap indicator
*-------------4.2.1: Schooling years
recode s4p12a (2 3 12 = 0) (4 = 6) (5/7 = 9) (8/9 = 11) (10 = 16) (11 = 18) ///
    (else = .), gen(school_level)
gen school_grade = s4p12b

egen    y_schooling = rowtotal(school_level school_grade)
lab var y_schooling "Years of education"

*-------------4.2.2: Schooling gap
recode s2p2a (6 = 1) (7 = 2) (8 = 3) (9 = 4) (10 = 5) (11 = 6) (12 = 7) (13 = 8) ///
    (14 = 9) (15 = 10) (16 = 11) (17 = 12), gen(ideal_grade)
gen gap_grade = ideal_grade - y_schooling
gen d_schoolgap = (gap_grade > 2)

bys i00: egen hh_d_schoolgap = max(d_schoolgap)
lab var       hh_d_schoolgap "Schooling gap"
replace       hh_d_schoolgap = 0 if missing(hh_d_schoolgap) 
tab           hh_d_schoolgap [aw=peso2]

egen     hh_tag = tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_d_attendance hh_d_schoolgap
save "${pjdatabase}/emnv14-hh-attendance-gap.dta", replace

restore //Unifying database
merge 1:1 i00 using "${pjdatabase}/emnv14-hh-attendance-gap.dta" ///
    , keepusing(hh_d_attendance hh_d_schoolgap)
drop _merge
preserve

*--------------------4.3: Adult schooling achievement indicator
use "${pjdatabase}/emnv14_04_poblacion.dta", clear
rename *, lower
numlabel, add

*---------------4.3.1: Schooling years
keep if (s2p2a >= 20) //Restricting to adults

recode s4p12a (2 3 12 = 0) (4 = 6) (5/7 = 9) (8/9 = 11) (10 = 16) (11 = 18) ///
    (else = .), gen(school_level)
recode s4p12b (8 9 = .), gen(school_grade)
egen y_schooling = rowtotal(school_level school_grade)

*---------------4.3.2: Schooling achievement
gen     d_adultschooling = (y_schooling < 9) if (s2p2a <= 59) & (i06 == 1) 
//The author makes a distinction between urban and rural areas
replace d_adultschooling = 1 if ((y_schooling < 6) & (s2p2a >= 60))
replace d_adultschooling = 1 if ((y_schooling < 6) & (i06 == 2))
replace d_adultschooling = 0 if missing(d_adultschool)

bys i00: egen hh_d_adultschooling = max(d_adultschool)
lab var       hh_d_adultschooling "Adult Schooling achievement"

egen     hh_tag = tag(i00) //Keeping relevant variables
keep if (hh_tag == 1)  
keep i00 hh_d_adultschooling
save "${pjdatabase}/emnv14-hh-adult-schooling.dta", replace

restore
merge 1:1 i00 using "${pjdatabase}/emnv14-hh-adult-schooling.dta" ///
    , keepusing(hh_d_adultschooling)
drop _merge

replace hh_d_adultschooling = 0 if missing(hh_d_adultschooling)
tab     hh_d_adultschooling [aw=peso2]
preserve

/*====================================================================
                 5: Employment and social protection dimension
====================================================================*/
*--------------------5.1: Employment indicator
use "${pjdatabase}/emnv14_04_poblacion.dta", clear
rename *, lower
numlabel, add

keep if ((s2p2a >= 15) & (s2p2a <= 65)) // Restricting to working age population

gen     d_emp = (s5p1 == 2)
replace d_emp = 0 if ((s5p2 == 1) | (s5p2 == 2) | (s5p2 == 3) | (s5p2 == 4) ///
                    | (s5p2 == 5))
replace d_emp = 0 if ((s5p4 == 1) | (s5p4 == 2) | (s5p4 == 3) | (s5p4 == 4)) 
replace d_emp = 0 if ((s5p10 == 1) | (s5p10 == 2) | (s5p10 == 3) | (s5p10 == 5) ///
                    | (s5p10 == 6))
replace d_emp = 1 if ((s5p1 == 1) & (s5p18 > 5))

bys i00: egen hh_d_emp = max(d_emp)
lab var       hh_d_emp "Employment indicator"

egen     hh_tag=tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_d_emp
save "${pjdatabase}/emnv14-hh-employment.dta", replace

restore
merge 1:1 i00 using "${pjdatabase}/emnv14-hh-employment.dta", keepusing(hh_d_emp)
drop _merge

replace hh_d_emp = 0 if missing(hh_d_emp) 
tab     hh_d_emp [aw=peso2]
preserve

*--------------------5.2: Social protection indicator
use "${pjdatabase}/emnv14_15_parte_c3_de_la_seccion_7.dta", clear

keep if (s7c3cod == 5) //Keeping social protection variable
gen h_pension = (s7p34 == 2)

keep i00 h_pension
save "${pjdatabase}/emnv14-hh-retirement.dta", replace
merge 1:m i00 using "${pjdatabase}/emnv14_04_poblacion.dta", gen (_merge)
drop _merge

replace s5p28a = . if (s5p28a == 9) //Defining relevant groups
gen eld = (s2p2a > 65) if !missing(s2p2a)
bys i00: egen h_elderly = max(eld)
replace h_pension = 0 if (h_elderly == 0)

gen     d_sp = (s3p11 == 6) if !missing(s3p11) //Social protection indicator
replace d_sp = 0 if (s5p28a == 1)
replace d_sp = 0 if (h_pension == 0)
 
bys i00: egen hh_d_sp = min(h_pension)
lab var       hh_d_sp "Social protection indicator"
tab           hh_d_sp [aw=peso2]

egen     hh_tag = tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_d_sp
save "${pjdatabase}/emnv14-hh-social-protection.dta", replace

restore
merge 1:1 i00 using "${pjdatabase}/emnv14-hh-social-protection.dta", keepusing(hh_d_sp)
drop _merge

/*====================================================================
                        6: Calculating MPI
====================================================================*/
*--------------------6.1: Defining weights
*--------------6.1.1:Housing 22.2%
gen w_materials    = (100 / 4.5 / 3)
gen w_overcrowding = (100 / 4.5 / 3)
gen w_tenure       = (100 / 4.5 / 3)

*--------------6.1.2:Basic services 22.2%
gen w_water      = (100 / 4.5 / 3)
gen w_sanitation = (100 / 4.5 / 3)
gen w_energy     = (100 / 4.5 / 3)

*--------------6.1.3:Living standard 22.2%
gen w_income = (100 / 4.5 / 1.5)
gen w_assets = (100 / 4.5 / 3)

*--------------6.1.4:Education 22.2%
gen w_attendance  = (100 / 4.5 / 3)
gen w_schoolgap   = (100 / 4.5 / 3)
gen w_adultschooling = (100 / 4.5 / 3)

*--------------6.1.5:Employment and social protection 11.1%
gen w_emp = (100 / 4.5 / 3)
gen w_sp  = (100 / 4.5 / 6)

*--------------------6.2: Weighted deprivation matrix
local indicators materials overcrowding tenure water sanitation energy income ///
    assets attendance schoolgap adultschooling emp sp
foreach indicator of local indicators {
    lab var w_`indicator' "Weight of `indicator'"
	lab var hh_d_`indicator' "Household deprivad in `indicator'"
	
	gen     w_hh_d_`indicator' = hh_d_`indicator' * w_`indicator'
    lab var w_hh_d_`indicator' "Weighted deprivation of `indicator'"
}

*--------------------6.3: Counting vector
egen cvec = rowtotal(w_hh_d_materials w_hh_d_overcrowding w_hh_d_tenure      ///
    w_hh_d_water w_hh_d_sanitation w_hh_d_energy w_hh_d_income w_hh_d_assets ///
	w_hh_d_attendance w_hh_d_schoolgap w_hh_d_adultschooling w_hh_d_emp w_hh_d_sp)
lab var cvec "Counting vector"

forvalue k = 10(1)100 {
    gen h_`k'p = cvec >= `k'
    lab var h_`k'p "Poverty indentification with k=`k'%"
	
    gen a_`k'p = cvec if (h_`k'p == 1)
    lab var a_`k'p "Individual deprivation share with k=`k'%"
	
    gen     m0_`k'p = 0
    replace m0_`k'p = cvec if (h_`k'p == 1)
    lab var m0_`k'p "Individual censored ci with k=`k'%"
}

sum h_*p a_*p m0_*p, sep(10)
sum h_25p a_25p m0_25p, sep(10)

*--------------------6.4: Headcount for multidimensional poor households
local k = 25

foreach indicator of local indicators {
    capture noisily
    gen     ch_`indicator'_k`k'p = ((cvec >= `k') & (hh_d_`indicator' == 1))
    lab var ch_`indicator'_k`k'p "Censored headcount of dimension `indicator' with k=`k'%"
}
sum ch_*_k25p, sep(12)

*--------------------6.5: Contributions per dimension
local k = 25
sum m0_`k'p

local m0_`k'p = r(mean)
foreach indicator of local indicators {
    gen     abs_cont_`indicator'_k`k'p = ch_`indicator'_k`k'p * w_`indicator'
    lab var abs_cont_`indicator'_k`k'p "Absolute contribution of dimension `indicator' to M0 with k=`k'%"
	 
    gen     rel_cont_`indicator'_k`k'p = (ch_`indicator'_k`k'p * w_`indicator') / (m0_`k'p)
    lab var rel_cont_`indicator'_k`k'p "Relative contribution of dimension `indicator' to M0 with k=`k'%"
}
sum abs_cont_*_k25p, sep(12)
sum rel_cont_*_k25p, sep(12)

/*====================================================================
                        7: Final steps
====================================================================*/
save "${pjdatabase}/emnv14-mpi.dta", replace

exit
*End of do-file

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1. Santos, M.E., Villatoro, P., Mancero, X., & Gerstenfeld, P. (2015). ///
A Multidimensional Poverty Index for Latin America (OPHI Working Paper No. 79). ///
Oxford University Press. https://www.ophi.org.uk/wp-content/uploads/OPHIWP079.pdf 
