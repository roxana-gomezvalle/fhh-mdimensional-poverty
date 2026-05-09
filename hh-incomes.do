/*====================================================================
Project      : Revisiting the linkages between female headship and ///
               (multidimensional) poverty: The case of Nicaragua
Author       : Roxana Gómez-Valle
Creation Date: 07 May 2022 
Output       : Household incomes - Based on CEPAL (2018)
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
                        1: Incomes, primary income
====================================================================*/
*---------------------------------1.1: Incomes from production
*-------------------------1.1.1: Employment incomes
*----------------------1.1.1.1: Wages and salaries
*------------------1.1.1.1: First job incomes
use "${pjdatabase}/emnv14_04_poblacion", clear
rename *, lower
numlabel, add
note: Wages and salaries from the first job are registered as net salary ///
     (gross incomes minus income taxes, social and health insurance)

**Wages
replace s5p19b = . if ((s5p19b == 98) | (s5p19b == 99))
replace s5p19a = . if ((s5p19a == 9999998)  | (s5p19a == 9999999))

gen     wages = s5p19a if (s5p19b == 5)
replace wages = s5p19a * 30.4166666667 if (s5p19b == 1)
replace wages = s5p19a * 4.2857142857 if (s5p19b == 2)
replace wages = s5p19a * 2.1428571429 if (s5p19b == 3) 
replace wages = s5p19a * 2 if (s5p19b == 4)
replace wages = s5p19a / 3 if (s5p19b == 6)
replace wages = s5p19a / 6 if (s5p19b == 7)
replace wages = s5p19a / 12 if (s5p19b == 8)

**Commissions, overtime, tips
gen commissions = s5p20b

**Thirteenth salary and paid leave
replace s5p21b = . if ((s5p21b == 9999998) | (s5p21b == 9999999))
replace s5p21c = . if (s5p21c == 99)

gen holidays = s5p21b / s5p21c

**Meals
gen meals = s5p22b

**Housing
gen housing = s5p23b

**Transportation
gen transport = s5p24b

**Clothing or uniforms
replace s5p25b = . if (s5p25b == 999999) 
gen clothing = (s5p25b * s5p25c) / 12

**Total wages and salaries first job
egen    i_wage = rsum (wages commissions holidays meals housing transport clothing)
replace i_wage = . if ((wages == .) & (commissions == .) & (holidays == .) ///
    & (meals == .) & (housing == .) & (transport == .) & (clothing == .))

*------------------1.1.1.2: Second job incomes
note: Wages and salaries from the second job are registered as net salary ///
    (gross incomes minus income taxes, social and health insurance)

**Wages
gen     wages2 = s5p35a if (s5p35b == 5)
replace wages2 = s5p35a * 30.4166666667 if (s5p35b == 1)
replace wages2 = s5p35a * 4.2857142857 if (s5p35b == 2)
replace wages2 = s5p35a * 2.1428571429 if (s5p35b == 3) 
replace wages2 = s5p35a * 2 if (s5p35b == 4)
replace wages2 = s5p35a / 3 if (s5p35b == 6)
replace wages2 = s5p35a / 6 if (s5p35b == 7)
replace wages2= s5p35a / 12 if (s5p35b == 8)

**Commissions, overtime, tips
gen commissions2 = s5p36b

**Thirtheenth salary and paid leave
gen holidays2 = s5p37b / s5p37c

**Meals
gen meals2 = s5p38b

**Housing
gen housing2 = s5p39b

**Transportation
gen transport2 = s5p40b

**Clothing and uniforms
gen clothing2 = (s5p41b * s5p41c) / 12

**Wages and salaries second job
egen    i_wage2 = rsum (wages2 commissions2 holidays2 meals2 housing2 transport2 clothing2)
replace i_wage2 = . if ((wages2 == .) & (commissions2 == .) & (holidays2 == .) ///
    & (meals2 == .) & (housing2 == .) & (transport2 == .) & (clothing2 == .))

*------------------1.1.1.3: Incomes from other job in the last 12 months
note: Wages and salaries from the second job are registered as net salary ///
    (gross incomes minus income taxes, social and health insurance)

**Wages
replace s5p50a = . if ((s5p50a == 999998) | (s5p50a == 999999))
replace s5p50b = . if ((s5p50b == 98) | (s5p50b == 99))

gen     wages3 = s5p50a if (s5p50b == 4)
replace wages3 = s5p50a * 4.2857142857 if (s5p50b == 1)
replace wages3 = s5p50a * 2.1428571429 if (s5p50b == 2) 
replace wages3 = s5p50a * 2 if (s5p50b == 3)
replace wages3 = s5p50a / 3 if (s5p50b == 5)
replace wages3 = s5p50a / 6 if (s5p50b == 6)
replace wages3 = s5p50a / 12 if (s5p50b == 7)

**Commissions, overtime, tips
replace s5p51b = . if ((s5p51b == 999998) | (s5p51b == 999999))
gen commissions3 = s5p51b 

**Thirtheenth salary and paid leave
replace s5p52b = . if ((s5p52b == 999998) | (s5p52b == 999999))
gen holidays3 = s5p52b / 12

**Meals, housing, transportation, clothing and uniforms
replace s5p53b = . if (s5p53b == 999999)
gen other3 = s5p53b

**Wages and salaries last 12 months
egen    i_wage3 = rsum (wages3 commissions3 holidays3 other3)
replace i_wage3 = . if ((wages3 == .) & (commissions3 == .) & (holidays3 == .) ///
    & (other3 == .)) 

*------------------1.1.1.4: Other job in the last 12 months
replace s5p57b = . if (s5p57b == 999999)
replace s5p57c = . if (s5p57c == 99)

gen i_wage4 = (s5p57b * s5p57c) / 12

*------------------1.1.1.5: Total incomes from wages and salaries
egen    i_wagesum = rsum(i_wage i_wage2 i_wage3 i_wage4)
replace i_wagesum = . if ((i_wage == .) & (i_wage2 == .) & (i_wage3 == .) ///
    & (i_wage4 == .))
lab var i_wagesum "Total incomes from wages and salaries"

*----------------------1.1.2: Independent employment incomes
*-----------------1.1.2.1: First independent employment
replace s5p26a = . if ((s5p26a == 9999999) | (s5p26a == 9999998)) 
replace s5p26b = . if (s5p26b > 97)

gen     i_se = s5p26a if (s5p26b == 5)
replace i_se = s5p26a * 30.4166666667 if (s5p26b == 1)
replace i_se = s5p26a * 4.2857142857 if (s5p26b == 2)
replace i_se = s5p26a * 2.1428571429 if (s5p26b == 3) 
replace i_se = s5p26a * 2 if (s5p26b == 4)
replace i_se = s5p26a / 3 if (s5p26b == 6)
replace i_se = s5p26a / 6 if (s5p26b == 7)
replace i_se = s5p26a / 12 if (s5p26b == 8)

*-----------------1.1.2.2: Second independent employment
replace s5p42b = . if (s5p42b > 97) 

gen     i_se2 = s5p42a if (s5p42b == 5)
replace i_se2 = s5p42a * 30.4166666667 if (s5p42b == 1)
replace i_se2 = s5p42a * 4.2857142857 if (s5p42b == 2)
replace i_se2 = s5p42a * 2.1428571429 if (s5p42b == 3) 
replace i_se2 = s5p42a * 2 if (s5p42b == 4)
replace i_se2 = s5p42a / 3 if (s5p42b == 6)
replace i_se2 = s5p42a / 6 if (s5p42b == 7)
replace i_se2 = s5p42a / 12 if (s5p42b == 8)

*-----------------1.1.2.3: Independent employment last 12 months
replace s5p54a = . if (s5p54a == 9999999)
replace s5p54b = . if (s5p54b == 99)

gen     i_se3 = s5p54a if (s5p54b == 5)
replace i_se3 = s5p54a * 30.4166666667 if (s5p54b == 1)
replace i_se3 = s5p54a * 4.2857142857 if (s5p54b == 2)
replace i_se3 = s5p54a * 2.1428571429 if (s5p54b == 3)  
replace i_se3 = s5p54a * 2 if (s5p54b == 4)
replace i_se3 = s5p54a / 3 if (s5p54b == 6)
replace i_se3 = s5p54a / 6 if (s5p54b == 7)
replace i_se3 = s5p54a / 12 if (s5p54b == 8)

*-----------------1.1.2.4: Total independent employment
egen    i_sesum = rsum(i_se i_se2 i_se3)
replace i_sesum = . if ((i_se == .) & (i_se2 == .) & (i_se3 == .))
lab var i_sesum "Total incomes from independent employment"

*----------------------1.1.3: Total employment incomes
egen    i_employment = rsum (i_sesum i_wagesum)
replace i_employment = . if ((i_sesum == .) & (i_wagesum == .))
lab var i_employment "Total employment incomes: wages and independent"

bys i00: egen hh_employment= sum(i_employment) //Creating database with primary incomes
lab var       hh_employment "HH monthly incomes from employment"
replace       hh_employment = . if (hh_employment == 0)
save "${pjdatabase}/emnv14-hh-income-person.dta", replace

egen     hh_tag = tag(i00)
keep if (hh_tag == 1)
keep i00 i06 dominio4 peso2 peso3 hh_employment
preserve

*-------------------------1.1.4: Imputed rent
use "${pjdatabase}/emnv14_02_datos_de_la_vivienda_y_el_hogar.dta", clear
replace s1p14a = . if (s1p14a >= 99998)
replace s1p14b = . if (s1p14b == 99998)
replace s1p14b = s1p14b * 26.3612 //Variable in USD

egen    hh_rent = rsum(s1p14a s1p14b)
replace hh_rent = . if ((s1p14a == .) & (s1p14b == .))
lab var hh_rent "Imputed rent"

keep i00 hh_rent //Keeping relevant variable
save "${pjdatabase}/emnv14-hh-imputed-rent.dta", replace

restore
merge m:1 i00 using "${pjdatabase}/emnv14-hh-imputed-rent.dta", gen (_merge)
drop _merge

*-------------------------1.1.5: Total incomes from production
note: Inputed rent is not added here. Income will be adjusted with the inputed rent at a later stage

gen     hh_prod = hh_employment
lab var hh_prod "Total household incomes from production except inputed rent"
preserve

*---------------------------------1.2: Asset ownership incomes
*-----------------------1.2.1: Leasing
use "${pjdatabase}/emnv14_15_parte_c3_de_la_seccion_7.dta", clear
numlabel, add

keep if ((s7c3cod == 1) | (s7c3cod == 2)) //Keeping incomes from leasing
replace s7p35b = s7p35b * 26.3612 //Variable in USD

egen    h_total = rsum (s7p35a s7p35b)
replace h_total = . if ((s7p35a == .) & (s7p35b == .))

bys i00: egen hh_leasing = sum(h_total)
replace       hh_leasing = . if (hh_leasing == 0)
lab var       hh_leasing "Incomes for leasing"

egen     hh_tag = tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_leasing
save "${pjdatabase}/emnv14-hh-leasing.dta", replace

restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-leasing.dta", gen (_merge)
drop _merge
preserve

*------------------1.2.2: Financial assets
use "${pjdatabase}/emnv14_16_parte_c4_de_la_seccion_7.dta", clear
numlabel, add

keep if ((s7c4cod < 4) | (s7c4cod == 5) | (s7c4cod == 6)) //Gains from financial sources

replace s7p37b = s7p37b * 26.3612 //Variable in USD
egen    h_total = rsum (s7p37a s7p37b)
replace h_total = h_total / 12
replace h_total = . if ((s7p37a == .) & (s7p37b == .))

bys i00: egen hh_financial = sum(h_total)
replace       hh_financial = . if (hh_financial == 0)
lab var       hh_financial "Incomes for financial assets"

egen     hh_tag = tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_financial
save "${pjdatabase}/emnv14-hh-financial.dta", replace

restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-financial.dta", gen (_merge)
drop _merge

*------------------1.2.3: Total assets
egen    hh_asset = rsum(hh_financial hh_leasing)
replace hh_asset = . if ((hh_financial == .) & (hh_leasing == .))
lab var hh_asset "Total asset ownoership incomes"

*---------------------------------1.3: Total primary incomes
egen    hh_primary = rsum (hh_asset hh_prod)
replace hh_primary = . if ((hh_asset == .) & (hh_prod == .))
lab var hh_primary "Household primary incomes"
preserve

/*====================================================================
                        2: Transfers
====================================================================*/
*---------------------------------2.1: Retirement and pensions
use "${pjdatabase}/emnv14_15_parte_c3_de_la_seccion_7.dta", clear
numlabel, add

keep if (s7c3cod > 3)
replace s7p35b = s7p35b * 26.3612

egen    pension = rsum(s7p35a s7p35b)
replace pension = . if ((s7p35a == .) & (s7p35b == .))

bys i00: egen hh_pension = sum(pension)
replace       hh_pension = . if (hh_pension == 0)
lab var       hh_pension "HH monthly pensions"

egen     hh_tag = tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_pension
save "${pjdatabase}/emnv14-hh-pensions.dta", replace 

restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-pensions.dta", gen (_merge)
drop _merge
preserve

*---------------------------------2.2: Other transfers
*-------------------2.2.1: School meals
use "${pjdatabase}/emnv14_13_parte_c1_de_la_seccion_7.dta", clear
numlabel, add

replace s7p29 = . if (s7p29 > 33)
gen i_meals = (s7p30 * s7p29 * s7p28 * 2) if (s7p27 == 1)

bys i00: egen hh_meals = sum(i_meals)
replace       hh_meals = . if (hh_meals == 0)
replace       hh_meals = 0 if (i_meals == 0)
lab var       hh_meals "Transfer received as school meals"

egen     hh_tag = tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_meals
save "${pjdatabase}/emnv14-hh-school-meals.dta", replace 

restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-school-meals.dta", gen (_merge)
drop _merge
preserve

*------------------2.2.2: School supplies
use "${pjdatabase}/emnv14_14_parte_c2_de_la_seccion_7.dta", clear
numlabel, add

drop if (s7c2cod == 6) //Undefined good
gen i_supplies = (s7p32 * s7p33) / 12

bys i00: egen hh_supplies = sum (i_supplies)
replace       hh_supplies = . if (hh_supplies == 0)
lab var       hh_supplies "Transfer received as school supplies"

egen     hh_tag = tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_supplies
save "${pjdatabase}/emnv14-hh-school-supplies.dta", replace 

restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-school-supplies.dta", gen (_merge)
drop _merge
preserve

*---------------2.2.3: Scholarships
use "${pjdatabase}/emnv14_15_parte_c3_de_la_seccion_7.dta", clear
numlabel, add

keep if (s7c3cod == 3)
replace s7p35b = s7p35b * 26.3612
egen    hh_scholarship = rsum(s7p35a s7p35b)
replace hh_scholarship = . if ((s7p35a == .) & (s7p35b == .))
lab var hh_scholarship "Transfer received as scholarship"

egen     hh_tag = tag(i00) //Keeping relevant variable
keep if (hh_tag == 1)
keep i00 hh_scholarship
save "${pjdatabase}/emnv14-hh-scholarships.dta", replace 

restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-scholarships.dta", gen (_merge)
drop _merge
preserve

*---------------2.2.4: Other transfers
use "${pjdatabase}/emnv14_16_parte_c4_de_la_seccion_7.dta", clear
numlabel, add

keep if ((s7c4cod==7) | (s7c4cod==8) | (s7c4cod==4)) // Other transfers in the last 12 months
replace s7p37b = s7p37b * 26.3612

egen    aux_other = rsum (s7p37a s7p37b)
replace aux_other = . if ((s7p37a == .) & (s7p37b == .))
gen i_other = aux_other / 12
bys i00: egen hh_other = sum(i_other)
replace       hh_other = . if (hh_other == 0)
lab var       hh_other "Other transfers received"

egen    hh_tag = tag(i00)
keep if (hh_tag == 1)

keep i00 hh_other
save "${pjdatabase}/emnv14-hh-other-transfers.dta", replace
restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-other-transfers.dta", gen (_merge)
drop _merge

*---------------2.2.5: Total other transfers
egen    hh_otransfers = rsum(hh_meals hh_supplies hh_scholarship hh_other)
lab var hh_otransfers "Total other transfers"
replace hh_otransfers = . if ((hh_meals == .) & (hh_supplies == .) ///
    & (hh_scholarship == .) & (hh_other == .))

*---------------------------------2.3: Total transfers
egen    hh_transfers = rsum (hh_pension hh_otransfers)
replace hh_transfers =.  if ((hh_pension == .) & (hh_otransfers == .))
lab var hh_transfers "Total transfers received in the household"

/*====================================================================
                        3: Other incomes
====================================================================*/
*------------------3.1: Remittances
preserve
use "${pjdatabase}/emnv14_17_parte_c5_de_la_seccion_7.dta", clear
numlabel, add

keep if (s7c5cod < 3)
replace s7p40b = s7p40b * 26.3612
egen aux_remittances = rowtotal(s7p40a s7p40b)
recode s7p39 (1 = 4.2857142857) (2 = 2) (3 = 1) (4 = 0.3333333333) (5 = 0.1666666667) ///
    (6 = 0.0833333333) (else = .), gen(freq)
gen     i_remittances = aux_remittances * freq
lab var i_remittances "Remittances by category"
drop freq

bys i00: egen hh_remittances = sum(i_remittances)
replace       hh_remittances = . if (hh_remittances == 0)
lab var       hh_remittances "Incomes received as remittances"

egen     hh_tag = tag(i00)
keep if (hh_tag == 1)
keep i00 hh_remittances
save "${pjdatabase}/emnv14-hh-remittances.dta", replace 
restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-remittances.dta", gen (_merge)
drop _merge

*------------------3.2: Inheritance
preserve
use "${pjdatabase}/emnv14_16_parte_c4_de_la_seccion_7.dta", clear
numlabel, add

keep if (s7c4cod == 9)
replace s7p37b = s7p37b * 26.3612
egen    i_inheritance = rsum (s7p37a s7p37b)
replace i_inheritance = . if ((s7p37a == .) & (s7p37b == .))
gen     hh_inheritance = i_inheritance / 12
lab var hh_inheritance "Incomes received as inheritance"

egen     hh_tag = tag(i00)
keep if (hh_tag == 1)
keep i00 hh_inheritance
save "${pjdatabase}/emnv14-hh-inheritance.dta", replace
restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-inheritance.dta", gen (_merge)
drop _merge

*------------------3.3: Other incomes
preserve
use "${pjdatabase}/emnv14_16_parte_c4_de_la_seccion_7.dta", clear
numlabel, add

keep if (s7c4cod == 10)
replace s7p37b = s7p37b * 26.3612
egen    aux_other = rsum (s7p37a s7p37b)
replace aux_other = . if ((s7p37a == .) & (s7p37b == .))
gen i_other = aux_other / 12

bys i00: egen hh_other2 = sum(i_other)
replace       hh_other2 = . if (hh_other2 == 0)
lab var       hh_other2 "Other incomes"

egen     hh_tag = tag(i00)
keep if (hh_tag == 1)
keep i00 hh_other2
save "${pjdatabase}/emnv14-hh-other-transfers2.dta", replace
restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-other-transfers2.dta", gen (_merge)
drop _merge

*------------------3.4: Total other incomes
egen    hh_otherinc = rsum (hh_remittances hh_other2 hh_inheritance)
replace hh_otherinc = . if ((hh_remittances == .) & (hh_other2 == .) ///
    & (hh_inheritance == .))
lab var hh_otherinc "Other incomes"

/*====================================================================
                        4: Total household incomes
====================================================================*/
egen    hh_incomes = rsum(hh_otherinc hh_transfers hh_primary)
replace hh_incomes = . if ((hh_otherinc == .) & (hh_transfers == .) ///
    & (hh_primary == .))
lab var hh_incomes "Total monthly household incomes"
preserve

/*====================================================================
                        5: Paid transfers
====================================================================*/
use "${pjdatabase}/emnv14_10_parte_b2_de_la_seccion_7.dta", clear
numlabel, add

keep if ((s7b2cod == 21) | (s7b2cod == 24))
bys i00: egen hh_ptransfers1 = sum(s7p20)

egen     hh_tag = tag(i00)
keep if (hh_tag == 1)
keep i00 hh_ptransfers1
save "${pjdatabase}/emnv14-paid-transfers.dta", replace
restore
merge 1:m i00 using "${pjdatabase}/emnv14-paid-transfers.dta", gen (_merge)
drop _merge

preserve
use "${pjdatabase}/emnv14_12_parte_b4_de_la_seccion_7.dta", clear
numlabel, add

keep if (s7b4cod == 13)
replace s7p25 = . if ((s7p25 == 9999998) | (s7p25 == 9999999))
gen hh_ptransfers2 = s7p25 / 12

keep i00 hh_ptransfers2
save "${pjdatabase}/emnv14-other-paid-transfers.dta", replace
restore
merge 1:m i00 using "${pjdatabase}/emnv14-other-paid-transfers.dta", gen (_merge)
drop _merge

egen    paid_transfers = rsum (hh_ptransfers1 hh_ptransfers2)
lab var paid_transfers "Transfer made in the household"

/*====================================================================
                        6: Household available incomes
====================================================================*/
gen     hh_avincomes = hh_incomes - paid_transfers
lab var hh_avincomes "Monthly household available incomes"
replace hh_avincomes = 0 if (hh_avincomes < 0) 

/*====================================================================
                        7: Adjusting imputed rent
====================================================================*/
gen aux_rent = hh_rent / hh_avincomes
replace hh_rent = 0 if (hh_avincomes == 0)
replace hh_rent = hh_avincomes if (hh_rent > hh_avincomes)

egen    hh_m_income = rsum(hh_rent hh_avincomes)
lab var hh_m_income "Total househol monthly available incomes with imputed rent"
preserve
/*====================================================================
                        8: Income per capita
====================================================================*/
#Number of household members
use "${pjdatabase}/emnv14_04_poblacion", clear
rename *, lower
numlabel, add
 
bysort i00: egen hh_members = count(i00)
lab var          hh_members "HH size"

egen     hh_tag = tag(i00)
keep if (hh_tag == 1)
keep i00 hh_members
save "${pjdatabase}/emnv14-hh-members.dta", replace
restore
merge 1:m i00 using "${pjdatabase}/emnv14-hh-members.dta", gen (_merge)
drop _merge

#Household income per capita
gen     hh_m_pcincome = hh_m_income / hh_members
lab var hh_m_pcincome "HH monthly total per capita income"

/*====================================================================
                        9: Poverty lines
====================================================================*/
*--------------------9.1: General poverty
note: Distinction of poverty lines between urban and rural areas
gen        poverty_gen = (hh_m_pcincome < 2371.03) if (i06 == 1) 
replace    poverty_gen = (hh_m_pcincome < 1733.79) if (i06 == 2)
lab define poverty_gen 0 "No poor" 1 "Poor"
lab values poverty_gen poverty_gen
lab var    poverty_gen "General poverty"

save "${pjdatabase}/emnv14-hh-income.dta", replace

exit

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1. CEPAL (2018) - Medición de la pobreza por ingresos: Actualización metodológica y resultados. ///
Metodologías CEPAL 2.




 


