#-------------------------------------------------------------#
#     Revisiting female headship and multidimensional poverty:
#     Insights from the Nicaraguan context
#     Last modified: 26 July 2026                  
#-------------------------------------------------------------#

#0. Loading libraries----
library(here)
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(lmtest)
library(robustbase)
library(janitor)
library(PMCMRplus)

#Household incomes estimation----
#1. Incomes----
#1.1. Incomes from production----
#1.1.1. Employment incomes----
#1.1.1.1. Wages and salaries: First job incomes----

#Loading the database
emnv14_hh_income_person <- 
  read_dta(here("emnv14_04_poblacion.dta"))

#Wages
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p19a = 
           if_else(s5p19a %in% c(9999998, 9999999), 
                   NA_real_, 
                   s5p19a))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p19b = 
           if_else(s5p19b %in% c(98, 99), 
                   NA_real_, 
                   s5p19b))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(wages = 
           case_when(
             s5p19b == 1  ~ s5p19a * 30.4166666667,
             s5p19b == 2  ~ s5p19a * 4.2857142857,
             s5p19b == 3  ~ s5p19a * 2.1428571429,
             s5p19b == 4  ~ s5p19a * 2,
             s5p19b == 5  ~ s5p19a,
             s5p19b == 6  ~ s5p19a / 3,
             s5p19b == 7  ~ s5p19a / 6,
             s5p19b == 8  ~ s5p19a / 12,
             s5p19b %in% c(98, 99) ~ NA_real_
           ))

#Commissions, overtime, tips
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(commissions = s5p20b)

#Thirteenth salary and paid leave
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p21b = 
           if_else(s5p21b %in% c(9999998, 9999999), 
                   NA_real_, 
                   s5p21b),
         s5p21c = 
           if_else(s5p21c %in% c(99), 
                   NA_real_, 
                   s5p21c))

emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate (holidays = s5p21b / s5p21c)

#Meals
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(meals = s5p22b)

#Housing
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(housing = s5p23b)

#Transportation
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(transport = s5p24b)

#Clothing or uniforms
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p25b = 
           if_else(s5p25b %in% c(999999), 
                   NA_real_, 
                   s5p25b),
         clothing = ((s5p25b * s5p25c) / 12))

#Total wages and salaries first job
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(
    i_wage = if_else(
      is.na(wages) & is.na(commissions) & is.na(holidays) & is.na(meals) & 
        is.na(housing) & is.na(transport) & is.na(clothing), 
      NA_real_,
      rowSums(across(c(wages, commissions, holidays, meals, housing, transport, 
                       clothing)), na.rm = TRUE)
    )
  )

#1.1.1.2: Wages and salaries: Second job incomes----
#Wages
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(wages2 = 
           case_when(
             s5p35b == 1  ~ s5p35a * 30.4166666667,
             s5p35b == 2  ~ s5p35a  * 4.2857142857,
             s5p35b == 3  ~ s5p35a * 2.1428571429,
             s5p35b == 4  ~ s5p35a  * 2,
             s5p35b == 5  ~ s5p35a,
             s5p35b == 6  ~ s5p35a  / 3,
             s5p35b == 7  ~ s5p35a  / 6,
             s5p35b == 8  ~ s5p35a  / 12,
           )
  )

#Commissions, overtime, tips
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(commissions2 = s5p36b)

#Thirteenth salary and paid leave
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(holidays2 = s5p37b / s5p37c)

#Meals
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(meals2 = s5p38b)

#Housing
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(housing2 = s5p39b)

#Transportation
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(transport2 = s5p40b)

#Clothing and uniforms
emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(clothing2 = ((s5p41b * s5p41c) / 12))

#Wages and salaries second job
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(
    i_wage2 = 
      if_else(
        is.na(wages2) & is.na(commissions2) & is.na(holidays2) & is.na(meals2) & 
          is.na(housing2) & is.na(transport2) & is.na(clothing2),
        NA_real_,
        rowSums(across(c(wages2, commissions2, holidays2, meals2, housing2, transport2, 
                         clothing2)), na.rm = TRUE)
      )
  )

#1.1.1.3. Incomes from other job in the last 12 months----
#Wages
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p50a = 
           if_else(s5p50a %in% c(999998, 999999), 
                   NA_real_, 
                   s5p50a),
         s5p50b = 
           if_else(s5p50b %in% c(98, 99), 
                   NA_real_, 
                   s5p50b))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(wages3 = 
           case_when(
             s5p50b == 1 ~ s5p50a  * 4.2857142857,
             s5p50b == 2 ~ s5p50a * 2.1428571429,
             s5p50b == 3 ~ s5p50a  * 2,
             s5p50b == 4 ~ s5p50a,
             s5p50b == 5 ~ s5p50a  / 3,
             s5p50b == 6 ~ s5p50a  / 6,
             s5p50b == 7 ~ s5p50a  / 12,
           )
  )

#Commissions, overtime, tips
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p51b = 
           if_else(s5p51b %in% c(999998, 999999), 
                   NA_real_, 
                   s5p51b))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(commissions3 = s5p51b)

#Thirteenth salary and paid leave
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p52b = 
           if_else(s5p52b %in% c(999998, 999999), 
                   NA_real_, 
                   s5p52b))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(holidays3 = s5p52b / 12)

#Meals, housing, transportation, clothing anf uniforms
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p53b = 
           if_else(s5p53b %in% c(999999), 
                   NA_real_, 
                   s5p53b))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(other3 = s5p53b)

#Wages and salaries last 12 months
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(
    i_wage3 = 
      if_else(
        is.na(wages3) & is.na(commissions3) & is.na(holidays3) & is.na(other3),
        NA_real_,
        rowSums(across(c(wages3, commissions3, holidays3, other3)), na.rm = TRUE)
      )
  )

#1.1.1.4. Other job in the last 12 months----
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p57b = 
           if_else(s5p57b %in% c(999999), 
                   NA_real_, 
                   s5p57b),
         s5p57c =
           if_else(s5p57c %in% c(99),
                   NA_real_,
                   s5p57c))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(i_wage4 = (s5p57b * s5p57c) / 12)

#1.1.1.5. Total incomes from wages and salaries
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(
    i_wagesum = 
      if_else(
        is.na(i_wage) & is.na(i_wage2) & is.na(i_wage3) & is.na(i_wage4),
        NA_real_,
        rowSums(across(c(i_wage, i_wage2, i_wage3, i_wage4)), na.rm = TRUE)
      )
  )

#1.1.2. Independent employment incomes----
#1.1.2.1 First independent employment----
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p26a = 
           if_else(s5p26a %in% c(9999998, 9999999), 
                   NA_real_, 
                   s5p26a),
         s5p26b = 
           if_else(s5p26b > 97, 
                   NA_real_, 
                   s5p26b))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(i_se = 
           case_when(
             s5p26b == 1 ~ s5p26a * 30.4166666667,
             s5p26b == 2 ~ s5p26a  * 4.2857142857,
             s5p26b == 3 ~ s5p26a * 2.1428571429,
             s5p26b == 4 ~ s5p26a  * 2,
             s5p26b == 5 ~ s5p26a,
             s5p26b == 6 ~ s5p26a  / 3,
             s5p26b == 7 ~ s5p26a  / 6,
             s5p26b == 8 ~ s5p26a  / 12,
             
           )
  )

#1.1.2.2. Second independent employment----
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p42b =
           if_else(s5p42b > 97,
                   NA_real_,
                   s5p42b))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(i_se2 = 
           case_when(
             s5p42b == 1 ~ s5p42a * 30.4166666667,
             s5p42b == 2 ~ s5p42a  * 4.2857142857,
             s5p42b == 3 ~ s5p42a * 2.1428571429,
             s5p42b == 4 ~ s5p42a  * 2,
             s5p42b == 5 ~ s5p42a,
             s5p42b == 6 ~ s5p42a  / 3,
             s5p42b == 7 ~ s5p42a  / 6,
             s5p42b == 8 ~ s5p42a  / 12,
             
           )
  )

#1.1.2.3. Independent employment last 12 months----
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(s5p54a = 
           if_else(s5p54a %in% c(9999999), 
                   NA_real_, 
                   s5p54a),
         s5p54b =
           if_else(s5p54b %in% c(99),
                   NA_real_,
                   s5p54b))

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(i_se3 = 
           case_when(
             s5p54b == 1 ~ s5p54a * 30.4166666667,
             s5p54b == 2 ~ s5p54a  * 4.2857142857,
             s5p54b == 3 ~ s5p54a * 2.1428571429,
             s5p54b == 4 ~ s5p54a  * 2,
             s5p54b == 5 ~ s5p54a,
             s5p54b == 6 ~ s5p54a  / 3,
             s5p54b == 7 ~ s5p54a  / 6,
             s5p54b == 8 ~ s5p54a  / 12,
             
           )
  )

#1.1.2.4. Total independent employment----
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(
    i_sesum = 
      if_else(
        is.na(i_se) & is.na(i_se2) & is.na(i_se3),
        NA_real_,
        rowSums(across(c(i_se, i_se2, i_se3)), na.rm = TRUE)
      )
  )

#1.1.3. Total employment incomes----
emnv14_hh_income_person <- emnv14_hh_income_person %>%
  mutate(
    i_employment = 
      if_else(
        is.na(i_sesum) & is.na(i_wagesum),
        NA_real_,
        rowSums(across(c(i_sesum, i_wagesum)), na.rm = TRUE)
      )
  )

emnv14_hh_income_person <- emnv14_hh_income_person %>%
  group_by(i00) %>% mutate(hh_employment = sum(i_employment, na.rm = TRUE))

emnv14_hh_income_person <- emnv14_hh_income_person %>% 
  mutate(hh_employment = if_else(hh_employment == 0, 
                                     NA_real_, 
                                     hh_employment))

#Selecting households
emnv14_hh_income_person_hh <- emnv14_hh_income_person %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>% 
  select(i00, dominio4, i06, peso2, peso3, hh_employment)

#1.1.4. Imputed rent----
emnv14_hh_imputed_rent <- 
  read_dta(here("emnv14_02_datos_de_la_vivienda_y_el_hogar.dta"))

emnv14_hh_imputed_rent <- emnv14_hh_imputed_rent %>%
  mutate(s1p14a = 
           if_else(s1p14a > 99997, 
                   NA_real_, 
                   s1p14a),
         s1p14b =
           if_else(s1p14b %in% c(99998),
                   NA_real_,
                   s1p14b))

emnv14_hh_imputed_rent <- emnv14_hh_imputed_rent %>% 
  mutate(s1p14b = s1p14b * 26.3612) 
#Amount in USD

emnv14_hh_imputed_rent <- emnv14_hh_imputed_rent %>% 
  mutate(hh_rent = 
           if_else(is.na(s1p14a) & is.na(s1p14b), 
                   NA_real_,
                   rowSums(across(c(s1p14a, s1p14b)), na.rm = TRUE)))

emnv14_hh_imputed_rent <- emnv14_hh_imputed_rent %>%
  select(i00, hh_rent)

#Joining the databases
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_imputed_rent,
            by = c ("i00"), 
            relationship = "one-to-one")

#1.1.5. Total incomes from production----
emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>% 
  mutate(hh_prod = hh_employment)

#1.2. Asset ownership incomes----
#1.2.1. Leasing----
emnv14_hh_leasing <- 
  read_dta(here("emnv14_15_parte_c3_de_la_seccion_7.dta"))

#Keeping only incomes from leasing
emnv14_hh_leasing <- emnv14_hh_leasing %>% 
  filter(s7c3cod %in% c(1,2))

emnv14_hh_leasing <- emnv14_hh_leasing %>%
  mutate(s7p35b = s7p35b * 26.3612)
#Amount in USD

emnv14_hh_leasing <- emnv14_hh_leasing %>% 
  mutate(h_total = 
           if_else(is.na(s7p35a) & is.na(s7p35b), 
                   NA_real_,
                   rowSums(across(c(s7p35a, s7p35b)), na.rm = TRUE)))

emnv14_hh_leasing <- emnv14_hh_leasing %>%
  group_by(i00) %>%
  mutate(hh_leasing = sum(h_total, na.rm = TRUE),
         hh_leasing = if_else(hh_leasing == 0, NA_real_, hh_leasing))

#Selecitng households
emnv14_hh_leasing <- emnv14_hh_leasing %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_leasing <- emnv14_hh_leasing %>%
  select(i00, hh_leasing)

#Joining the databases
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_leasing,
            by = c ("i00"), 
            relationship = "one-to-one")

#1.2.2. Financial assets----
emnv14_hh_financial <- 
  read_dta(here("emnv14_16_parte_c4_de_la_seccion_7.dta"))

#Keeping gains from financial resources
emnv14_hh_financial <- emnv14_hh_financial %>% 
  filter(s7c4cod %in% c(1,2,3, 5, 6))

#Amount in USD
emnv14_hh_financial <- emnv14_hh_financial %>%
  mutate(s7p37b = s7p37b * 26.3612)

emnv14_hh_financial <- emnv14_hh_financial %>% 
  mutate(h_total = 
           if_else(is.na(s7p37a) & is.na(s7p37b), 
                   NA_real_,
                   rowSums(across(c(s7p37a, s7p37b)), na.rm = TRUE)))

emnv14_hh_financial <- emnv14_hh_financial %>%
  mutate(h_total = h_total / 12)

emnv14_hh_financial <- emnv14_hh_financial %>%
  group_by(i00) %>%
  mutate(hh_financial = sum(h_total, na.rm = TRUE),
         hh_financial = if_else(hh_financial == 0, NA_real_, hh_financial))

#Selecting households
emnv14_hh_financial <- emnv14_hh_financial %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_financial <- emnv14_hh_financial %>%
  select(i00, hh_financial)

#Joining the databases
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_financial,
            by = c ("i00"), 
            relationship = "one-to-one")

#1.2.3. Total assets----
emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_asset = 
           if_else(is.na(hh_financial) & is.na(hh_leasing),
                   NA_real_,
                   rowSums(across(c(hh_financial, hh_leasing)), na.rm = TRUE)))

#1.3. Total primary incomes----
emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_primary = 
           if_else(is.na(hh_asset) & is.na(hh_prod),
                   NA_real_,
                   rowSums(across(c(hh_asset, hh_prod)), na.rm = TRUE)))

#2. Transfers----
#2.1. Retirement and pensions----
emnv14_hh_pensions <- 
  read_dta(here("emnv14_15_parte_c3_de_la_seccion_7.dta"))

emnv14_hh_pensions <- emnv14_hh_pensions %>%
  filter(s7c3cod > 3)

emnv14_hh_pensions <- emnv14_hh_pensions %>%
  mutate(s7p35b = s7p35b * 26.3612)
#Amount in USD

emnv14_hh_pensions <- emnv14_hh_pensions %>%
  mutate(pension = 
           if_else(is.na(s7p35a) & is.na(s7p35b),
                   NA_real_,
                   rowSums(across(c(s7p35a, s7p35b)), na.rm = TRUE)))

emnv14_hh_pensions <- emnv14_hh_pensions %>%
  group_by(i00) %>%
  mutate(hh_pension = sum(pension, na.rm = TRUE))

emnv14_hh_pensions <- emnv14_hh_pensions %>%
  mutate(hh_pension = 
  if_else(hh_pension == 0, NA_real_, hh_pension))

emnv14_hh_pensions <- emnv14_hh_pensions %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_pensions <- emnv14_hh_pensions %>%
  select(i00, hh_pension)

#Joining the databases
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_pensions,
            by = c ("i00"), 
            relationship = "one-to-one")

#2.2. Other transfers----
#2.2.1. School meals----
emnv14_hh_school_meals <- 
  read_dta(here("emnv14_13_parte_c1_de_la_seccion_7.dta"))

emnv14_hh_school_meals <- emnv14_hh_school_meals %>%
  mutate(s7p29 = 
           if_else(s7p29 > 33, NA_real_, s7p29))

emnv14_hh_school_meals <- emnv14_hh_school_meals %>%
  mutate(i_meals = 
           if_else(s7p27 == 1, s7p30 * s7p29 * s7p28 * 2, NA_real_))

emnv14_hh_school_meals <- emnv14_hh_school_meals %>%
  group_by(i00) %>%
  mutate(hh_meals = sum(i_meals, na.rm = TRUE))

emnv14_hh_school_meals <- emnv14_hh_school_meals %>%
  mutate(hh_meals = 
           if_else(hh_meals == 0, NA_real_, hh_meals))

emnv14_hh_school_meals <- emnv14_hh_school_meals %>%
  mutate(hh_meals = 
           if_else(i_meals == 0, 0, hh_meals, missing = hh_meals))

emnv14_hh_school_meals <- emnv14_hh_school_meals %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_school_meals <- emnv14_hh_school_meals %>%
  select(i00, hh_meals)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_school_meals,
            by = c ("i00"), 
            relationship = "one-to-one")

#2.2.2 School supplies----
emnv14_hh_school_supplies <- 
  read_dta(here("emnv14_14_parte_c2_de_la_seccion_7.dta"))

emnv14_hh_school_supplies <- emnv14_hh_school_supplies %>%
  filter(!s7c2cod %in% c(6))

emnv14_hh_school_supplies <- emnv14_hh_school_supplies %>%
  mutate(i_supplies = (s7p32 * s7p33) / 12)

emnv14_hh_school_supplies <- emnv14_hh_school_supplies %>%
  group_by(i00) %>%
  mutate(hh_supplies = sum(i_supplies, na.rm = TRUE))

emnv14_hh_school_supplies <- emnv14_hh_school_supplies %>%
  mutate(hh_supplies =
           if_else(hh_supplies == 0, NA_real_, hh_supplies))

emnv14_hh_school_supplies <- emnv14_hh_school_supplies %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_school_supplies <- emnv14_hh_school_supplies %>%
  select(i00, hh_supplies)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_school_supplies,
            by = c ("i00"), 
            relationship = "one-to-one")

#2.2.3. Scholarships----
emnv14_hh_scholarships <- 
  read_dta(here("emnv14_15_parte_c3_de_la_seccion_7.dta"))

emnv14_hh_scholarships <- emnv14_hh_scholarships %>%
  filter(s7c3cod %in% c(3))

emnv14_hh_scholarships <- emnv14_hh_scholarships %>%
  mutate(s7p35b = s7p35b * 26.3612)

emnv14_hh_scholarships <- emnv14_hh_scholarships %>%
  mutate(hh_scholarship = 
           if_else(is.na(s7p35a) & is.na(s7p35b),
                   NA_real_,
                   rowSums(across(c(s7p35a, s7p35b)), na.rm = TRUE)))

emnv14_hh_scholarships <- emnv14_hh_scholarships %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_scholarships <- emnv14_hh_scholarships %>%
  select(i00, hh_scholarship)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_scholarships,
            by = c ("i00"), 
            relationship = "one-to-one")

#2.2.4. Other transfers----
emnv14_hh_other_transfers <- 
  read_dta(here("emnv14_16_parte_c4_de_la_seccion_7.dta"))

emnv14_hh_other_transfers <- emnv14_hh_other_transfers %>%
  filter(s7c4cod %in% c(7, 8, 4))

emnv14_hh_other_transfers <- emnv14_hh_other_transfers %>%
  mutate(s7p37b = s7p37b * 26.3612)
#Amount in USD

emnv14_hh_other_transfers <- emnv14_hh_other_transfers %>%
  mutate(aux_other = 
           if_else(is.na(s7p37a) & is.na(s7p37b),
                   NA_real_,
                   rowSums(across(c(s7p37a, s7p37b)), na.rm = TRUE)))

emnv14_hh_other_transfers <- emnv14_hh_other_transfers %>%
  mutate(i_other = aux_other / 12)

emnv14_hh_other_transfers <- emnv14_hh_other_transfers %>%
  group_by(i00) %>% 
  mutate(hh_other = sum(i_other, na.rm = TRUE))

emnv14_hh_other_transfers <- emnv14_hh_other_transfers %>%
  mutate(hh_other = 
           if_else(hh_other == 0, NA_real_, hh_other))

emnv14_hh_other_transfers <- emnv14_hh_other_transfers %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_other_transfers <- emnv14_hh_other_transfers %>%
  select(i00, hh_other)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_other_transfers,
            by = c ("i00"), 
            relationship = "one-to-one")

#2.2.5. Total other transfers----
emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_otransfers = 
           if_else(is.na(hh_meals) & is.na(hh_supplies) & is.na(hh_scholarship)
                   & is.na(hh_other),
                   NA_real_,
                   rowSums(across(c(hh_meals, hh_scholarship, hh_supplies,
                                    hh_other)), na.rm = TRUE)))

#2.3. Total transfers----
emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_transfers = 
           if_else(is.na(hh_pension) & is.na(hh_otransfers),
                   NA_real_,
                   rowSums(across(c(hh_pension, hh_otransfers)), na.rm = TRUE)))

#3. Other incomes----
#3.1 Remittances----
emnv14_hh_remittances <- 
  read_dta(here("emnv14_17_parte_c5_de_la_seccion_7.dta"))

emnv14_hh_remittances <- emnv14_hh_remittances %>%
  filter(s7c5cod < 3)

emnv14_hh_remittances <- emnv14_hh_remittances %>% 
  mutate(s7p40b = s7p40b * 26.3612)
#Amount in USD

emnv14_hh_remittances <- emnv14_hh_remittances %>%
  mutate(aux_remittances = 
           if_else(is.na(s7p40a) & is.na(s7p40b),
                   NA_real_,
                   rowSums(across(c(s7p40a, s7p40b)), na.rm = TRUE)))

emnv14_hh_remittances <- emnv14_hh_remittances %>%
  mutate(freq = 
           case_when(
             s7p39 == 1  ~ 4.2857142857,
             s7p39 == 2  ~ 2,
             s7p39 == 3  ~ 1,
             s7p39 == 4  ~ 0.3333333333,
             s7p39 == 5  ~ 0.1666666667,
             s7p39 == 6  ~ 0.0833333333
           ))

emnv14_hh_remittances <- emnv14_hh_remittances %>%
  mutate(i_remittances = aux_remittances * freq)

emnv14_hh_remittances <- emnv14_hh_remittances %>%
  group_by(i00) %>%
  mutate(hh_remittances = sum(i_remittances, na.rm = TRUE))

emnv14_hh_remittances <- emnv14_hh_remittances %>%
  mutate(hh_remittances =
           if_else(hh_remittances == 0, NA_real_, hh_remittances))

emnv14_hh_remittances <- emnv14_hh_remittances %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_remittances <- emnv14_hh_remittances %>%
  select(i00, hh_remittances)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_remittances,
            by = c ("i00"), 
            relationship = "one-to-one")

#3.2. Inheritance----
emnv14_hh_inheritance <- 
  read_dta(here("emnv14_16_parte_c4_de_la_seccion_7.dta"))

emnv14_hh_inheritance <- emnv14_hh_inheritance %>%
  filter(s7c4cod %in% c(9))

emnv14_hh_inheritance <- emnv14_hh_inheritance %>%
  mutate(s7p37b = s7p37b * 26.3612)
#Amount in USD

emnv14_hh_inheritance <- emnv14_hh_inheritance %>%
  mutate(i_inheritance = 
           if_else(is.na(s7p37a) & is.na(s7p37b),
                   NA_real_,
                   rowSums(across(c(s7p37a, s7p37b)), na.rm = TRUE)))

emnv14_hh_inheritance <- emnv14_hh_inheritance %>%
  mutate(hh_inheritance = i_inheritance/12)

emnv14_hh_inheritance <- emnv14_hh_inheritance %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_inheritance <- emnv14_hh_inheritance %>%
  select(i00, hh_inheritance)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_inheritance,
            by = c ("i00"), 
            relationship = "one-to-one")

#3.3. Other incomes----
emnv14_hh_other_transfers_2 <- 
  read_dta(here("emnv14_16_parte_c4_de_la_seccion_7.dta"))

emnv14_hh_other_transfers_2 <- emnv14_hh_other_transfers_2 %>%
  filter(s7c4cod %in% c(10))

emnv14_hh_other_transfers_2 <- emnv14_hh_other_transfers_2 %>%
  mutate(s7p37b = s7p37b * 26.3612)
#Amount in USD

emnv14_hh_other_transfers_2 <- emnv14_hh_other_transfers_2 %>%
  mutate(aux_other = 
           if_else(is.na(s7p37a) & is.na(s7p37b),
                   NA_real_,
                   rowSums(across(c(s7p37a, s7p37b)), na.rm = TRUE)))

emnv14_hh_other_transfers_2 <- emnv14_hh_other_transfers_2 %>%
  mutate(i_other = aux_other / 12)

emnv14_hh_other_transfers_2 <- emnv14_hh_other_transfers_2 %>%
  group_by(i00) %>%
  mutate(hh_other2 = sum(i_other, na.rm = TRUE))

emnv14_hh_other_transfers_2 <- emnv14_hh_other_transfers_2 %>%
  mutate(hh_other2 = 
           if_else(hh_other2 == 0, NA_real_, hh_other2))

emnv14_hh_other_transfers_2 <- emnv14_hh_other_transfers_2 %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_other_transfers_2 <- emnv14_hh_other_transfers_2 %>%
  select(i00, hh_other2)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_other_transfers_2,
            by = c ("i00"), 
            relationship = "one-to-one")

#3.4. Total other incomes----
emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_otherinc = 
           if_else(is.na(hh_remittances) & is.na(hh_other2) & is.na(hh_inheritance),
                   NA_real_,
                   rowSums(across(c(hh_remittances, hh_other2, hh_inheritance)), na.rm = TRUE)))

#4. Total household incomes----
emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_incomes = 
           if_else(is.na(hh_otherinc) & is.na(hh_transfers) & is.na(hh_primary),
                   NA_real_,
                   rowSums(across(c(hh_otherinc, hh_transfers, hh_primary)), na.rm = TRUE)))

#5. Paid transfers----
emnv14_hh_paid_transfers <- 
  read_dta(here("emnv14_10_parte_b2_de_la_seccion_7.dta"))

emnv14_hh_paid_transfers <- emnv14_hh_paid_transfers %>%
  filter(s7b2cod %in% c(21,24))

emnv14_hh_paid_transfers <- emnv14_hh_paid_transfers %>%
  group_by(i00) %>%
  mutate(hh_ptransfers1 = sum(s7p20, na.rm = TRUE))

emnv14_hh_paid_transfers <- emnv14_hh_paid_transfers %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_paid_transfers <- emnv14_hh_paid_transfers %>%
  select(i00, hh_ptransfers1)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_paid_transfers,
            by = c ("i00"), 
            relationship = "one-to-one")

emnv14_hh_other_paid_transfers <- 
  read_dta(here("emnv14_12_parte_b4_de_la_seccion_7.dta"))

emnv14_hh_other_paid_transfers <- emnv14_hh_other_paid_transfers %>%
  filter(s7b4cod %in% c(13))

emnv14_hh_other_paid_transfers <- emnv14_hh_other_paid_transfers %>%
  mutate(s7p25 =
           if_else(s7p25 == 9999998 | s7p25 == 9999999, 
                   NA_real_,
                   s7p25))

emnv14_hh_other_paid_transfers <- emnv14_hh_other_paid_transfers %>%
  mutate(hh_ptransfers2 = s7p25 / 12)

emnv14_hh_other_paid_transfers <- emnv14_hh_other_paid_transfers %>%
  select(i00, hh_ptransfers2)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_other_paid_transfers,
            by = c ("i00"), 
            relationship = "one-to-one")

emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(paid_transfers = 
           rowSums(across(c(hh_ptransfers1, hh_ptransfers2)), na.rm = TRUE))

#6. Household available incomes----
emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_avincomes = hh_incomes - paid_transfers)

emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_avincomes = 
           if_else(hh_avincomes < 0, 0, hh_avincomes))

#7. Adjusting imputed rent----
emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(aux_rent = (hh_rent / hh_avincomes), na.rm = TRUE)

emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(aux_rent =
           if_else(hh_avincomes == 0, NA_real_, aux_rent))

emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_rent2 = hh_rent)

emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_rent =
           if_else(aux_rent > 1, hh_avincomes, hh_rent))

emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_rent =
           if_else((is.na(aux_rent) & !is.na(hh_avincomes)), 
                   hh_avincomes, hh_rent))

emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_m_income = rowSums(across(c(hh_rent, hh_avincomes)), na.rm = TRUE))

#8. Income per capita----
emnv14_hh_members <-
  read_dta(here("emnv14_04_poblacion.dta"))

emnv14_hh_members <- emnv14_hh_members %>%
  group_by(i00) %>%
  mutate(hh_members = n()) %>%
  ungroup()

emnv14_hh_members <- emnv14_hh_members %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_members <- emnv14_hh_members %>%
  select(i00, hh_members)

#Joining the database
emnv14_hh_income_person_hh <- 
  full_join(emnv14_hh_income_person_hh, emnv14_hh_members,
            by = c ("i00"), 
            relationship = "one-to-one")

emnv14_hh_income_person_hh <- emnv14_hh_income_person_hh %>%
  mutate(hh_m_pcincome = hh_m_income / hh_members)

#9. Poverty lines----
#Distinction of poverty lines between urban and rural areas
emnv14_hh_income <- emnv14_hh_income_person_hh %>%
  mutate(poverty_gen = 
           if_else(i06 == 1, hh_m_pcincome < 2371.03, NA_real_))

emnv14_hh_income <- emnv14_hh_income %>%
  mutate(poverty_gen = 
           if_else(i06 == 2, hh_m_pcincome < 1733.79, poverty_gen))

#MPI estimation----
#1. Housing dimension----
emnv14_mpi <-
  read_dta(here("emnv14_02_datos_de_la_vivienda_y_el_hogar.dta"))

#1.1. Housing materials indicators----
#1.1.1. Floor
table(emnv14_mpi$s1p5)

emnv14_mpi <- emnv14_mpi %>%
  mutate(d_floor = 
           case_when(
             s1p5 < 5  ~ 0,
             s1p5 == 5  ~ 1,
           ))

#1.1.2. Roof
table(emnv14_mpi$s1p6)

emnv14_mpi <- emnv14_mpi %>%
  mutate(d_roof = 
           case_when(
             s1p6 < 5  ~ 0,
             s1p6 %in% c(5, 6)  ~ 1,
           ))

#1.1.3. Wall
table(emnv14_mpi$s1p4)

emnv14_mpi <- emnv14_mpi %>%
  mutate(d_wall = 
           case_when(
             s1p4 < 13  ~ 0,
             s1p4 %in% c(13, 14, 15) ~ 1,
           ))

#1.1.4. Overall housing materials indicator----
emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_materials =
           if_else(
             is.na(d_wall) & is.na(d_floor) & is.na(d_roof),
             NA_real_,
             if_else(d_wall == 1 | d_floor == 1 | d_roof == 1,
                     1,
                     0)
           ))
View(emnv14_mpi[emnv14_mpi$hh_d_materials == NA_real_, ])

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_materials =
           if_else(
             is.na(d_floor) & (d_roof == 0) & (d_wall == 0),
             0,
             hh_d_materials))

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_materials =
           if_else(
             is.na(d_roof) & (d_floor == 0) & (d_wall == 0),
             0,
             hh_d_materials))

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_materials =
           if_else(
             is.na(d_wall) & (d_floor == 0) & (d_roof == 0),
             0,
             hh_d_materials))

#1.2. People per room indicator----
emnv14_mpi <- 
  full_join(emnv14_mpi, emnv14_hh_members,
            by = c ("i00"), 
            relationship = "one-to-one")

emnv14_mpi <- emnv14_mpi %>%
  mutate(pproom = hh_members / s1p8)

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_overcrowding =
           if_else(is.na(s1p8),
           NA_real_,
          (pproom >= 3)))


#1.3. Housing tenure indicator----
emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_tenure = 
           case_when(
             s1p11 %in% c(1, 2, 3, 4, 6) ~ 0,
             s1p11 %in% c(5, 7) ~ 1,
           ))

#2. Basic services dimension----
#2.1. Improved water indicator----
emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_water = 
           case_when(
             s1p15 == 1 ~ 0,
             s1p15 %in% c(2:9) ~ 1,
           ))

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_water =
           if_else(((s1p15 == 2) | (s1p15 == 3) | (s1p15 == 4)) & (i06 == 2),
                   0,
                   hh_d_water))

#2.2. Improved sanitation indicator----
emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_sanitation = 
           case_when(
             s1p18 %in% c(3, 4) ~ 0,
             s1p18 %in% c(1, 2, 5, 6)  ~ 1,
           ))

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_sanitation = 
           if_else((s1p18 == 2) & (i06 == 2),
                   0,
                   hh_d_sanitation))

#2.3. Energy indicator----
emnv14_mpi <- emnv14_mpi %>%
  mutate(d_lighting = 
           case_when(
             s1p21 %in% c(1, 2, 3) ~ 0,
             s1p21 %in% c(4, 5, 6, 7, 9)  ~ 1,
           ))

#2.3.2. Cooking fuel----
emnv14_mpi <- emnv14_mpi %>%
  mutate(d_fuel = 
           case_when(
             s1p25 %in% c(2, 4, 5) ~ 0,
             s1p25 %in% c(1, 3)  ~ 1,
           ))

#2.3.3. Overall energy indicator----
emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_energy =
           if_else(
             is.na(d_lighting) & is.na(d_fuel),
             NA_real_,
             (d_lighting == 1 | d_fuel == 1)))

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_energy = 
           if_else(d_lighting == 0 & is.na(d_fuel),
                   0,
                   hh_d_energy))

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_energy = 
           if_else(d_fuel == 0 & is.na(d_lighting),
                   0,
                   hh_d_energy))
    
emnv14_mpi <- emnv14_mpi %>%
  select(i00, i06, dominio4, peso2, peso3, hh_d_materials, hh_d_overcrowding,
         hh_d_tenure, hh_d_water, hh_d_sanitation, hh_d_energy)

#3. Living standard dimension----
emnv14_mpi <- 
  full_join(emnv14_mpi, emnv14_hh_income,
            by = c ("i00"), 
            relationship = "one-to-one")

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_income = poverty_gen)

#3.2. Durable goods indicator----
emnv14_hh_assets <- 
  read_dta(here("emnv14_18_parte_d_de_la_seccion_7.dta"))

emnv14_hh_assets <- emnv14_hh_assets %>%
  mutate(tv_bw = 
           if_else((s7dcod == 2) & (s7p47 == 1),
                   1,
                   0))

emnv14_hh_assets <- emnv14_hh_assets %>%
  group_by(i00) %>%
  mutate(tv = max(tv_bw))

emnv14_hh_assets <- emnv14_hh_assets %>%
  mutate(s7p47 = 
           if_else((s7dcod == 3) & (tv == 1),
                   1,
                   s7p47))

emnv14_hh_assets <- emnv14_hh_assets %>%
  filter(s7dcod %in% c(3, 4, 16, 22))
#Keeping relevant assets

emnv14_hh_assets <- emnv14_hh_assets %>%
  mutate(d_assets =
           if_else(s7p47 == 2, 1, 0))

emnv14_hh_assets <- emnv14_hh_assets %>%
  mutate(d_assets =
           if_else((i06 == 1) & (s7dcod == 3), 
                    NA_real_, 
                    d_assets))

emnv14_hh_assets <- emnv14_hh_assets %>%
  mutate(d_assets =
           if_else((i06 == 2) & (s7dcod == 16), 
                   NA_real_, 
                   d_assets))

emnv14_hh_assets <- emnv14_hh_assets %>%
  group_by(i00) %>%
  mutate(hh_d_assets = min(d_assets, na.rm = TRUE))

emnv14_hh_assets <- emnv14_hh_assets %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_assets <- emnv14_hh_assets %>%
  select(i00, hh_d_assets)

emnv14_mpi <- 
  full_join(emnv14_mpi, emnv14_hh_assets,
            by = c ("i00"), 
            relationship = "one-to-one")

#4. Education dimension----
emnv14_hh_attendance_gap <- 
  read_dta(here("emnv14_04_poblacion.dta"))

#4.1. Children's school attendance indicator
#Restricting the database to children

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  filter((s2p2a >= 6) & (s2p2a <= 17))

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(d_attendance = 
           if_else(s4p15 == 1, 0, 1), na.rm = TRUE)

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(d_attendance = 
           if_else(is.na(s4p15), 1, d_attendance), na.rm = TRUE)

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  group_by(i00) %>%
  mutate(hh_d_attendance = max(d_attendance, na.rm = TRUE))

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(hh_d_attendance =
           if_else(is.na(d_attendance),
                   0,
                   hh_d_attendance))

#4.2. Schooling gap indicator----
#4.2.1. Schooling years----
emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(school_level = 
           case_when(
             s4p12a %in% c(2, 3, 12) ~ 0,
             s4p12a %in% c(4)  ~ 6,
             s4p12a %in% c(5, 7)  ~ 9,
             s4p12a %in% c(8, 9)  ~ 11,
             s4p12a %in% c(10)  ~ 16,
             s4p12a %in% c(11)  ~ 18,
           ))

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(school_grade = s4p12b)

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(y_schooling =
           rowSums(across(c(school_level, school_grade)), na.rm = TRUE))

#4.2.2. Schooling gap----
emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(ideal_grade = 
           case_when(
             s2p2a %in% c(6) ~ 1,
             s2p2a %in% c(7) ~ 2,
             s2p2a %in% c(8)  ~ 3,
             s2p2a %in% c(9)  ~ 4,
             s2p2a %in% c(10)  ~ 5,
             s2p2a %in% c(11)  ~ 6,
             s2p2a %in% c(12)  ~ 7,
             s2p2a %in% c(13)  ~ 8,
             s2p2a %in% c(14)  ~ 9,
             s2p2a %in% c(15)  ~ 10,
             s2p2a %in% c(16)  ~ 11,
             s2p2a %in% c(17)  ~ 12,
           ))

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(gap_grade = ideal_grade - y_schooling)

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(d_schoolgap = 
           if_else(gap_grade > 2, 1, 0))

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  group_by(i00) %>% 
  mutate(hh_d_schoolgap = max(d_schoolgap), na.rm = TRUE)

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  mutate(hh_d_schoolgap = 
           if_else(is.na(hh_d_schoolgap), 0, hh_d_schoolgap))

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_attendance_gap <- emnv14_hh_attendance_gap %>%
  select(i00, hh_d_attendance, hh_d_schoolgap)

emnv14_mpi <- 
  full_join(emnv14_mpi, emnv14_hh_attendance_gap,
            by = c ("i00"), 
            relationship = "one-to-one")

#4.3. Adult schooling achievement indicator----
#4.3.1. Schooling years
#Restricting to adults
emnv14_hh_adult_schooling <- 
  read_dta(here("emnv14_04_poblacion.dta"))

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  filter(s2p2a > 19)

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  mutate(school_level = 
           case_when(
             s4p12a %in% c(2, 3, 12) ~ 0,
             s4p12a %in% c(4)  ~ 6,
             s4p12a %in% c(5, 6, 7)  ~ 9,
             s4p12a %in% c(8, 9)  ~ 11,
             s4p12a %in% c(10)  ~ 16,
             s4p12a %in% c(11)  ~ 18,
           ))

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  mutate(school_grade = 
           if_else(s4p12b == 8 | s4p12b == 9, 
                   NA_real_,
                   s4p12b))

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  mutate(y_schooling =
           rowSums(across(c(school_level, school_grade)), na.rm = TRUE))

#4.3.2. Schooling achievement----
emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  mutate(d_adultschooling = 
           if_else((s2p2a <= 59) & (i06 == 1),
                   y_schooling < 9,
                   0))
#The author makes a distinction between urban and rural

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  mutate(d_adultschooling = 
           if_else((y_schooling < 6) & (s2p2a >= 60),
                   1,
                   d_adultschooling))

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  mutate(d_adultschooling = 
           if_else((y_schooling < 6) & (i06 == 2),
                   1,
                   d_adultschooling))

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  mutate(d_adultschooling = 
           if_else(is.na(d_adultschooling),
                   0,
                   d_adultschooling))

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  group_by(i00) %>% 
  mutate(hh_d_adultschooling = max(d_adultschooling, na.rm = TRUE))

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_adult_schooling <- emnv14_hh_adult_schooling %>%
  select(i00, hh_d_adultschooling)

emnv14_mpi <- 
  full_join(emnv14_mpi, emnv14_hh_adult_schooling,
            by = c ("i00"), 
            relationship = "one-to-one")

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_adultschooling =
           if_else(is.na(hh_d_adultschooling), 0, hh_d_adultschooling))

#5. Employment and social employment----
emnv14_hh_employment <- 
  read_dta(here("emnv14_04_poblacion.dta"))

emnv14_hh_employment <- emnv14_hh_employment %>%
  filter((s2p2a >= 15) & (s2p2a <= 65))

emnv14_hh_employment <- emnv14_hh_employment %>%
  mutate(d_emp =
           if_else(s5p1 == 2, 1, 0))

emnv14_hh_employment <- emnv14_hh_employment %>%
  mutate(d_emp =
           if_else(s5p2%in%c(1, 2, 3, 4, 5), 0, d_emp))

emnv14_hh_employment <- emnv14_hh_employment %>%
  mutate(d_emp =
           if_else(s5p4%in%c(1, 2, 3, 4), 0, d_emp))

emnv14_hh_employment <- emnv14_hh_employment %>%
  mutate(d_emp =
           if_else(s5p10%in%c(1, 2, 3, 5, 6), 0, d_emp))

emnv14_hh_employment <- emnv14_hh_employment %>%
  mutate(d_emp =
             if_else((s5p1 == 1) & (s5p18 > 5), 1, d_emp))

emnv14_hh_employment <- emnv14_hh_employment %>%
  mutate(d_emp = 
           if_else(is.na(d_emp), 0, d_emp))

emnv14_hh_employment <- emnv14_hh_employment %>%
  group_by(i00) %>%
  mutate(hh_d_emp = max(d_emp, na.rm = TRUE))

emnv14_hh_employment <- emnv14_hh_employment %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_employment <- emnv14_hh_employment %>%
  select(i00, hh_d_emp)

emnv14_mpi <- 
  full_join(emnv14_mpi, emnv14_hh_employment,
            by = c ("i00"), 
            relationship = "one-to-one")

emnv14_mpi <- emnv14_mpi %>%
  mutate(hh_d_emp =
           if_else(is.na(hh_d_emp), 0, hh_d_emp))

#5.2. Social protection indicator----
emnv14_hh_social_protection <- 
  read_dta(here("emnv14_15_parte_c3_de_la_seccion_7.dta"))

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  filter(s7c3cod == 5)

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  mutate(h_pension = 
           if_else(s7p34 == 2, 1, 0))

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  select(i00, h_pension)

emnv14_poblacion <- 
  read_dta(here("emnv14_04_poblacion.dta"))

emnv14_hh_social_protection <- 
  full_join(emnv14_hh_social_protection, emnv14_poblacion,
            by = c ("i00"), 
            relationship = "one-to-many")

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  mutate(s5p28a =
           if_else(s5p28a == 9, NA_real_, s5p28a))
#Defining relevant groups

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  mutate(eld =
           if_else(!is.na(s2p2a),
                   (s2p2a > 65),
                   0))

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  group_by(i00) %>%
  mutate(h_elderly = max(eld, na.rm = TRUE))

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  mutate(h_pension =
           if_else(h_elderly == 0,
                   0,
                   h_pension))

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  mutate(d_sp =
           if_else(is.na(s3p11),
                   NA_real_,
                   (s3p11 == 6)))

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  mutate(d_sp =
           if_else(s5p28a == 1 & !is.na(s5p28a),
                   0,
                   d_sp))

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  mutate(d_sp =
           if_else(h_pension == 0,
                   0,
                   d_sp))

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  group_by(i00) %>%
  mutate(hh_d_sp = min(h_pension, na.rm = TRUE))

emnv14_hh_social_protection <- emnv14_hh_social_protection %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hh_social_protection <- emnv14_hh_social_protection %>%
  select(i00, hh_d_sp)

emnv14_mpi <- 
  full_join(emnv14_mpi, emnv14_hh_social_protection,
            by = c ("i00"), 
            relationship = "one-to-one")

#6. Calculating MPI----
#6.1. Defining weights----
#6.1.1. Housing 22.2%----
emnv14_mpi <- emnv14_mpi %>%
  mutate(w_materials    = (100 / 4.5 / 3),
         w_overcrowding = (100 / 4.5 / 3),
         w_tenure       = (100 / 4.5 / 3))

#6.1.2. Basic services 22.2%----
emnv14_mpi <- emnv14_mpi %>%
  mutate(w_water      = (100 / 4.5 / 3),
         w_sanitation = (100 / 4.5 / 3),
         w_energy     = (100 / 4.5 / 3))

#6.1.3. Living standard 22.2%----
emnv14_mpi <- emnv14_mpi %>%
  mutate(w_income = (100 / 4.5 / 1.5),
         w_assets = (100 / 4.5 / 3))

#6.1.4. Education 22.2%----
emnv14_mpi <- emnv14_mpi %>%
  mutate(w_attendance  = (100 / 4.5 / 3),
         w_schoolgap   = (100 / 4.5 / 3),
         w_adultschooling = (100 / 4.5 / 3))

#6.1.5. Employment and social protection 11.1%----
emnv14_mpi <- emnv14_mpi %>%
  mutate(w_emp = (100 / 4.5 / 3),
         w_sp  = (100 / 4.5 / 6))

#6.2. Weighted deprivation matrix----
indicators <- c("materials", "overcrowding", "tenure", "water", "sanitation",
                "energy", "income", "assets", "attendance", "schoolgap", 
                "adultschooling", "emp", "sp")

for (indicator in indicators) {
  emnv14_mpi <- emnv14_mpi %>%
    mutate(!!paste0("w_hh_d_", indicator) :=
             .data[[paste0("hh_d_", indicator)]] *
             .data[[paste0("w_", indicator)]])
}

#6.3. Counting vector----
emnv14_mpi <- emnv14_mpi %>%
  mutate(cvec = 
           rowSums(across(c(w_hh_d_materials, w_hh_d_overcrowding,
                            w_hh_d_tenure, w_hh_d_water, w_hh_d_sanitation,
                            w_hh_d_energy, w_hh_d_income, w_hh_d_assets,
                            w_hh_d_attendance, w_hh_d_schoolgap, w_hh_d_adultschooling,
                            w_hh_d_emp, w_hh_d_sp)), na.rm = TRUE))

for (k in 10:100) {
  emnv14_mpi <- emnv14_mpi %>%
    mutate(
      !!paste0("h_", k, "p")  := if_else(cvec >= k, 1, 0),
      !!paste0("a_", k, "p")  := if_else(cvec >= k, cvec, NA_real_),
      !!paste0("m0_", k, "p") := if_else(cvec >= k, cvec, 0)
    )
}

#6.4. Headcount for multidimensional poor households----

for (k in 25) {
  for (indicator in indicators) {
    emnv14_mpi <- emnv14_mpi %>%
      mutate(
        !!paste0("ch_", indicator, "_k", k, "p") := 
          if_else((cvec >= k) & (.data[[paste0("hh_d_", indicator)]] == 1), 1, 0)
      )
  }
}

k25 <- c("ch_materials_k25p", "ch_overcrowding_k25p", "ch_tenure_k25p",
         "ch_water_k25p", "ch_sanitation_k25p", "ch_energy_k25p",
         "ch_income_k25p", "ch_assets_k25p", "ch_attendance_k25p",
         "ch_schoolgap_k25p", "ch_adultschooling_k25p", "ch_emp_k25p",
         "ch_sp_k25p")

for (variable in k25) {
  emnv14_mpi <- emnv14_mpi %>%
    mutate(
      !!variable := if_else(
        is.na(.data[[variable]]),
        0,
        .data[[variable]]
      )
    )
}

#6.5. Contributions per dimension----
summary(emnv14_mpi$m0_25p)

for (indicator in indicators) {
  emnv14_mpi <- emnv14_mpi %>%
    mutate(
      !!paste0("abs_cont_", indicator, "_k", k, "p") :=
        .data[[paste0("ch_", indicator, "_k", k, "p")]] *
        .data[[paste0("w_", indicator)]],
      
      !!paste0("rel_cont_", indicator, "_k", k, "p") :=
        (.data[[paste0("ch_", indicator, "_k", k, "p")]] *
           .data[[paste0("w_", indicator)]]) / m0_25p
    )
}

#Master Database----
#1. Defining headship----

fhh_definition_14 <- 
  read_dta(here("emnv14_04_poblacion.dta"))

#2.1. Self-reported headship----
#2.1.1. General self-reported headship----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(self_fhh =
           if_else((s2p4 == 1) & (s2p5 == 2), 1, 0))

#2.1.2. Self-reported headship classification----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(self_composition = self_fhh)

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(aux_partner =
           if_else(s2p4 == 2, 1, 0))

fhh_definition_14 <- fhh_definition_14 %>%
  group_by(i00) %>%
  mutate(partner = max(aux_partner, na.rm = TRUE))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(self_composition =
           if_else(((s2p7 < 3) & (partner == 0)) & (self_fhh == 1),
                   2,
                   self_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(self_composition =
           if_else(((s2p7 < 3) & (partner == 1)) & (self_fhh == 1),
                   3,
                   self_composition))
  
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(self_composition =
           if_else((self_fhh == 0) & (s2p7 > 2),
                   4,
                   self_composition))

#2.2. Alternative definitions----
#2.2.1. General demographic definition----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(men =
           if_else(s2p2a > 17 & s2p5 == 1,
                   1,
                   0))

fhh_definition_14 <- fhh_definition_14 %>%
  group_by(i00) %>%
  mutate(aux_demographic = max(men, na.rm = TRUE))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(demographic =
           if_else(aux_demographic == 0, 1, 0))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(adult_earner =
           if_else(s2p2a > 17, 1, 0))

fhh_definition_14 <- fhh_definition_14 %>%
  group_by(i00) %>%
  mutate(n_ae = sum(adult_earner, na.rm = TRUE))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(unemployed =
           if_else(is.na(s5p1),
                   NA_real_,
                   s5p1 == 2 & s5p2 == 11))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(adult_employed = adult_earner - unemployed)

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(adult_employed =
           if_else(adult_employed == -1,
                   NA_real_,
                   adult_employed))

fhh_definition_14 <- fhh_definition_14 %>%
  group_by(i00) %>%
  mutate(ae = sum(adult_employed, na.rm = TRUE))

#2.2.2. Demographic definition----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(demographic_composition = demographic)

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(demographic_composition =
           if_else((s2p7 < 3) & (partner == 0) & (demographic == 1),
                   2,
                   demographic_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(demographic_composition =
           if_else((demographic ==0) & (s2p7 > 2),
                   3,
                   demographic_composition))
       
#2.2.3. Economic working head definition----
#Calculated as defined by Rosenhouse (1989)

#2.2.3.1. Hours in first job (waged and self-employed)----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(aux_week = 
           case_when(
             s5p16b %in% c(1) ~ 0.16666667,
             s5p16b %in% c(2)  ~ 1,
             s5p16b %in% c(3)  ~ 4.33333,
           ))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(freq1 = s5p16a * aux_week)

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(s5p17 =
           if_else(s5p17 == 998, NA_real_, s5p17))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(hours_primary = s5p17 * freq1)

#2.2.3.2. Hours in second job (waged and self-employed)----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(aux_week2 = 
           case_when(
             s5p32b %in% c(1) ~ 0.1666666,
             s5p32b %in% c(2)  ~ 1,
             s5p32b %in% c(3)  ~ 4.33333,
           ))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(freq2 = s5p32a * aux_week2)

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(hours_second = s5p33 * freq2)

#2.2.3.3. Total worked hours----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(worked_hours =
           if_else(is.na(hours_primary) & is.na(hours_second), 
                   NA_real_,
                   rowSums(across(c(hours_primary, hours_second)), na.rm = TRUE)))

#2.2.3.4 Defining headship working hours----
fhh_definition_14 <- fhh_definition_14 %>%
  group_by(i00) %>%
  mutate(
    main_hours = if_else(
      all(is.na(worked_hours)),
      NA_real_,
      max(worked_hours, na.rm = TRUE)
    )
  )

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(hh_female_w =
         if_else(s2p5 == 2,
                 (worked_hours == main_hours),
         0))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(hh_female_w =
           if_else(s2p5 == 2 & is.na(worked_hours) & is.na(main_hours),
                   1,
                   hh_female_w))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(hh_female_w = 
           case_when(
             i00 %in% c(176201, 331701, 469601) ~ 0,
             hh_female_w ==0 ~ 0,
             hh_female_w == 1 ~ 1,
             is.na(hh_female_w) ~ 0
           ))

fhh_definition_14 <- fhh_definition_14 %>%
  group_by(i00) %>% 
  mutate(fhh_work = if_else(
    all(is.na(hh_female_w)),
    NA_real_,
    max(hh_female_w, na.rm = TRUE)))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(fhh_work = if_else(
    is.na(fhh_work),
    0,
    fhh_work
  ))
  
#2.2.3.5. Headship working hours with hh composition----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(work_composition = fhh_work)

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(work_composition = 
           if_else(((s2p7 < 3) & (partner == 0)) & (fhh_work == 1),
                   2,
                   work_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(work_composition =
           if_else(is.na(work_composition),
                   1,
                   work_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(work_composition = 
           if_else((((s2p7 < 3) & (partner == 1)) & (fhh_work == 1)),
                   3,
                   work_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(work_composition =
           if_else(is.na(work_composition),
                   1,
                   work_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(work_composition = 
           if_else(((fhh_work == 0) & (s2p7 > 2)),
                   4,
                   work_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(work_composition =
           if_else(is.na(work_composition),
                   4,
                   work_composition))

#2.2.4. Cash head----
hh_cash_head <- emnv14_hh_income %>%
  select(i00, hh_m_income)

hh_cash_head <- 
  full_join(hh_cash_head, emnv14_hh_income_person,
            by = c ("i00"), 
            relationship = "one-to-many")

hh_cash_head <- hh_cash_head %>%
  select(i00, dominio4, i06, s2p00, s2p2a, s2p2b, s2p3, miembro, s2p4, s2p5,
         s2p7, i_employment, hh_employment, hh_m_income)

#2.2.4.1. Major earner definition----
hh_cash_head <- hh_cash_head %>%
  mutate(share_earner = i_employment / hh_employment)
summary(hh_cash_head$share_earner)

hh_cash_head <- hh_cash_head %>%
  group_by(i00) %>% 
  mutate(main_earner = if_else(
    all(is.na(share_earner)),
    NA_real_,
    max(share_earner, na.rm = TRUE)))

hh_cash_head <- hh_cash_head %>%
  mutate(hh_earner =
           if_else((main_earner == share_earner) & s2p5 == 2,
         1,
         0))

hh_cash_head <- hh_cash_head %>%
  mutate(hh_earner = 
           if_else(is.na(hh_earner),
                   0,
                   hh_earner))

hh_cash_head <- hh_cash_head %>%
  group_by(i00) %>%
  mutate(fhh_earner = max(hh_earner))

#2.2.4.2. Major income contributor definition----
hh_cash_head <- hh_cash_head %>%
  mutate(share_contributor = 
           if_else(hh_m_income == 0,
                   NA_real_,
             i_employment / hh_m_income))

hh_cash_head <- hh_cash_head %>%
  group_by(i00) %>% 
  mutate(main_contributor = if_else(
    all(is.na(share_contributor)),
    NA_real_,
    max(share_contributor, na.rm = TRUE)))

hh_cash_head <- hh_cash_head %>%
  mutate(hh_contributor =
           if_else((main_contributor == share_contributor) & s2p5 == 2,
                   1,
                   0))

hh_cash_head <- hh_cash_head %>%
  mutate(hh_contributor =
           if_else(is.na(hh_contributor),
                   0,
                   hh_contributor))

hh_cash_head <- hh_cash_head %>%
  group_by(i00) %>%
  mutate(fhh_contributor = max(hh_contributor, na.rm = TRUE))

hh_cash_head <- hh_cash_head %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

hh_cash_head <- hh_cash_head %>%
  select(i00, fhh_earner, fhh_contributor)

fhh_definition_14 <- 
  full_join(fhh_definition_14, hh_cash_head,
            by = c ("i00"), 
            relationship = "many-to-one")

fhh_definition_14 <- fhh_definition_14 %>%
  filter(s2p4 == 1)

#2.2.4.3. Major earner composition----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(earner_composition = fhh_earner)

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(earner_composition =
           if_else(((s2p7 < 3) & (partner == 0)) & (fhh_earner == 1),
                   2,
                   earner_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(earner_composition =
           if_else(((s2p7 < 3) & (partner ==1 )) & (fhh_earner == 1),
                   3,
                   earner_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(earner_composition =
           if_else((fhh_earner == 0) & (s2p7 > 2),
                   4,
                   earner_composition))

#2.2.4.4. Major income contributor hh composition----
fhh_definition_14 <- fhh_definition_14 %>%
  mutate(contributor_composition = fhh_contributor)

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(contributor_composition =
           if_else(((s2p7 < 3) & (partner == 0)) & (fhh_contributor == 1),
                   2,
                   contributor_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(contributor_composition =
           if_else(((s2p7 < 3) & (partner == 1)) & (fhh_contributor == 1),
                   3,
                   contributor_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  mutate(contributor_composition =
           if_else((fhh_contributor == 0) & (s2p7 > 2),
                   4,
                   contributor_composition))

fhh_definition_14 <- fhh_definition_14 %>%
  select(i00, dominio4, i06, s2p00, s2p2a, s2p2b, s2p3, miembro, s2p4, s2p5,
         s2p6a, s2p6b, s2p6c, s2p7, s4p12a, s5p1, s5p2, s5p3, peso2, peso3,
         self_fhh, self_composition, demographic, demographic_composition,
         fhh_work, work_composition, fhh_earner, fhh_contributor,
         earner_composition, contributor_composition)

#3. Summary statistics----
#3.1. Data merge----
emnv14_mpi_fhh <- 
  full_join(fhh_definition_14, emnv14_mpi,
            by = c ("i00"), 
            relationship = "one-to-one")

#3.2. Descriptives----
apply(emnv14_mpi_fhh[c("self_composition", "demographic_composition", "work_composition",
                       "earner_composition")], 2, table)

summary(emnv14_mpi_fhh$s2p2a[emnv14_mpi_fhh$self_composition == 3])
table(emnv14_mpi_fhh$s4p12a[emnv14_mpi_fhh$self_composition == 3])

emnv14_mpi_fhh <- emnv14_mpi_fhh %>%
  mutate(employment = s5p1)

emnv14_mpi_fhh <- emnv14_mpi_fhh %>%
  mutate(employment =
           if_else((!s5p2%in%c(11)) & (!is.na(s5p2)),
                   1,
                   employment))

emnv14_mpi_fhh <- emnv14_mpi_fhh %>%
  mutate(employment =
           if_else(s5p3 == 1 & !is.na(s5p3),
                   1,
                   employment))

emnv14_mpi_fhh %>% tabyl(self_fhh, employment)%>%
  adorn_percentages("row")

emnv14_mpi_fhh %>% tabyl(self_fhh, fhh_work)%>%
  adorn_percentages("col")

emnv14_mpi_fhh %>% tabyl(self_fhh, fhh_contributor)%>%
  adorn_percentages("col")

emnv14_mpi_fhh %>% tabyl(fhh_work, s2p7)%>%
  adorn_percentages("row")

emnv14_mpi_fhh %>% tabyl(fhh_contributor, s2p7)%>%
  adorn_percentages("row")

emnv14_mpi_fhh %>% tabyl(h_25p, h_50p)

#4. Household comparison----
#4.1. General self-reported headship----
indicators <- c("m0_25p", "m0_50p")
results <- list()
for (indicator in indicators) {
  summary_stats <- emnv14_mpi_fhh %>%
    group_by(self_fhh) %>%
    summarise(
      mean = mean(.data[[indicator]], na.rm = TRUE),
      sd   = sd(.data[[indicator]], na.rm = TRUE),
      min  = min(.data[[indicator]], na.rm = TRUE),
      max  = max(.data[[indicator]], na.rm = TRUE),
      n    = sum(!is.na(.data[[indicator]])),
      .groups = "drop"
    )
  
  test <- wilcox.test(
    emnv14_mpi_fhh[[indicator]] ~ emnv14_mpi_fhh$self_fhh,
    exact = FALSE
  )
  
  results[[indicator]] <- list(
    summary = summary_stats,
    wilcox  = test
  )
}

#4.2. Demographic----
results <- list()

for (indicator in indicators) {
  summary_stats <- emnv14_mpi_fhh %>%
    group_by(demographic) %>%
    summarise(
      mean = mean(.data[[indicator]], na.rm = TRUE),
      sd   = sd(.data[[indicator]], na.rm = TRUE),
      min  = min(.data[[indicator]], na.rm = TRUE),
      max  = max(.data[[indicator]], na.rm = TRUE),
      n    = sum(!is.na(.data[[indicator]])),
      .groups = "drop"
    )
  
  test <- wilcox.test(
    emnv14_mpi_fhh[[indicator]] ~ emnv14_mpi_fhh$demographic,
    exact = FALSE
  )
  
  results[[indicator]] <- list(
    summary = summary_stats,
    wilcox  = test
  )
}

#4.3. Economic working head----
results <- list()
for (indicator in indicators) {
  summary_stats <- emnv14_mpi_fhh %>%
    group_by(fhh_work) %>%
    summarise(
      mean = mean(.data[[indicator]], na.rm = TRUE),
      sd   = sd(.data[[indicator]], na.rm = TRUE),
      min  = min(.data[[indicator]], na.rm = TRUE),
      max  = max(.data[[indicator]], na.rm = TRUE),
      n    = sum(!is.na(.data[[indicator]])),
      .groups = "drop"
    )
  
  test <- wilcox.test(
    emnv14_mpi_fhh[[indicator]] ~ emnv14_mpi_fhh$fhh_work,
    exact = FALSE
  )
  
  results[[indicator]] <- list(
    summary = summary_stats,
    wilcox  = test
  )
}

#4.4. Cash head: Major income contributor----
results <- list()
for (indicator in indicators) {
  summary_stats <- emnv14_mpi_fhh %>%
    group_by(fhh_contributor) %>%
    summarise(
      mean = mean(.data[[indicator]], na.rm = TRUE),
      sd   = sd(.data[[indicator]], na.rm = TRUE),
      min  = min(.data[[indicator]], na.rm = TRUE),
      max  = max(.data[[indicator]], na.rm = TRUE),
      n    = sum(!is.na(.data[[indicator]])),
      .groups = "drop"
    )
  
  test <- wilcox.test(
    emnv14_mpi_fhh[[indicator]] ~ emnv14_mpi_fhh$fhh_contributor,
    exact = FALSE
  )
  
  results[[indicator]] <- list(
    summary = summary_stats,
    wilcox  = test
  )
}

#4.5. Self-reported headship classification----
results <- list()
for (indicator in indicators) {
  summary_stats <- emnv14_mpi_fhh%>%
    group_by(self_composition) %>%
    summarise(
      mean = mean(.data[[indicator]], na.rm = TRUE),
      sd   = sd(.data[[indicator]], na.rm = TRUE),
      min  = min(.data[[indicator]], na.rm = TRUE),
      max  = max(.data[[indicator]], na.rm = TRUE),
      n    = sum(!is.na(.data[[indicator]])),
      .groups = "drop"
    )
  
  kw <- kruskal.test(emnv14_mpi_fhh[[indicator]] ~ emnv14_mpi_fhh$self_composition)

  conover <- kwAllPairsConoverTest(
    x = emnv14_mpi_fhh[[indicator]],
    g = emnv14_mpi_fhh$self_composition,
    p.adjust.method = "none"
  )
  
  raw_p <- conover$p.value
  m <- sum(!is.na(raw_p))
  sidak_p <- 1 - (1 - raw_p)^m
  sidak_p[sidak_p > 1] <- 1
  conover$sidak.p.value <- sidak_p
  
  results[[indicator]] <- list(
    summary = summary_stats,
    kruskal = kw,
    conover = conover
  )
}

#4.6. Demographic definition classification----
results <- list()
for (indicator in indicators) {
  summary_stats <- emnv14_mpi_fhh%>%
    group_by(demographic_composition) %>%
    summarise(
      mean = mean(.data[[indicator]], na.rm = TRUE),
      sd   = sd(.data[[indicator]], na.rm = TRUE),
      min  = min(.data[[indicator]], na.rm = TRUE),
      max  = max(.data[[indicator]], na.rm = TRUE),
      n    = sum(!is.na(.data[[indicator]])),
      .groups = "drop"
    )
  
  kw <- kruskal.test(emnv14_mpi_fhh[[indicator]] ~ emnv14_mpi_fhh$demographic_composition)
  
  conover <- kwAllPairsConoverTest(
    x = emnv14_mpi_fhh[[indicator]],
    g = emnv14_mpi_fhh$demographic_composition,
    p.adjust.method = "none"
  )
  
  raw_p <- conover$p.value
  m <- sum(!is.na(raw_p))
  sidak_p <- 1 - (1 - raw_p)^m
  sidak_p[sidak_p > 1] <- 1
  conover$sidak.p.value <- sidak_p
  
  results[[indicator]] <- list(
    summary = summary_stats,
    kruskal = kw,
    conover = conover
  )
}

#4.7. Economic working headship classification with hh composition----
results <- list()
for (indicator in indicators) {
  summary_stats <- emnv14_mpi_fhh%>%
    group_by(work_composition) %>%
    summarise(
      mean = mean(.data[[indicator]], na.rm = TRUE),
      sd   = sd(.data[[indicator]], na.rm = TRUE),
      min  = min(.data[[indicator]], na.rm = TRUE),
      max  = max(.data[[indicator]], na.rm = TRUE),
      n    = sum(!is.na(.data[[indicator]])),
      .groups = "drop"
    )
  
  kw <- kruskal.test(emnv14_mpi_fhh[[indicator]] ~ emnv14_mpi_fhh$work_composition)
  
  conover <- kwAllPairsConoverTest(
    x = emnv14_mpi_fhh[[indicator]],
    g = emnv14_mpi_fhh$work_composition,
    p.adjust.method = "none"
  )
  
  raw_p <- conover$p.value
  m <- sum(!is.na(raw_p))
  sidak_p <- 1 - (1 - raw_p)^m
  sidak_p[sidak_p > 1] <- 1
  conover$sidak.p.value <- sidak_p
  
  results[[indicator]] <- list(
    summary = summary_stats,
    kruskal = kw,
    conover = conover
  )
}

#4.8. Cash head: major income contributor - composition----
results <- list()
for (indicator in indicators) {
  summary_stats <- emnv14_mpi_fhh%>%
    group_by(contributor_composition) %>%
    summarise(
      mean = mean(.data[[indicator]], na.rm = TRUE),
      sd   = sd(.data[[indicator]], na.rm = TRUE),
      min  = min(.data[[indicator]], na.rm = TRUE),
      max  = max(.data[[indicator]], na.rm = TRUE),
      n    = sum(!is.na(.data[[indicator]])),
      .groups = "drop"
    )
  
  kw <- kruskal.test(emnv14_mpi_fhh[[indicator]] ~ emnv14_mpi_fhh$contributor_composition)
  
  conover <- kwAllPairsConoverTest(
    x = emnv14_mpi_fhh[[indicator]],
    g = emnv14_mpi_fhh$contributor_composition,
    p.adjust.method = "none"
  )
  
  raw_p <- conover$p.value
  m <- sum(!is.na(raw_p))
  sidak_p <- 1 - (1 - raw_p)^m
  sidak_p[sidak_p > 1] <- 1
  conover$sidak.p.value <- sidak_p
  
  results[[indicator]] <- list(
    summary = summary_stats,
    kruskal = kw,
    conover = conover
  )
}
