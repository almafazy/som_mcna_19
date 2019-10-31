# horizontal aggregation
## Pre-existing vulnerability indicators
response <-
  response %>%
  # 1.1 Vulnerable heads of household
  new_recoding(target = hhh_score, source = household_expenditure) %>%
  recode_to(to = 1, where.selected.any = "adult_male") %>%
  recode_to(to = 2, where.selected.any = "adult_female") %>%
  recode_to(to = 3, where.selected.any = "eldery_male") %>%
  recode_to(to = 4, where.selected.any = "eldery_female") %>%
  recode_to(to = 5, where.selected.any = "male_14_17") %>%
  recode_to(to = 6, where.selected.any = "female_14_17") %>%
  recode_to(to = 7, where.selected.any = "male_13") %>%
  recode_to(to = 8, where.selected.any = "female_13") %>%
  # 1.1b primary income earner
  new_recoding(target = primary_income_earner_score, source = breadwinner) %>%
  recode_to(to = 1, where.selected.any = "adult_male") %>%
  recode_to(to = 2, where.selected.any = "adult_female") %>%
  recode_to(to = 3, where.selected.any = "eldery_male") %>%
  recode_to(to = 4, where.selected.any = "eldery_female") %>%
  recode_to(to = 5, where.selected.any = "male_14_17") %>%
  recode_to(to = 6, where.selected.any = "female_14_17") %>%
  recode_to(to = 7, where.selected.any = "male_13") %>%
  recode_to(to = 8, where.selected.any = "female_13") %>%
  # 1.2 Vulnerable members in household
  new_recoding(target = vulnerable_members_score) %>%
  recode_to(to = 1, where = person_with_disabilities == "no") %>%
  recode_to(to = 2, where = no_difficulty > 0) %>%
  recode_to(to = 2, where = person_with_disabilities == "yes") %>%
  recode_to(to = 3, where = minor_difficulties > 0 | chronic == "yes" | plw == "yes") %>%
  recode_to(to = 4, where = some_difficulties > 0) %>%
  recode_to(to = 4, where = plw == "yes" & minor_difficulties > 0) %>%
  recode_to(to = 4, where = plw == "yes" & chronic == "yes") %>%
  recode_to(to = 4, where = chronic == "yes" & minor_difficulties > 0) %>%
  recode_to(to = 5, where = a_lot > 0) %>%
  recode_to(to = 5, where = plw == "yes" & some_difficulties > 0) %>%
  recode_to(to = 5, where = chronic == "yes" & some_difficulties > 0) %>%
  recode_to(to = 6, where = plw == "yes" & a_lot > 0) %>%
  recode_to(to = 6, where = chronic == "yes" & a_lot > 0) %>%
  recode_to(to = 7, where = cannot_carry > 0) %>%
  recode_to(to = 8, where = plw == "yes" & cannot_carry > 0) %>%
  recode_to(to = 8, where = chronic == "yes" & cannot_carry > 0) %>%
  # 2.1 Documentation
  new_recoding(target = id_score) %>%
  recode_to(to = 1, where = hh_ids == "all") %>%
  recode_to(to = 2, where = hh_ids != "all" & hh_ids_renew == "yes") %>%
  recode_to(to = 3, where = hh_ids != "all" & hh_ids_renew == "dontknow") %>%
  recode_to(to = 4, where = hh_ids != "all" & hh_ids_renew == "no") %>%
  # Dependency level
  ##3.1 ADR
  new_recoding(target = adr) %>%
  recode_directly(to_expression = (males_0_6m + females_0_6m + males_6m_4y + females_6m_4y + males_5_12 + females_5_12+
                                     males_13_15 + females_13_15 + males_60_over + females_60_over) / 
                    (males_16_17 + females_16_17 + males_18_40 + females_18_40 + males_41_59+ females_41_59)) %>%
  new_recoding(target = adr_score) %>%
  recode_to(to = 1, where = adr <= .20) %>%
  recode_to(to = 2, where = adr > .21 & adr <= .30) %>%
  recode_to(to = 3, where = adr >.31 & adr <= .40) %>%
  recode_to(to = 4, where = adr >.41 & adr <= .60) %>%
  recode_to(to = 5, where = adr >.61 & adr <= .70) %>%
  recode_to(to = 6, where = adr >.71 & adr <= .80) %>%
  recode_to(to = 7, where = adr >.81 & adr <= .90) %>%
  recode_to(to = 8, where = adr >.90) %>%
  ##3.2 WDR
  new_recoding(target = wdr) %>%
  recode_directly(to_expression = (total_hh - working_persons) / working_persons) %>%
  new_recoding(target = wdr_score) %>%
  recode_to(to = 1, where = wdr <= .20) %>%
  recode_to(to = 2, where = wdr > .21 & wdr <= .30) %>%
  recode_to(to = 3, where = wdr >.31 & wdr <= .40) %>%
  recode_to(to = 4, where = wdr >.41 & wdr <= .60) %>%
  recode_to(to = 5, where = wdr >.61 & wdr <= .70) %>%
  recode_to(to = 6, where = wdr >.71 & wdr <= .80) %>%
  recode_to(to = 7, where = wdr >.81 & wdr <= .90) %>%
  recode_to(to = 8, where = wdr >.90) %>%
  ##3.3 CGT
  new_recoding(target = cgt_score, source = care_giving_time) %>%
  recode_to(to = 2, where.selected.exactly = "less_1h") %>%
  recode_to(to = 3, where.selected.exactly = "1h_2h") %>%
  recode_to(to = 4, where.selected.exactly = "2h_3h") %>%
  recode_to(to = 6, where.selected.exactly = "3h_4h") %>%
  recode_to(to = 8, where.selected.exactly = "4hmore") %>%
  # Poverty levels
  ## 4.1 poverty_level_income
  new_recoding(target = income_per_capita) %>%
  recode_directly(to_expression = income_middle / total_hh) %>%
  new_recoding(target = income_score) %>%
  recode_to(to = 7, where = income_per_capita <= 30) %>%
  recode_to(to = 6, where = income_per_capita > 30) %>%
  recode_to(to = 5, where = income_per_capita > 60) %>%
  recode_to(to = 4, where = income_per_capita > 100) %>%
  recode_to(to = 3, where = income_per_capita > 150) %>%
  recode_to(to = 2, where = income_per_capita > 200) %>%
  recode_to(to = 8, where = average_income == "none") %>%
  # kept in case we reverting to the income_score rather than income_per_capita
  # new_recoding(target = income_score, source = average_income) %>%
  # recode_to(to = 2, where.selected.exactly = "200more") %>%
  # recode_to(to = 3, where.selected.exactly = "151_200") %>%
  # recode_to(to = 4, where.selected.exactly = "101_150") %>%
  # recode_to(to = 5, where.selected.exactly = "61_100") %>%
  # recode_to(to = 6, where.selected.exactly = "31_60") %>%
  # recode_to(to = 7, where.selected.exactly = "less30") %>%
  # recode_to(to = 8, where.selected.exactly = "none") %>%
  ## 4.5 debt income ratio
  ### income_middle_point
  #see horizontal aggregation for all expenditure convertion
  ### debt_middle_point
  new_recoding(target = debt_middle, source = average_debt) %>%
  recode_to(to = 200, where.selected.exactly = "200more") %>%
  recode_to(to = 175, where.selected.exactly = "151_200") %>%
  recode_to(to = 125, where.selected.exactly = "101_150") %>%
  recode_to(to = 80, where.selected.exactly = "61_100") %>%
  recode_to(to = 45, where.selected.exactly = "31_60") %>%
  recode_to(to = 15, where.selected.exactly = "less30") %>%
  recode_to(to = 10, where.selected.exactly = "none") %>%
  ### dir
  new_recoding(target = dir) %>%
  recode_directly(to_expression = debt_middle / income_middle) %>%
  new_recoding(target = dir_score) %>%
  recode_to(to = 1, where = dir == 0) %>%
  recode_to(to = 2, where = dir <= .25) %>%
  recode_to(to = 3, where = dir > .25 & dir <= .50) %>%
  recode_to(to = 4, where = dir >.50 & dir <= .60) %>%
  recode_to(to = 5, where = dir >.61 & dir <= .80) %>%
  recode_to(to = 6, where = dir >.80 & dir <= 1) %>%
  recode_to(to = 7, where = dir >1 & dir <= 1.5) %>%
  recode_to(to = 8, where = dir >1.5) %>%
  # 5.1 Expenditure on basic goods and services
  ## total expenditure
  #see horizontal aggregation for all expenditure convertion
  ## total_expenditure_middle
  new_recoding(target = total_expenditure_middle) %>%
  recode_directly(to_expression = sum(spent_education_middle, spent_health_middle, spent_water_middle,
                    spent_food_middle, na.rm = T)) %>%
  ## hhex
  new_recoding(target = hhex) %>%
  recode_directly(to_expression = total_expenditure_middle / income_middle) %>%
  ## hhex_score
  new_recoding(target = hhex_score) %>%
  recode_to(to = 1, where = hhex <= .25) %>%
  recode_to(to = 2, where = hhex > .25 & hhex <= .50) %>%
  recode_to(to = 3, where = hhex > .50 & hhex <= .75) %>%
  recode_to(to = 4, where = hhex > .75 & hhex <= .80) %>%
  recode_to(to = 5, where = hhex > .80 & hhex <= .90) %>%
  recode_to(to = 6, where = hhex > .90 & hhex <= 1) %>%
  recode_to(to = 7, where = hhex > 1 & hhex <= 2) %>%
  recode_to(to = 8, where = hhex > 2) %>%
  # 6.1 displacement (see horizontal aggregation.R for months convertion)
  new_recoding(target = length_of_displacement_score) %>%
  recode_to(to = 1, where.selected.exactly = "yes", source = yes_no_host) %>%
  recode_to(to = 3, where = diff_today_aoo_months > 6 & diff_today_aoo_months <= 12) %>%
  recode_to(to = 4, where = diff_today_aoo_months > 12 & diff_today_aoo_months <= 24) %>%
  recode_to(to = 5, where = diff_today_aoo_months > 24 & diff_today_aoo_months <= 36) %>%
  recode_to(to = 6, where = diff_today_aoo_months > 36 & diff_today_aoo_months <= 48) %>%
  recode_to(to = 7, where = diff_today_aoo_months > 48) %>%
  recode_to(to = 7, where = diff_today_aoo_months > 3 & diff_today_aoo_months <= 6) %>%
  recode_to(to = 8, where = diff_today_aoo_months <= 3) %>%
  end_recoding()