source("smith_analysis.R")
# test cases
employment_income <- 200000
home_value <- 2000000
mortgage_balance <- 1000000
loc_interest_rate <- 0.04
loc_ballance <- 0.65 * (home_value - mortgage_balance)
dividend_yeild <- 0.11

for (i in 1:10) {
  if (i == 1) {
    interest_expense <- loc_interest_rate * loc_ballance
    dividend_income <- loc_ballance * dividend_yeild
    scenarios <- calculate_tax_liability_sm(employment_income = employment_income, 
                                            dividend_income = 0, 
                                            interest_expense = 0, 
                                            dividend_type_split = 0.5)
  } else {
    loc_ballance <- loc_ballance + scenarios$sm_effective_value[i - 1]
    if (loc_ballance > 0.65 * (home_value)) {
      loc_ballance <- 0.65 * (home_value)
    }
    interest_expense <- loc_interest_rate * loc_ballance
    dividend_income <- loc_ballance * dividend_yeild
    scenarios <- rbind(scenarios, calculate_tax_liability_sm(employment_income = employment_income, 
                                                             dividend_income = dividend_income, 
                                                             dividend_type_split = 0.5, 
                                                             interest_expense = interest_expense)
    )
  }
}

scenarios

# test cases
for (i in 1:10) {
  if (i == 1) {
    interest_expense <- loc_interest_rate * loc_ballance
    dividend_income <- loc_ballance * dividend_yeild
    scenarios2 <- calculate_tax_liability_sm(employment_income = employment_income, 
                                             dividend_income = 0, 
                                             interest_expense = 0, 
                                             dividend_type_split = 0.5)
  } else {
    mortgage_balance <- mortgage_balance - 100000
    if (mortgage_balance < 0) {
      mortgage_balance <- 0
    }
    loc_ballance <- 0.65 * (home_value - mortgage_balance)
    interest_expense <- loc_interest_rate * loc_ballance
    dividend_income <- loc_ballance * dividend_yeild
    scenarios2 <- rbind(scenarios2, calculate_tax_liability_sm(employment_income = employment_income, 
                                                               dividend_income = dividend_income, 
                                                               dividend_type_split = 0.5, 
                                                               interest_expense = interest_expense)
    )
  }
}

scenarios2
