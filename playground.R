# run app first to get latest functions and load the same lib env for consistency

nYears = 10

pgInputs = list(
      purchasePrice = 1500000, 
      downPayment = 500000, 
      interestRate = 4.25, 
      loanTerm = 30, 
      improvementRatio = 87, 
      yearsDepreciation = 27.5, 
      grossIncomeExpected = 105600, 
      losses = 5, 
      propertyTaxes = 14500, 
      insurance = 1600, 
      electricity = 1200, 
      gas = 0, 
      oil = 0, 
      water = 600, 
      trash = 300, 
      management = 7, 
      maintenance = 5, 
      advertising = 600, 
      telephone = 0, 
      other = 0, 
      incomeIncrease = 5, 
      expenseIncrease = 5, 
      appreciationRate = 5, 
      investorTax = 22, 
      capitalGain = 15, 
      cgtRecapturedDep = 20, 
      capitalImprovements = 0, 
      buyingCosts = 0, 
      salesCosts = 7 
)



simulate_dsc = function(inputList, year = 1) {
   
   # First year testing (x = 1)
   
   x = year
   
   
   # Operating Income
   
   incomeFactor = (1 + inputList$incomeIncrease / 100) ^ x
   expenseFactor = (1 + inputList$expenseIncrease / 100) ^ x
   xGrossIncome = inputList$grossIncomeExpected * incomeFactor
   loss = xGrossIncome * (inputList$losses / 100)
   grossIncome = xGrossIncome - loss
   
   
   # Operating expenses
   
   propertyTaxes = inputList$propertyTaxes * expenseFactor
   insurance = inputList$insurance * expenseFactor
   electricity = inputList$electricity * expenseFactor
   gas = inputList$gas * expenseFactor
   oil = inputList$oil * expenseFactor
   water = inputList$water * expenseFactor
   trash = inputList$trash * expenseFactor
   management = grossIncome * inputList$management / 100
   maintenance = grossIncome * inputList$maintenance / 100
   advertising = inputList$advertising * expenseFactor
   telephone = inputList$telephone * expenseFactor
   other = inputList$other * expenseFactor
   operatingExpenses = sum(
      propertyTaxes,  
      insurance,  
      electricity,  
      gas, 
      oil, 
      water, 
      trash, 
      management, 
      maintenance, 
      advertising, 
      telephone, 
      other,
      na.rm = TRUE
   ) 
   
   
   # Cash flow
   
   netOperatingIncome = grossIncome - operatingExpenses
   principal = inputList$purchasePrice - inputList$downPayment
   pniMonthly = pmt(
      principal = principal, 
      rate = inputList$interestRate / 100, 
      term = inputList$loanTerm
   )
   pni = 12 * pniMonthly
   cashFlow = netOperatingIncome - pni
   
   
   # Debt service coverage
   
   dsc = netOperatingIncome / pni
   
   return(dsc)
}


seq(from = 0, to = pgInputs$purchasePrice, by = 10000) %>%
   map_dbl(function(downPayment) {
      list(
         purchasePrice = 1500000, 
         downPayment = downPayment, 
         interestRate = 4.25, 
         loanTerm = 30, 
         improvementRatio = 87, 
         yearsDepreciation = 27.5, 
         grossIncomeExpected = 105600, 
         losses = 5, 
         propertyTaxes = 14500, 
         insurance = 1600, 
         electricity = 1200, 
         gas = 0, 
         oil = 0, 
         water = 600, 
         trash = 300, 
         management = 7, 
         maintenance = 5, 
         advertising = 600, 
         telephone = 0, 
         other = 0, 
         incomeIncrease = 5, 
         expenseIncrease = 5, 
         appreciationRate = 5, 
         investorTax = 22, 
         capitalGain = 15, 
         cgtRecapturedDep = 20, 
         capitalImprovements = 0, 
         buyingCosts = 0, 
         salesCosts = 7 
      ) %>%
         simulate_dsc
   })




