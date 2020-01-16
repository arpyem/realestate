# use .RData to store init values and load here

inputUI = div(
      
      # General ----------------------------------------------------------------
      
      div(
            div(
                  div("Purchase price", class = "hlabel"),
                  div("Down payment", class = "hlabel"),
                  div("Interest rate on loan", class = "hlabel"),
                  div("Term of loan", class = "hlabel"),
                  div("Improvement ratio", class = "hlabel"),
                  div("Years of depreciation", class = "hlabel"),
                  div("Annual scheduled gross income", class = "hlabel"),
                  div("Vacancy/collection losses", class = "hlabel"),
                  style = "width: 200px; text-align: right"
            ),
            div(
                  numericInput(
                        inputId = "purchasePrice", 
                        label = NULL,
                        value = 1500000, 
                        min = 0, 
                        step = 100000, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "downPayment", 
                        label = NULL,
                        value = 500000, 
                        min = 0, 
                        step = 100000, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "interestRate", 
                        label = NULL,
                        value = 4.25, 
                        min = 0, 
                        max = 100,
                        step = 0.25, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "loanTerm", 
                        label = NULL,
                        value = 30, 
                        min = 1, 
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "improvementRatio", 
                        label = NULL,
                        value = 87, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "yearsDepreciation", 
                        label = NULL,
                        value = 27.5, 
                        min = 0, 
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "grossIncomeExpected", 
                        label = NULL,
                        value = 105600, 
                        min = 0, 
                        step = 10000, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "losses", 
                        label = NULL,
                        value = 5, 
                        min = 0, 
                        step = 1, 
                        width = "100%"
                  ),
                  style = "width: 75px"
            ),
            div(
                  div("$", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("yrs", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("yrs", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  style = "margin-left: 3px"
            ),
            style = "display: flex"
      ),
      
      # Operating Expenses ----------------------------------------------------------------
      
      div(
            div(
                  div("Property taxes", class = "hlabel"),
                  div("Insurance", class = "hlabel"),
                  div("Electricity", class = "hlabel"),
                  div("Gas", class = "hlabel"),
                  div("Oil", class = "hlabel"),
                  div("Water", class = "hlabel"),
                  div("Trash", class = "hlabel"),
                  div("Management", class = "hlabel"),
                  div("Maintenance", class = "hlabel"),
                  div("Advertising", class = "hlabel"),
                  div("Telephone", class = "hlabel"),
                  div("Other", class = "hlabel"),
                  style = "width: 125px; text-align: right"
            ),
            div(
                  numericInput(
                        inputId = "propertyTaxes", 
                        label = NULL, 
                        value = 14500, 
                        min = 0, 
                        step = 10000, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "insurance", 
                        label = NULL, 
                        value = 1600, 
                        min = 0, 
                        step = 100, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "electricity", 
                        label = NULL, 
                        value = 1200, 
                        min = 0, 
                        step = 100, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "gas", 
                        label = NULL, 
                        value = 0, 
                        min = 0, 
                        step = 100, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "oil", 
                        label = NULL, 
                        value = 0, 
                        min = 0, 
                        step = 100, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "water", 
                        label = NULL, 
                        value = 600, 
                        min = 0, 
                        step = 100, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "trash", 
                        label = NULL, 
                        value = 300, 
                        min = 0, 
                        step = 100, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "management", 
                        label = NULL, 
                        value = 7, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "maintenance", 
                        label = NULL, 
                        value = 5, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "advertising", 
                        label = NULL, 
                        value = 600, 
                        min = 0, 
                        step = 100, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "telephone", 
                        label = NULL, 
                        value = 0, 
                        min = 0, 
                        step = 100, 
                        width = "100%"
                  ),
                  # make reactive
                  numericInput(
                        inputId = "other", 
                        label = NULL, 
                        value = 0, 
                        min = 0, 
                        step = 100, 
                        width = "100%"
                  ),
                  style = "width: 75px"
            ),
            div(
                  div("$", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  style = "margin-left: 3px"
            ),
            style = "display: flex"
      ),
      
      # Annual Adjustments ----------------------------------------------------------------
      
      div(
            div(
                  div("Annual income increase", class = "hlabel"),
                  div("Annual expense increase", class = "hlabel"),
                  div("Annual appreciation rate", class = "hlabel"),
                  div("Investor tax bracket", class = "hlabel"),
                  div("Capital gain tax rate", class = "hlabel"),
                  div("CGT Recaptured Depreciation Rate", class = "hlabel"),
                  div("Expected capital improvement", class = "hlabel"),
                  div("Approximate buying costs", class = "hlabel"),
                  div("Approximate selling costs", class = "hlabel"),
                  style = "width: 200px; text-align: right"
            ),
            div(
                  numericInput(
                        inputId = "incomeIncrease", 
                        label = NULL, 
                        value = 5, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "expenseIncrease", 
                        label = NULL, 
                        value = 5, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "appreciationRate", 
                        label = NULL, 
                        value = 5, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "investorTax", 
                        label = NULL, 
                        value = 22, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "capitalGain", 
                        label = NULL, 
                        value = 15, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "cgtRecapturedDep", 
                        label = NULL, 
                        value = 20, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "capitalImprovements", 
                        label = NULL, 
                        value = 0, 
                        min = 0, 
                        step = 1000, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "buyingCosts", 
                        label = NULL, 
                        value = 0, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  numericInput(
                        inputId = "salesCosts", 
                        label = NULL, 
                        value = 7, 
                        min = 0, 
                        max = 100,
                        step = 1, 
                        width = "100%"
                  ),
                  style = "width: 75px"
            ),
            div(
                  div("%", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("$", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  div("%", class = "labelUnit"),
                  style = "margin-left: 3px"
            ),
            style = "display: flex"
      ),
      style = "display: flex"
)















