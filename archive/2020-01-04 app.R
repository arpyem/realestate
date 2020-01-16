library(shiny)
library(shinyjs)
library(tidyverse)

codeVersion = "2020.01.04.1"

# ?s
# property tax?? in inputs no dependencies
# capital gain tax using blank cells

# TODO
# save input profiles and allow them to be selected
# troubleshoot capital gain tax calculation

source("inputs.R")
# nYears = 10

clean = function(x, digits = 0, prefix = "", suffix = "") {
   paste0(prefix, format(x, digits = digits, big.mark = ",", big.interval = 3, scientific = FALSE), suffix)
}

pmt = function(principal, rate, term = 30, period = 12) {
   rateP = rate / period
   n = term * period
   principal * rateP / (1 - (1 + rateP) ^ -n)
}

reducePrincipal = function(principal, rate, term = 30, period = 12, currentYear = 1) {
   rateP = rate / period
   v1 = principal * (1 + rateP) ^ (period * currentYear)
   v2 = (1 + rateP) ^ (period * currentYear) - 1
   mthly = pmt(principal = principal, rate = rate, term = term, period = period)
   v3 = mthly / rateP * v2
   v1 - v3
}






# UI =======================================================================

ui = div(
   useShinyjs(),
   includeCSS(path = "custom.css"),
   navbarPage(
      title = "Real Estate Analysis", 
      id = "tab",
      selected = "calc",
      tabPanel(
         title = "",
         value = "inputs",
         icon = icon("calculator"),
         div(
            verbatimTextOutput(outputId = "test"),
            div(
               div("YEAR 1 PREVIEW", style = "color: #b0b0b0; margin-bottom: 5px; text-align: center"),
               uiOutput(outputId = "ui_preview"),
               style = "margin-bottom: 30px"
            ),
            inputUI,
            hidden(div(
               id = "div_saveInputs",
               actionButton(inputId = "b_saveInputs", label = "Save inputs", width = 200)
            ))
         )
      ),
      tabPanel(
         title = "",
         value = "calc",
         icon = icon("business-time"),
         div(
            div(uiOutput(outputId = "ui_summaryData"), style = "margin-bottom: 30px"),
            uiOutput(outputId = "ui_annualOperatingIncome"),
            uiOutput(outputId = "ui_annualOperatingExpenses"),
            uiOutput(outputId = "ui_cashFlow"),
            uiOutput(outputId = "ui_principalReduction"),
            uiOutput(outputId = "ui_taxBenefit"),
            uiOutput(outputId = "ui_propertyAppreciation"),
            uiOutput(outputId = "ui_equityReturnInitial"),
            uiOutput(outputId = "ui_equityReturnTotal"),
            uiOutput(outputId = "ui_adjCostBasis"),
            uiOutput(outputId = "ui_capitalGain"),
            uiOutput(outputId = "ui_saleProceeds"),
            uiOutput(outputId = "ui_dsc"),
            style = "padding: 15px; overflow-x: auto"
         )
      ),
      tabPanel(
         title = "",
         value = "settings",
         icon = icon("gear"),
         div(
            numericInput(inputId = "nYears", label = "Years to calculate", value = 10, min = 1, step = 1, width = "100%"),
            numericInput(inputId = "dscCutoff", label = "DSCR Benchmark", value = 1.15, min = 0, step = 0.1, width = "100%"),
            style = "width: 125px"
         ),
         div(
            "VERSION: ",
            codeVersion,
            " | ",
            a(href = "mailto:rmcardle5241@gmail.com?subject=Web%20app", icon("envelope")),
            style = "margin: 15px 0 15px 0; color: #b0b0b0"
         )
      )
   ),
   style = "margin: 0 0 45px 0"
) 


# SERVER ========================================================================

server = function(input, output, session) {
   
   output$test = renderPrint({
      req(NULL)
      # identical(lInputs(), l)
      # dfCalc()
   })
   
   
   # Save inputs ----------------------------------------------------------------
   
   lInputs = reactive({
      input$b_saveInputs # trigger check for changes from default inputs
      list(
         purchasePrice = input$purchasePrice, 
         downPayment = input$downPayment, 
         interestRate = input$interestRate, 
         loanTerm = input$loanTerm, 
         improvementRatio = input$improvementRatio, 
         yearsDepreciation = input$yearsDepreciation, 
         grossIncomeExpected = input$grossIncomeExpected, 
         losses = input$losses, 
         propertyTaxes = input$propertyTaxes, 
         insurance = input$insurance, 
         electricity = input$electricity, 
         gas = input$gas, 
         oil = input$oil, 
         water = input$water, 
         trash = input$trash, 
         management = input$management, 
         maintenance = input$maintenance, 
         advertising = input$advertising, 
         telephone = input$telephone, 
         other = input$other, 
         incomeIncrease = input$incomeIncrease, 
         expenseIncrease = input$expenseIncrease, 
         appreciationRate = input$appreciationRate, 
         investorTax = input$investorTax, 
         capitalGain = input$capitalGain, 
         cgtRecapturedDep = input$cgtRecapturedDep, 
         capitalImprovements = input$capitalImprovements, 
         buyingCosts = input$buyingCosts, 
         salesCosts = input$salesCosts 
      )
   })
   
   observeEvent(lInputs(), {
      if (identical(x = lInputs(), y = l)) 
         hide(id = "div_saveInputs", anim = TRUE) 
      else 
         shinyjs::show(id = "div_saveInputs",anim = TRUE)
   })
   
   observeEvent(input$b_saveInputs, {
      saveRDS(object = lInputs(), file = "inputDefaults.RData")
      l <<- readRDS(file = "inputDefaults.RData")
   })
   
   
   # Calculations -------------------------------------------------------------------
   
   calcMatrix = reactive({
      withProgress({
         0:(input$nYears - 1) %>%
            map(function(x) {
               
               # Operating income
               
               incomeFactor = (1 + input$incomeIncrease / 100) ^ x
               expenseFactor = (1 + input$expenseIncrease / 100) ^ x
               xGrossIncome = input$grossIncomeExpected * incomeFactor
               loss = xGrossIncome * (input$losses / 100)
               grossIncome = xGrossIncome - loss
               
               
               # Operating expenses
               
               propertyTaxes = input$propertyTaxes * expenseFactor
               insurance = input$insurance * expenseFactor
               electricity = input$electricity * expenseFactor
               gas = input$gas * expenseFactor
               oil = input$oil * expenseFactor
               water = input$water * expenseFactor
               trash = input$trash * expenseFactor
               management = grossIncome * input$management / 100
               maintenance = grossIncome * input$maintenance / 100
               advertising = input$advertising * expenseFactor
               telephone = input$telephone * expenseFactor
               other = input$other * expenseFactor
               operatingExpenses = propertyTaxes + 
                  insurance + 
                  electricity + 
                  gas +
                  oil +
                  water +
                  trash +
                  management +
                  maintenance +
                  advertising +
                  telephone +
                  other
               
               
               # Cash flow
               
               netOperatingIncome = grossIncome - operatingExpenses
               principal = input$purchasePrice - input$downPayment
               pni = 12 * pmt(
                  principal = principal, 
                  rate = input$interestRate / 100, 
                  term = input$loanTerm
               )
               cashFlow = netOperatingIncome - pni
               
               
               # Principal reduction
               
               initBalance = reducePrincipal(
                  principal = principal, 
                  rate = input$interestRate / 100, 
                  term = input$loanTerm,
                  currentYear = x
               )
               eoyBalance = reducePrincipal(
                  principal = principal, 
                  rate = input$interestRate / 100, 
                  term = input$loanTerm,
                  currentYear = x + 1
               )
               principalReduction = initBalance - eoyBalance
               
               
               # Tax benefit
               
               annualInterest = pni - principalReduction
               buyingCosts = input$buyingCosts / 100 * input$purchasePrice
               improvementValue = input$improvementRatio / 100 * input$purchasePrice + buyingCosts + input$capitalImprovements
               annualDepreciation = improvementValue / input$yearsDepreciation
               taxableIncome = netOperatingIncome - annualInterest - annualDepreciation
               taxBenefit = -taxableIncome * input$investorTax / 100
               
               
               # Property appreciation
               
               initValue = input$purchasePrice * (1 + input$appreciationRate / 100) ^ x
               eoyValue = input$purchasePrice * (1 + input$appreciationRate / 100) ^ (x + 1)
               annualAppreciation = eoyValue - initValue
               
               
               # Equity return
               
               initEquityReturn = cashFlow + principalReduction + taxBenefit + annualAppreciation
               initEquityReturnRatio = initEquityReturn / (input$downPayment + input$capitalImprovements)
               totalEquity = initValue + input$capitalImprovements - initBalance
               totalEquityReturn = initEquityReturn / totalEquity
               
               
               # Adjusted cost basis
               
               originalBasis = input$purchasePrice + buyingCosts
               salesCosts = eoyValue * input$salesCosts / 100
               cumulativeDepreciation = annualDepreciation * (x + 1)
               adjustedCostBasis = originalBasis + 
                  salesCosts + 
                  input$capitalImprovements - 
                  cumulativeDepreciation
               
               
               # Capital gain
               
               nadjCostBasis = originalBasis + 
                  salesCosts + 
                  input$capitalImprovements
               trueGainLoss = eoyValue - nadjCostBasis
               capitalGain = eoyValue - adjustedCostBasis
               capitalGainTax = (cumulativeDepreciation * input$capitalGain / 100) +
                  (trueGainLoss * input$investorTax / 100)
               
               
               # Sale proceeds
               
               proceedsBeforeTax = eoyValue - salesCosts - eoyBalance
               netSaleProceeds = proceedsBeforeTax - capitalGainTax
               
               
               # Debt service coverage
               
               dsc = netOperatingIncome / pni
               
               
               # Return list of values
               
               incProgress(amount = (x + 1) / input$nYears, message = "Loading")
               
               list(
                  year = x + 1,
                  annualOperatingIncome = list(
                     xGrossIncome = xGrossIncome,
                     loss = loss,
                     grossIncome = xGrossIncome - loss
                  ),
                  annualOperatingExpenses = list(
                     propertyTaxes = propertyTaxes,
                     insurance = insurance,
                     electricity = electricity,
                     gas = gas,
                     oil = oil,
                     water = water,
                     trash = trash,
                     management = management,
                     maintenance = maintenance,
                     advertising = advertising,
                     telephone = telephone,
                     other = other,
                     operatingExpenses = operatingExpenses,
                     expenseIncomeRatio = operatingExpenses / grossIncome
                  ),
                  cashFlow = list(
                     netOperatingIncome = netOperatingIncome,
                     principalInterest = pni,
                     cashFlow = cashFlow
                  ),
                  principalReduction = list(
                     startBalance = initBalance,
                     endBalance = eoyBalance,
                     principalReduction = principalReduction
                  ),
                  taxBenefit = list(
                     annualInterest = annualInterest,
                     annualDepreciation = annualDepreciation,
                     taxableIncome = taxableIncome,
                     taxBenefit = taxBenefit
                  ),
                  propertyAppreciation = list(
                     startValue = initValue,
                     endValue = eoyValue,
                     annualAppreciation = annualAppreciation
                  ),
                  equityReturn = list(
                     initEquityReturn = initEquityReturn,
                     initEquityReturnRatio = initEquityReturnRatio,
                     totalEquity = totalEquity,
                     totalEquityReturn = totalEquityReturn
                  ),
                  adjCostBasis = list(
                     originalBasis = originalBasis,
                     salesCosts = salesCosts,
                     cumulativeDepreciation = cumulativeDepreciation,
                     adjustedCostBasis = adjustedCostBasis
                  ),
                  capitalGain = list(
                     nadjCostBasis = nadjCostBasis,
                     trueGainLoss = trueGainLoss,
                     capitalGain = capitalGain,
                     capitalGainTax = capitalGainTax
                  ),
                  saleProceeds = list(
                     proceedsBeforeTax = proceedsBeforeTax,
                     netSaleProceeds = netSaleProceeds
                  ),
                  debtServiceCoverage = list(
                     dsc = dsc
                  )
               )
            })
      }, message = "Loading")
   })
   
   
   dfCalc = reactive({
      calcMatrix() %>%
         map_df(function(x) {
            x %>%
               purrr::flatten() %>%
               data.frame(stringsAsFactors = FALSE)
         })
   })
   
   
   output$ui_preview = renderUI({
      dsc1 = clean(calcMatrix()[[1]][["debtServiceCoverage"]][["dsc"]], digits = 3)
      noi = clean(calcMatrix()[[1]][["cashFlow"]][["netOperatingIncome"]], prefix = "$")
      pni = clean(calcMatrix()[[1]][["cashFlow"]][["principalInterest"]], prefix = "$")
      cf = clean(calcMatrix()[[1]][["cashFlow"]][["cashFlow"]], prefix = "$")
      er = clean(calcMatrix()[[1]][["equityReturn"]][["initEquityReturnRatio"]] * 100, digits = 3, suffix = "%")
      cclass = if (dsc1 >= input$dscCutoff) "dscGood card" else "dscBad card"
      div(
         div(
            div("DSCR", style = "color: #e6e6e6"),
            div(dsc1, style = "font-size: 2em; font-weight: bold; color: white"),
            class = cclass,
            style = "text-align: center; margin: 0 5px 0 5px; width: 125px"
         ),
         div(
            div("NOI", style = "color: #b0b0b0"),
            div(noi, style = "font-size: 2em; font-weight: bold"),
            class = "card",
            style = "text-align: center; margin: 0 5px 0 5px; width: 125px"
         ),
         div(
            div("P&I", style = "color: #b0b0b0"),
            div(pni, style = "font-size: 2em; font-weight: bold"),
            class = "card",
            style = "text-align: center; margin: 0 5px 0 5px; width: 125px"
         ),
         div(
            div("CASH FLOW", style = "color: #b0b0b0"),
            div(cf, style = "font-size: 2em; font-weight: bold"),
            class = "card",
            style = "text-align: center; margin: 0 5px 0 5px; width: 125px"
         ),
         div(
            div("EQUITY RETURN", style = "color: #b0b0b0"),
            div(er, style = "font-size: 2em; font-weight: bold"),
            class = "card",
            style = "text-align: center; margin: 0 5px 0 5px; width: 125px"
         ),
         style = "display: flex; justify-content: center"
      )
   })
   
   
   
   # Output -----------------------------------------------------------------------------
   
   output$ui_summaryData = renderUI({
      pni = calcMatrix()[[1]][["cashFlow"]][["principalInterest"]]
      dep = calcMatrix()[[1]][["taxBenefit"]][["annualDepreciation"]]
      div(
         div(
            div(
               "ACQUISITION DATA",
               style = "color: #b0b0b0"
            ),
            div(
               div("PRICE"),
               div(clean(input$purchasePrice, prefix = "$")),
               class = "pair"
            ),
            div(
               div("DOWN PAYMENT"),
               div(clean(input$downPayment, prefix = "$")),
               class = "pair"
            ),
            div(
               div("LOAN AMOUNT"),
               div(clean(input$purchasePrice - input$downPayment, prefix = "$")),
               class = "pair"
            ),
            div(
               div("BUY COSTS"),
               div(clean(input$buyingCosts / 100 * input$purchasePrice, prefix = "$")),
               class = "pair"
            ),
            div(
               div("CAPITAL IMPROVEMENTS"),
               div(clean(input$capitalImprovements, prefix = "$")),
               class = "pair"
            ),
            class = "card",
            style = "width: 250px; margin: 0 5px 0 5px"
         ),
         div(
            div(
               div("LOAN DATA"),
               style = "color: #b0b0b0"
            ),
            div(
               div("INTEREST RATE"),
               div(clean(input$interestRate, digits = 3, suffix = "%")),
               class = "pair"
            ),
            div(
               div("LOAN TERM"),
               div(clean(input$loanTerm, suffix = " yrs")),
               class = "pair"
            ),
            div(
               div("MONTHLY P&I"),
               div(clean(pni / 12, prefix = "$")),
               class = "pair"
            ),
            div(
               div("YEARLY P&I"),
               div(clean(pni, prefix = "$")),
               class = "pair"
            ),
            class = "card",
            style = "width: 250px; margin: 0 5px 0 5px"
         ),
         div(
            div(
               div("DEPRECIATION DATA"),
               style = "color: #b0b0b0"
            ),
            div(
               div("LAND"),
               div(clean(100 - input$improvementRatio, suffix = "%")),
               class = "pair"
            ),
            div(
               div("IMPROVEMENT"),
               div(clean(input$improvementRatio, suffix = "%")),
               class = "pair"
            ),
            div(
               div("TIME"),
               div(clean(input$yearsDepreciation, digits = 3, suffix = " yrs")),
               class = "pair"
            ),
            div(
               div("YEARLY DEPRECIATION"),
               div(clean(dep, prefix = "$")),
               class = "pair"
            ),
            class = "card",
            style = "width: 250px; margin: 0 5px 0 5px"
         ),
         style = "display: flex; justify-content: center"
      )
   })
   
   
   # Annual Operating Income
   
   output$ui_annualOperatingIncome = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$annualOperatingIncome$xGrossIncome)),
               div(clean(y$annualOperatingIncome$loss)),
               div(clean(y$annualOperatingIncome$grossIncome), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("ANNUAL OPERATING INCOME", style = "color: #b0b0b0"),
               div("EXPECTED GROSS INCOME"),
               div("VACANCY/COLLECTION LOSSES"),
               div("EFFECTIVE GROSS INCOME", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Annual Operating Expenses
   
   output$ui_annualOperatingExpenses = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$annualOperatingExpenses$propertyTaxes)),
               div(clean(y$annualOperatingExpenses$insurance)),
               div(clean(y$annualOperatingExpenses$electricity)),
               div(clean(y$annualOperatingExpenses$gas)),
               div(clean(y$annualOperatingExpenses$oil)),
               div(clean(y$annualOperatingExpenses$water)),
               div(clean(y$annualOperatingExpenses$management)),
               div(clean(y$annualOperatingExpenses$maintenance)),
               div(clean(y$annualOperatingExpenses$advertising)),
               div(clean(y$annualOperatingExpenses$telephone)),
               div(clean(y$annualOperatingExpenses$other)),
               div(clean(y$annualOperatingExpenses$operatingExpenses), style = "font-weight: bold"),
               div(clean(y$annualOperatingExpenses$expenseIncomeRatio * 100, digits = 3, suffix = "%"), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("ANNUAL OPERATING EXPENSES", style = "color: #b0b0b0"),
               div("PROPERTY TAXES"),
               div("INSURANCE"),
               div("ELECTRICITY"),
               div("GAS"),
               div("OIL"),
               div("WATER"),
               div("MANAGEMENT"),
               div("MAINTENANCE"),
               div("ADVERTISING"),
               div("TELEPHONE"),
               div("OTHER"),
               div("OPERATING EXPENSES", style = "font-weight: bold"),
               div("EXPENSE-INCOME RATIO", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Cash flow (before taxes)
   
   output$ui_cashFlow = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$cashFlow$netOperatingIncome)),
               div(clean(y$cashFlow$principalInterest)),
               div(clean(y$cashFlow$cashFlow), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("CASH FLOW (PRE-TAX)", style = "color: #b0b0b0"),
               div("NET OPERATING INCOME"),
               div("P&I"),
               div("CASH FLOW", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Principal Reduction
   
   output$ui_principalReduction = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$principalReduction$startBalance)),
               div(clean(y$principalReduction$endBalance)),
               div(clean(y$principalReduction$principalReduction), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("MORTGAGE PRINCIPAL REDUCTION", style = "color: #b0b0b0"),
               div("STARTING BALANCE"),
               div("ENDING BALANCE"),
               div("PRINCIPAL REDUCTION", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Tax Benefit
   
   output$ui_taxBenefit = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$cashFlow$netOperatingIncome)),
               div(clean(y$taxBenefit$annualInterest)),
               div(clean(y$taxBenefit$annualDepreciation)),
               div(clean(y$taxBenefit$taxableIncome)),
               div(clean(y$taxBenefit$taxBenefit), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("TAX BENEFIT", style = "color: #b0b0b0"),
               div("NET OPERATING INCOME"),
               div("ANNUAL INTEREST"),
               div("ANNUAL DEPRECIATION"),
               div("TAXABLE INCOME"),
               div("TAX BENEFIT", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Property Appreciation
   
   output$ui_propertyAppreciation = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$propertyAppreciation$startValue)),
               div(clean(y$propertyAppreciation$endValue)),
               div(clean(y$propertyAppreciation$annualAppreciation), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("PROPERTY APPRECIATION", style = "color: #b0b0b0"),
               div("START OF YEAR VALUE"),
               div("END OF YEAR VALUE"),
               div("ANNUAL APPRECIATION", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Return on Equity
   
   output$ui_equityReturnInitial = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$cashFlow$cashFlow)),
               div(clean(y$taxBenefit$taxBenefit)),
               div(clean(y$principalReduction$principalReduction)),
               div(clean(y$propertyAppreciation$annualAppreciation)),
               div(clean(y$equityReturn$initEquityReturn), style = "font-weight: bold"),
               div(clean(y$equityReturn$initEquityReturnRatio * 100, digits = 3, suffix = "%"), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("RETURN ON INITIAL EQUITY", style = "color: #b0b0b0"),
               div("CASH FLOW"),
               div("TAX BENEFIT"),
               div("DEBT REDUCTION"),
               div("ANNUAL APPRECIATION"),
               div("RETURN ON INITIAL EQUITY", style = "font-weight: bold"),
               br(),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   output$ui_equityReturnTotal = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$propertyAppreciation$startValue)),
               div(clean(y$principalReduction$startBalance)),
               div(clean(y$equityReturn$totalEquity), style = "font-weight: bold"),
               div(clean(y$equityReturn$totalEquityReturn * 100, digits = 3, suffix = "%"), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("RETURN ON TOTAL EQUITY", style = "color: #b0b0b0"),
               div("START OF YEAR VALUE"),
               div("START OF YEAR BALANCE"),
               div("TOTAL EQUITY", style = "font-weight: bold"),
               div("TOTAL EQUITY RETURN", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Adjusted Cost Basis
   
   output$ui_adjCostBasis = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$adjCostBasis$originalBasis)),
               div(clean(y$adjCostBasis$salesCosts)),
               div(clean(y$adjCostBasis$cumulativeDepreciation)),
               div(clean(y$adjCostBasis$adjustedCostBasis), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("ADJUSTED COST BASIS", style = "color: #b0b0b0"),
               div("ORIGINAL BASIS"),
               div("SALES COSTS"),
               div("CUMULATIVE DEPRECIATION"),
               div("ADJUSTED COST BASIS", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Capital Gain
   
   output$ui_capitalGain = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$propertyAppreciation$endValue)),
               div(clean(y$capitalGain$nadjCostBasis)),
               div(clean(y$capitalGain$trueGainLoss), style = "font-weight: bold"),
               div(clean(y$adjCostBasis$adjustedCostBasis)),
               div(clean(y$capitalGain$capitalGain), style = "font-weight: bold"),
               div(clean(y$capitalGain$capitalGainTax), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("CAPITAL GAIN", style = "color: #b0b0b0"),
               div("SALES PRICE"),
               div("NON-ADJUSTED COST"),
               div("TRUE GAIN/LOSS", style = "font-weight: bold"),
               div("ADJUSTED COST BASIS"),
               div("CAPITAL GAIN", style = "font-weight: bold"),
               div("CAPITAL GAIN TAX", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Est. Net Sale Proceeds
   
   output$ui_saleProceeds = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$propertyAppreciation$endValue)),
               div(clean(y$principalReduction$endBalance)),
               div(clean(y$saleProceeds$proceedsBeforeTax), style = "font-weight: bold"),
               div(clean(y$capitalGain$capitalGainTax)), # capital gain tax not calculating correctly --------
               div(clean(y$saleProceeds$netSaleProceeds), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("EST. NET SALE PROCEEDS", style = "color: #b0b0b0"),
               div("SALES PRICE"),
               div("LOAN BALANCE"),
               div("SALE PROCEEDS (PRE-TAX)", style = "font-weight: bold"),
               div("CAPITAL GAIN TAX"),
               div("NET SALE PROCEEDS", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })
   
   
   # Debt Service Coverage
   
   output$ui_dsc = renderUI({
      cells = calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$cashFlow$netOperatingIncome)),
               div(clean(y$cashFlow$principalInterest)),
               div(clean(y$debtServiceCoverage$dsc, digits = 3), style = "font-weight: bold"),
               style = "text-align: right; align-self: flex-end; width: 125px"
            ) 
         })
      div(
         div(
            div(
               div("DEBT SERVICE COVERAGE", style = "color: #b0b0b0"),
               div("NET OPERATING INCOME"),
               div("P&I"),
               div("DSC RATIO", style = "font-weight: bold"),
               style = "width: 250px"
            ),
            cells, 
            style = "display: flex; justify-content: space-around"
         ),
         style = "margin-bottom: 15px"
      )
   })

   
   
      
}

shinyApp(ui = ui, server = server)