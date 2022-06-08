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


getAnalyses = function(path = "data") {
    list.files(path) %>%
        map_df(function(x) {
            m = readRDS(file = file.path(path, x))
            m$analysisId = as.character(m$analysisId)
            m
        }) %>% 
        mutate(analysisDate = as_datetime(analysisDate, tz = "EST")) %>% 
        arrange(desc(analysisDate))
}


# Horizontal inputs with units
hinput <- function(label, input, unit) {
    div(
        div(label, class = "hinput-label"),
        div(input, class = "hinput-input"),
        div(unit, class = "hinput-unit"),
        class = "hinput"
    )
}


# Calculate KPIs for a specified year
run_analysis <- function(inputs, year = 1) {
    
    # Project X years out
    x <- year - 1
    
    # Operating Income ----
    incomeFactor <- (1 + inputs$incomeIncrease / 100) ^ x
    expenseFactor <- (1 + inputs$expenseIncrease / 100) ^ x
    xGrossIncome <- inputs$grossIncomeExpected * incomeFactor
    loss <- xGrossIncome * (inputs$losses / 100)
    grossIncome <- xGrossIncome - loss
    
    # Operating expenses ----
    propertyTaxes <- inputs$propertyTaxes * expenseFactor
    insurance <- inputs$insurance * expenseFactor
    electricity <- inputs$electricity * expenseFactor
    gas <- inputs$gas * expenseFactor
    oil <- inputs$oil * expenseFactor
    water <- inputs$water * expenseFactor
    trash <- inputs$trash * expenseFactor
    management <- grossIncome * inputs$management / 100
    maintenance <- grossIncome * inputs$maintenance / 100
    advertising <- inputs$advertising * expenseFactor
    telephone <- inputs$telephone * expenseFactor
    other <- inputs$other * expenseFactor
    operatingExpenses <- sum(
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
    
    # Cash Flow ----
    netOperatingIncome <- grossIncome - operatingExpenses
    principal <- inputs$purchasePrice - inputs$downPayment
    pniMonthly <- pmt(
        principal = principal, 
        rate = inputs$interestRate / 100, 
        term = inputs$loanTerm
    )
    pni <- 12 * pniMonthly
    cashFlow <- netOperatingIncome - pni
    
    # Principal reduction ----
    initBalance <- reducePrincipal(
        principal = principal, 
        rate = inputs$interestRate / 100, 
        term = inputs$loanTerm,
        currentYear = x
    )
    eoyBalance <- reducePrincipal(
        principal = principal, 
        rate = inputs$interestRate / 100, 
        term = inputs$loanTerm,
        currentYear = x + 1
    )
    principalReduction <- initBalance - eoyBalance
    
    # Tax benefit ----
    annualInterest <- pni - principalReduction
    buyingCosts <- inputs$buyingCosts / 100 * inputs$purchasePrice
    improvementValue <- inputs$improvementRatio / 100 * inputs$purchasePrice + buyingCosts + inputs$capitalImprovements
    annualDepreciation <- improvementValue / inputs$yearsDepreciation
    taxableIncome <- netOperatingIncome - annualInterest - annualDepreciation
    taxBenefit <- -taxableIncome * inputs$investorTax / 100
    
    # Property appreciation ----
    initValue <- inputs$purchasePrice * (1 + inputs$appreciationRate / 100) ^ x
    eoyValue <- inputs$purchasePrice * (1 + inputs$appreciationRate / 100) ^ (x + 1)
    annualAppreciation <- eoyValue - initValue
    
    # Equity return ----
    initEquityReturn <- cashFlow + principalReduction + taxBenefit + annualAppreciation
    initEquityReturnRatio <- initEquityReturn / (inputs$downPayment + inputs$capitalImprovements)
    totalEquity <- initValue + inputs$capitalImprovements - initBalance
    totalEquityReturn <- initEquityReturn / totalEquity
    
    # Adjusted cost basis ----
    originalBasis <- inputs$purchasePrice + buyingCosts
    salesCosts <- eoyValue * inputs$salesCosts / 100
    cumulativeDepreciation <- annualDepreciation * (x + 1)
    adjustedCostBasis <- originalBasis + 
        salesCosts + 
        inputs$capitalImprovements - 
        cumulativeDepreciation
    
    # Capital gain ----
    nadjCostBasis = originalBasis + 
        salesCosts + 
        inputs$capitalImprovements
    trueGainLoss <- eoyValue - nadjCostBasis
    capitalGain <- eoyValue - adjustedCostBasis
    capitalGainTax <- (cumulativeDepreciation * inputs$capitalGain / 100) +
        (trueGainLoss * inputs$investorTax / 100)
    
    # Sale proceeds ----
    proceedsBeforeTax <- eoyValue - salesCosts - eoyBalance
    netSaleProceeds <- proceedsBeforeTax - capitalGainTax
    
    # Debt service coverage ----
    dsc <- netOperatingIncome / pni
    
    result <- list(
        dsc = dsc,
        noi = netOperatingIncome,
        pni = pni,
        cash_flow = cashFlow,
        equity_return = initEquityReturnRatio
    )
    
    return(result)
}

