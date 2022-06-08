library(shiny)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(scales)

codeVersion = "2.0.1"

source("setup.R")

df <- getAnalyses()

last_analysis <- readRDS("last_analysis.rds")
if (!last_analysis %in% df$analysisId) {
   last_analysis <- head(df$analysisId, 1)
}

# monitor usage

# usage = readRDS('usage')
# usage = c(Sys.time(), usage)
# saveRDS(usage, 'usage')

# ?s
# property tax?? in inputs no dependencies
# capital gain tax using blank cells

# TODO
# save input profiles and allow them to be selected
# calculate to 1.15 dscr or other back calculations - means solving for dsc through pmt function
# plot ranges

# source("inputs.R")



# UI =======================================================================

ui = div(
   useShinyjs(),
   includeCSS(path = "custom.css"),
   navbarPage(
      title = "Real Estate Analysis", 
      id = "tab",
      # selected = "calc",
      
      ## UI: Inputs ----
      tabPanel(
         title = "",
         value = "inputs",
         icon = icon("calculator"),
         div(
            verbatimTextOutput(outputId = "test"),
            
            # Analysis Title
            uiOutput(outputId = "ui_title"),
            
            # Preview
            div(
               div("YEAR 1 PREVIEW", style = "color: #b0b0b0; margin-bottom: 5px; text-align: center"),
               uiOutput(outputId = "ui_preview"),
               style = "margin-bottom: 30px"
            ),
            
            # Inputs
            div(
               uiOutput(outputId = "ui_inputs"),
               style = "margin-bottom: 10px"
            )
         )
      ),
      
      ## UI: Outputs ----
      tabPanel(
         title = "",
         value = "calc",
         icon = icon("table"),
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
            uiOutput(outputId = "ui_dsc")
         )
      ),
      
      ## UI: Settings ----
      tabPanel(
         title = "",
         value = "settings",
         icon = icon("gear"),
         div(
            numericInput(inputId = "nYears", label = "Years to calculate", value = 10, min = 1, step = 1, width = "100%"),
            numericInput(inputId = "dscCutoff", label = "DSCR Benchmark", value = 1.15, min = 0, step = 0.1, width = "100%"),
            numericInput(inputId = "font_size", label = "Font size", value = 18, min = 10, max = 20, step = 1, width = "100%"),
            style = "width: 10em"
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
   style = "margin: 0 0 120px 0"
) 


# SERVER ========================================================================

server = function(input, output, session) {
   
   output$test = renderPrint({
      req(NULL)
      list(
         rv$selected_analysis,
         c(
            analysisId = max(as.numeric(rv$data$analysisId)) + 1,
            analysisDate = Sys.time(),
            analysisName = input$analysisName,
            lInputs()
         ),
         as.list(c(
            analysisId = rv$selected_analysis,
            analysisDate = Sys.time(),
            analysisName = input$analysisName,
            lInputs()
         ))
      )
   })
   
   
   # Reactive values -------------------------
   
   rv = reactiveValues(
      l = readRDS(file = "inputDefaults.rds"),
      data = df,
      trigger_analyses = NA,
      selected_analysis = last_analysis,
      delete_analysis = NA
   )
   
   
   
   # Selected Analysis ----
   observe({
      if (!is.na(rv$selected_analysis)) {
         removeModal()
         updateTabsetPanel(inputId = "tab", selected = "inputs")
      }
   }) %>%
      bindEvent(rv$selected_analysis, ignoreInit = TRUE)
   
   # Title
   output$ui_title <- renderUI({
      
      x <- filter(rv$data, analysisId == rv$selected_analysis)
      
      div(
         div(
            x$analysisName,
            class = "analysis-title"
         ),
         div(
            paste0(
               x$analysisId, " - ",
               format(x$analysisDate, "%b %d, %Y")
            ),
            class = "analysis-subtitle"
         )
      )
      
   })
   
   
   
   # Inputs -----------------------------------------------------------------------------------------------------------------
   
   output$ui_inputs <- renderUI({
      req(rv$data, rv$selected_analysis)
      message("selected: ", rv$selected_analysis)
      
      l <- filter(rv$data, analysisId == rv$selected_analysis)
      # l <- head(rv$data, 1)
      
      inputs <- div(
         
         ## General ----
         div(
            
            hinput(
               label = "Purchase price",
               input = numericInput(
                  inputId = "purchasePrice", 
                  label = NULL,
                  value = l$purchasePrice, 
                  min = 0, 
                  step = 100000, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Down payment",
               input = numericInput(
                  inputId = "downPayment", 
                  label = NULL,
                  value = l$downPayment, 
                  min = 0, 
                  step = 100000, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Interest rate on loan",
               input = numericInput(
                  inputId = "interestRate", 
                  label = NULL,
                  value = l$interestRate, 
                  min = 0, 
                  max = 100,
                  step = 0.25, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Term of loan",
               input = numericInput(
                  inputId = "loanTerm", 
                  label = NULL,
                  value = l$loanTerm, 
                  min = 1, 
                  step = 1, 
                  width = "100%"
               ),
               unit = "yrs"
            ),
            
            hinput(
               label = "Improvement ratio",
               input = numericInput(
                  inputId = "improvementRatio", 
                  label = NULL,
                  value = l$improvementRatio, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Years of depreciation",
               input = numericInput(
                  inputId = "yearsDepreciation", 
                  label = NULL,
                  value = l$yearsDepreciation, 
                  min = 0, 
                  step = 1, 
                  width = "100%"
               ),
               unit = "yrs"
            ),
            
            hinput(
               label = "Annual scheduled gross income",
               input = numericInput(
                  inputId = "grossIncomeExpected", 
                  label = NULL,
                  value = l$grossIncomeExpected, 
                  min = 0, 
                  step = 10000, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Vacancy/collection losses",
               input = numericInput(
                  inputId = "losses", 
                  label = NULL,
                  value = l$losses, 
                  min = 0, 
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            )
            
         ), 
         
         
         # Operating Expenses ----
         
         div(
            
            hinput(
               label = "Property taxes",
               input = numericInput(
                  inputId = "propertyTaxes", 
                  label = NULL, 
                  value = l$propertyTaxes, 
                  min = 0, 
                  step = 10000, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Insurance",
               input = numericInput(
                  inputId = "insurance", 
                  label = NULL, 
                  value = l$insurance, 
                  min = 0, 
                  step = 100, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Electricity",
               input = numericInput(
                  inputId = "electricity", 
                  label = NULL, 
                  value = l$electricity, 
                  min = 0, 
                  step = 100, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Gas",
               input = numericInput(
                  inputId = "gas", 
                  label = NULL, 
                  value = l$gas, 
                  min = 0, 
                  step = 100, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Oil",
               input = numericInput(
                  inputId = "oil", 
                  label = NULL, 
                  value = l$oil, 
                  min = 0, 
                  step = 100, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Water",
               input = numericInput(
                  inputId = "water", 
                  label = NULL, 
                  value = l$water, 
                  min = 0, 
                  step = 100, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Trash",
               input = numericInput(
                  inputId = "trash", 
                  label = NULL, 
                  value = l$trash, 
                  min = 0, 
                  step = 100, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Management",
               input = numericInput(
                  inputId = "management", 
                  label = NULL, 
                  value = l$management, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Maintenance",
               input = numericInput(
                  inputId = "maintenance", 
                  label = NULL, 
                  value = l$maintenance, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Advertising",
               input = numericInput(
                  inputId = "advertising", 
                  label = NULL, 
                  value = l$advertising, 
                  min = 0, 
                  step = 100, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Telephone",
               input = numericInput(
                  inputId = "telephone", 
                  label = NULL, 
                  value = l$telephone, 
                  min = 0, 
                  step = 100, 
                  width = "100%"
               ),
               unit = "$"
            ),
            
            hinput(
               label = "Other",
               input = numericInput(
                  inputId = "other", 
                  label = NULL, 
                  value = l$other, 
                  min = 0, 
                  step = 100, 
                  width = "100%"
               ),
               unit = "$"
            )
            
         ),
         
         # Annual Adjustments ----
         div(
            
            hinput(
               label = "Annual income increase",
               input = numericInput(
                  inputId = "incomeIncrease", 
                  label = NULL, 
                  value = l$incomeIncrease, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Annual expense increase",
               input = numericInput(
                  inputId = "expenseIncrease", 
                  label = NULL, 
                  value = l$expenseIncrease, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Annual appreciation rate",
               input = numericInput(
                  inputId = "appreciationRate", 
                  label = NULL, 
                  value = l$appreciationRate, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Investor tax bracket",
               input = numericInput(
                  inputId = "investorTax", 
                  label = NULL, 
                  value = l$investorTax, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Capital gain tax rate",
               input = numericInput(
                  inputId = "capitalGain", 
                  label = NULL, 
                  value = l$capitalGain, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "CGT Recaptured Depreciation Rate",
               input = numericInput(
                  inputId = "cgtRecapturedDep", 
                  label = NULL, 
                  value = 20, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Expected capital improvement",
               input = numericInput(
                  inputId = "capitalImprovements", 
                  label = NULL, 
                  value = l$capitalImprovements, 
                  min = 0, 
                  step = 1000, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Approximate buying costs",
               input = numericInput(
                  inputId = "buyingCosts", 
                  label = NULL, 
                  value = l$buyingCosts, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            ),
            
            hinput(
               label = "Approximate selling costs",
               input = numericInput(
                  inputId = "salesCosts", 
                  label = NULL, 
                  value = l$salesCosts, 
                  min = 0, 
                  max = 100,
                  step = 1, 
                  width = "100%"
               ),
               unit = "%"
            )
            
         ),
         
         style = "display: flex; justify-content: center; flex-wrap: wrap"
         
      ) # end Inputs
      
      inputs
      
   })
   
   
   
   
   # Analyses ----------------------------------------------------------------
   
   # Add tab button
   insertUI(
      selector = "#tab li:last-child", 
      where = "beforeBegin", 
      ui = tags$li(
         tags$a(
            id = "analyses",
            icon(name = "folder-open"),
            style = "cursor: pointer"
         )
      ), 
      multiple = FALSE
   )
   
   # Add event to tab button
   onclick(id = "analyses", {rv$trigger_analysis <- Sys.time()})
   
   
   
   ## Library ----
   output$ui_analysis_library <- renderUI({
      
      headers <- div(
         div("#", style = "width: 3em; color: #b0b0b0"),
         div("NAME", style = "width: 15em"),
         div("DATE", style = "width: 13em"),
         div("DSC", style = "width: 5em"),
         div("NOI", style = "width: 5em"),
         div("PNI", style = "width: 5em"),
         div("CF", style = "width: 5em"),
         div("ER", style = "width: 5em"),
         div("", style = "width: 3em"),
         div("", style = "width: 3em"),
         class = "analysis-headers"
      )
      
      rows <- rv$data %>%
         mutate(analysisId = fct_reorder(analysisId, analysisDate, .desc = TRUE)) %>%
         split(.$analysisId) %>%
         map(function(x) {
            
            preview <- run_analysis(x)
            
            id_row <- paste0("a_row", x$analysisId)
            onclick(id_row, {rv$row_analysis <- x$analysisName})
            
            id_load <- paste0("a_load_", x$analysisId)
            onclick(id_load, {rv$selected_analysis <- x$analysisId})
            
            id_delete <- paste0("a_delete_", x$analysisId)
            onclick(id_delete, {rv$delete_analysis <- x$analysisId})
            
            div(
               id = id_row,
               div(
                  x$analysisId, 
                  style = "width: 3em; color: #b0b0b0"
               ),
               div(
                  x$analysisName, 
                  style = "width: 15em"
               ),
               div(
                  format(x$analysisDate, "%b %d, %Y %R %Z"), 
                  style = "width: 13em"
               ),
               div(
                  round(preview$dsc, 2), 
                  style = "width: 5em"
               ),
               div(
                  dollar(preview$noi, accuracy = 1), 
                  style = "width: 5em"
               ),
               div(
                  dollar(preview$pni, accuracy = 1), 
                  style = "width: 5em"
               ),
               div(
                  dollar(preview$cash_flow, accuracy = 1), 
                  style = "width: 5em"
               ),
               div(
                  percent(preview$equity_return, accuracy = 0.1), 
                  style = "width: 5em"
               ),
               div(
                  actionButton(
                     inputId = id_load, 
                     label = NULL, 
                     icon = icon("folder-open"),
                     class = "btn-info"
                  ), 
                  style = "width: 3em"
               ),
               div(
                  actionButton(
                     inputId = id_delete, 
                     label = NULL, 
                     icon = icon("times"),
                     class = "btn-danger"
                  ), 
                  style = "width: 3em"
               ),
               class = "analysis-row"
            )
            
         })
      
      div(
         headers,
         div(rows, class = "analysis-rows"),
         class = "analysis-container"
      )
      
   })
   
   observe({
      if (!is.na(rv$row_analysis)) updateTextInput(inputId = "analysisName", value = rv$row_analysis)
   }) %>%
      bindEvent(rv$row_analysis, ignoreInit = TRUE)
   
   
   
   ## Modal ----
   observe({
      
      showModal(modalDialog(
         title = "Analysis Library",
         div(
            id = "div_saveInputs",
            div(
               div(
                  div(
                     textInput(
                        inputId = "analysisName", 
                        label = NULL,
                        width = "15em", 
                        placeholder = "Save current analysis"
                     ),
                     style = "text-align: left; margin: 0"
                  ),
                  div(
                     actionButton(
                        inputId = "b_saveInputs", 
                        label = NULL, 
                        icon = icon("plus"), 
                        class = "btn-info", 
                        style = "margin-left: 3px; height: 3em; width: 3em"
                     )
                  ),
                  class = "analysis-new"
               ),
               uiOutput(outputId = "ui_analysis_library"),
               style = "width: 100%"
            ),
            style = "display: flex; justify-content: center; margin-top: 10px"
         ), 
         size = "l", 
         easyClose = TRUE
      ))
      
   }) %>%
      bindEvent(rv$trigger_analysis, ignoreInit = TRUE)
   
   
   
   
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
   
   
   ## Save Analysis ----
   
   observe({
      if (trimws(input$analysisName) == "" | is.na(input$analysisName)) {
         disable(id = "b_saveInputs")
      } else {
         enable(id = "b_saveInputs")
      }
   }) %>%
      bindEvent(input$analysisName)
   
   observe({
      
      # Overwrite check
      if (input$analysisName %in% rv$data$analysisName) {
         
         showModal(modalDialog(
            title = "Analysis already exists",
            div(
               div(
                  "Do you want to overwrite ", 
                  tags$b(input$analysisName), "?", 
                  style = "text-align: center; margin-bottom: 15px"
               ),
               div(
                  actionButton(inputId = "b_overwrite_confirm", label = "Yes", width = "45%", class = "btn-danger"),
                  actionButton(inputId = "b_overwrite_cancel", label = "No", width = "45%"),
                  style = "display: flex; justify-content: space-around"
               )
            ), 
            footer = NULL, 
            size = "m", 
            easyClose = FALSE
         ))
         
      } else {
         
         withProgress({
            
            d <- as.list(c(
               analysisId = max(as.numeric(rv$data$analysisId)) + 1,
               analysisDate = Sys.time(),
               analysisName = input$analysisName,
               lInputs()
            ))
            
            if (trimws(d$analysisName) == "" | is.na(d$analysisName)) {
               d$analysisName = paste0("analysis", d$analysisId)
            }
            
            path = paste0("data/analysis", d$analysisId, ".rds")
            saveRDS(object = d, file = path)
            rv$data = getAnalyses()
            
            updateTextInput(session = session, inputId = "analysisName", value = "")
            rv$selected_analysis <- d$analysisId
            
         }, message = "Saving new analysis")
         
      }
      
   }) %>%
      bindEvent(input$b_saveInputs)
   
   
   ### Overwrite Analysis ----
   
   # Confirm overwrite
   observe({
      
      withProgress({
         
         d <- as.list(c(
            analysisId = rv$selected_analysis,
            analysisDate = Sys.time(),
            analysisName = input$analysisName,
            lInputs()
         ))
         
         path = paste0("data/analysis", d$analysisId, ".rds")
         saveRDS(object = d, file = path)
         rv$data = getAnalyses()
         
         updateTextInput(session = session, inputId = "analysisName", value = "")
         rv$selected_analysis <- d$analysisId
         
      }, message = "Overwriting analysis")
      
      removeModal()
      
   }) %>%
      bindEvent(input$b_overwrite_confirm)
   
   
   # Cancel overwrite
   observe({
      removeModal()
   }) %>% 
      bindEvent(input$b_overwrite_cancel)
   
   
   
   ## Delete Analysis ----
   
   observe({
      
      if (!is.na(rv$delete_analysis)) {
         
         analysis_name <- rv$data %>%
            filter(analysisId == rv$delete_analysis) %>%
            pull(analysisName)
         
         showModal(modalDialog(
            title = "Delete Analysis",
            div(
               div(
                  "Do you want to delete ", 
                  tags$b(analysis_name), "?", 
                  style = "text-align: center; margin-bottom: 15px"
               ),
               div(
                  actionButton(inputId = "b_delete_confirm", label = "Yes", width = "45%", class = "btn-danger"),
                  actionButton(inputId = "b_delete_cancel", label = "No", width = "45%"),
                  style = "display: flex; justify-content: space-around"
               )
            ), 
            footer = NULL, 
            size = "m", 
            easyClose = FALSE
         ))
         
      }
      
   }) %>%
      bindEvent(rv$delete_analysis)
   
   
   # Cancel delete
   
   
   observe({
      removeModal()
      rv$delete_analysis <- NA
   }) %>%
      bindEvent(input$b_delete_cancel)
   
   
   # Confirm delete
   observe({
      
      file <- paste0("analysis", rv$delete_analysis, ".rds")
      from <- file.path("data", file)
      to <- file.path("recycling_bin", file)
      
      # Don't overwrite recycled files
      if (file.exists(to)) {
         to <- file.path("recycling_bin", paste0("analysis", rv$delete_analysis, Sys.Date(), ".rds"))
      }
      
      # Copy data to recycling bin and remove from data directory
      if (file.exists(from)) {
         file.copy(from = from, to = to, overwrite = FALSE)
         file.remove(from)
      }
      
      removeModal()
      rv$delete_analysis <- NA
      rv$data <- getAnalyses()
      
      if (last_analysis %in% rv$data$analysisId) {
         rv$selected_analysis <- last_analysis
      } else {
         rv$selected_analysis <- head(rv$data$analysisId, 1)
      }
      
   }) %>%
      bindEvent(input$b_delete_confirm)
   
   
   # Calculations -------------------------------------------------------------------
   
   # df_summary <- reactive({
   #     req(lInputs(), input$nYears)
   #     
   #     inputs <- lInputs()
   #     i[is.na(i)] = 0 # set blanks to zero
   #     
   #     1:input$nYears %>%
   #         map(~as_tibble(run_analysis(inputs = inputs, year = .x))) %>%
   #         bind_rows(.id = "year")
   # })
   
   
   
   
   calcMatrix = reactive({
      req(lInputs())
      
      i = lInputs()
      i[is.na(i)] = 0 # set blanks to zero
      
      withProgress({
         
         0:(input$nYears - 1) %>%
            map(function(x) {
               
               ## Operating income ----
               
               incomeFactor = (1 + i$incomeIncrease / 100) ^ x
               expenseFactor = (1 + i$expenseIncrease / 100) ^ x
               xGrossIncome = i$grossIncomeExpected * incomeFactor
               loss = xGrossIncome * (i$losses / 100)
               grossIncome = xGrossIncome - loss
               
               
               ## Operating expenses ----
               
               propertyTaxes = i$propertyTaxes * expenseFactor
               insurance = i$insurance * expenseFactor
               electricity = i$electricity * expenseFactor
               gas = i$gas * expenseFactor
               oil = i$oil * expenseFactor
               water = i$water * expenseFactor
               trash = i$trash * expenseFactor
               management = grossIncome * i$management / 100
               maintenance = grossIncome * i$maintenance / 100
               advertising = i$advertising * expenseFactor
               telephone = i$telephone * expenseFactor
               other = i$other * expenseFactor
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
               
               
               ## Cash flow ----
               
               netOperatingIncome = grossIncome - operatingExpenses
               principal = i$purchasePrice - i$downPayment
               pniMonthly = pmt(
                  principal = principal, 
                  rate = i$interestRate / 100, 
                  term = i$loanTerm
               )
               pni = 12 * pniMonthly
               cashFlow = netOperatingIncome - pni
               
               
               ## Principal reduction ----
               
               initBalance = reducePrincipal(
                  principal = principal, 
                  rate = i$interestRate / 100, 
                  term = i$loanTerm,
                  currentYear = x
               )
               eoyBalance = reducePrincipal(
                  principal = principal, 
                  rate = i$interestRate / 100, 
                  term = i$loanTerm,
                  currentYear = x + 1
               )
               principalReduction = initBalance - eoyBalance
               
               
               ## Tax benefit ----
               
               annualInterest = pni - principalReduction
               buyingCosts = i$buyingCosts / 100 * i$purchasePrice
               improvementValue = i$improvementRatio / 100 * i$purchasePrice + buyingCosts + i$capitalImprovements
               annualDepreciation = improvementValue / i$yearsDepreciation
               taxableIncome = netOperatingIncome - annualInterest - annualDepreciation
               taxBenefit = -taxableIncome * i$investorTax / 100
               
               
               ## Property appreciation ----
               
               initValue = i$purchasePrice * (1 + i$appreciationRate / 100) ^ x
               eoyValue = i$purchasePrice * (1 + i$appreciationRate / 100) ^ (x + 1)
               annualAppreciation = eoyValue - initValue
               
               
               ## Equity return ----
               
               initEquityReturn = cashFlow + principalReduction + taxBenefit + annualAppreciation
               initEquityReturnRatio = initEquityReturn / (i$downPayment + i$capitalImprovements)
               totalEquity = initValue + i$capitalImprovements - initBalance
               totalEquityReturn = initEquityReturn / totalEquity
               
               
               ## Adjusted cost basis ----
               
               originalBasis = i$purchasePrice + buyingCosts
               salesCosts = eoyValue * i$salesCosts / 100
               cumulativeDepreciation = annualDepreciation * (x + 1)
               adjustedCostBasis = originalBasis + 
                  salesCosts + 
                  i$capitalImprovements - 
                  cumulativeDepreciation
               
               
               ## Capital gain ----
               
               nadjCostBasis = originalBasis + 
                  salesCosts + 
                  i$capitalImprovements
               trueGainLoss = eoyValue - nadjCostBasis
               capitalGain = eoyValue - adjustedCostBasis
               capitalGainTax = (cumulativeDepreciation * i$capitalGain / 100) +
                  (trueGainLoss * i$investorTax / 100)
               
               
               ## Sale proceeds ----
               
               proceedsBeforeTax = eoyValue - salesCosts - eoyBalance
               netSaleProceeds = proceedsBeforeTax - capitalGainTax
               
               
               ## Debt service coverage ----
               
               dsc = netOperatingIncome / pni
               
               
               # TESTING
               
               cashFlowReinvest = 50 / 100 # potential input for reinvesting % of cash flow
               
               
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
                  ),
                  misc = list(
                     cashFlowReinvest = 50
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
   
   
   # Preview ----
   output$ui_preview = renderUI({
      dsc1 = clean(calcMatrix()[[1]][["debtServiceCoverage"]][["dsc"]], digits = 3)
      noi = clean(calcMatrix()[[1]][["cashFlow"]][["netOperatingIncome"]], prefix = "$")
      pni = clean(calcMatrix()[[1]][["cashFlow"]][["principalInterest"]], prefix = "$")
      cf = clean(calcMatrix()[[1]][["cashFlow"]][["cashFlow"]], prefix = "$")
      er = clean(calcMatrix()[[1]][["equityReturn"]][["initEquityReturnRatio"]] * 100, digits = 3, suffix = "%")
      cclass = if (dsc1 >= input$dscCutoff) "dscGood card y1-preview" else "dscBad card y1-preview"
      div(
         div(
            div("DSCR", style = "color: #e6e6e6"),
            div(dsc1, style = "font-size: 2em; font-weight: bold; color: white"),
            class = cclass
         ),
         div(
            div("NOI", style = "color: #b0b0b0"),
            div(noi, style = "font-size: 2em; font-weight: bold"),
            class = "card y1-preview"
         ),
         div(
            div("P&I", style = "color: #b0b0b0"),
            div(pni, style = "font-size: 2em; font-weight: bold"),
            class = "card y1-preview"
         ),
         div(
            div("CASH FLOW", style = "color: #b0b0b0"),
            div(cf, style = "font-size: 2em; font-weight: bold"),
            class = "card y1-preview"
         ),
         div(
            div("EQUITY RETURN", style = "color: #b0b0b0"),
            div(er, style = "font-size: 2em; font-weight: bold"),
            class = "card y1-preview"
         ),
         style = "display: flex; justify-content: center"
      )
   })
   
   
   
   # Output -----------------------------------------------------------------------------
   
   ## Summary ----
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
            style = "width: 20em; margin: 0 5px 0 5px"
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
            style = "width: 20em; margin: 0 5px 0 5px"
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
            style = "width: 20em; margin: 0 5px 0 5px"
         ),
         style = "display: flex; justify-content: center"
      )
   })
   
   
   ## Annual Operating Income ----
   output$ui_annualOperatingIncome <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$annualOperatingIncome$xGrossIncome)),
               div(clean(y$annualOperatingIncome$loss)),
               div(clean(y$annualOperatingIncome$grossIncome), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("ANNUAL OPERATING INCOME", style = "color: #b0b0b0"),
            div("EXPECTED GROSS INCOME"),
            div("VACANCY/COLLECTION LOSSES"),
            div("EFFECTIVE GROSS INCOME", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Annual Operating Expenses ----
   output$ui_annualOperatingExpenses <- renderUI({
      
      cells <- calcMatrix() %>%
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
               class = "analysis-table-cells"
            ) 
         })
      
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
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Cash flow (before taxes) ----
   output$ui_cashFlow <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$cashFlow$netOperatingIncome)),
               div(clean(y$cashFlow$principalInterest)),
               div(clean(y$cashFlow$cashFlow), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("CASH FLOW (PRE-TAX)", style = "color: #b0b0b0"),
            div("NET OPERATING INCOME"),
            div("P&I"),
            div("CASH FLOW", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Principal Reduction ----
   output$ui_principalReduction <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$principalReduction$startBalance)),
               div(clean(y$principalReduction$endBalance)),
               div(clean(y$principalReduction$principalReduction), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("MORTGAGE PRINCIPAL REDUCTION", style = "color: #b0b0b0"),
            div("STARTING BALANCE"),
            div("ENDING BALANCE"),
            div("PRINCIPAL REDUCTION", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Tax Benefit ----
   output$ui_taxBenefit <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$cashFlow$netOperatingIncome)),
               div(clean(y$taxBenefit$annualInterest)),
               div(clean(y$taxBenefit$annualDepreciation)),
               div(clean(y$taxBenefit$taxableIncome)),
               div(clean(y$taxBenefit$taxBenefit), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("TAX BENEFIT", style = "color: #b0b0b0"),
            div("NET OPERATING INCOME"),
            div("ANNUAL INTEREST"),
            div("ANNUAL DEPRECIATION"),
            div("TAXABLE INCOME"),
            div("TAX BENEFIT", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Property Appreciation ----
   output$ui_propertyAppreciation <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$propertyAppreciation$startValue)),
               div(clean(y$propertyAppreciation$endValue)),
               div(clean(y$propertyAppreciation$annualAppreciation), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("PROPERTY APPRECIATION", style = "color: #b0b0b0"),
            div("START OF YEAR VALUE"),
            div("END OF YEAR VALUE"),
            div("ANNUAL APPRECIATION", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Return on Equity ----
   output$ui_equityReturnInitial <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$cashFlow$cashFlow)),
               div(clean(y$taxBenefit$taxBenefit)),
               div(clean(y$principalReduction$principalReduction)),
               div(clean(y$propertyAppreciation$annualAppreciation)),
               div(clean(y$equityReturn$initEquityReturn), style = "font-weight: bold"),
               div(clean(y$equityReturn$initEquityReturnRatio * 100, digits = 3, suffix = "%"), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("RETURN ON INITIAL EQUITY", style = "color: #b0b0b0"),
            div("CASH FLOW"),
            div("TAX BENEFIT"),
            div("DEBT REDUCTION"),
            div("ANNUAL APPRECIATION"),
            div("RETURN ON INITIAL EQUITY", style = "font-weight: bold"),
            br(),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   output$ui_equityReturnTotal <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$propertyAppreciation$startValue)),
               div(clean(y$principalReduction$startBalance)),
               div(clean(y$equityReturn$totalEquity), style = "font-weight: bold"),
               div(clean(y$equityReturn$totalEquityReturn * 100, digits = 3, suffix = "%"), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("RETURN ON TOTAL EQUITY", style = "color: #b0b0b0"),
            div("START OF YEAR VALUE"),
            div("START OF YEAR BALANCE"),
            div("TOTAL EQUITY", style = "font-weight: bold"),
            div("TOTAL EQUITY RETURN", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Adjusted Cost Basis ----
   output$ui_adjCostBasis <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$adjCostBasis$originalBasis)),
               div(clean(y$adjCostBasis$salesCosts)),
               div(clean(y$adjCostBasis$cumulativeDepreciation)),
               div(clean(y$adjCostBasis$adjustedCostBasis), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("ADJUSTED COST BASIS", style = "color: #b0b0b0"),
            div("ORIGINAL BASIS"),
            div("SALES COSTS"),
            div("CUMULATIVE DEPRECIATION"),
            div("ADJUSTED COST BASIS", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Capital Gain ----
   output$ui_capitalGain <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$propertyAppreciation$endValue)),
               div(clean(y$capitalGain$nadjCostBasis)),
               div(clean(y$capitalGain$trueGainLoss), style = "font-weight: bold"),
               div(clean(y$adjCostBasis$adjustedCostBasis)),
               div(clean(y$capitalGain$capitalGain), style = "font-weight: bold"),
               div(clean(y$capitalGain$capitalGainTax), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("CAPITAL GAIN", style = "color: #b0b0b0"),
            div("SALES PRICE"),
            div("NON-ADJUSTED COST"),
            div("TRUE GAIN/LOSS", style = "font-weight: bold"),
            div("ADJUSTED COST BASIS"),
            div("CAPITAL GAIN", style = "font-weight: bold"),
            div("CAPITAL GAIN TAX", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Est. Net Sale Proceeds ----
   output$ui_saleProceeds <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$propertyAppreciation$endValue)),
               div(clean(y$principalReduction$endBalance)),
               div(clean(y$saleProceeds$proceedsBeforeTax), style = "font-weight: bold"),
               div(clean(y$capitalGain$capitalGainTax)), # FIXME capital gain tax not calculating correctly 
               div(clean(y$saleProceeds$netSaleProceeds), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("EST. NET SALE PROCEEDS", style = "color: #b0b0b0"),
            div("SALES PRICE"),
            div("LOAN BALANCE"),
            div("SALE PROCEEDS (PRE-TAX)", style = "font-weight: bold"),
            div("CAPITAL GAIN TAX"),
            div("NET SALE PROCEEDS", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   ## Debt Service Coverage ----
   output$ui_dsc <- renderUI({
      
      cells <- calcMatrix() %>%
         map(function(y) {
            div(
               div("YEAR ", y$year, style = "color: #b0b0b0"),
               div(clean(y$cashFlow$netOperatingIncome)),
               div(clean(y$cashFlow$principalInterest)),
               div(clean(y$debtServiceCoverage$dsc, digits = 3), style = "font-weight: bold"),
               class = "analysis-table-cells"
            ) 
         })
      
      div(
         div(
            div("DEBT SERVICE COVERAGE", style = "color: #b0b0b0"),
            div("NET OPERATING INCOME"),
            div("P&I"),
            div("DSC RATIO", style = "font-weight: bold"),
            class = "analysis-table-headers"
         ),
         cells, 
         class = "analysis-table"
      )
      
   })
   
   
   
   # Settings -------------------
   
   # Update font size
   font_size <- reactive({
      input$font_size
   }) %>%
      debounce(1000)
   
   observe({
      
      if (is.numeric(input$font_size)) {
         if (input$font_size >= 10 & input$font_size <= 24) {
            runjs(paste0("document.body.style.fontSize = '", input$font_size, "px'"))
         }
      }
      
   }) %>%
      bindEvent(input$font_size)
   
   
   # Last Analysis
   
   onStop(function() {
      
      isolate({
         if (!is.na(rv$selected_analysis)) {
            if (rv$selected_analysis %in% rv$data$analysisId) {
               saveRDS(object = rv$selected_analysis, file = "last_analysis.rds")
            }
         }
      })
      
   })
   
   
   
}

shinyApp(ui = ui, server = server)