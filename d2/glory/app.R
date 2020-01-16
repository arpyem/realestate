library(shiny)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(reticulate)
library(RSQLite)
library(DBI)
library(jsonlite)



# TODO
# 


# Python

use_condaenv("/home/arpyem/anaconda3/envs/d2/bin/python")


# Setup

source("setup.R")


# UI

ui = fluidPage(
   useShinyjs(),
   includeCSS("custom.css"),
   div(
      verbatimTextOutput(outputId = "test"),
      div(
         id = "div_settings",
         # div(textInput(inputId = "clanId", label = "Clan ID", value = "3130534", width = "100%"), style = "width: 125px; margin-right: 15px"),
         div(textInput(inputId = "clanName", label = "Clan Name", value = "Mythical Outlaws", width = "100%"), style = "width: 250px; margin-right: 15px"),
         div(actionButton(inputId = "b_clanName", label = "Search clan", width = "100%"), style = "width: 125px; padding-top: 26px; margin-right: 15px"),
         div(numericInput(inputId = "nActivities", label = "Number of activities", value = 25, min = 1, max = 250, step = 1, width = "100%"), style = "width: 150px; margin-right: 15px"),
         div(selectInput(inputId = "activityMode", label = "Activity modes", choices = c("All Crucible" = 5, "Survival" = 37), selected = 5, width = "100%"), style = "width: 150px; margin-right: 15px"),
         div(uiOutput(outputId = "ui_member"), style = "width: 250px; margin-right: 15px"),
         div(checkboxInput(inputId = "getGlory", label = "Show Glory", value = TRUE, width = "100%"), style = "width: 100px; margin-right: 15px; align-self: center"),
         # div(
         #    icon("steam"), 
         #    icon("xbox"), 
         #    icon("playstation"),
         #    style = "display: flex; justify-content: space-around; height: 40px; background-color: var(--boxColor_focus); border-radius: 10px"
         # ),
         class = "card",
         style = "display: flex; flex-wrap: wrap"
      ),
      div(
         id = "div_loading",
         icon("sync-alt"),
         style = "text-align: center; margin: 20px"
      ),
      div(
         uiOutput(outputId = "ui_pgcr"),
         style = "margin: 10px 0 10px 0"
      ),
      div(
         uiOutput(outputId = "ui_activityHistory")
      ),
      style = "margin: 10px 0 10px 0"
   )
)


# Server

server = function(input, output, session) {
   
   onclick(id = "fa-steam", expr = {rv$platform = "fa-steam"})
   
   # TEST ----
   
   output$test = renderPrint({
      req(NULL)
      rv$Message
   })
   
   
   # Reactive Values ----
   
   rv = reactiveValues(
      platform = "fa-steam",
      clanName = "",
      clanMembers = NA,
      member = list(), # selected member
      instanceId = NA,
      pgcr = NA,
      pgcrCache = list(),
      pgcrUI = list(),
      pgcrMember = list(), # Destiny user info for the selected player in the PGCR
      pgcrMemberStats = list(), # Account stats for the selected player in the PGCR
      pgcrMember_showing = list(), # List of players with account stats currently shown (to prevent duplicates)
      savedPGCRs = readRDS(file = "savedPGCRs.RData"),
      Message = ""
   )
   
   
   # Platform
   
   observeEvent(rv$platform, {
      addClass(id = rv$platform, class = "")
   })
   
   
   # Clan
   
   observe({
      input$b_clanName
      isolate({
         if (input$clanName != rv$clanName) {
            addClass(class = "load", selector = ".fa-sync-alt")
            withProgress({
               clanInfo = get_clanInfo(group = input$clanName, method = "name")
               incProgress(amount = 1/2, message = "Getting clan")
               if (clanInfo$ErrorCode == 1) {
                  rv$clanName = input$clanName # prevent searching the same clan 
                  rv$Message = clanInfo$Message
                  clanId = clanInfo$Response$detail$groupId
                  clanMembers = get_clanMembers(groupId = clanId)
                  incProgress(amount = 1/2, message = "Getting clan")
                  if (clanMembers$ErrorCode == 1) {
                     rv$Message = clanMembers$Message
                     rv$clanMembers = clanMembers
                  } else {
                     rv$Message = clanMembers$Message
                  }
               } else {
                  rv$clanName = input$clanName
                  rv$Message = clanInfo$Message
               }
            }, message = "Getting clan")
         }
      })
   })
   
   clanList = reactive({
      req(rv$clanMembers)
      rv$clanMembers$Response$results$destinyUserInfo
   })
   
   output$ui_member = renderUI({
      req(clanList())
      selectInput(inputId = "member", label = "Player", choices = clanList()$LastSeenDisplayName %>% unique %>% sort, width = "100%", selected = "arpyem")#selected = clanList()$LastSeenDisplayName[43], width = "100%")
   })
   
   
   # Selected member
   
   observeEvent(input$member, {
      addClass(class = "load", selector = ".fa-sync-alt")
      member = clanList() %>%
         filter(displayName == input$member) %>%
         head(1) # for players with the same displayName on multiple platforms
      rv$member = list(
         displayName = member$displayName,
         membershipType = member$membershipType,
         membershipId = member$membershipId
      )
      removeClass(class = "load", selector = ".fa-sync-alt")
   })
   
   
   # Get member characters
   
   memberCharacters = reactive({
      req(rv$member$membershipType)
      addClass(class = "load", selector = ".fa-sync-alt")
      withProgress({
         p = get_profile(rv$member$membershipType, rv$member$membershipId, components$Characters)
         incProgress(amount = 1, message = "Getting profile")
         removeClass(class = "load", selector = ".fa-sync-alt")
         if (p$ErrorCode == 1) {
            rv$Message = p$Message
            return(p)
         } else {
            rv$Message = p$Message
         }
      }, message = "Getting profile")
   })
   
   
   # Get activity history
   
   activityHistory = reactive({
      req(memberCharacters())
      addClass(class = "load", selector = ".fa-sync-alt")
      rv$instanceId = NA # reset selected instance
      rv$pgcrCache = list() # reset pgcrCache
      withProgress({
         characterIds = names(memberCharacters()$Response$characters$data)
         characterIds %>%
            map_df(function(characterId) {
               
               activityHistory = get_activityHistory(rv$member$membershipType, rv$member$membershipId, characterId, count = input$nActivities, mode = input$activityMode)
               incProgress(amount = 1 / length(characterIds), message = "Getting characters")
               
               if (activityHistory$ErrorCode == 1) {
                  
                  rv$Message = activityHistory$Message
                  activities = activityHistory$Response$activities
                  
                  data.frame(
                     period = activities$period,
                     instanceId = activities$activityDetails$instanceId,
                     referenceId = activities$activityDetails$referenceId,
                     directorActivityHash = activities$activityDetails$directorActivityHash,
                     mode = activities$activityDetails$mode,
                     activities$values %>%
                        map(function(value) value$basic$displayValue) %>%
                        data.frame(stringsAsFactors = FALSE),
                     stringsAsFactors = FALSE
                  )
                  
               } else {
                  rv$Message = activityHistory$Message
               }
            })
      }, message = "Getting characters")
   })
   
   
   # Render activity history
   
   output$ui_activityHistory = renderUI({
      if (rv$Message == "Ok" | rv$Message == "") {
         req(activityHistory())
         activities = activityHistory() %>%
            mutate(period = ymd_hms(period)) %>%
            arrange(desc(period)) %>%
            head(input$nActivities)

         info = div(
            div(
               div("Win %", style = "color: gray"),
               div(round(sum(activities$standing == "Victory") / nrow(activities) * 100) %>% paste0("%"), style = "font-weight: bold"),
               class = "card",
               style = "padding: 5px; margin: 0 10px 0 0; text-align: center"
            ),
            div(
               div("K/D", style = "color: gray"),
               div(round(sum(as.numeric(activities$kills)) / sum(as.numeric(activities$deaths)), 2), style = "font-weight: bold"),
               class = "card",
               style = "padding: 5px; margin: 0 10px 0 0; text-align: center"
            ),
            style = "display: flex"
         )

         headers = div(
            div("MODE", style = "width: 150px; padding: 5px;"),
            div(style = "width: 30px; padding: 5px"),
            div("DATE", style = "width: 150px; padding: 5px"),
            div("MAP", style = "width: 150px; padding: 5px"),
            div("RESULT", style = "width: 150px; padding: 5px"),
            div("EFFICIENCY", style = "width: 150px; padding: 5px"),
            div("TIME", style = "width: 150px; padding: 5px"),
            div("REASON", style = "width: 150px; padding: 5px"),
            class = "spread activity-headers",
            style = "padding-left: 10px"
         )

         rows = 1:nrow(activities) %>%
            map(function(x) {
               activity = activities[x, ]
               onclick(id = as.character(activity$instanceId), expr = {rv$instanceId = activity$instanceId}) # set instanceId
               activityInfo = DestinyActivityDefinition[[as.character(activity$referenceId)]]
               activityDefinition = DestinyActivityDefinition[[as.character(activity$directorActivityHash)]]
               activityModeInfo = DestinyActivityModeDefinition[[as.character(activityInfo$directActivityModeHash)]]

               style = if (activity$standing == "Victory" | activity$standing == 1) {
                  "border-left: 4px solid var(--win); padding-left: 10px"
               } else {
                  "border-left: 4px solid var(--loss); padding-left: 10px"
               }

               div(
                  id = as.character(activity$instanceId),
                  div(
                     class = "image",
                     style = paste0("background-image: url(https://www.bungie.net", activityDefinition$displayProperties$icon, "); width: 30px; height: 30px")
                  ),
                  div(
                     activityDefinition$displayProperties$name,
                     class = "activity-item",
                     style = "width: 150px"
                  ),
                  div(
                     activity$period %>% format("%d %b %Y"),
                     class = "activity-item",
                     style = "width: 150px"
                  ),
                  div(
                     activityInfo$displayProperties$name,
                     class = "activity-item",
                     style = "width: 150px"
                  ),
                  div(
                     activity$standing,
                     class = "activity-item",
                     style = "width: 150px"
                  ),
                  div(
                     activity$efficiency,
                     class = "activity-item",
                     style = "width: 150px"
                  ),
                  div(
                     activity$timePlayedSeconds,
                     class = "activity-item",
                     style = "width: 150px"
                  ),
                  div(
                     activity$completionReason,
                     class = "activity-item",
                     style = "width: 150px"
                  ),
                  class = "spread activity-row",
                  style = style
               )
            })

         removeClass(class = "load", selector = ".fa-sync-alt")
         removeClass(selector = ".activity-row", class = "activity-row-selected")
         div(
            info,
            headers,
            rows,
            class = "activity card"
         )

      } else {
         removeClass(class = "load", selector = ".fa-sync-alt")
         div(
            rv$Message,
            class = "card"
         )
      }

   })
   
   
   # Get PGCR when an activity is selected
   
   observeEvent(rv$instanceId, {
      req(rv$instanceId)
      addClass(class = "load", selector = ".fa-sync-alt")
      removeClass(selector = ".activity-row", class = "activity-row-selected")
      addClass(id = rv$instanceId, class = "activity-row-selected")
      rv$pgcrMember <- list() # reset PGCR selections
      rv$pgcrMemberStats <- list() 
      rv$pgcrMember_showing <- list() 
      withProgress({
         if (! is.na(rv$instanceId)) {
            pgcr = get_pgcr(activityId = rv$instanceId)
         }
         if (pgcr$ErrorCode == 1) {
            rv$Message = pgcr$Message
            rv$pgcr = pgcr$Response
         } else {
            rv$Message = pgcr$Message
         }
      }, message = "Getting PGCR")
   })
   
   
   # Process PGCR
   
   pgcr = reactive({
      req(rv$instanceId, rv$pgcr)
      if (as.character(rv$instanceId) %in% names(rv$pgcrCache)) {
         removeClass(class = "load", selector = ".fa-sync-alt")
         return(rv$pgcrCache[[as.character(rv$instanceId)]])
      } else {
         players = rv$pgcr$entries$player$destinyUserInfo
         
         # archive pgcrs
         
         if (! rv$instanceId %in% rv$savedPGCRs) { # & "4611686018475738587" %in% players$membershipId) {
            saveRDS(object = rv$pgcr, file = file.path("PGCRs", paste0(rv$instanceId, ".RData")))
            saveRDS(object = c(rv$instanceId, savedPGCRs), file = "savedPGCRs.RData")
            rv$savedPGCRs = readRDS(file = "savedPGCRs.RData")
            message("saved PGCR ", rv$instanceId)
         }
         
         if (input$getGlory) {
            withProgress({
               glory = 1:nrow(players) %>%
                  map(function(x) {
                     progression = get_profile(players$membershipType[x], players$membershipId[x], components$CharacterProgressions)
                     incProgress(amount = 1/(nrow(players) + 1), message = paste0("Getting Glory (", x, "/", nrow(players), ")"))
                     glory = get_glory(progression$Response)
                     if (is.null(glory)) "" else glory
                  })
            }, message = "Processing PGCR")
         } else {
            glory = rep("-", nrow(players))
         }
         
         topWeapons = 1:nrow(players) %>%
            map_chr(function(x) {
               if (is.null(rv$pgcr$entries$extended$weapons[[x]])) {
                  topWeaponIcon = ""
               } else {
                  weapons = rv$pgcr$entries$extended$weapons[[x]]
                  topWeapon = which.max(weapons$values$uniqueWeaponKills$basic$value)
                  topWeapon = DestinyInventoryItemDefinition[[as.character(weapons$referenceId[topWeapon])]]
                  if (! is.null(topWeapon$displayProperties$icon)) topWeapon$displayProperties$icon else ""
               }
            })
         
         df_pgcr = process_pgcr(rv$pgcr) %>%
            mutate(glory = glory, topWeaponIcon = topWeapons)
         rv$pgcrCache[[as.character(rv$instanceId)]] = df_pgcr
         removeClass(class = "load", selector = ".fa-sync-alt")
         return(df_pgcr)
      }
   })
   
   
   # Render PGCR UI
   
   output$ui_pgcr <- renderUI({
      req(pgcr())
      df = pgcr()
      referenceId = as.character(rv$pgcr$activityDetails$referenceId)
      activityInfo = DestinyActivityDefinition[[referenceId]]
      
      teams = df$team %>%
         unique %>%
         map(function(Team) {
            m = df %>%
               filter(team == Team) %>%
               arrange(-as.numeric(kills))
            
            avgGlory = if (input$getGlory) {
               m$glory %>% as.numeric %>% mean(na.rm = TRUE) %>% format(digits = 0, big.mark = ",")
            } else {
               "-"
            }
            
            results = div(
               div(
                  div(toupper(m$result[1]), style = "font-size: 2.25em"),
                  style = "width: 150px; text-align: center; margin-right: 15px"
               ),
               div(
                  div(toupper(m$team[1]), style = "font-size: 0.75em"),
                  div(m$teamScore[1], style = "font-size: 1.5em"),
                  style = "width: 100px; text-align: center; margin-right: 15px"
               ),
               div(
                  div("AVERAGE GLORY", style = "font-size: 0.75em"),
                  div(avgGlory, style = "font-size: 1.5em"),
                  style = "width: 125px; text-align: center"
               ),
               style = "display: flex; align-items: flex-end; font-weight: 700; margin-bottom: 15px"
            )
            
            headers = div(
               div("PLAYER", style = "width: 15%; margin-right: 5px"),
               div(style = "width: 40px; margin-right: 10px"),
               div("GLORY", style = "width: 10%; margin-right: 5px"),
               div("KILLS", style = "width: 10%; margin-right: 5px"),
               div("DEATHS", style = "width: 10%; margin-right: 5px"),
               div("K/D", style = "width: 10%; margin-right: 5px"),
               div("SPREAD", style = "width: 10%; margin-right: 5px"),
               div("ASSISTS", style = "width: 10%; margin-right: 5px"),
               div("MOST USED WEAPON"),
               class = "activity-headers",
               style = "display: flex; padding-left: 10px"
            )
            
            rows = 1:nrow(m) %>%
               map(function(player) {
                  rowId = paste0("membershipId", m$membershipId[player])
                  onclick(id = rowId, expr = {rv$pgcrMember = list(displayName = m$displayName[player], membershipType = m$membershipType[player], membershipId = m$membershipId[player])})
                  rowClass = if (m$completed[player] == "No") "activity-row incomplete" else "activity-row"
                  killSpread = as.numeric(m$kills[player]) - as.numeric(m$deaths[player])
                  killSpread = if (killSpread > 0) paste0("+", killSpread) else as.character(killSpread)
                  div(
                     id = rowId,
                     div(class = "image", style = paste0("background-image: url(https://www.bungie.net", m$icon[player], "); width: 40px; height: 40px; margin-right: 10px")),
                     div(
                        div(m$displayName[player]),
                        div(
                           div(m$light[player], style = "width: 30px"),
                           div(m$class[player], style = "width: 30px"),
                           style = "display: flex; font-size: 0.75em; color: gray"
                        ),
                        style = "width: 15%; margin-right: 5px"
                     ),
                     div(format(m$glory[player], big.mark = ","), style = "width: 10%; margin-right: 5px; font-size: 1.5em"),
                     div(m$kills[player], style = "width: 10%; margin-right: 5px; font-size: 1.5em"),
                     div(m$deaths[player], style = "width: 10%; margin-right: 5px; font-size: 1.5em"),
                     div(m$killsDeathsRatio[player], style = "width: 10%; margin-right: 5px; font-size: 1.5em"),
                     div(killSpread, style = "width: 10%; margin-right: 5px; font-size: 1.5em"),
                     div(m$assists[player], style = "width: 10%; margin-right: 5px; font-size: 1.5em"),
                     div(class = "image", style = paste0("background-image: url(https://www.bungie.net", m$topWeaponIcon[player], "); width: 40px; height: 40px")),
                     class = rowClass,
                     style = "display: flex; flex-wrap: wrap; align-items: center; padding-left: 10px"
                  )
               })
            
            style = if (m$result[1] == "Victory" | m$result[1] == 1) {
               "margin-bottom: 20px; border-left: 4px solid var(--win); background-color: var(--boxColor); padding: 10px 0 10px 0; opacity: 0.91"
            } else {
               "margin-bottom: 20px; border-left: 4px solid var(--loss); background-color: var(--boxColor); padding: 10px 0 10px 0; opacity: 0.91"
            }
            
            div(
               results,
               headers,
               rows,
               style = style
            )
         })
      
      pgcrUI = div(
         teams,
         style = ""#paste0("background-image: url(https://www.bungie.net", activityInfo$pgcrImage, "); padding: 25px 0 25px 0")
      )
      
      return(pgcrUI)
      
   })
   
   
   # Select a row in the PGCR
   
   observeEvent(rv$pgcrMember, {
      req(rv$pgcrMember$membershipType)
      rv$pgcrMemberStats = list()
      
      if (! rv$pgcrMember$membershipId %in% names(rv$pgcrMember_showing)) {
         
         rv$pgcrMember_showing[[rv$pgcrMember$membershipId]] = rv$pgcrMember$displayName
         withProgress({
            accountStats = get_accountStats(membershipType = rv$pgcrMember$membershipType, membershipId = rv$pgcrMember$membershipId, groups = "General")$Response
            incProgress(amount = 1, message = "Getting stats")
            accountStats = accountStats$mergedAllCharacters$results$allPvP$allTime
            rv$pgcrMemberStats = list(
               "ACTIVITIES ENTERED" = accountStats$activitiesEntered$basic$displayValue %>% as.numeric %>% format(big.mark = ","),
               "ACTIVITIES WON" = accountStats$activitiesWon$basic$displayValue %>% as.numeric %>% format(big.mark = ","),
               "WIN/LOSS RATIO" = accountStats$winLossRatio$basic$displayValue,
               "K/D" = accountStats$killsDeathsRatio$basic$displayValue,
               "KILLS" = accountStats$kills$basic$displayValue %>% as.numeric %>% format(big.mark = ","),
               "DEATHS" = accountStats$deaths$basic$displayValue %>% as.numeric %>% format(big.mark = ","),
               "ASSISTS" = accountStats$assists$basic$displayValue %>% as.numeric %>% format(big.mark = ","),
               "KILLS/GAME" = accountStats$kills$pga$displayValue,
               "DEATHS/GAME" = accountStats$deaths$pga$displayValue,
               "ASSISTS/GAME" = accountStats$assists$pga$displayValue,
               "MOST KILLS IN A GAME" = accountStats$bestSingleGameKills$basic$displayValue,
               "LONGEST SPREE" = accountStats$longestKillSpree$basic$displayValue,
               "COMBAT RATING" = accountStats$combatRating$basic$displayValue
            )
         }, message = "Getting stats")
         
      }
      
   })
   
   
   # Generate stats UI for selected player
   
   observeEvent(rv$pgcrMemberStats, {
      if (length(rv$pgcrMemberStats) > 0) {
         
         statsId = paste0("pgcrMemberStats", rv$pgcrMember$membershipId)
         
         stats = names(rv$pgcrMemberStats) %>%
            map(function(accountStat) {
               div(
                  div(accountStat, style = "color: gray; font-size: 0.8em"),
                  div(rv$pgcrMemberStats[[accountStat]], style = "font-weight: bold"),
                  class = "card",
                  style = "padding: 5px; margin: 10px 10px 10px 0; text-align: center; font-size: 0.9em"
               )
            })
         
         onclick(id = paste0(statsId, "close"), expr = {rv$remove = statsId})
         
         insertUI(
            selector = paste0("#membershipId", rv$pgcrMember$membershipId),
            where = "afterEnd",
            ui = div(
               id = statsId,
               stats,
               div(id = paste0(statsId, "close"), icon("times"), style = "font-size: 1.5em; cursor: pointer; color: gray"),
               class = "overflow",
               style = "display: flex; align-items: center; padding-left: 10px"
            )
         )
         
      }
   })
   
   
   # Remove stats when clicking the close element
   
   observeEvent(rv$remove, {
      removeUI(selector = paste0("#", rv$remove), multiple = TRUE) # remove stats when close button is clicked
      rv$pgcrMember_showing[[gsub(pattern = "pgcrMemberStats", replacement = "", x = rv$remove)]] = NULL # remove selected member from stats showing list
   })
   
   
   
   
} # end server


shinyApp(ui = ui, server = server)