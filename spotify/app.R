library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)
library(lubridate)
library(reticulate)
library(jsonlite)

source("/srv/shiny-server/templates/custom.R")

# source_python(file = 'spotifyRequest.py', envir = NULL)

track_data = fromJSON("data/arpyem_top.txt")

usage = readRDS('usage')
usage = c(Sys.time(), usage)
saveRDS(usage, 'usage')

# checkTrackList = function(track_data) {
#     track_data$tracks %>% 
#         map(function(track) {track$info$name})
# } # for manual check of tracks


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# UI ===================================================================================================================================================================
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

ui = fluidPage(
    useShinyjs(),
    includeCSS("custom.css"),
    tabsetPanel(
        tabPanel(
            title = span("ARPYEM", style = "letter-spacing: 10px; color: #efefef;"), 
            value = "tracks",
            uiOutput(outputId = "ui_topTracks") 
        ),
        tabPanel(
            title = a(href = "https://open.spotify.com/artist/2CiRpkhwIUTCnAThfOxnZW", icon(name = "spotify"), target = "_blank"), 
            value = "spotify"
        ),
        tabPanel(
            title = a(href = "https://soundcloud.com/arpyem", icon(name = "soundcloud"), target = "_blank"), 
            value = "sc"
        ),
        tabPanel(
            title = a(href = "https://facebook.com/arpyem", icon(name = "facebook-square"), target = "_blank"),
            value = "fb"
        ),
        tabPanel(
            title = a(href = "https://www.twitch.tv/arpyem", icon(name = "twitch"), target = "_blank"),
            value = "twitch"
        ),
        tabPanel(
            title = a(href = "https://www.youtube.com/channel/UCvlQU6G2IAL2LVLNEbGnDaA", icon(name = "youtube"), target = "_blank"),
            value = "yt"
        ),
        tabPanel(
            title = a(href = "https://arpyem.bandcamp.com/", icon(name = "bandcamp"), target = "_blank"),
            value = "bc"
        ),
        tabPanel(
            title = span("about", style = "letter-spacing: 5px; color: #efefef;"), 
            value = "about",
            div(
                id = 'refreshSpotify',
                icon('refresh'),
                style = 'display: inline-block; margin: 15px; font-size: 2em; cursor: pointer'
            )
        )
    ),
    div(
        div(
            verbatimTextOutput("test")
            
        ),
        style = ""
    ),
    style = "padding: 0;"
)


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# SERVER ===============================================================================================================================================================
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

server = function (input, output, session) {
    
    # Testing ----
    
    output$test = renderPrint({
        req(NULL)
    })
    
    
    
    # Reactive values ----
    
    rv = reactiveValues(
        refresh = FALSE
    )
    
    
    # Top tracks
    
    output$ui_topTracks = renderUI({
        track_data$tracks %>%
            map(function(track) {
                trackArtists = track$info$artists
                artists = 1:nrow(trackArtists) %>%
                    map(function(artist) {
                        div(
                            tags$a(toupper(trackArtists$name[artist]), href = trackArtists$external_urls$spotify[artist], target = "_blank"),
                            class = "aH",
                            style = "text-align: center; margin: 10px 10px 0 10px;"
                        )
                    })
                albumImage = track$info$album$images[1, ]
                div(
                    div(
                        div(
                            div(div(tags$a(track$info$name %>% toupper, href = track$info$external_urls$spotify, target = "_blank"), class = "aH", style = "display: inline-block; font-weight: bold; font-size: 1.1em;")),
                            div(artists, style = "display: flex; justify-content: center; color: #cbcbcb;"),
                            class = "caption"
                        ),
                        style = "background-color: rgba(0,0,0,.33); height: 100%;"
                    ),
                    style = paste0("background-image: url(", albumImage$url, "); min-height: 100%;"),
                    class = "parallax"
                )
            })
    })
    
    
    # Refresh Spotify data
    
    shinyjs::onclick(id = 'refreshSpotify', expr = {rv$refresh = TRUE})
    
    observeEvent(rv$refresh, {
        if (rv$refresh) {
            addCssClass(id = 'refreshSpotify', class = 'rotate')
            source_python('spotifyRequest.py')
            hide(id = 'refreshSpotify', anim = TRUE)
            removeCssClass(id = 'refreshSpotify', class = 'rotate')
            rv$refresh = FALSE
        } 
    })
    
    
    
} # End server

shinyApp(ui, server)