
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(shiny.semantic)

ships <- readRDS("data/ship.rds")

ui <- navbarPage(
    
    
    tabPanel(
        "placeholder",
        
        div(class = "row",
            h1("Vessel Maximum Distance Calculator"),
            style = "text-align:center;"
        ),
        
        div(class = "outer",
            tags$head(
                includeCSS("styles.css")
            ),
            
            leaflet::leafletOutput("map", width="100%", height="100%"),
            
            absolutePanel(
                id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                width = 200, height = "auto",
                
                semanticPage(
                    title = "",
                    
                    h4("Select Vessel Type:", style = "margin-top:1rem;"),
                    dropdown_input("dropdown_type", choices = unique(ships$ship_type)),
                    
                    h4("Select Vessel Name:"),
                    dropdown_input("dropdown_name", choices = unique(ships$ship_name)),
                    
                    
                    h4("Set View:"),
                    toggle("tog", "Dark", FALSE)
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    observe({
        showNotification(
            "Please select vessel type and name to <br/> find the maximum distance of the trip.",
            duration = 20, type = "message")
    }) 
    
    observeEvent(input$dropdown_type, {
        new_shipnames <- filter(ships, ship_type == input$dropdown_type) %>%
            pull(ship_name) %>%
            unique()
        update_dropdown_input(session, "dropdown_name", choices = new_shipnames, value = "")
    })
    
    observeEvent(input$dropdown_name, {
        
        max_distance <- filter(ships, ship_type == input$dropdown_type, ship_name == input$dropdown_name) %>%
            filter(distance == max(distance, na.rm = TRUE))
        
        content <- 
            paste(
                sep = "<br/>",
                span("Maximum Distance", style="font-size:14px;font-weight:600;"),
                paste("<b>Distance:</b>", prettyNum(max_distance$distance, big.mark=","), "(meters)"),
                paste("<b>Date:</b>", as.Date(max_distance$date)),
                paste("<b>Destination:</b>", max_distance$destination)
            )
        
        output$map <- renderLeaflet({
            map <- leaflet(data = ships) %>%
                addTiles() %>%
                addCircleMarkers(
                    lng = max_distance$lon,
                    lat = max_distance$lat,
                    weight = 4,
                    fill = TRUE,
                    radius = 5,
                    label = "Start",
                    labelOptions = labelOptions(
                        noHide = TRUE,
                        direction = "top",
                        textsize = "12px",
                        offset=c(0,-15)
                    )
                ) %>%
                addCircleMarkers(
                    lng = max_distance$lon1,
                    lat = max_distance$lat1,
                    weight = 4,
                    fill = TRUE,
                    radius = 5,
                    label = "Finish",
                    labelOptions = labelOptions(
                        noHide = TRUE,
                        direction = "top",
                        textsize = "12px",
                        offset=c(0,-15)
                    )
                ) %>%
                addPolylines(
                    lng = c(max_distance$lon, max_distance$lon1),
                    lat = c(max_distance$lat, max_distance$lat1),
                    weight = 3,
                    fillOpacity = 0.5,
                    color = "red") %>%
                addPopups(
                    lng = (max_distance$lon+max_distance$lon1)/2,
                    lat = (max_distance$lat+max_distance$lat1)/2,
                    popup = content,
                    popupOptions(textsize = "14px")
                )
        })
    })
    
    observe({
        if(input$tog) {
            leafletProxy("map", data = ships) %>%
                addProviderTiles(providers$CartoDB.DarkMatter)
        }else {
            leafletProxy("map", data = ships) %>%
                addProviderTiles(providers$OpenStreetMap.Mapnik)
        }
    })
}


shinyApp(
    ui = list(
        tagList(
            tags$head(
                tags$title("Vessel Maximum Distance Calculator")
            ),
            ui 
        )
    ), 
    server = server
)
