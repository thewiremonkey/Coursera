library(shiny)
library(leaflet)
library(RColorBrewer)
library(quantmod)
d<-read.csv("d.csv")
vars<-c("Summary", "latitude", "longitude", "Year", "Date")

function(input, output, session) {
        
        # Reactive expression for the data subsetted to what the user selected
        filteredData <- reactive({
                d<-d[d$Year >= input$range[1] & d$Year <= input$range[2], ]
                     
        })
        
        # This reactive expression represents the palette function,
        # which changes as the user makes selections in UI.
        colorpal <- reactive({
                colorNumeric(input$colors, d[, input$harm])
        })
        
        output$map <- renderLeaflet({
                # Use leaflet() here, and only include aspects of the map that
                # won't need to change dynamically (at least, not unless the
                # entire map is being torn down and recreated).
                leaflet() %>% addTiles() %>%
                        fitBounds(-123.02203, 25.79649, -71.07283, 48.05082)
        })
        
        # Incremental changes to the map (in this case, replacing the
        # circles when a new color is chosen) should be performed in
        # an observer. Each independent set of things that can change
        # should be managed in its own observer.
        observe({
                startYear<-input$range[1]
                endYear<-input$range[2]
                pal <- colorpal()
                harmtype<-input$harm
                harm<-d[, input$harm]
                
                leafletProxy("map", data = filteredData()) %>%
                        clearShapes() %>%
                        addCircleMarkers(radius = harm/2, weight = 1, color = "#777777",  fillColor = pal(harm), fillOpacity = 0.7, popup = ~paste(d$Summary, harmtype, d[,input$harm]))
                
                
        })
        
        # Use a separate observer to recreate the legend as needed.
        observe({
                proxy <- leafletProxy("map", data = d)
                
                # Remove any existing legend, and only if the legend is
                # enabled, create a new one.
                proxy %>% clearControls()
                if (input$legend) {
                        pal <- colorpal()
                        proxy %>% addLegend(position = "bottomright",
                                            pal = pal, values = harm, labels=""
                        )
                }
        })
}