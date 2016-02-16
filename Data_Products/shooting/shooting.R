library(shiny)
library(leaflet)
library(RColorBrewer)
d<-read.csv("d.csv")
vars<-c("Summary", "latitude", "longitude", "Year", "Date")

ui <- bootstrapPage(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        leafletOutput("map", width = "80%", height = "80%"),
        absolutePanel(top = 10, right = 10,
                      sliderInput("range", "Year", min(d$Year), max(d$Year),
                                  value = range(d$Year), step = 1
                      ),
                        

                      selectInput("colors", "Color Scheme",
                                  rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                      ),
                      
                      selectInput(inputId = "harm", label = "Type of Harm",choices = 
                                        c("Total.victims", "Fatalities", "Injured")
                      ),
                      checkboxInput("legend", "Show legend", TRUE)
        )
)

server <- function(input, output, session) {
        
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
                leaflet(d) %>% addTiles() %>%
                        fitBounds(-123.02203, 25.79649, -71.07283, 48.05082)
        })
        
        # Incremental changes to the map (in this case, replacing the
        # circles when a new color is chosen) should be performed in
        # an observer. Each independent set of things that can change
        # should be managed in its own observer.
        observe({
                pal <- colorpal()
                
                leafletProxy("map", data = filteredData()) %>%
                        clearShapes() %>%
                        addCircleMarkers(radius = d[, input$harm]/2, weight = 1, color = "#777777",  fillColor = pal(d[, input$harm]), fillOpacity = 0.7, popup = ~paste(d$Summary, input$harm, d[,input$harm]))
                        
                        
        })
        observe({
                harm<-filteredData()
                leafletProxy("map", data=harm[,input$harm])%>%
                        clearShapes() %>%
                        addCircleMarkers(radius = d[, input$harm]/2, weight = 1, color = "#777777",  fillColor = pal(d[, input$harm]), fillOpacity = 0.7, popup = ~paste(d$Summary, input$harm, d[,input$harm]))
                
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
                                            pal = pal, values = d[, input$harm], labels=""
                        )
                }
        })
}

shinyApp(ui, server)