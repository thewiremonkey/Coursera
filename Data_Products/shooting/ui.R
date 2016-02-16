library(leaflet)
bootstrapPage(
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
