require(rCharts)
library(rCharts)
library(shiny)
# library(ggplot2)
library(leaflet)


fluidPage(
        titlePanel("Mass/Spree Shootings in US, 1982-2015"),
        
        sidebarPanel(
        checkboxInput("check", "Use Filter", FALSE),
        
        p("Check to filter by shooting venue and known history of Mental illness"),
        
        selectInput(inputId="harm", 
                    label = "Type of Harm",
                    choices = c(
                            "Total victims" = "Total.victims",
                            "Fatalities",
                            "Injured"
                    )),
        
        selectInput(inputId="mh",
                    label="Prior Mental Health Issues Identified",
                    choices=c(
                            "yes",
                            "no",
                            "unknown"
                    )),

        
        selectInput(inputId="venue",
                    label="Venue",
                    choices=c("School",
                              "Workplace", 
                              "Military", 
                              "Religious",
                              "Other")),
        p("The data for this app was downloaded from: "), a(href="http://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data", "Mother Jones, US Mass Shootings, 1982-2015: Data From Mother Jones'Investigation"),
        br(),br(),
        p("Original article from December 28, 2012, but updated through 2015."),
        p("The map and chart looks at casualties of mass shooting events and factors that pundits and researchers alike claim contribute to the phenomenon of mass shootings in the United States including mental health of the shooter and whether the firearms were obtained legally.  No claim is made regarding the degree to which these factors contributed to outcome."),
        p('Because gun laws vary by state, "weapons obtained legally" refers to the status of the firearm purchase in the state the purchase occurred.')
        
        
        ),
       
        mainPanel(
                 tabsetPanel(
                        tabPanel("Map", 
                                 leafletOutput(outputId = "map"),
                                 p("Click circles for summary details of the event. Zoom in to separate events in high-occurrance areas."),
                                 p("Circle radius indicates number of casualties.")
                                 ),
                        tabPanel("Charts",

                                plotOutput("plot"),

                                 tableOutput("table")
                         
                         ))
          
                        
                        
                               
        
                 
        
        
        )
)
