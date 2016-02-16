require(rCharts)
library(rCharts)
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)



d<-read.csv("d.csv",na.strings = NA, stringsAsFactors = T)
# d$Date<-as.Date(d$Date)
d<-arrange(.data = d, Date)
d$Auto<-grepl("auto", d$Weapon.details, ignore.case = TRUE)
d$Venue<-trimws(tolower(d$Venue))
d
function(input, output, session){
# subset<-reactive({
#         
#         vars<-select(d,  which(names(d)==input$harm), 1:6, 10:23)
#         vars
#         
# })


filter<-reactive({
     data<-d
     as.data.frame(data)
     data$Type<-as.factor(data$Type)
     data$Value<-data[,input$harm]
     
             data[if(input$check==TRUE){d$PriorMI==input$mh & d$venue==input$venue}else {!is.null(d$PriorMI) & !is.null(d$Venue)},c("Value", "latitude","longitude","Type","Weapons.obtained.legally", "PriorMI", "Date", "Year", "Venue", "Summary", "Case")]
})


output$table<-renderTable({
        tabledata<-filter()

        tabledata<-select(.data=filter(), Case, Date, Summary, "Casualties"=Value)
        tabledata
        
},include.rownames=FALSE
)

output$plot<-renderPlot({

        
        ggplot(filter(), aes(x=Year, y=Value,shape=Weapons.obtained.legally, color=Venue )) +
              geom_point(size=6, alpha=.7)
})

output$summary<-renderPrint({
        summ<-filter()["Summary"]
        print(summ)
}
        
)

output$map<-renderLeaflet({
        # data<-filter()
        leaflet()%>%

                addProviderTiles("Stamen.TonerLite") %>%
                setView(lng = -98.35, lat= 39.50, zoom=4)%>%
                # fitBounds(-124.848974, 24.396308,-66.885444 ,49.384358)%>%
                addLegend(title = "Weapons Obtained Legally",position = "topright", colors=c("Olive", "Steelblue", "Purple"),  labels=c("Yes", "No", "Unknown"))
})      


observe({
        leafletProxy("map", data=filter())%>%
                clearMarkers()%>%
                addCircleMarkers(
                        ~longitude, 
                        ~latitude,
                        radius=~Value, 
                        stroke=FALSE,
                       color=~ifelse(Weapons.obtained.legally=="Yes", "Olive",ifelse(Weapons.obtained.legally=="No", "Steelblue", "Purple")),
                        popup=~paste("<b>",Case,"</b><br>",Date, "<br>", Summary, "<br>", input$harm, ": ",  Value)
                                )
                
        
})














                }
       
