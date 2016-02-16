

library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(leaflet)
library(shiny)

# setwd("E:/Coursera/Data_Products")
URL<-"https://docs.google.com/spreadsheet/pub?key=0AswaDV9q95oZdG5fVGJTS25GQXhSTDFpZXE0RHhUdkE&output=csv"

d<-read.csv(URL, stringsAsFactors = FALSE)


d$Date<-as.Date(d$Date, "%m/%d/%Y")
d$Weapons.obtained.legally<-trimws(d$Weapons.obtained.legally)


d$venue<-trimws(d$Venue)
d$venue[1]<-"Other"
d$venue<-as.factor(d$venue)
d$Weapons.obtained.legally[3]<-"Yes"
d$Weapons.obtained.legally<-as.factor(d$Weapons.obtained.legally)
d$Prior.signs.of.possible.mental.illness<-as.factor(d$Prior.signs.of.possible.mental.illness)     
names(d)[names(d)=="Prior.signs.of.possible.mental.illness"]<-"PriorMI"
d$PriorMI<-tolower(trimws(d$PriorMI))
d$PriorMI<-gsub("unclear", "unknown", x=d$PriorMI)
d$PriorMI<-as.factor(d$PriorMI)
d$Type<-as.factor(d$Type)
d$Date<-as.Date(d$Date)
d$Date<-strptime(d$Date, "%Y-%m-%d")
d$Year<-format(d$Date, "%Y")

plot(d$Fatalities~d$PriorMI, col=d$venue)

d<-d[!is.na(d$latitude),]
d<-d[!is.na(d$longitude),]
d<-write.csv(d, file = "d.csv")
d<-read.csv("d.csv")
# map('state')
# points(symbols(x=d$longitude, y=d$latitude, circles=sqrt(d$Total.victims/pi), inches=1/10, ann=F, bg="steelblue2", fg=NULL))

pal<-colorFactor(c("red", "yellow", "green"), d$Weapons.obtained.legally)
pal2<-colorFactor(c("blue", "black"), d$Type)

m<-leaflet(data = d) %>%
        # addTiles()%>%
        addProviderTiles("CartoDB.Positron")%>%
        setView(lng=-90, lat=40, zoom=4)%>%
        addCircleMarkers(lng=d$longitude, lat=d$latitude, radius=d$Total.victims,color = ~pal2(Type), stroke = .5, fillColor = ~pal(Weapons.obtained.legally), popup=paste(d$Summary,  paste("Total Harmed: ", d$Total.victims), paste("Killed: ", d$Fatalities), paste("Injured: ", d$Injured), sep="<br/>"))


print(m)

plot(d$Total.victims~d$Year)
