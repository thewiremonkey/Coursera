require(rCharts)
require(dplyr)
data<-read.csv("../d.csv", stringsAsFactors = F)
data<-as.data.frame(data)
data$Year<-as.factor(data$Year)
data$Venue<-trimws(data$Venue)
data$Venue<-as.factor(data$Venue)

data<-arrange(.data = data, Year)
# n1<-nPlot(Freq ~ Hair, group = 'Eye', type='multiBarChart', data = subset(haireye, Sex=='Male'))
# n1$save('fig/n1.html', cdn=TRUE)
# cat('<iframe src="fig/n1.html", width=100%, height=600></iframe>')

# n2<-nPlot(Total.victims ~ Year, group = 'Venue', type='multiBarChart', data=data)
# # print(n2)
# n2$chart(tooltipContent = "#!function(key, x, y, e){
#          return 'Venue: ' + key + ': '+ e.point.Case + ' Casualties: ' + y
# }!#")

# n2$save('fig/n2.html', cdn=FALSE)
# cat('<iframe src="fig/n2.html", width=100%, height=600></iframe>')
# n2$show("fig/n2.html", cdn=FALSE)