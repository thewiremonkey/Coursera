# library(UsingR); data(galton); library(reshape2)
# long<-melt(galton)
# g<-ggplot(long, aes(x=value, fill=variable))
# g<-g+geom_histogram(color="black", binwidth=1)+
#         facet_grid(.~variable)
# library(manipulate)
# myHist<-function(mu){
#         mse<-mean((galton$child-mu)^2)
#         g<-ggplot(galton, aes(x=child))+geom_histogram(fill="salmon", colour="black", binwidth=1)+
#                 geom_vline(xintercept=mu, size=3)+
#                 ggtitle(paste("mu= ", mu, ", MSE= ", round(mse,2), sep = ""))
#         g
# 
# }
# 
# manipulate(myHist(mu), mu=slider(62, 74, step = 0.5))

library(UsingR)
library(ggplot2)
library(manipulate)
library(reshape2)

data("galton")
long <- melt(galton)


y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)

freqData <- as.data.frame(table(x,y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))

NonZeroFreqData <- freqData[freqData$freq > 0,]

myPlot2 <- function(beta){
        mse <- mean( (y - x*beta) ^2 )
        g2 <- ggplot(NonZeroFreqData, aes(x = parent, y = child))
        p2 <- g2  + scale_size(range = c(2, 20), guide = "none" ) +
                geom_point(color="grey50", aes(size = freq+2, show_guide = FALSE)) +
                geom_point(aes(color= freq, size = freq)) +
                scale_colour_gradient(low = "lightblue", high="white") + 
                geom_abline(intercept = 0, slope = beta, size = 3) +
                ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        p2
}
manipulate(myPlot2(beta), beta = slider(0.6,1.2, step = 0.01))
