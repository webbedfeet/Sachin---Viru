#!/usr/bin/R
rm(list=ls())
library(ggplot2)
library(chron)
source('srt.R')
source('vs.R')
save.image('srt.vs.rda')

plot1 <- function(d,title){
    p <- ggplot(d, aes(x=Start.Date, y=Runs))+
        geom_point()+ stat_smooth(method='loess', se=F, span=0.1)+
        scale_x_date(limits=range(srt$Start.Date))+ xlab('Date')+
        opts(title=title)
    print(p)
    return(p)
}

plot.srt <- plot1(srt,'Tendulkar')
plot.vs <- plot1(vs,'Sehwag')

vs.innings <- nrow(vs)
srt.innings <- nrow(srt)

bl <- rbind(srt[1:vs.innings,],vs)
bl$player <- rep(c('SRT','VS'),c(vs.innings,vs.innings))
ggplot(bl, aes(x=1:vs.innings, y=Runs, color=player))+
    geom_point()+stat_smooth(se=F)

bl2 <- rbind(srt,vs); 
bl2$player <- rep(c('SRT','VS'),c(srt.innings,vs.innings))
ggplot(bl2, aes(x=Start.Date, y=Runs, color=player))+
    geom_point()+ scale_x_date()+ stat_smooth(se=F, span=0.4)+
    xlab('Date')

ggplot(bl2, aes(x=yrs2, y=Runs,
       color=player))+ geom_boxplot()+
       opts(axis.text.x=theme_text(angle=45,hjust=1)) 


