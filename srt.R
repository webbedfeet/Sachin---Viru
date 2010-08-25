library(lattice)
library(ggplot2)

srt = read.csv('SRT.csv', stringsAsFactor=F)
srt[srt=='DNB']=NA
srt[srt=='-']=NA
srt$Runs = gsub('\\*','',srt$Runs)

for(i in c(1:7)) srt[,i] <- as.numeric(srt[,i])
for(i in c(7,8,9,10,11,13)) srt[,i] <- as.factor(srt[,i])
require(chron)
srt$Start.Date <- chron(srt$Start.Date)
srt <- transform(srt, yr =  as.numeric(as.character(years(Start.Date))))
srt <- transform(srt, yrs = cut(yr, seq(1985,2010,by=5)),
        yrs2=cut(yr, seq(1988,2010,by=2)))
srt <- transform(srt, 
        centuries = Runs > 99, 
        dcenturies = Runs > 199, 
        fifties = Runs > 49
        )

m.srt <-  ddply(srt,.(Opposition), summarize, 
        centfreq=sum(centuries, na.rm=T),
        fiftyfreq=sum(fifties, na.rm=T),
        innings=sum(!is.na(centuries)))
m.srt <- transform(m.srt, 
        centrate=centfreq/innings, 
        fiftyrate = fiftyfreq/innings, 
        diffrate = (fiftyfreq-centfreq)/innings,
        inningsrate = innings/innings, 
        conversionrate = centfreq/fiftyfreq)

p.srt <- ggplot(data=m.srt) + 
    geom_bar(aes(Opposition,inningsrate),color='black',fill='white') +
    geom_bar(aes(Opposition,fiftyrate), fill='black')+
    geom_bar(aes(Opposition,centrate), fill='red')+
    scale_y_continuous('Rate per innings')+
    opts(axis.text.x=theme_text(angle=45, hjust=1))

m1.1.srt <- melt(m.srt[,c(1,5,7)],id='Opposition')
levels(m1.1.srt$variable)=c('Century','Fifty')
p1.1.srt <- ggplot(m1.1.srt, aes(x=Opposition, y = value, fill=variable))+ 
    geom_bar()+
    scale_y_continuous(name='Rate per innings', limits=c(0,1))+
    opts(axis.text.x=theme_text(angle=45, hjust=1))+
    scale_fill_hue(name='')
p1.1.srt

m2.srt <-  ddply(srt,.(yrs), summarize, 
        centfreq=sum(centuries, na.rm=T),
        fiftyfreq=sum(fifties, na.rm=T),
        innings=sum(!is.na(centuries)))
m2.srt <- transform(m2.srt, 
        centrate=centfreq/innings, 
        fiftyrate = fiftyfreq/innings, 
        diffrate = (fiftyfreq-centfreq)/innings,
        inningsrate = innings/innings, 
        conversionrate = centfreq/fiftyfreq)
m2.1.srt <- melt(m2.srt[,c(1,5,7)], id='yrs')
levels(m2.1.srt$variable)=c('Century','Fifty')
p2.1.srt <- ggplot(m2.1.srt, aes(x=yrs, y=value, fill=variable))+
    geom_bar()+
    scale_y_continuous(name='Rate per innings', limits=c(0,1))+
    scale_x_discrete(name='Time period')+
    scale_fill_hue(name='')
p2.1.srt

m3.srt <- ddply(srt,.(yrs,Opposition),summarize, 
        centfreq=sum(centuries, na.rm=T),
        fiftyfreq=sum(fifties, na.rm=T),
        innings=sum(!is.na(centuries)))

m3.srt <- transform(m3.srt, 
        centrate=centfreq/innings, 
        fiftyrate = fiftyfreq/innings, 
        diffrate = (fiftyfreq-centfreq)/innings,
        inningsrate = innings/innings, 
        conversionrate = centfreq/fiftyfreq)

id1 <- c('yrs','Opposition')
m3.1.srt <- melt(m3.srt[,c(1,2,6,8)],id=id1)
levels(m3.1.srt$variable) <- c('Century','Fifty')
p3.1.srt <- ggplot(m3.1.srt, aes(x=Opposition, y=value, fill=variable))+
    geom_bar()+
    facet_wrap(~yrs,ncol=2)+
    scale_y_continuous(name='Rate per innings',limits=c(0,1))+
    opts(axis.text.x=theme_text(angle=45, hjust=1))+
    scale_fill_hue(name='')
p3.1.srt
