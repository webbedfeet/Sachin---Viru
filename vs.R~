library(ggplot2)

vs <- read.csv('VS.csv', stringsAsFactors=F)
vs[vs=='DNB'] <- NA
vs[vs=='-'] <- NA
vs$Runs <- gsub('\\*','',vs$Runs)

for(i in c(1:7)) vs[,i] <- as.numeric(vs[,i])
for(i in c(7,8,9,10,11,13)) vs[,i] <- as.factor(vs[,i])
require(chron)
vs$Start.Date <- chron(vs$Start.Date)
vs <- transform(vs, yr = as.numeric(as.character(years(Start.Date))))
vs <- transform(vs, yrs = cut(yr, seq(1985,2010,by=5)),
        yrs2=cut(yr, seq(1988,2010,by=2)))
vs <- transform(vs, 
        centuries = Runs > 99,
        dcenturies = Runs > 199,
        fifties = Runs > 49
        )

m.vs <- ddply(vs, .(Opposition), summarize, 
        centfreq=sum(centuries, na.rm=T),
        fiftyfreq=sum(fifties, na.rm=T),
        innings = sum(!is.na(centuries)))
m.vs <- transform(m.vs,
        centrate=centfreq/innings,
        fiftyrate = fiftyfreq/innings,
        diffrate = (fiftyfreq-centfreq)/innings,
        inningsrate=innings/innings,
        conversionrate = centfreq/fiftyfreq)

p.vs <- ggplot(data=m.vs) + 
    geom_bar(aes(Opposition,inningsrate),color='black',fill='white') +
    geom_bar(aes(Opposition,fiftyrate), fill='black')+
    geom_bar(aes(Opposition,centrate), fill='red')+
    scale_y_continuous('Rate per innings')+
    opts(axis.text.x=theme_text(angle=45, hjust=1))

m1.1.vs <- melt(m.vs[,c(1,5,7)],id='Opposition')
levels(m1.1.vs$variable)=c('Century','Fifty')
p1.1.vs <- ggplot(m1.1.vs, aes(x=Opposition, y = value, fill=variable))+ 
    geom_bar()+
    scale_y_continuous(name='Rate per innings', limits=c(0,1))+
    opts(axis.text.x=theme_text(angle=45, hjust=1))+
    scale_fill_hue(name='')
p1.1.vs

m2.vs <-  ddply(vs,.(yrs), summarize, 
        centfreq=sum(centuries, na.rm=T),
        fiftyfreq=sum(fifties, na.rm=T),
        innings=sum(!is.na(centuries)))
m2.vs <- transform(m2.vs, 
        centrate=centfreq/innings, 
        fiftyrate = fiftyfreq/innings, 
        diffrate = (fiftyfreq-centfreq)/innings,
        inningsrate = innings/innings, 
        conversionrate = centfreq/fiftyfreq)
m2.1.vs <- melt(m2.vs[,c(1,5,7)], id='yrs')
levels(m2.1.vs$variable)=c('Century','Fifty')
p2.1.vs <- ggplot(m2.1.vs, aes(x=yrs, y=value, fill=variable))+
    geom_bar()+
    scale_y_continuous(name='Rate per innings', limits=c(0,1))+
    scale_x_discrete(name='Time period')+
    scale_fill_hue(name='')
p2.1.vs

m3.vs <- ddply(vs,.(yrs,Opposition),summarize, 
        centfreq=sum(centuries, na.rm=T),
        fiftyfreq=sum(fifties, na.rm=T),
        innings=sum(!is.na(centuries)))

m3.vs <- transform(m3.vs, 
        centrate=centfreq/innings, 
        fiftyrate = fiftyfreq/innings, 
        diffrate = (fiftyfreq-centfreq)/innings,
        inningsrate = innings/innings, 
        conversionrate = centfreq/fiftyfreq)

id1 <- c('yrs','Opposition')
m3.1.vs <- melt(m3.vs[,c(1,2,6,8)],id=id1)
levels(m3.1.vs$variable) <- c('Century','Fifty')
p3.1.vs <- ggplot(m3.1.vs, aes(x=Opposition, y=value, fill=variable))+
    geom_bar()+
    facet_wrap(~yrs,ncol=2)+
    scale_y_continuous(name='Rate per innings',limits=c(0,1))+
    opts(axis.text.x=theme_text(angle=45, hjust=1))+
    scale_fill_hue(name='')
p3.1.vs
