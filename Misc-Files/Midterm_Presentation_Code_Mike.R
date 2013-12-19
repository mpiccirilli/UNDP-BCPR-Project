require(ggplot2)
require(plyr)
require(grid)

setwd("/Users/michaelpiccirilli/Desktop/UNDP-BCPR-Project")
all <- read.csv(file="june11_may13.csv", header=T,sep=",")
actor_ratio<- read.csv(file="Action_Freq_Ratio.csv", header=T,sep=",")
colnames(actor_ratio) <- c("Actor1CountryCode","As_Actor1_Freq","Avg_Goldstein","As_Actor2_Freq","Ratio")

min(all$AvgTone)

#clean out the Georgia,USA data:
all_less_usa <- subset(all, Actor1CountryCode!="USA")

usacleanrall <- subset(all, Actor2Name!="UNITED STATES" & Actor2Geo_CountryCode!="US" & Actor1Geo_CountryCode!="US" | Actor1Name=="UNITED STATES")
USAsub1 <- subset(usacleanrall, Actor1CountryCode=="USA")


#order the actor ratio by Actor 1
actor_ratio <- actor_ratio[order(-actor_ratio$As_Actor1_Freq),]

#top 5 actors:  USA, RUS, EUR, ARM, GEO
headtop5actors <- head(actor_ratio,5)



####analyze top 5 actors and plot bargrphs
#Begin with USA, need to take out entries involving the state Georgia
USAsub <- subset(all, Actor2Name!="UNITED STATES" & Actor2Geo_CountryCode!="US"&Actor1Geo_CountryCode!="US" & Actor1Name=="UNITED STATES" & Actor1CountryCode=="USA")
RUSsub <- subset(all, Actor1CountryCode=="RUS")
EURsub <- subset(all, Actor1CountryCode=="EUR")
ARMsub <- subset(all, Actor1CountryCode=="ARM")
GEOsub <- subset(all, Actor1CountryCode=="GEO")

top5actors <- rbind(USAsub, RUSsub, EURsub, ARMsub, GEOsub)

#bargraphs
ggplot(USAsub, aes(factor(MonthYear))) + geom_bar()
ggplot(RUSsub, aes(factor(MonthYear))) + geom_bar()
ggplot(EURsub, aes(factor(MonthYear))) + geom_bar()
ggplot(ARMsub, aes(factor(MonthYear))) + geom_bar()
ggplot(GEOsub, aes(factor(MonthYear))) + geom_bar()

#graph the combined plot

ggplot(top5actors, aes(factor(MonthYear),fill=Actor1CountryCode)) + geom_bar() + theme(axis.text.x = element_text(colour = 'black', angle = 75, size = 13, hjust = .5, vjust = .5),axis.title.x=element_blank()) + labs(title = "Frequency of Top 5 Countries with Events Occuring in Georgia")
#####

######table USAsu
usa_freqtable <- table(USAsub$MonthYear) 
usa_freqtable <- as.data.frame(usa_freqtable)
colnames(usa_freqtable) <- c("MonthYear","Freq") #change colnames
usa_aggregate <- aggregate(cbind(GoldsteinScale,AvgTone) ~ Actor1CountryCode+MonthYear, USAsub,mean)
usa_merge <- merge(usa_aggregate, usa_freqtable, by="MonthYear")


##### checking out event code frequencies 
eventcode <- table(all$EventCode)
eventcode <- as.data.frame(eventcode)
eventcode <- eventcode[order(-eventcode$Freq),]
sum(eventcode$Freq)

#subset for only armed conflict, 190, 194
armedconflict <- subset(all, EventCode==190 | EventCode==194)
#take out US only conflicts
armedconflict <- subset(armedconflict, Actor2Name!="UNITED STATES" & Actor2Geo_CountryCode!="US"&Actor1Geo_CountryCode!="US")

armed_table <- table(armedconflict$Actor1Geo_CountryCode)
armed_table <- as.data.frame(armed_table)
armed_table <- armed_table[order(-armed_table$Freq),]

armed_country <- table(armedconflict$Actor1CountryCode)
armed_country <- as.data.frame(armed_country)
armed_country <- armed_country[order(-armed_country$Freq),]
armed_country$Percent <- armed_country$Freq/sum(armed_country$Freq)
#top 5 countries involved are USA, RUS, AFG, SYR, ISR
#revert back to armed conflict and filter for just these countries to plot
armedconflictsub <- subset(armedconflict, Actor1CountryCode=="USA" | Actor1CountryCode=="RUS" | Actor1CountryCode=="AFG" | Actor1CountryCode=="SYR" | Actor1CountryCode=="ISR")

qplot(factor(MonthYear),fill=Actor1CountryCode,data=armedconflictsub,main="Top 5 Mentioned Countries Related to Armed Conflict",geom="histogram")
ggplot(armedconflictsub, aes(factor(MonthYear),fill=Actor1CountryCode)) + geom_bar()

##### end of armed conflic #####


#look at goldstein scale and the average tone from top 5 actors
GoldTone <- aggregate(cbind(AvgTone,GoldsteinScale) ~ Actor1CountryCode+MonthYear+Year, top5actors,mean)
qplot(factor(MonthYear),GoldsteinScale,data=GoldTone,main="Average Goldstein Score for Top 5 Actors",geom=c("boxplot"))
ggplot(GEOsub, aes(factor(MonthYear),y=GoldsteinScale)) + geom_boxplot() + coord_cartesian(ylim=c(-2.5,5)) + geom_jitter()


qplot(factor(MonthYear),GoldsteinScale,data=GEOsub,main="Average Goldstein Score for Georgia",geom=c("point","line"))


#Goldstein Score
ggplot(GoldTone, aes(x=factor(MonthYear),y=GoldsteinScale,colour=Actor1CountryCode,group=Actor1CountryCode)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 75, size = 13, hjust = .5, vjust = .5),axis.title.x=element_blank()) + labs(title = "Average Goldstein Score of Top 5 Mentioned Countries")
#Average Tone
ggplot(GoldTone, aes(x=factor(MonthYear),y=AvgTone,colour=Actor1CountryCode,group=Actor1CountryCode)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 75, size = 13, hjust = .5, vjust = .5),axis.title.x=element_blank()) + labs(title = "Average Tone of Top 5 Mentioned Countries")

#Combine the plots
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
Goldplot <- ggplot(GoldTone, aes(x=factor(MonthYear),y=GoldsteinScale,colour=Actor1CountryCode,group=Actor1CountryCode)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 75, size = 13, hjust = .5, vjust = .5),axis.title.x=element_blank()) + labs(title = "Top 5 Actor's Average Goldstein Score")
Toneplot <- ggplot(GoldTone, aes(x=factor(MonthYear),y=AvgTone,colour=Actor1CountryCode,group=Actor1CountryCode)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 75, size = 13, hjust = .5, vjust = .5),axis.title.x=element_blank()) + labs(title = "Top 5 Actor's Average Tone")
pushViewport(viewport(layout = grid.layout(2, 1)))
print(Goldplot, vp = vplayout(1, 1))
print(Toneplot, vp = vplayout(2, 1))

##############


#We could talk about some aspects of the dataset that we're looking at
#and the pitfalls and troubles that we'll encounter. 
#Our initial analysis of the datasets involved in us filtering the data 
#specifically for events that have occured in Georgia.
#Unfortunally, machines aren't perfect so many of the enties that are classified
#for the country Georgia, are actually for the state Georgia. 
#To give a rough estimate, there were 25k entries listed with the USA 
#as Actor 1
#However after taking a closer look at the data, and cleaning it 
table(USAsub$Actor2CountryCode)


table(all$Actor1KnownGroupCode)

