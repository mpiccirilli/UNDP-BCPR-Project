# Load required packages
require(ggplot2)
require(plyr)
require(e1071)
require(caret)
library(igraph)
require(expm)
require(car)


##### Load in necessary files #####

# Set current working directory
setwd("/Users/michaelpiccirilli/Desktop")


#Cleaned Dataset:
all.s <- read.csv(file="GDELT-Georgian-Events-2007-2013.csv", header=T, sep=",")

#Eventcodes/GoldsteinScale codes
event.codes <- read.table(file="CAMEO.eventcodes.txt", header=T,sep="\t")
event.codes <- event.codes[-c(1,12,40,69,77,86,92,99,125,131,159,172,199,222,249,255,269,282,296,304),]
goldstein.scale <- read.table(file="CAMEO.goldsteinscale.txt", header=T,sep="\t")
goldstein.scale <- goldstein.scale[-c(1,12,40,69,77,86,92,99,125,131,159,172,199,222,249,255,269,282,296,304),]
event.scale <- merge(event.codes, goldstein.scale, by="CAMEOEVENTCODE")
event.scale <- event.scale[order(-event.scale$GOLDSTEINSCALE),]
rownames(event.scale) <- NULL



##### DO NOT LOAD/RUN -- UNNECESSARY CODE #####

# This set of code is to load and clean the original dataset. 
# the csv file above has already been cleaned

all <- read.csv(file="allevents.csv", header=T,sep=",")

# Clean the dataset
all.s <- subset(all, Actor1CountryCode!="BAG")
all.s <- subset(all.s, Actor1CountryCode!="AFR")
all.s <- subset(all.s, Actor1CountryCode!="ASA")
all.s <- subset(all.s, Actor1CountryCode!="BLK")
all.s <- subset(all.s, Actor1CountryCode!="CRB")
all.s <- subset(all.s, Actor1CountryCode!="CFR")
all.s <- subset(all.s, Actor1CountryCode!="CAS")
all.s <- subset(all.s, Actor1CountryCode!="CEU")
all.s <- subset(all.s, Actor1CountryCode!="EAF")
all.s <- subset(all.s, Actor1CountryCode!="EEU")
all.s <- subset(all.s, Actor1CountryCode!="LAM")
all.s <- subset(all.s, Actor1CountryCode!="MEA")
all.s <- subset(all.s, Actor1CountryCode!="MDT")
all.s <- subset(all.s, Actor1CountryCode!="NAF")
all.s <- subset(all.s, Actor1CountryCode!="NMR")
all.s <- subset(all.s, Actor1CountryCode!="PGS")
all.s <- subset(all.s, Actor1CountryCode!="SAM")
all.s <- subset(all.s, Actor1CountryCode!="SAS")
all.s <- subset(all.s, Actor1CountryCode!="SEA")
all.s <- subset(all.s, Actor1CountryCode!="SAF")
all.s <- subset(all.s, Actor1CountryCode!="WAF")
all.s <- subset(all.s, Actor1CountryCode!="WST")
all.s <- subset(all.s, Actor2CountryCode!="AFR")
all.s <- subset(all.s, Actor2CountryCode!="ASA")
all.s <- subset(all.s, Actor2CountryCode!="BLK")
all.s <- subset(all.s, Actor2CountryCode!="CRB")
all.s <- subset(all.s, Actor2CountryCode!="CFR")
all.s <- subset(all.s, Actor2CountryCode!="CAS")
all.s <- subset(all.s, Actor2CountryCode!="CEU")
all.s <- subset(all.s, Actor2CountryCode!="EAF")
all.s <- subset(all.s, Actor2CountryCode!="EEU")
all.s <- subset(all.s, Actor2CountryCode!="LAM")
all.s <- subset(all.s, Actor2CountryCode!="MEA")
all.s <- subset(all.s, Actor2CountryCode!="MDT")
all.s <- subset(all.s, Actor2CountryCode!="NAF")
all.s <- subset(all.s, Actor2CountryCode!="NMR")
all.s <- subset(all.s, Actor2CountryCode!="PGS")
all.s <- subset(all.s, Actor2CountryCode!="SAM")
all.s <- subset(all.s, Actor2CountryCode!="SAS")
all.s <- subset(all.s, Actor2CountryCode!="SEA")
all.s <- subset(all.s, Actor2CountryCode!="SAF")
all.s <- subset(all.s, Actor2CountryCode!="WAF")
all.s <- subset(all.s, Actor2CountryCode!="WST")
all.s <- subset(all.s, Actor1CountryCode!="NA")
all.s <- subset(all.s, Actor2CountryCode!="NA")
all.s <- subset(all.s, Actor1Geo_Type!="2")
all.s <- subset(all.s, Actor1Geo_Type!="3")
all.s <- subset(all.s, Actor2Geo_Type!="2")
all.s <- subset(all.s, Actor2Geo_Type!="3")
all.s <- subset(all.s, Actor1Name!="ATLANTA")
all.s <- subset(all.s, Actor2Name!="ATLANTA")
all.s <- subset(all.s, Actor1Code!="USACOP")
all.s <- subset(all.s, Actor2Code!="USACOP")
all.s <- subset(all.s, IsRootEvent==1)
all.s$ActorPair <- do.call(paste, c(all.s[c("Actor1CountryCode", "Actor2CountryCode")], sep = "-"))


# Clean the Actor/Action Geo City/Country information
all.s$ActionGeo_FullName <- gsub("Georgia \\(general\\)\\,[[:blank:]]","",all.s$ActionGeo_FullName, ignore.case=T)
all.s$ActionGeo_FullName <- gsub("[[:alpha:]]+[[:punct:]]{3}[[:blank:]]","",all.s$ActionGeo_FullName, ignore.case=T)
all.s$Actor1Geo_FullName <- gsub("\\(general\\)\\,[[:blank:]][[:alpha:]]+","",all.s$Actor1Geo_FullName, ignore.case=T)
all.s$Actor2Geo_FullName <- gsub("\\(general\\)\\,[[:blank:]][[:alpha:]]+","",all.s$Actor2Geo_FullName, ignore.case=T)

#separate the month/day information
all.s$month <- substr(all.s$SQLDATE, 5,6)
all.s$day <- substr(all.s$SQLDATE, 7,8)
all.s$date <- do.call(paste, c(all.s[c("Year","month", "day")], sep = "-"))
all.s$YearMonth <- do.call(paste, c(all.s[c("Year","month")], sep = "-"))

###### END OF UNNECCSARY CODE ########



#### Test Set ####

# Separate the data into training and test set, based on time.
# The test set will be the months leading up to the recent presidential election
test.set <- subset(all.s, MonthYear=="201310" | MonthYear=="201309" | 
                     MonthYear=="201308" | MonthYear=="201307" | 
                     MonthYear=="201306" | MonthYear=="201305")


# Look for unique pairs in the Test set and count their occurances
# filter out low-frequency actor pairs
test.unique.pairs <- count(test.set,c("Actor1CountryCode","Actor2CountryCode"))
colnames(test.unique.pairs)[3] <- "Frequency"
test.unique.pairs <- test.unique.pairs[order(-test.unique.pairs$Frequency),]
test.unique.pairs <- subset(test.unique.pairs, Frequency>20)
test.unique.pairs$ActorPair <- do.call(paste, c(test.unique.pairs[c("Actor1CountryCode", "Actor2CountryCode")], sep = "-"))

# Merge the unique pairs DF with the test.set to show only 
# those actor-pairs w/ highest frequency
test.set <- merge(test.unique.pairs,test.set, by="ActorPair")
colnames(test.set)
test.set <- test.set[,-c(12,22)] #removes extra columns from merge
colnames(test.set)[2:3] <- c("Actor1CountryCode","Actor2CountryCode")




##### Training Set #####

# The data will be based on 6 months prior to other electorial periods
# such as the parlimentary and presidential elections in 2008
# as well as the parlimentary elections in the 2012
train.set <- subset(all.s, MonthYear=="200712" | MonthYear=="200801" |
                      MonthYear=="200802" | MonthYear=="200803" |
                      MonthYear=="200804" | MonthYear=="200805" |
                      
                      MonthYear=="200807" | MonthYear=="200808" |
                      MonthYear=="200809" | MonthYear=="200810" |
                      MonthYear=="200811" | MonthYear=="200812" |
                      
                      MonthYear=="201204" | MonthYear=="201205" |
                      MonthYear=="201206" | MonthYear=="201207" |
                      MonthYear=="201208" | MonthYear=="201209")



# Find frequent unique pairs in the training dataset
train.unique.pairs <- count(train.set,c("Actor1CountryCode","Actor2CountryCode"))
colnames(train.unique.pairs)[3] <- "Frequency"
train.unique.pairs <- train.unique.pairs[order(-train.unique.pairs$Frequency),]
train.unique.pairs <- subset(train.unique.pairs, Frequency>250)
train.unique.pairs$ActorPair <- do.call(paste, c(train.unique.pairs[c("Actor1CountryCode", "Actor2CountryCode")], sep = "-"))

# Merge the train.unique.pairs DF with the train.set to show only those
# selected actor-pairs with highest frequency
train.set <- merge(train.unique.pairs,train.set, by="ActorPair")
colnames(train.set)
train.set <- train.set[,-c(12,22)]  #removes extra columns from merge
colnames(train.set)[2:3] <- c("Actor1CountryCode","Actor2CountryCode")



#### Hidden Markov Model ####
# Currently this is using random probabilities for "dummy" events
# We need to find actual probablities for both transmission matrix & emission matrix


# Transition matrix:
# This matrix contains the probabilities of switching states.
# States are either Positive or Negative, which is based the Goldstein Score of each event
# The actual probabilities would be based on how often the actors either stay in the
# their current state, or switch to the other. 
# Example sequence: Positive, Positive, Positive, Negative, Positive, Negative, Positive, etc..

states <- c("Positive", "Negative") 
Positive <- c(0.7, 0.3) 
Negative <- c(0.1, 0.9) 
thetransitionmatrix <- matrix(c(Positive, Negative), 2, 2, byrow = TRUE) 
rownames(thetransitionmatrix) <- colnames(thetransitionmatrix) <- states
thetransitionmatrix 


# Emission matrix:
# this matrix will hold the probabilities of each event withing each state

events <- c("A", "C", "G", "T")   # Event names
pos.state.prob    <- c(0.39, 0.1, 0.1, 0.41) #probabilities of events in positive state
nev.state.prob    <- c(0.1, 0.41, 0.39, 0.1) #probabilities of events in negative state
theemissionmatrix <- matrix(c(pos.state.prob, nev.state.prob), 2, 4, byrow = TRUE) 
rownames(theemissionmatrix) <- states
colnames(theemissionmatrix) <- events
theemissionmatrix

# HMM Function to generate sequence 
# inputs: transition matrix, emission matrix, initial probabilities, length of the sequence
generatehmmseq <- function(transitionmatrix, emissionmatrix, initialprobs, seqlength)
{
  events     <- c("A", "C", "G", "T")   # define the events
  states          <- c("Positive", "Negative") # define the states
  mysequence      <- character() # vector to sotre the new sequence
  mystates        <- character() # vector to storing the state for each position in the new sequence
  
  # Choose the state for the first position in the sequence:
  firststate      <- sample(states, 1, rep=TRUE, prob=initialprobs)
  
  # Probabilities of the current event, given that we are in the state "firststate":
  probabilities   <- emissionmatrix[firststate,]
  
  # Choose the event for the first position in the sequence:
  firstevent <- sample(events, 1, rep=TRUE, prob=probabilities)
  
  mysequence[1]   <- firstevent # Store the event for the first position of the sequence
  mystates[1]     <- firststate # Store the state that the first position in the sequence
  
  for (i in 2:seqlength)
  {
    # Get the state that the previous event in the sequence
    prevstate    <- mystates[i-1] 
    
    # Get the probabilities of the current state, given that the previous event was generated by state "prevstate"
    stateprobs   <- transitionmatrix[prevstate,]
    
    # Choose the state for the ith position in the sequence:
    state        <- sample(states, 1, rep=TRUE, prob=stateprobs)
    
    # Get the probabilities of the current event, given that we are in the state "state":
    probabilities <- emissionmatrix[state,]
    
    # Choose the event for the ith position in the sequence:
    event   <- sample(events, 1, rep=TRUE, prob=probabilities)
    
    mysequence[i] <- event # Store the event for the current position of the sequence
    mystates[i]  <- state # Store the state that the current position in the sequence
  }
  
  for (i in 1:length(mysequence))
  {
    event   <- mysequence[i]
    state        <- mystates[i]
    print(paste("Position", i, ", State:", state, ", Event = ", event))
  }
}

theinitialprobs <- c(0.5, 0.5)

generatehmmseq(thetransitionmatrix, theemissionmatrix, theinitialprobs, 20)




#### PLOTS/GRAPHS OF THE DATA FROM PRESENTATION ####

# Plot the number of articles of the data over time
ggplot(all.s, aes(x=YearMonth)) + geom_bar() + theme(axis.text.x = element_text(colour = 'black', angle = 60, size = 9, hjust = 1, vjust = 1),axis.title.x=element_text())


# Plot an adjacency matrix using made up, but relevant connections
connections <- matrix(c(1,0,1,4,3,0,2,1,0,1, #USA
                        1,1,2,1,2,1,1,0,1,1, #RUS
                        1,1,0,1,0,0,1,0,0,0, #SWE
                        1,1,1,1,1,2,0,1,1,1, #GEO
                        1,1,0,0,0,0,1,0,0,0, #TUR
                        1,1,0,1,1,0,0,0,0,1, #EUR
                        0,0,1,0,0,0,0,0,0,0, #POL
                        1,1,0,0,1,1,0,0,1,0, #AZE
                        1,1,0,1,0,1,0,1,0,0, #ARM
                        2,1,0,0,1,0,0,0,0,1  #AFG
),10,10)
rownames(connections) <- colnames(connections) <- c("USA", "RUS", "SWE", "GEO",
                                                    "TUR", "EUR", "POL", "AZE",
                                                    "ARM", "AFG")
g <- graph.adjacency(connections, mode="undirected",weighted=TRUE,diag=TRUE)
plot(g)



# Plot the average goldstein score by month for certain pairs
# to justify the use of Hidden Markov Model -- there are different states in which
# very positive, or very negative, and there will be different probabilities of 
# events occuring in those states

rus.geo <- subset(all.s, ActorPair=="RUS-GEO")
ggplot(rus.geo, aes(x=YearMonth, y=GoldsteinScale, color=ActorPair, group=ActorPair)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 60, size = 9, hjust = 1, vjust = 1),axis.title.x=element_text())

usa.geo <- subset(all.s, ActorPair=="USA-GEO")
ggplot(usa.geo, aes(x=YearMonth, y=GoldsteinScale, color=ActorPair, group=ActorPair)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 60, size = 9, hjust = 1, vjust = 1),axis.title.x=element_text())

rus.aze <- subset(all.s, ActorPair=="RUS-AZE")
ggplot(rus.aze, aes(x=YearMonth, y=GoldsteinScale, color=ActorPair, group=ActorPair)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 60, size = 9, hjust = 1, vjust = 1),axis.title.x=element_text()) # monthly average
ggplot(rus.aze, aes(x=date,y=GoldsteinScale, color=ActorPair, group=ActorPair)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 60, size = 9, hjust = 1, vjust = 1),axis.title.x=element_text()) #each date

geo.tur <- subset(all.s, ActorPair=="GEO-TUR")
ggplot(geo.tur, aes(x=YearMonth, y=GoldsteinScale, color=ActorPair, group=ActorPair)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 60, size = 9, hjust = 1, vjust = 1),axis.title.x=element_text())


# Standard Markov Chain Simulation
set.seed(100)
markov <- matrix(rnorm(16)^2,ncol=4)
markov <- markov/rowSums(markov)
matplot( t(sapply(1:20, function(x) matrix(c(1,0,0,1), ncol=4) %*% (markov %^% x) ) ),ylab = "Probabilities", xlab="Trials", main="Markov Simulation")


# Show the distribution of the pairs of actors
all.actor.count <- count(all.s, c("Actor1CountryCode","Actor2CountryCode"))
all.actor.count$Frequency.Percent <- (all.actor.count$freq/sum(all.actor.count$freq))*100
colnames(all.actor.count) 
colnames(all.actor.count)[3] <- "Frequency"
all.actor.count <- all.actor.count[order(-all.actor.count$Frequency),] #order descending
sum(all.actor.count$Frequency.Percent) #check to make sure all = 100%

# First plot:  Shows entire distribution of actor pairs
ggplot(all.actor.count, aes(x=Frequency.Percent)) + geom_density() + labs(title = "Frequency of all Actor Pairs")
# Second Plot:  Focuses on top 10 most frequently occuring actor pairs
ggplot(all.actor.count[all.actor.count$Frequency>300,], aes(x=Frequency.Percent)) + geom_density() + labs(title = "Frequency > 300")



# Scatterplot Matrices ...these will 10-45 seconds take some time to populate
scatterplotMatrix(~GoldsteinScale+EventRootCode+AvgTone, data=all.s)

# Event Magnitude
all.s$EventMagnitude <- (all.s$GoldsteinScale*all.s$AvgTone)
scatterplotMatrix(~EventMagnitude+EventRootCode, data=all.s)
