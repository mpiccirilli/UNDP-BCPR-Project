require(GDELTtools)
gdeltJune2009 <- GetGDELT(start.date="2012-06-01", end.date="2012-06-30", 
                          filter=list(ActionGeo_CountryCode="GG"))
#examined 6 months, describe actors, the average Goldstein Scale
#Victoria: June 2012- November 2012
sapply(gdeltJune2012, mean, na.rm=TRUE)
sapply(gdeltJune2012, max, na.rm=TRUE)
#mean June 2012 Goldstein Scale: 1.016242
#highest June 2012 Goldstein Scale: 10
#mean average June 2012 Tone: 5.843358
#highest average June 2012 Tone: 24.137931
gdeltJuly2012 <- GetGDELT(start.date="2012-07-01", end.date="2012-07-31", 
                          filter=list(ActionGeo_CountryCode="GG"))
sapply(gdeltJuly2012, mean, na.rm=TRUE)
sapply(gdeltJuly2012, max, na.rm=TRUE)
#mean July 2012 Goldstein Scale: 1.762425
#highest July 2012 Goldstein Scale: 10
#mean average July 2012 Tone: 6.205942
#highest average July 2012 Tone: 22.27
gdeltAug2012 <- GetGDELT(start.date="2012-08-01", end.date="2012-08-31", 
                          filter=list(ActionGeo_CountryCode="GG"))
sapply(gdeltAug2012, mean, na.rm=TRUE)
sapply(gdeltAug2012, max, na.rm=TRUE)
#mean August 2012 Goldstein Scale