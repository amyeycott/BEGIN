#This is a new data loading script for loading BEGIN data started by Amy in July 2018. We need to decide how much we can correct in the original files and how much to do in script. They're our files but there are some copies drifting around.

#corrections done by hand. In 2007-2010, the first cell title is changed (had contained forbidden symbols), D81-0 changed to D8-10, double space taken out of carex nigra, Brachytecium corrected in 2007-2010, Atrichum undulatim fixed in 2012
#corrections needed: 2012 has brachythecium in A10 and Yellow friend in C4 which are not used. Anemone not anenome, lots of years. Revove columns where there were no ocurrences.

library(tidyverse)
library(lubridate)
library(readxl)


#### enter excel files ####
# Baseline 2007, years with treatment 2008-2014, years with cutting treatment 2015-2017 so far...
veg2007_2010<-read_excel("../Data/Species composition Revne Norway 2007 to 2010 ANALYSEDATA.xls", sheet = "Spp tp r ready")
#the names are in row 1 and th
vegdata_2011<-read_excel("../Data/BEGIN Revne 2011 data.xlsx", sheet = "veg", na = c("NA", "")) #we have to decide if 9 plots are worth it.We've got more if the moss notes happen to be somewhere.  hastag worldsmosthonestpaper?
vegdata_2012<-read_excel("../Data/BEGIN Revne 2012 data.xlsx", sheet = "vegetation", na = c("NA", ""))#last column removed and dots for edge species removed
vegdata_2013<-read_excel("../Data/BEGIN Revne 2013 data.xlsx", sheet = "Revna 2013 veg", na = c("NA", ""))#sum row removed
vegdata_2014<-read_excel("../Data/BEGIN Revne 2014 data.xls", sheet = "Revna 2014 veg", na = c("NA", ""))#sum row and sum column removed
vegdata_2015<-read_excel("../Data/BEGIN Revne 2015 data.xls", sheet = "Revna 2015 veg", na = c("NA", ""))#lose last two columns and get moss ids.
vegdata_2016<-read_excel("../Data/BEGIN Revne 2016 data.xlsx", sheet = "Revna 2016 veg", na = c("NA", ""))#Three summation columns and one summation row removed
vegdata_2017<-read_excel("../Data/BEGIN Revne 2017 data.xlsx", sheet = "Revna 2017 veg", na = c("NA", ""))#Three summation columns and one summation row removed, year fixed

#### get rid of unwanted rows and columns, fix names ####
#steps needed: kill rows witout plot names (do in this step because of the plot-year naming confusion), fill fully blank rows, kill fully blank columns, kill names in this list: "Vascular", "mosses", "litter", "bare", "X__1", "X__2", "VASCSUM","CF TOTVASC", "BRYOSUM", "CF TOTBRYO"

#veg2007_2010 <- veg2007_2010 %>% rename(replace=c("Oxalis pratense"="Oxalis acetosella")) %>% rename(replace=c("Festuca pratense"="Festuca pratensis")) #currently not working

blankrowkiller<-function(x){x<-x[rowSums(is.na(x)) != ncol(x),]}
veg2007_2010<-blankrowkiller(veg2007_2010)
vegdata_2011<-blankrowkiller(vegdata_2011)
vegdata_2012<-blankrowkiller(vegdata_2012)
vegdata_2013<-blankrowkiller(vegdata_2013)
vegdata_2014<-blankrowkiller(vegdata_2014)
vegdata_2015<-blankrowkiller(vegdata_2015)
vegdata_2016<-blankrowkiller(vegdata_2016)
vegdata_2017<-blankrowkiller(vegdata_2017)

#SUSPENDED COS ITS BREAKING THE NEXT BIT
#Setting up to fix using functions and lapply. Sometimes this means it runs functions over dataframes which it won't affect, e.g. they already have the right name formats, but that's ok.
#alltheBEGINdfs<-list(vegdata_2011, vegdata_2012, vegdata_2013, vegdata_2014, vegdata_2015, vegdata_2016, vegdata_2017)
#names(alltheBEGINdfs) <- c("vegdata_2011", "vegdata_2012", "vegdata_2013", "vegdata_2014", "vegdata_2015", "vegdata_2016", "vegdata_2017")#so annoying that I lose the names when storing in a list

#Removing blank rows
#blankrowkiller<-function(x){x<-x[rowSums(is.na(x)) != ncol(x),]}
#alltheBEGINdfs<-lapply(alltheBEGINdfs, blankrowkiller)
#fixing species naming, starting with spaces
#alltheBEGINdfs<-lapply(alltheBEGINdfs, function(df){within(df, names(df)<-gsub(" ", "_", names(df)))})

#then put it all back to individual objects at the end
#list2env(alltheBEGINdfs, envir = .GlobalEnv)#and then take them all back out of the list ONCE THEY ARE DONE.

#TRYINGITRICHARDSWAY#




#### new objects ####
dirtymerge2007_2017<-bind_rows(veg2007_2010, vegdata_2012, vegdata_2013, vegdata_2014, vegdata_2015, vegdata_2016, vegdata_2017)

#we need year_plot for any ordinations but separate year, block, plot for any modelling and for the maps
dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Plot_year)]<-paste(dirtymerge2007_2017$Block[is.na(dirtymerge2007_2017$Plot_year)], dirtymerge2007_2017$Plot[is.na(dirtymerge2007_2017$Plot_year)], "-", substr(dirtymerge2007_2017$Year[is.na(dirtymerge2007_2017$Plot_year)], 3,4), sep = "")
dirtymerge2007_2017$Plot[is.na(dirtymerge2007_2017$Plot)]<-gsub("-", "", substr(dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Plot)], 2,3))#Note this turns NA into AN!
dirtymerge2007_2017<-dirtymerge2007_2017[!(dirtymerge2007_2017$Plot)=="AN",]#done here as the only place where it doesn't drop 2007-2010 data but the Year replacement doesn't break because of NAs.
dirtymerge2007_2017$Year[is.na(dirtymerge2007_2017$Year)]<-gsub("-", "", regmatches(x=dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Year)], regexpr("(-)\\d\\d", dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Year)])))# a very ugly way to extract the year but the first parts are of different lengths so...
dirtymerge2007_2017$Block[is.na(dirtymerge2007_2017$Block)]<-substr(dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Block)],1,1)

#take out any non-squares so that they're white in the maps
dirtymerge2007_2017<-dirtymerge2007_2017 %>% filter(!(paste(Block, Plot, sep=""))%in%c("A10", "B6", "C4", "D11", "E6"))

#little things for data improvement, kill once data is ready
firstlist<-sort(names(dirtymerge2007_2017)) #2011 has some y's, so will need converting to numeric before it can be used here
#write.csv(firstlist, "Species_list_first_glance.csv")
dirtymerge2007_2017[!(is.na(dirtymerge2007_2017$`Cardamine pratnsis`)),c("Plot_year", "Block", "Plot", "Year")]

#This is an object for maps and modelling, thin format.
BeginThinDraft<-dirtymerge2007_2017 %>% select(-c(Site, Plot_year)) %>% gather(key="Species", value="Cover", -c(Plot, Block, Year)) 
BeginThinDraft$Cover[is.na(BeginThinDraft$Cover)]<-0

#This is an object for vegan (ordination, turnover) - it needs to be a df not a tibble so that we can use the row names for plot names (makes labelling ordination plots waaaaay easier)
Beginfatdraft<-dirtymerge2007_2017 %>% select(-c(Site, Year, Block, Plot, `three mystery acrocarps in C`)) %>% as.data.frame(.)
rownames(Beginfatdraft)<-Beginfatdraft$Plot_year
Beginfatdraft$Plot_year<-NULL
Beginfatdraft<-Beginfatdraft[,colSums(is.na(Beginfatdraft))!=nrow(Beginfatdraft),]
Beginfatdraft[is.na(Beginfatdraft)]<-0
