#This is a new data loading script for loading BEGIN data started by Amy in July 2018. We need to decide how much we can correct in the original files and how much to do in script. They're our files but there are some copies drifting around.
library(tidyverse)
library(lubridate)
library(readxl)


#### enter excel files ####
# Baseline 2007, years with treatment 2008-2014, years with cutting treatment 2015-2017 so far...
veg2007_2010<-read_excel("../Data/Species composition Revne Norway 2007 to 2010 ANALYSEDATA.xls", sheet = "Spp tp r ready")
#the names are in row 1 and th
vegdata_2011<-read_excel("../Data/BEGIN Revne 2011 data.xlsx", sheet = "veg", na = c("NA", "")) #we have to decide if 9 plots are worth it.We've got more if the moss notes happen to be somewhere.  hastag worldsmosthonestpaper?
vegdata_2012<-read_excel("../Data/BEGIN Revne 2012 data.xlsx", sheet = "vegetation", skip=1, na = c("NA", ""))#need to lose last column, also Brachythecium and Plagiomnium went in as character
vegdata_2013<-read_excel("../Data/BEGIN Revne 2013 data.xlsx", sheet = "Revna 2013 veg", na = c("NA", ""))#need to lose last column
vegdata_2014<-read_excel("../Data/BEGIN Revne 2014 data.xls", sheet = "Revna 2014 veg", na = c("NA", ""))#need to lose last column
vegdata_2015<-read_excel("../Data/BEGIN Revne 2015 data.xls", sheet = "Revna 2015 veg", na = c("NA", ""))#lose last two columns and get moss ids.
vegdata_2016<-read_excel("../Data/BEGIN Revne 2016 data.xls", sheet = "Revna 2015 veg", na = c("NA", ""))#year field reads 14 and sheet number reads 15, check before fixing
vegdata_2017<-read_excel("../Data/BEGIN Revne 2017 data.xlsx", sheet = "Revna 2017 veg", na = c("NA", ""))#year field reads 16, check before fixing

#### get rid of unwanted rows and columns, fix names ####
#steps needed: kill rows witout plot names (do in this step because of the plot-year naming confusion), fill fully blank rows, kill fully blank columns, kill names in this list: "Vascular", "mosses", "litter", "bare", "X__1", "X__2", "VASCSUM","CF TOTVASC", "BRYOSUM", "CF TOTBRYO"


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


#### new objects ####
dirtymerge2007_2017<-bind_rows(veg2007_2010, vegdata_2012, vegdata_2013, vegdata_2014, vegdata_2015, vegdata_2016, vegdata_2017)
dirtymerge2007_2017<-select(dirtymerge2007_2017, -c("Vascular", "mosses", "litter", "bare", "X__1", "X__2", "VASCSUM","CF TOTVASC", "BRYOSUM", "CF TOTBRYO"))
dirtymerge2007_2017<-dirtymerge2007_2017[is.na(dirtymerge2007_2017$Block),]
dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Plot_year)]<-paste(dirtymerge2007_2017$Block[is.na(dirtymerge2007_2017$Plot_year)], dirtymerge2007_2017$Plot[is.na(dirtymerge2007_2017$Plot_year)], "-", substr(dirtymerge2007_2017$Year[is.na(dirtymerge2007_2017$Plot_year)], 3,4), sep = "")#we need year_plot for any ordinations but separate year, block, plot for any modelling

names(dirty)


#little things for data improvement, kill once data is ready
firstlist<-sort(names(dirtymerge2007_2017)) #2011 has some y's, so will need converting to numeric before it can be used here
#write.csv(firstlist, "Species_list_first_glance.csv")
dirtymerge2007_2017[!(is.na(dirtymerge2007_2017$`Anthriscus sylvestris`)),c("Plot_year", "Block", "Plot", "Year")]
