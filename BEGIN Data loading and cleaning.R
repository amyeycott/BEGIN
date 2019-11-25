#This is a new data loading script for loading BEGIN data started by Amy in July 2018. We need to decide how much we can correct in the original files and how much to do in script. They're our files but there are some copies drifting around.

#corrections done by hand. In 2007-2010, the first cell title is changed (had contained forbidden symbols), D81-0 changed to D8-10, double space taken out of carex nigra, Brachytecium corrected in 2007-2010, Atrichum undulatim fixed in 2012 and merged with the other atrichum column, 2012 cardamines merged and yellow friend into ag cap

#corrections needed: SCANS 2012 has brachythecium in A10 and Yellow friend in C4 which are not used (need to look at data sheets). Anemone not anenome, lots of years. 2014 is missing mosses in A-C, vascular plants are missing from 2015 C 10-12,

#next step: treatment and environment data. 
#taxonomic decisions Aegopodium podagraria is angelica [not working], merge Agrostis to sp [not working], all alchemillas to sp [not working], Brachythecium from 2012 is Cirriphyllum, from 2013 goes with the Brachythecium sp. Butch POa is pratense, Calliergon/Brachythecium goes in Brachytecium sp, Carvum is conpodium, Deschampsia=avenella, Holcus mollis 2013 take the midpoint between 2012 and 2014, put that in and take off 2013 holcus lanatus.  [Merge all Atrichum and Mnium] do another year, All levermose er levermose, [Mnium sp (trolig...) call Mnium/atrichum] leave for later, Centaurea nigra is Succisa, Taraxacum use sp, Yellow friend is agrostis
#kept: Anthriscus - is in same squares as conpodium so not a confusion

library(tidyverse)
library(lubridate)
library(readxl)
library(assertr)

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

treatdata<-read_excel("../Original BEGIN project documentation/Treatments.xls")#treatdata has the non used rows missing
species_corrections<-read_excel("../Data/BEGIN_species_corrections.xlsx", sheet="Changes")

#make treatdata more useful
treatdata<-treatdata %>% filter(year==2007) %>% mutate(Plot=str_extract(id, pattern="^(.{2,3})(?=-)"))#this is a regular expression which tells it to take 2 or 3 characters from before a hyphen


blankrowkiller<-function(x){x<-x[rowSums(is.na(x)) != ncol(x),]}
veg2007_2010<-blankrowkiller(veg2007_2010)
vegdata_2011<-blankrowkiller(vegdata_2011)
vegdata_2012<-blankrowkiller(vegdata_2012)
vegdata_2013<-blankrowkiller(vegdata_2013)
vegdata_2014<-blankrowkiller(vegdata_2014)
vegdata_2015<-blankrowkiller(vegdata_2015)
vegdata_2016<-blankrowkiller(vegdata_2016)
vegdata_2017<-blankrowkiller(vegdata_2017)

#### new objects ####

#first fat format merge
dirtymerge2007_2017<-bind_rows(veg2007_2010, vegdata_2012, vegdata_2013, vegdata_2014, vegdata_2015, vegdata_2016, vegdata_2017)
names(dirtymerge2007_2017)<-gsub(" ", "_", x=names(dirtymerge2007_2017))

#we need year_plot for any ordinations but separate year, block, plot for any modelling and for the maps
dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Plot_year)]<-paste(dirtymerge2007_2017$Block[is.na(dirtymerge2007_2017$Plot_year)], dirtymerge2007_2017$Plot[is.na(dirtymerge2007_2017$Plot_year)], "-", substr(dirtymerge2007_2017$Year[is.na(dirtymerge2007_2017$Plot_year)], 3,4), sep = "")
dirtymerge2007_2017$Plot[is.na(dirtymerge2007_2017$Plot)]<-gsub("-", "", substr(dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Plot)], 2,3))#Note this turns NA into AN!
dirtymerge2007_2017<-dirtymerge2007_2017[!(dirtymerge2007_2017$Plot)=="AN",]#done here as the only place where it doesn't drop 2007-2010 data but the Year replacement doesn't break because of NAs.
dirtymerge2007_2017$Year[is.na(dirtymerge2007_2017$Year)]<-gsub("-", "", regmatches(x=dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Year)], regexpr("(-)\\d\\d", dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Year)])))# a very ugly way to extract the year but the first parts are of different lengths so...
dirtymerge2007_2017$Block[is.na(dirtymerge2007_2017$Block)]<-substr(dirtymerge2007_2017$Plot_year[is.na(dirtymerge2007_2017$Block)],1,1)
#take out any non-squares so that they're white in the maps
dirtymerge2007_2017<-dirtymerge2007_2017 %>% filter(!(paste(Block, Plot, sep=""))%in%c("A10", "B6", "C4", "D11", "E6"))

#This is an object for fixing species names but is also needed for maps and modelling, thin format.
BeginThinDraft<-dirtymerge2007_2017 %>% select(-Site) %>%  #do I really need to remove Plot_year here? It's annoying to have to make it again
  pivot_longer(names_to = "Species", values_to = "Cover", -c(Plot, Block, Year, Plot_year)) 
BeginThinDraft$Cover[is.na(BeginThinDraft$Cover)]<-0

#### Taxonomic harmonisation and spelling corrections across all years
#new strategy using thin table format to do corrections from a lookup table then re-making the fat format all corrected nicely. 
BeginThinDraft<-left_join(BeginThinDraft, species_corrections, by = c("Species"="Old_name")) %>%
  mutate(Species = coalesce(New_name, Species)) %>%
  group_by(Year, Block, Plot, Plot_year, Species) %>% #Here it will drop unused variables (in this case New_name and Notes)
  summarise(Cover=sum(Cover)) %>%  #summarise is like baking a cake, if you ungroup after it won't unsummerise
  ungroup() %>% 
  mutate("Block_plot"=paste(Block, Plot, sep="")) %>% 
  assert(within_bounds(0,100), Cover) # if it says assertr stoppped execution, then that means there are values not


BeginFatDraft<-BeginThinDraft %>% 
  select(-c(Plot, Block, Year)) %>% 
  pivot_wider(names_from = Species, values_from = Cover)

#This has to be made into an object for vegan (ordination, turnover) - it needs to be a df not a tibble so that we can use the row names for plot names (makes labelling ordination plots waaaaay easier)
BeginFatDraft<- BeginFatDraft%>% as.data.frame(.)
rownames(BeginFatDraft)<-BeginFatDraft$Plot_year
BeginFatDraft<-BeginFatDraft[,colSums(is.na(BeginFatDraft))!=nrow(BeginFatDraft),]
BeginFatDraft[is.na(BeginFatDraft)]<-0


#we need the environment data 11 times over for the PRC and in the same order
BeginTreat11years<-BeginFatDraft %>% left_join(treatdata, by=c("Block_plot"= "Plot")) %>% 
  mutate(year=str_extract(Plot_year, pattern="(?<=-)(\\d{2})"))%>% 
  select(Plot_year, id:Block) 

#tidy back up in BeginFatDraft for vegan 
BeginFatDraft<-BeginFatDraft %>% select(-c(Plot_year, Block_plot))

BeginThin2007_2014<-BeginThinDraft %>% filter(as.numeric(Year)<2015)
#BeginFat2007_2014<-BeginFatDraft %>% filter()