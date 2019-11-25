source("BEGIN Data loading and cleaning.R")
library(vegan)#tidyverse loads with the source file

#### turnover ####
#we'll want betadisp I think, it's in vegan. But first we want the environment data


#### ordination ####
dca.begin.all<-decorana(BeginFatDraft) #axis 1 length under 2 so it's ok to base the pricipal response curves on rda which is the default
plot(dca.begin.all, display = "both")
scores.dcabeginall<-as.data.frame(scores(dca.begin.all))
scores.dcabeginall[scores.dcabeginall$DCA1>1,]#it's the holcus mollis plot. This is going to cause us problems.


dca.begin.noholcmoll<-BeginFatDraft %>% select(-Holcus_mollis)%>% decorana()
plot(dca.begin.noholcmoll)
str(scores.dcabeginall)

identical(rownames(BeginFatDraft), BeginTreat11years$Plot_year) #they have to be the same class as well ir you can't compare string and factor. If this returns false it is bad time. 
myfirstprc<-prc(sqrt(BeginFatDraft), treatment=factor(BeginTreat11years$Ndose), time=factor(BeginTreat11years$year)) #
plot(myfirstprc) #is being broken by two extreme species. need a square root transform


mysecondprc<-prc(sqrt(BeginFatDraft), treatment=factor(BeginTreat11years$Ndose), time=factor(BeginTreat11years$year)) #
plot(myfirstprc) 




#ggvegan is the way to go