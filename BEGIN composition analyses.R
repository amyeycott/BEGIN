source("BEGIN Data loading and cleaning.R")
library(vegan)#tidyverse loads with the source file
library()

#### turnover ####
#we'll want betadisp I think, it's in vegan. But first we want the environment data


#### ordination ####
dca.begin.all<-decorana(Beginfatdraft)
plot(dca.begin.all)#axis lengths are actually relatively low.
scores.dcabeginall<-as.data.frame(scores(dca.begin.all))
scores.dcabeginall[scores.dcabeginall$DCA1>1,]#it's the holcus mollis plot. This is going to cause us problems.


