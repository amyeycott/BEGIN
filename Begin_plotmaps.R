#These are BEGIN plot maps. Run this and then run the R markdown to make a fancy document.

## ---- Maps

library(assertthat)
library(tidyverse)
source("BEGIN Data loading and cleaning.R")

#set up the coordinates for the squares
BEGINplot<-select(vegdata_2017[!is.na(vegdata_2017$Plot),], Block, Plot)#just as a source of the plot names. lazy arse put a filter in
BEGINplot$Row<-rep(1:20, each=3)#backwards so that A is at the bottom, like standing in the field.
BEGINplot$Column<-rep(1:3)


BeginThinDraft %>% filter(Species == "Anthoxanthum odoratum") %>% 
  mutate(Cover = as.numeric(Cover)) %>% 
  ggplot(aes(x = Block, y = Plot, fill = Cover)) +
  geom_raster() +
  facet_wrap(~ Year) +
  scale_fill_viridis()


#g sets up plots with one year and then another.
g<-ggplot(lichens.maps, aes(x=as.factor(bycol), y=byrow, fill=dominant, size=as.factor(Acrocordia_gemmata)))+
  theme(panel.grid = element_blank(), panel.background = element_blank())+
  geom_tile(colour="black", size=0.5, linetype="dashed")+
  geom_point()+
  scale_fill_manual(limits=coloury$Phytosociology_Latin, values=coloury$Colour_softer, labels=coloury$Community_in_1990)+
  scale_size_manual(breaks = c(1,2,3), limits=c(1,2,3), values=c(0.5,1.5,3))+#this stops it from displaying zeros and allows us to build our own size for the dots - the default was too similar between 2 and 3
  facet_wrap(~Year)+
  coord_equal()+
  labs(x="",y="",fill="Forest type in 1990", size="Frequency")+
  ggtitle("Acrocordia gemmata")
g#sets up the plot, using the first species as an example



## ---- lichens_maps
for (i in gsub(" ","_", unique(c(new.harm.db$Species, old.harm.db$Species)))) {
  h<-g+aes_string(size=paste0("as.factor(",i,")"))+
    ggtitle(gsub("_"," ", i))+
    labs(size="Frequency")
  #ggsave(paste0("Maps/",i,".png"))
  print(h)
}