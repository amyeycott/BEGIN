#These are BEGIN plot maps. Run this and then run the R markdown to make a fancy document.

## ---- Maps

source("BEGIN Data loading and cleaning.R")



# BeginThinDraft %>% filter(Species == "Holcus mollis") %>% 
#   mutate(Cover = as.numeric(Cover), 
#          Plot = as.numeric(Plot), 
#          Block = factor(Block, levels = rev(LETTERS[1:5])),
#          Cover = if_else(Cover == 0, NA_real_, Cover)) %>% 
#   ggplot(aes(x = (Plot - 1) %% 3, y = (Plot - 1) %/% 3, fill = Cover)) +
#   geom_raster() +
#   facet_grid(Block ~ Year) +
#   scale_fill_viridis_c()+
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_classic() +
#   theme(axis.title = element_blank())
  



## ---- begin_maps
for (sp in sort(unique(BeginThinDraft$Species))) {
  h <- BeginThinDraft %>% filter(Species == sp) %>% 
    mutate(Cover = as.numeric(Cover), 
           Plot = as.numeric(Plot), 
           Block = factor(Block, levels = rev(LETTERS[1:5])),
           Cover = if_else(Cover == 0, NA_real_, Cover)) %>% 
    ggplot(aes(x = (Plot - 1) %% 3, y = (Plot - 1) %/% 3, fill = Cover)) +
    geom_raster() +
    facet_grid(Block ~ Year) +
    scale_fill_viridis_c(limits = c(0, 100)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic() +
    theme(axis.title = element_blank()) +
    ggtitle(sp)
  #ggsave(paste0("Maps/",i,".png"))
  print(h)
}