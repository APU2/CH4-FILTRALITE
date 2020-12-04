
# taxonomy of filter designs

library(tidyverse)
library(treemap)
library(gt)


backwash_taxonomy<-crossing(#`Media size` = c("coarse","fine","dual/multi"),
                         # `Bed depth` = c("deep","shallow"),
                       `Backwash type` = c("Separate air & water", "combined air & water"),
                       `High-rate rinse` = c("Sub-fluidising rinse", "Fluidising rinse","Expanding"),
                       `Surface-wash` = c("no-surface wash", "surface wash"),
                       `Transport distance` = c("long","short")
                       )


write.csv(filter_taxonomy, "DATA/backwash taxa.csv")



filter_taxonomy<- read_csv("DATA/RGF taxa.csv")%>%
    rowwise()%>%
    mutate(Performance = min(c(`Head loss`,`Particle capture`,`Backwash effectiveness`)))


ggplot(filter_taxonomy,
       aes(x = interaction(`Bed depth`,`Media size`),
           y = interaction(`Surface-wash`,`Transport distance`),
           fill = Performance))+
    geom_tile()+
    facet_grid(`Backwash type`~ `High-rate rinse`)+
    theme(axis.text.x = element_text(angle = -90, vjust = -0.1))+
    labs(x = "Media bed",
         y = "")

filter_taxonomy_tab<-filter_taxonomy%>%
    dplyr::select(`Media size`,`Bed depth`,`Backwash type`,`High-rate rinse`,
                  `Surface-wash`,`Transport distance`, Performance )%>%
    pivot_wider(names_from = c(`Media size`,`Bed depth`,), values_from = Performance)%>%
    group_by( `High-rate rinse`)

colnames(filter_taxonomy_tab)

gt(filter_taxonomy_tab)%>%
    tab_spanner(label = "Deep Bed",
                columns = colnames(filter_taxonomy_tab)[grep("deep",colnames(filter_taxonomy_tab))])%>%
    tab_spanner(label = "Shallow Bed",
                columns = colnames(filter_taxonomy_tab)[grep("shallow",colnames(filter_taxonomy_tab))])%>%
    tab_style(
        style = cell_fill(color = "red"),
        locations = cells_body(
            columns = colnames(filter_taxonomy_tab)[5:10],
            rows = currency < 100
        )
    )
 

treemap(filter_taxonomy,
        index = c("Media size",
                  "Bed depth",
                    "Backwash type",
                  "High-rate rinse",
                  "Surface-wash",
                  "Transport distance"),
        vSize = "Performance",
        vColour = "Performance")
