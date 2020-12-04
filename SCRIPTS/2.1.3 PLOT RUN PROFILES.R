
##### TURBIDITY PLOTS #####


profile_plot_data<-particle_profile_data%>%
    dplyr::filter(run_id %in% c("17_JULB", "17_JUNA","17_FEBA","17_FEBI","17_FEBH")==F,
                  max(hr) > 12,
                  col != "C3",
                  upwash_rate != 20,
                  upwash_rate !=40,
                  #run_issue == "INSTRUMENT FAILURE"| is.na(run_issue)==T,
                  feed =="clar"| is.na(feed)==T)


turb_profiles<- ggplot(profile_plot_data, 
                       aes(x = vol, y = TURB,colour = media_label, group = media_label,  shape = factor(`Filtration rate (m/hr)`)))+
    geom_point()+
    xlim(0,400)+
    #scale_y_log10()+
    ylim(0,0.2)+
    facet_grid(blend_fct~upwash_label)+
    #ggtitle("Comparison of turbidity profiles between media under different conditions")+
    labs(x = "Bed volumes filtered",
         y = "Turbidity (NTU)")+
    theme_minimal(base_size = 14)+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    scale_shape(name = "Filtration rate (m3/m2/hr)")+
    theme(legend.position="bottom",
          legend.direction = "horizontal")+
    geom_text( aes( x = 30, y = 0.18, label = letter), colour = "black", size = 5)



turb_profiles


ggsave(turb_profiles, filename = "PLOTS/gg_turb_facets.png", width = 40, height = 20, units = "cm")


for(i in(1: length(unique(profile_plot_data$letter)))){
    
    sub_plot_data<-profile_plot_data%>%
        dplyr::filter(letter == unique(profile_plot_data$letter)[i])
    
    blend<- tolower(sub_plot_data$blend_fct[1])
    rinse<- sub_plot_data$upwash_rate[1]
    
    turb_profile<- ggplot(sub_plot_data, 
                          aes(x = vol, y = TURB, colour = media_label, group = media_label,  shape = factor(`Filtration rate (m/hr)`)))+
        geom_point()+
        xlim(0,400)+
        #scale_y_log10()+
        ylim(0,0.2)+
        #ggtitle("Comparison of turbidity profiles between media under different conditions")+
        labs(title = paste("Comparison of turbidity profiles during pilot trials at WTW A"),
             subtitle = paste("Columns treating", blend ,"with rinse rate of", rinse, "m/hr"),
             x = "Bed volumes filtered",
             y = "Turbidity (NTU)")+
        theme_minimal(base_size = 14)+
        scale_color_brewer(name = "Media", palette = "Dark2")+
        scale_shape(name = "Filtration rate (m3/m2/hr)")+
        theme(legend.position="bottom",
              legend.direction = "vertical")+
        geom_text( aes( x = 30, y = 0.18, label = letter), colour = "black", size = 5)
    
    ggsave(turb_profile, filename = paste0("PLOTS/gg_turb_facet_ind_",unique(profile_plot_data$letter)[i],".png"), width = 25, height = 15, units = "cm")
    
}


#### DP FACET PLOT ####


dp_profiles<- ggplot(profile_plot_data, 
                     aes(x = vol, y = DP, colour = media_label, group = media_label,  shape = factor(`Filtration rate (m/hr)`)))+
    geom_point()+
    xlim(0,400)+
    #scale_y_log10()+
    facet_grid(blend_fct~upwash_label)+
    #ggtitle("Comparison of turbidity profiles between media under different conditions")+
    xlab("Bed volumes filtered")+
    ylab("Head loss (m)")+
    theme_minimal(base_size = 14)+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    scale_shape(name = "Filtration rate (m3/m2/hr)")+
    theme(legend.position="bottom",
          legend.direction = "horizontal")+
    geom_text( aes( x = 30, y = 2.8, label = letter), colour = "black", size = 5)

dp_profiles

ggsave(dp_profiles, filename = "PLOTS/gg_dp_facets.png", width = 40, height = 20, units = "cm")



for(i in(1: length(unique(profile_plot_data$letter)))){
    
    sub_plot_data<-profile_plot_data%>%
        dplyr::filter(letter == unique(profile_plot_data$letter)[i])
    
    blend<- tolower(sub_plot_data$blend_fct[1])
    rinse<- sub_plot_data$upwash_rate[1]
    
    dp_profiles<- ggplot(sub_plot_data, 
                         aes(x = vol, y = DP, colour = media_label, group = media_label,  shape = factor(`Filtration rate (m/hr)`)))+
        geom_point()+
        xlim(0,400)+
        #scale_y_log10()+
        #ggtitle("Comparison of turbidity profiles between media under different conditions")+
        labs(title = paste("Comparison of head loss profiles during pilot trials at WTW A"),
             subtitle = paste("Columns treating", blend ,"with rinse rate of", rinse, "m/hr"),
             x = "Bed volumes filtered",
             y = "Head loss (m)")+
        theme_minimal(base_size = 14)+
        scale_color_brewer(name = "Media", palette = "Dark2")+
        scale_shape(name = "Filtration rate (m3/m2/hr)")+
        theme(legend.position="bottom",
              legend.direction = "vertical")+
        geom_text( aes( x = 30, y = 2.8, label = letter), colour = "black", size = 5)
    
    ggsave(dp_profiles, filename = paste0("PLOTS/gg_dp_facet_ind_",unique(profile_plot_data$letter)[i],".png"), width = 25, height = 15, units = "cm")
    
}



normdp_profiles<- ggplot(profile_plot_data, 
                         aes(x = vol, y = temp_norm_hl, colour = media_label, group = media_label))+
    geom_point()+
    xlim(0,400)+
    #scale_y_log10()+
    facet_grid(blend_fct~upwash_label)+
    #ggtitle("Comparison of turbidity profiles between media under different conditions")+
    xlab("Bed volumes filtered")+
    ylab("Normalised head loss (m)")+
    theme_minimal(base_size = 14)+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    scale_shape(name = "Filtration rate (m3/m2/hr)")+
    theme(legend.position="bottom",
          legend.direction = "horizontal")+
    geom_text( aes( x = 30, y = 2.8, label = letter), colour = "black", size = 5)


normdp_profiles

ggsave(tag_facet(normdp_profiles), filename = "PLOTS/gg_norm_dp_facets.png", width = 40, height = 20, units = "cm")


for(i in(1: length(unique(profile_plot_data$letter)))){
    
    sub_plot_data<-profile_plot_data%>%
        dplyr::filter(letter == unique(profile_plot_data$letter)[i])
    
    blend<- tolower(sub_plot_data$blend_fct[1])
    rinse<- sub_plot_data$upwash_rate[1]
    
    dp_profiles<- ggplot(sub_plot_data, 
                         aes(x = vol, y = temp_norm_hl, colour = media_label, group = media_label,  shape = factor(`Filtration rate (m/hr)`)))+
        geom_point()+
        xlim(0,400)+
        #scale_y_log10()+
        #ggtitle("Comparison of turbidity profiles between media under different conditions")+
        labs(title = paste("Comparison of normalised head loss profiles during pilot trials at WTW A"),
             subtitle = paste("Columns treating", blend ,"with rinse rate of", rinse, "m/hr"),
             x = "Bed volumes filtered",
             y = "Head loss (m)")+
        theme_minimal(base_size = 14)+
        scale_color_brewer(name = "Media", palette = "Dark2")+
        scale_shape(name = "Filtration rate (m3/m2/hr)")+
        theme(legend.position="bottom",
              legend.direction = "vertical")+
        geom_text( aes( x = 30, y = 2.8, label = letter), colour = "black", size = 5)
    
    ggsave(dp_profiles, filename = paste0("PLOTS/gg_norm_dp_facet_ind_",unique(profile_plot_data$letter)[i],".png"), width = 25, height = 15, units = "cm")
    
}




ptcl_profiles<- ggplot(profile_plot_data, 
                       aes(x = vol, y = ptcls,colour = media_label, group = media_label,  shape = factor(`Filtration rate (m/hr)`)))+
    geom_point()+
    xlim(0,400)+
    #scale_y_log10()+
    scale_y_log10( limits= c(10,1000))+
    facet_grid(blend_fct~upwash_label)+
    #ggtitle("Comparison of turbidity profiles between media under different conditions")+
    labs(x = "Bed volumes filtered",
         y = "Particles (Nbr/ml)")+
    theme_minimal(base_size = 14)+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    scale_shape(name = "Filtration rate (m3/m2/hr)")+
    theme(legend.position="bottom",
          legend.direction = "horizontal")+
    geom_text( aes( x = 30, y = 950, label = letter), colour = "black", size = 5)



ptcl_profiles


ggsave(ptcl_profiles, filename = "PLOTS/gg_ptcl_facets.png", width = 40, height = 20, units = "cm")

