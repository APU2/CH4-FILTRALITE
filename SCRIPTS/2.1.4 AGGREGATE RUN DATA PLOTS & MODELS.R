

# present aggregated run data

# summary tabs

run_data%>%
    group_by(blend_fct, media_label)%>%
    summarise(brkthru = mean(brkthru, na.rm = T))


## plots vs wash rate ####

#backwash rate vs CBHL
norm_cbhl_blend<-ggplot(run_data%>%dplyr::filter(is.na(dp_data_quality)==T),
                        aes(x = upwash_rate, y = NORM_CBHL , colour = media_label ))+
    geom_point()+
    #geom_text(aes(label = run_id))+
    geom_smooth(method = "lm",formula = y~poly(x,2))+
    facet_wrap(~blend_fct, ncol = 3)+
    theme_minimal()+
    labs(x = "Backwash rate (m/hr)",
         y= "Normalised CBHL (m)")+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    theme(legend.position = "bottom")



norm_cbhl_blend

ggsave(norm_cbhl_blend, filename = "PLOTS/gg_norm_cbhl_blend.png", width = 20, height = 10, units = "cm")



#backwash rate vs headloss accumulation rate
norm_hl_acc<-ggplot(run_data%>%dplyr::filter(is.na(dp_data_quality)==T),
                    aes(x = upwash_rate, y = vol_adj_norm_hl , colour = media_label ))+
    geom_point()+
    #geom_text(aes(label = run_id))+
    geom_smooth(method = "lm",formula = y~poly(x,1))+
    facet_wrap(~blend_fct, ncol = 3)+
    theme_minimal()+
    labs(x = "Backwash rate (m/hr)",
         y= "Normalised HL (mm/EBV)")+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    theme(legend.position = "bottom")

norm_hl_acc

ggsave(norm_hl_acc, filename = "PLOTS/gg_norm_hl_acc.png", width = 20, height = 10, units = "cm")



# backwash rate vs breakthrough
brkthru_blend<-ggplot(run_data,
                      aes(x = upwash_rate, y = brkthru , colour = media_label))+
    geom_point(aes(shape = censored))+
    #geom_text(aes(label = run_id))+
    geom_smooth(method = "lm",formula = y~poly(x,1), data = run_data%>%
                    dplyr::filter(is.na(brkthru)==F,
                                  censored ==F))+
    facet_wrap(~blend_fct, ncol = 3)+
    theme_minimal()+
    labs(x = "Backwash rate (m/hr)",
         y= "Empty bed volumes at breakthrough")+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    scale_shape(name =  "Censored")+
    theme(legend.position = "bottom", legend.direction = "vertical")

brkthru_blend

ggsave(brkthru_blend, filename = "PLOTS/gg_brkthru_blend.png", width = 20, height = 10, units = "cm")


## plots vs rel to vmf ####

#rel_to_vmf vs CBHL
norm_cbhl_blend_vmf<-ggplot(run_data%>%dplyr::filter(is.na(dp_data_quality)==T),
                        aes(x = rel_to_vmf, y = NORM_CBHL , colour = media_label ))+
    geom_point()+
    #geom_text(aes(label = run_id))+
    geom_smooth(method = "lm",formula = y~poly(x,1))+
    facet_wrap(~blend_fct, ncol = 3)+
    theme_minimal()+
    labs(x = "Backwash rate (m/hr) / vmf",
         y= "Normalised CBHL (m)")+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    theme(legend.position = "bottom")



norm_cbhl_blend_vmf

ggsave(norm_cbhl_blend_vmf, filename = "PLOTS/gg_norm_cbhl_blend_vmf.png", width = 20, height = 10, units = "cm")


#rel_to_vmf vs headloss accumulation rate
norm_hl_acc_vmf<-ggplot(run_data%>%dplyr::filter(is.na(dp_data_quality)==T),
                    aes(x = rel_to_vmf, y = vol_adj_norm_hl , colour = media_label ))+
    geom_point()+
    #geom_text(aes(label = run_id))+
    geom_smooth(method = "lm",formula = y~poly(x,1))+
    facet_wrap(~blend_fct, ncol = 3)+
    theme_minimal()+
    labs(x = "Backwash rate (m/hr) / vmf",
         y= "Normalised HL (mm/EBV)")+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    theme(legend.position = "bottom")

norm_hl_acc_vmf

ggsave(norm_hl_acc_vmf, filename = "PLOTS/gg_norm_hl_acc_vmf.png", width = 20, height = 10, units = "cm")


#rel_to_vmf vs breakthrough
brkthru_blend_vmf<-ggplot(run_data%>%dplyr::filter(is.na(brkthru)==F),
                      aes(x = rel_to_vmf, y = brkthru , colour = media_label))+
    geom_point(aes(shape = censored))+
    #geom_text(aes(label = run_id))+
    geom_smooth(method = "lm",formula = y~poly(x,1), data = run_data%>%
                    dplyr::filter(is.na(brkthru)==F,
                                  censored ==F))+
    facet_wrap(~blend_fct, ncol = 3)+
    theme_minimal()+
    labs(x = "Backwash rate (m/hr) / vmf",
         y= "Empty bed volumes at breakthrough")+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    scale_shape(name =  "Censored")+
    theme(legend.position = "bottom", legend.direction = "vertical")

brkthru_blend_vmf

ggsave(brkthru_blend_vmf, filename = "PLOTS/gg_brkthru_blend_vmf.png", width = 20, height = 10, units = "cm")



### NCBHL over all runs

norm_cbhl_all<-ggplot(run_data%>%dplyr::filter(is.na(dp_data_quality)==T)%>%
                                dplyr::select(rel_to_vmf,upwash_rate,NORM_CBHL,media_label, run_id,blend_fct)%>%
                                pivot_longer(cols = c(rel_to_vmf, upwash_rate))%>%
                                mutate(facet = ifelse(name == "rel_to_vmf", "Backwash rate", "Scaled backwash rate")),
                            aes(x = value, y = NORM_CBHL , colour = media_label, shape = blend_fct, linetype = blend_fct ))+
    geom_point()+
    #geom_text(aes(label = run_id))+
    geom_smooth(method = "lm",formula = y~poly(x,2))+
    facet_wrap(~facet, ncol = 2, scales = "free_x")+
    theme_minimal()+
    labs(x = "Backwash rate (m/hr) or (m/hr/vmf)",
         y= "Normalised CBHL (m)")+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    theme(legend.position = "bottom")



norm_cbhl_all

ggsave(norm_cbhl_blend_vmf, filename = "PLOTS/gg_norm_cbhl_all.png", width = 20, height = 10, units = "cm")


cbhl_all<-ggplot(run_data%>%dplyr::filter(is.na(dp_data_quality)==T)%>%
                          dplyr::select(rel_to_vmf,upwash_rate,CBHL,media_label, run_id, blend_fct)%>%
                          pivot_longer(cols = c(rel_to_vmf, upwash_rate))%>%
                          mutate(facet = ifelse(name == "rel_to_vmf", "Backwash rate", "Scaled backwash rate")),
                      aes(x = value, y = CBHL , colour = media_label , shape = blend_fct, linetype = blend_fct))+
    geom_point()+
    #geom_text(aes(label = run_id))+
    geom_smooth(method = "lm",formula = y~x)+
    facet_wrap(~facet, ncol = 2, scales = "free_x")+
    theme_minimal()+
    labs(x = "Backwash rate (m/hr) or (m/hr/vmf)",
         y= "CBHL (m)")+
    scale_color_brewer(name = "Media", palette = "Dark2")+
    theme(legend.position = "bottom")



cbhl_all

ggsave(norm_cbhl_blend_vmf, filename = "PLOTS/gg_cbhl_all.png", width = 20, height = 10, units = "cm")




