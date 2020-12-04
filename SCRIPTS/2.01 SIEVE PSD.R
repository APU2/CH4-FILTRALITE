

# script to analyse sieve test data and vmf



sieve_test<- read.csv("DATA/MEDIA/SIEVE TEST.csv")%>%
    mutate(passing = 1-cum_pct_retained/100,
           passing = ifelse(passing<0,0, passing))

unique(sieve_test$Media)

sand_16_30_psd<- glm(passing~poly(sieve_mm,3), family = "quasibinomial", data = sieve_test%>%dplyr::filter(Media == "Sand 16/30"))
filtralite_HC_0.5_psd<- glm(passing~poly(sieve_mm,3), family = "quasibinomial", data = sieve_test%>%dplyr::filter(Media == "Filtralite HC 0.5-1"))
filtralite_NC_0.8_psd<- glm(passing~poly(sieve_mm,3), family = "quasibinomial", data = sieve_test%>%dplyr::filter(Media == "Filtralite NC 0.8-1.6"))
sand_10_18_psd<-glm(passing~poly(sieve_mm,3), family = "quasibinomial", data = sieve_test%>%dplyr::filter(Media == "Sand 10/18"))

predicted_psd<- data.frame(sieve_mm = seq(0.1,2.5,0.01))

predicted_psd$`Sand 16/30`<- predict(sand_16_30_psd, newdata = predicted_psd, type = "response")
predicted_psd$`Filtralite HC 0.5-1`<- predict(filtralite_HC_0.5_psd, newdata = predicted_psd, type = "response")
predicted_psd$`Filtralite NC 0.8-1.6`<- predict(filtralite_NC_0.8_psd, newdata = predicted_psd, type = "response")
predicted_psd$`Sand 10/18`<- predict(sand_10_18_psd, newdata = predicted_psd, type = "response")


predicted_psd_summary<- predicted_psd%>%
    pivot_longer(cols = 2:5,names_to = "media")%>%
    mutate(round_pctile = round(value,1))%>%
    dplyr::filter(round_pctile %in% c(0.1,0.6,0.9))%>%
    group_by(media, round_pctile)%>%
    summarise(mm = round(mean(sieve_mm),2))%>%
    pivot_wider(names_from =round_pctile, values_from = mm )%>%
    mutate(uniformity = round(`0.6`/`0.1`,2))


predicted_psd_long<-predicted_psd%>%
    pivot_longer(cols = 2:5,names_to = "Media", values_to ="passing")

gg_psd<-ggplot(sieve_test, aes(x = sieve_mm, y= passing, colour = Media))+
    geom_point()+
    geom_line(data = predicted_psd_long, aes(x = sieve_mm,y=passing, colour = Media))+
    theme_minimal()+
    scale_color_brewer(palette = "Set1")+
    labs(x = "Sieve (mm)",
         y = "Cumulative proportion passing")



ggsave(gg_psd, filename = "PLOTS/gg_sieve_psd.png", width = 15, height = 8, units = "cm")





# predicted psd2



predicted_psd_summary2<- predicted_psd%>%
    pivot_longer(cols = 2:5,names_to = "media")%>%
    mutate(round_pctile = round(value,1))%>%
    dplyr::filter(round_pctile %in% c(0.1,0.6,0.9))%>%
    group_by(media, round_pctile)%>%
    summarise(mm = round(mean(sieve_mm),2))



# function to calculate vmf using Wen & Yu


Ga <- function(particle_d90, water_density, particle_density, viscosity_abs){
    Ga<- (particle_d90^3* water_density*(particle_density - water_density)* 9.81)/viscosity_abs^2
    return(Ga)
}



vmf_wen_yu<- function(viscosity_abs, water_density, particle_d90, galileo_no){
    vmf = (viscosity_abs/(water_density*particle_d90))*
        (33.7^2 + 0.0408* galileo_no)^0.5 -
        ((33.7* viscosity_abs)/(water_density*particle_d90))
    return(vmf)
}




hyd_lkup<-read.csv("DATA/hydraulics_lkup.csv")%>%transmute(temp = Temp,
                                                           Density = Density*1000,
                                                           DynViscosity = DynViscosity/1000)

media_density<- data.frame(media = c("Filtralite HC 0.5-1","Filtralite NC 0.8-1.6" ,"Sand 10/18","Sand 16/30"  ),
                           media_density = c(1800,1260,2650,2650))


psd_vmf<- crossing(media = unique(predicted_psd_summary2$media),
                   round_pctile = c(0.1,0.6,0.9),
                   temp = c(5,15))%>%
            left_join(., predicted_psd_summary2)%>%
            left_join(.,hyd_lkup)%>%
            left_join(.,media_density)%>%
            mutate(Gal = Ga(particle_d90 = mm/1000,
                    water_density = Density,
                    particle_density = media_density,
                    viscosity_abs = DynViscosity ),
           vmf_ms = vmf_wen_yu(viscosity_abs = DynViscosity,
                               water_density = Density,
                               particle_d90 = mm/1000,
                               galileo_no = Gal),
           vmf_mh = vmf_ms*3600)



# porosity comparison

SA_voids<-((700*0.43)+(300*0.5))/1000
FL_voids<-((700*0.55)+(300*0.61))/1000

(FL_voids-SA_voids)/SA_voids
