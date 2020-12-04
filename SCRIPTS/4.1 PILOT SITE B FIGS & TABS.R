


# Script to plot pilot results for camps



# load packages
library(tidyverse)
library(lubridate)
library(RSQLite)
library(DBI)
library(data.table)

knitr::opts_chunk$set(echo = FALSE, message = F, warning = F)


expdat<- src_sqlite(path="DATA/PilotSiteB.sqlite3")

src_tbls(expdat)

pilot_runs<- tbl(expdat,"filter_run_data")%>%collect()%>%
          dplyr::group_by(col,run_id)%>%
          mutate(runtime = (obstime- min(obstime))/3600)





measure_lab<- data.frame(measurement = c("DP", "Q", "TURB", "ch2"),
                         Measure= c("HL (M)", "Rate (m/hr)", "Turb (NTU)","Particles (5-10 micron /ml)"))


run_flows<- pilot_runs%>%
          dplyr::filter(measurement =="Q")%>%
          dplyr::group_by(col,  run_id)%>%
          summarise(runflow = round(mean(value, na.rm = T)))


run_log<-tbl(expdat, "run_log")%>%collect()

run_info<-tbl(expdat, "run_log")%>%collect()%>%
          dplyr::transmute(run_id = run_id,
                           col = col,
                           run_issue = run_issue,
                           rinse_rate = round((upwash_only_flow_lperh/1000)/(pi*(0.15/2)^2)),
                           bed_depth = ifelse(is.na(media2_depth), media1_depth/1000, (media1_depth+media2_depth)/1000),
                           rinse_bed_vols = (rinse_rate/60*upwash_only_time)/bed_depth,
                           combined_rate = (comb_water_flow_lperh/1000)/(pi*(0.15/2)^2),
                           combined_bed_vols = round((combined_rate/60*comb_time_min)/bed_depth),
                           total_bed_volumes = round(ifelse(is.na(combined_bed_vols), rinse_bed_vols, rinse_bed_vols+combined_bed_vols)),
                           media_lab = paste(media1_material,media2_material, sep = " & "),
                           media_lab = gsub(" & NA", "", media_lab))



camps_24hrs<- pilot_runs%>%
          dplyr::filter(is.na(run_id)==FALSE,
                        measurement %in% c("DP", "Q","TURB", "ch2"))%>%
          mutate(#value = ifelse(measurement == "ch1" & value > 500, 500,value),
                    value = ifelse(measurement == "ch2" & value > 500, 500,value)#,
                    #value = ifelse(measurement == "ch3" & value > 200, 200,value)
                    )%>%
          left_join(.,measure_lab)%>%
          left_join(.,run_info)%>%
          left_join(., run_flows)%>%
          dplyr::filter(is.na(run_id)== FALSE)%>%
          dplyr::filter(run_issue=="")%>%
          dplyr::filter(grepl("AUG", run_id)==F)%>%
          mutate(hour = round(runtime),
                 timestamp = as_datetime(obstime))%>%
          group_by(col,instrument,measurement,run_id,hour, Measure, media_lab,rinse_rate,combined_rate,rinse_bed_vols,combined_bed_vols, total_bed_volumes, runflow, run_issue)%>%
          summarise(timestamp = first(timestamp),
                    value = mean(value))%>%
          #dplyr::filter(media_lab != "0.5-1mm Filtralite HC & 1.5-2.5mm Filtralite HC")%>%
          ungroup()%>%
          mutate(run_id = factor(run_id),
                 run_id = fct_reorder(run_id, timestamp, .fun = "min"))%>%
          dplyr::filter(hour < 45|media_lab !="0.8-1.6mm Sand")%>%
          
          dplyr::filter(rinse_rate < 20)%>%
          dplyr::filter(media_lab =="0.8-1.6mm Sand"|
                                  media_lab =="0.8-1.6mm Filtralite NC")%>%
          mutate(total_rinse_vol = combined_bed_vols + rinse_bed_vols,
                 total_rinse_vol =  ifelse(is.na(total_rinse_vol)==T,rinse_bed_vols,total_rinse_vol ),
                 total_rinse_vol = round(total_rinse_vol))%>%
          dplyr::filter(run_id != "18_OCTD" |media_lab =="0.8-1.6mm Sand")%>%
          dplyr::filter(hour == 24,
                        media_lab !="208 EA",
                        media_lab !="0.5-1mm Filtralite HC & 1.5-2.5mm Filtralite HC")%>%
          dplyr::select(run_id,col, hour,media_lab,measurement,value,rinse_rate,combined_bed_vols,total_bed_volumes,total_rinse_vol)%>%
          mutate(filtralite = media_lab=="0.8-1.6mm Filtralite NC")%>%
          spread(key = measurement, value = value)


mean_24hr_camps<- camps_24hrs%>%
          group_by(media_lab)%>%
          summarise(DP = mean(DP),
                    TURB = mean(TURB))


lm_24hr_camps_turb<- lm(TURB~filtralite,
                        data = camps_24hrs)


summary(lm_24hr_camps_turb)


lm_24hr_camps_dp<- lm(DP~filtralite,
                      data = camps_24hrs)

summary(lm_24hr_camps_dp)




plot_pilot_runs<- pilot_runs%>%
          dplyr::filter(is.na(run_id)==FALSE,
                        measurement %in% c("DP", "Q","TURB", "ch2"))%>%
          # mutate(#value = ifelse(measurement == "ch1" & value > 500, 500,value),
          #           #value = ifelse(measurement == "ch2" & value > 500, 500,value),
          #           #value = ifelse(measurement == "ch3" & value > 200, 200,value),
          #           value = ifelse(measurement == "TURB" & value > 1, 1,value),
          #           value = ifelse(measurement == "DP" & value > 3, 3,value))%>%
          left_join(.,measure_lab)%>%
          left_join(.,run_info)%>%
          left_join(., run_flows)%>%
          dplyr::filter(is.na(run_id)== FALSE)%>%
          dplyr::filter(run_issue=="")%>%
          dplyr::filter(grepl("AUG", run_id)==F)





plot_pilot_runs_summarised<- plot_pilot_runs%>%
          mutate(hour = round(runtime),
                 timestamp = as_datetime(obstime))%>%
          group_by(col,instrument,measurement,run_id,hour, Measure, media_lab,rinse_rate,combined_rate,rinse_bed_vols,combined_bed_vols, total_bed_volumes, runflow, run_issue)%>%
          summarise(timestamp = first(timestamp),
                    value = mean(value))%>%
          #dplyr::filter(media_lab != "0.5-1mm Filtralite HC & 1.5-2.5mm Filtralite HC")%>%
          ungroup()%>%
          mutate(run_id = factor(run_id),
                 run_id = fct_reorder(run_id, timestamp, .fun = "min"))%>%
          dplyr::filter(hour < 45|media_lab !="0.8-1.6mm Sand")%>%
          
          dplyr::filter(rinse_rate < 20)%>%
          dplyr::filter(media_lab =="0.8-1.6mm Sand"|
                                  media_lab =="0.8-1.6mm Filtralite NC")%>%
          mutate(total_rinse_vol = combined_bed_vols + rinse_bed_vols,
                 total_rinse_vol =  ifelse(is.na(total_rinse_vol)==T,rinse_bed_vols,total_rinse_vol ),
                 total_rinse_vol = round(total_rinse_vol))%>%
          dplyr::filter(run_id != "18_OCTD" |media_lab =="0.8-1.6mm Sand")



model_pilot_runs_summarised

#selected_runs<- c("18_OCTB", "18_OCTC", "18_OCTD","18_SEPD", "18_SEPE", "18_SEPG")

selected_run_info<- tbl(expdat, "run_log")%>%collect()%>%
          dplyr::filter(run_id %in% selected_runs,
                        col != "C2")

gg_direct_turb<-ggplot(plot_pilot_runs_summarised%>%dplyr::filter(measurement == "TURB"
                                                  )%>%
                 dplyr::filter(media_lab !="208 EA",
                                        media_lab !="0.5-1mm Filtralite HC & 1.5-2.5mm Filtralite HC")%>%
                 group_by(col, run_id)
       , aes(x = hour, y = value, colour = media_lab, linetype = factor(total_rinse_vol), group = interaction(media_lab,run_id)))+
          geom_line()+
          xlim(0,60)+
          ylim(0,3)+
          theme_minimal()+
          scale_color_brewer(palette = "Dark2", name = "Media")+
          theme(legend.position = "bottom")+
          scale_linetype(name = "Wash water volume (empty beds)")+
          labs(#title = "Turbidity profiles for direct filtration with sand and filtralite",
               x = "Hours in run",
               y = "Turbidity (NTU)")+
          geom_hline(yintercept = 0.1)
          
gg_direct_turb

ggsave(gg_direct_turb, file = "PLOTS/camps_turb.png", width = 20, height=15, units = "cm")


gg_direct_turb2<-ggplot(plot_pilot_runs_summarised%>%dplyr::filter(measurement == "TURB"
)%>%
          dplyr::filter(media_lab !="208 EA",
                        media_lab !="0.5-1mm Filtralite HC & 1.5-2.5mm Filtralite HC")%>%
          group_by(col, run_id)
, aes(x = timestamp, y = value, colour = media_lab, linetype = factor(total_rinse_vol), group = interaction(media_lab,run_id)))+
          geom_line()+
          ylim(0,4)+
          theme_minimal()+
          scale_color_brewer(palette = "Dark2", name = "Media")+
          theme(legend.position = "bottom")+
          scale_linetype(name = "Wash water volume (empty beds)")+
          labs(#title = "Turbidity profiles for direct filtration with sand and filtralite",
                    x = "Hours in run",
                    y = "Turbidity (NTU)")

gg_direct_turb2

ggsave(gg_direct_turb, file = "PLOTS/camps_turb.png", width = 20, height=15, units = "cm")


gg_direct_headloss<-ggplot(plot_pilot_runs_summarised%>%dplyr::filter(measurement == "DP"
)%>%
          dplyr::filter(media_lab !="208 EA",
                        media_lab !="0.5-1mm Filtralite HC & 1.5-2.5mm Filtralite HC")%>%
          group_by(col, run_id)
, aes(x = hour, y = value, colour = media_lab, linetype = factor(total_rinse_vol), group = interaction(media_lab,run_id)))+
          geom_line()+
          xlim(0,60)+
          theme_minimal()+
          scale_color_brewer(palette = "Dark2", name = "Media")+
          scale_linetype(name = "Rinse bed volumes")+
          theme(legend.position = "bottom")+
          labs(#title = "Head loss profiles for direct filtration with sand and filtralite",
               x = "Hours in run",
               y = "Head loss (m)")

gg_direct_headloss

ggsave(gg_direct_headloss, file = "PLOTS/camps_hl.png", width = 20, height=15, units = "cm")

colnames(camps_24hrs)






library(nlme)

model_data_camps<-plot_pilot_runs_summarised%>%
          dplyr::filter(media_lab !="208 EA",
                        media_lab !="0.5-1mm Filtralite HC & 1.5-2.5mm Filtralite HC")%>%
          dplyr::select(run_id,col, hour,media_lab,measurement,value,rinse_rate,combined_bed_vols,total_bed_volumes,total_rinse_vol)%>%
          mutate(filtralite = media_lab=="0.8-1.6mm Filtralite NC")%>%
          spread(key = measurement, value = value)%>%
          dplyr::filter(is.na(DP)==F)%>%
          group_by(run_id,col)%>%
          mutate(bed_vol = cumsum(Q),
                 hl_growth_rate = DP/bed_vol)


ggplot(model_data_camps, aes(x =hour, y = TURB, colour = media_lab, linetype = factor(total_rinse_vol), group = interaction(media_lab,run_id)))+
          geom_line()+
          facet_wrap(rinse_rate~media_lab)



camps_dp_lme<-lme(DP~filtralite,
                  correlation = corAR1(~1|col_run_id),
                  random = ~1|col_run_id,
          data = headloss_model_data_camps)



