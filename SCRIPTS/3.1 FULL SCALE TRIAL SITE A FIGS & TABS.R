

# script to produce run statistics for filter media full scale trial

library(tidyverse)
library(zoo)
library(dygraphs)
library(lubridate)
library(egg)
library(gt)

# database connections
rosebery<- src_postgres(dbname = "rosebery", host = "localhost", port = 5432, user = "postgres", password = "password")




filter_data<- tbl(rosebery, "filt_sigs")%>%
          filter(filter >4,
                    obstime > "2017-10-12 11:13:00",
                 inservice == "ONLINE")%>%
          collect(n = Inf)

src_tbls(rosebery)

# hydraulics reference data

hyd_lkup<-read.csv("DATA/hydraulics_lkup.csv")%>%transmute(temp = Temp,
                                                           Density = Density*1000,
                                                           DynViscosity = DynViscosity/1000)

# data for turbidity profiles

turb_profle_data<- filter_data%>%
          dplyr::filter(filter==6|filter==7)%>%
          mutate(facet = NA,
                 facet = ifelse(obstime > ymd_hms("2017-11-27 00:00:00")&
                                obstime <  ymd_hms("2017-12-15 00:00:00"),
                              "Extended run at same flow", facet),
                 facet = ifelse(obstime >  ymd_hms("2018-02-01 00:00:00")&
                                          obstime <  ymd_hms("2018-02-28 00:00:00"),
                                "20% increased flow", facet))%>%
          dplyr::filter(is.na(facet)==F)%>%
          # dplyr::filter(obstime > "2017-11-15 00:00:00",
          #               obstime < "2017-12-15 00:00:00")%>%
          mutate(hrs= timeinrun/3600,
                 diff_hrs = c(NA,diff(hrs)))%>%
          group_by(filter, runid)%>%
          mutate(v = (fl/1000*3600)/52.5,
                 vol = cumsum(v*diff_hrs))%>%
          mutate(runlenth = max(hrs, na.rm = T))%>%
          dplyr::filter(runlenth < 100)%>%
          dplyr::filter(runlenth < 30 | filter==6)%>%
          dplyr::filter(runlenth < 24 | facet=="Extended run at same flow")%>%
          dplyr::filter(fl > 3)%>%
          mutate(media = ifelse(filter == 6 , "Filtralite - Anthracite", "Sand - Pumice"))
          
          
gg_turb_profiles<-ggplot(turb_profle_data%>%
                                   ungroup()%>%
                                   sample_n(50000),
                 aes(x = hrs, y = turb, colour = media, group = interaction(filter)))+
          geom_point()+
          scale_color_brewer(name = "Media", palette = "Dark2")+
          theme_minimal()+
          theme( legend.position = "bottom")+
          labs(#title = "Comparison of filter turbidity profiles under full scale trial conditions",
               x = "Hrs in run",
               y = "")+
          ylim(0,0.25)+
          facet_wrap(~facet, ncol = 1 )

tag_facet(gg_turb_profiles)

ggsave(tag_facet(gg_turb_profiles), filename = "gg_turb_profiles.png", path = "PLOTS/",
       width = 20, height = 18, units = "cm")


# filter run summary data


filter_runs<-filter_data%>%
            dplyr::filter(#week(obstime)> 47,
                  (filter == 6 | filter ==7),
                  inservice == "ONLINE",
                  fl>10,
                  fl<100)%>%
            mutate(temp = round(raw_water_inlet_temperature))%>%
            left_join(.,hyd_lkup, by = c("temp" = "temp"))%>%
            mutate(`NHL (m)` = hl * (((3*52.5*1000)/3600)/fl),
                 `NHL (m)` = `NHL (m)`*(0.0013059/DynViscosity),
                 `NHL (m)` = ifelse(is.infinite(`NHL (m)`), NA, `NHL (m)`))%>%
            group_by(filter, runid)%>%
            mutate(difftis = c(NA ,diff(timeinrun)),
                    EBV = ((difftis*fl)/1000)/52.5
                   )%>%
            dplyr::filter(EBV >0,
                          EBV < 1,
                          difftis/3600 <60,
                          difftis>5)%>%
            mutate(EBV = cumsum(EBV))%>%
            summarise(obstime = last(obstime),
                    `Run time (hrs)` = max(timeinrun, na.rm = T)/3600,
                    `Filtration rate (m3/m2/hr)` = (mean(fl)*3600/1000)/52.5,
                    `Turbidity mean (NTU)` = mean(turb, na.rm = TRUE),
                    `Turbidity 95thpctile (NTU)` = quantile(turb, 0.95),
                    `Turbidity 95thpctile first 4 EBVs (NTU)` = quantile(turb[EBV < 4],0.95, na.rm = TRUE),
                    `Turbidity mean last hour (NTU)` = mean(turb[max(timeinrun/3600) - timeinrun/3600 < 1], na.rm = TRUE),
                    `CBHL (m)` = mean(hl[timeinrun/3600 < 2], na.rm = T),
                    `NCBHL (m)` = mean(`CBHL (m)`*(3/`Filtration rate (m3/m2/hr)`)), # normalise to 3 m/hr
                    `NCBHL (m)` = `NCBHL (m)`*mean(0.0013059/DynViscosity), # normalise to viscosity at 10 deg
                    `VNHL (mm/EBV)` = mean((`NHL (m)`[timeinrun/3600 > 2]-`NCBHL (m)`)/EBV[timeinrun/3600 > 2], na.rm = T)*1000,
                    `Terminal NHL (m)` = mean(`NHL (m)`[max(timeinrun/3600) - timeinrun/3600 < 1], na.rm = T),
                    `UFRV (m3/m2/run)` = `Filtration rate (m3/m2/hr)`*`Run time (hrs)`,
                    `Temp (C)` = mean(raw_water_inlet_temperature, na.rm = T),
                    `Clarified turbidity (NTU)` = mean(daf_str_2_outlet_turb, na.rm = T))%>%
            dplyr::filter(`Run time (hrs)` < 60,
                        `Filtration rate (m3/m2/hr)`>2.5,
                        `Turbidity 95thpctile first 4 EBVs (NTU)`<2.5,
                        `Turbidity 95thpctile (NTU)`<0.5,
                        `UFRV (m3/m2/run)` < 185,
                        `UFRV (m3/m2/run)` > 50)%>%
            dplyr::select(-`CBHL (m)`)



filter_runs_long<- filter_runs%>%ungroup()%>%
          gather(key = signal, value = val, -filter,-runid,-obstime)%>%
          mutate(media = ifelse(filter == 6 , "Filtralite - Anthracite", "Sand - Pumice"))%>%
          dplyr::filter(filter!=8,
                        filter!=5)%>%
    mutate(phase = cut(as.numeric(obstime), breaks = as.numeric(ymd_hms(c("2000-11-24 23:08:30","2017-11-21 00:00:30", "2017-12-16 00:05:00", "2018-02-28 00:00:00","2099-12-16 23:05:00"))), 
                       labels = c("Phase 1", "Phase 2", "Phase 3", "Phase 4")))



filter_runs_long_summary<- filter_runs_long%>%
    dplyr::filter(obstime < "2018-08-07 00:00:00")%>%
    group_by(media, signal, phase)%>%
    summarise( min = round(min(val, na.rm = T),2),
               mean = round(mean(val, na.rm = T),2),
               max = round(max(val, na.rm = T),2))%>%
    mutate(`Mean (Min-Max)` = paste0(mean, " (",min, "-",max,")"))%>%
    select(-min, -mean,-max)%>%
    ungroup()%>%
    unite(col = "phase_media",  phase, media, sep =" ")%>%
    pivot_wider(names_from = phase_media, values_from =`Mean (Min-Max)` )%>%
    select(signal, contains("Phase 1"), contains("Phase 2"), contains("Phase 3"), contains("Phase 4"))
    


gt(filter_runs_long_summary )%>%
    tab_header(title = "Summary of performance during full scale trial")%>%
    tab_spanner(label = "Phase 1",columns = contains("Phase 1"))%>%
    tab_spanner(label = "Phase 2",columns = contains("Phase 2"))%>%
    tab_spanner(label = "Phase 3",columns = contains("Phase 3"))%>%
    tab_spanner(label = "Phase 4",columns = contains("Phase 4"))%>%
    gtsave(., filename = "WTWA_fss.html")



gg_runstats<-ggplot(filter_runs_long%>%
                 dplyr::filter(obstime < "2018-08-07 00:00:00")%>%
                    mutate(signal = factor(signal
                                           , levels = unique(filter_runs_long$signal)
                                           , labels =paste(c("A: ","B: ","C: ","D: ","E: ","F: ",
                                                             "G: ", "H: ","I: ", "J: ", "K: ", "L: ")
                                                           ,unique(filter_runs_long$signal)) ))
                 , aes(x = obstime, y = val, group = filter, colour = media, shape = phase))+
          geom_point()+
          facet_wrap(~signal, scales = "free_y", nrow = 3)+
          scale_color_brewer(name = "Media", palette = "Dark2")+
          scale_x_datetime()+
          theme_minimal()+
          theme( legend.position = "bottom")+
          labs(#title = "Filter run statistics for full scale trial WTW A",
               x = "Date",
               y = "")
gg_runstats

ggsave(gg_runstats, filename = "run_summary_stats.png", path = "PLOTS/",
       width = 40, height = 20, units = "cm")
    
