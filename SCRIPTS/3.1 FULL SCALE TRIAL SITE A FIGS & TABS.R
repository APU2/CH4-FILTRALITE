

# script to produce run statistics for filter media full scale trial

library(tidyverse)
library(zoo)
library(dygraphs)
library(lubridate)
library(egg)

# database connections
rosebery<- src_postgres(dbname = "rosebery", host = "localhost", port = 5432, user = "postgres", password = "password")


#sigexp<- tbl(rosebery, "sigs")%>%filter(obstime  > "2017-11-15 00:00:00")%>%collect()


filter_data<- tbl(rosebery, "filt_sigs")%>%
          filter(filter >4,
                    obstime > "2017-10-12 11:13:00",
                 inservice == "ONLINE")%>%
          collect(n = Inf)


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
          dplyr::filter(runlenth < 60)%>%
          dplyr::filter(runlenth < 30 | filter==6)%>%
          dplyr::filter(runlenth < 24 | facet=="Extended run at same flow")%>%
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


# dydat<-filter_data%>%
#           dplyr::filter(year(obstime) ==2017,
#                         filter ==6)%>%
#           group_by(obstime)%>%
#           select(obstime, turb)%>%
#           summarise(turb = first(turb))
# 
# 
# dygraph(zoo(dydat$turb, dydat$obstime))


filter_runs<-filter_data%>%dplyr::filter(#week(obstime)> 47,
          filter >4,
          inservice == "ONLINE")%>%
          group_by(filter, runid)%>%
          mutate(difftis = c(NA ,diff(timeinrun)),
                 vol = (difftis*fl)/1000)%>%
          summarise(obstime = last(obstime),
                    `Run time (hrs)` = max(timeinrun)/3600,
                    `Filtration rate (m3/m2/hr)` = (mean(fl)*3600/1000)/52.5,
                    `Turbidity mean (NTU)` = mean(turb, na.rm = TRUE),
                    `Turbidity 95thpctile (NTU)` = quantile(turb, 0.95),
                    `Max HL (m)` = max(hl),
                    `UFRV m3/m2/run` = `Filtration rate (m3/m2/hr)`*`Run time (hrs)`)%>%
          dplyr::filter(`Run time (hrs)` < 60,
                        `Filtration rate (m3/m2/hr)`>1.5)





filter_runs_long<- filter_runs%>%ungroup()%>%
          gather(key = signal, value = val, -filter,-runid,-obstime)%>%
          mutate(media = ifelse(filter == 6 , "Filtralite - Anthracite", "Sand - Pumice"))%>%
          dplyr::filter(filter!=8,
                        filter!=5)

gg_runstats<-ggplot(filter_runs_long%>%
                 dplyr::filter(obstime < "2018-02-28 00:00:00")%>%
                    mutate(signal = factor(signal
                                           , levels = unique(filter_runs_long$signal)
                                           , labels =paste(c("A: ","B: ","C: ","D: ","E: ","F: ")
                                                           ,unique(filter_runs_long$signal)) ))
                 , aes(x = obstime, y = val, group = filter, colour = media))+
          geom_point()+
          facet_wrap(~signal, scales = "free_y", ncol = 2)+
          scale_color_brewer(name = "Media", palette = "Dark2")+
          scale_x_datetime()+
          theme_minimal()+
          theme( legend.position = "bottom")+
          labs(#title = "Filter run statistics for full scale trial WTW A",
               x = "date",
               y = "")
gg_runstats

ggsave(gg_runstats, filename = "run_summary_stats.png", path = "PLOTS/",
       width = 20, height = 18, units = "cm")

