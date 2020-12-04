#### DESCRIPTION ####

# Script connects to databases and extracts relevant data & performs final cleaning & mods before modelling & plotting

#### DB CONNECTION ####

expdat<- src_sqlite(path="DATA/PilotSiteA.sqlite3")
#src_tbls(expdat)



#### VISCOSITY CALCS ####

visc<-read.csv("DATA/VISCOSITY_LKUP.csv", stringsAsFactors = F)

# get temperature data lookup

temperature<- tbl(expdat, "filter_run_data")%>%
    dplyr::filter(measurement == "temp")%>%
    select(obstime, value)%>%
    collect(n = Inf)%>%
    transmute(obstime = as.POSIXct(obstime, origin="1970-01-01"),
              date = round_date(obstime, "day"),
              temp = value)%>%
    group_by(date)%>%
    summarise(temp = round(mean(temp)))%>%
    left_join(., visc)




##### FINAL CLEANING #####


    
    

# edit plot labelling data

filter_details<- tbl(expdat, "run_log")%>%collect()%>%
    # select(run_id, col, upwash_only_flow, run_issue)%>%
    mutate(upwash_rate = round((upwash_only_flow/(pi*0.075^2))/1000),
           upwash_label = paste(upwash_rate, "m/hr upwash"),
           media_label = ifelse(media1_material == "0.5-1mm Sand" & media2_material =="0.8-1.6mm Anthracite",
                                "Sand 0.5-1mm & Anthracite 0.8-1.6mm","OTHER"),
           media_label = ifelse(media1_material == "0.5-1mm Filtralite HC" & media2_material =="0.8-1.6mm Anthracite",
                                "Filtralite HC 0.5-1mm & Anthracite 0.8-1.6mm",media_label),
           media_label = ifelse(media1_material == "0.5-1mm Filtralite HC" & media2_material =="0.8-1.6mm Filtralite NC",
                                "Filtralite HC 0.5-1mm & Filtralite NC 0.8-1.6mm",media_label),
           start = as.POSIXct(start, origin="1970-01-01"),
           date = round_date(start, "day"),
           blend_fct = ifelse(date< "2017-03-27", "Winter Blend",
                              ifelse(date > "2017-05-31", "Summer Blend",
                                     "Spring Blend")))%>%
    left_join(.,temperature)


### data quality 
runs_with_issues<-filter_details%>%
    dplyr::select(run_id, col, feed,run_issue,run_actions,turb_data_quality,flow_data_quality, dp_data_quality, particles_data_quality )%>%
    pivot_longer(cols = run_issue:particles_data_quality)%>%
    dplyr::filter(value != "")%>%
    distinct()%>%
    pivot_wider(names_from = name, values_from= value)

# selct instruments of interest
instrumentation<- c("counter","sqrl")


# select filter run data needed
filter_run_data<- tbl(expdat, "filter_run_data")%>%
    collect(n = Inf)%>%
    dplyr::mutate(obstime = as.POSIXct(obstime, origin="1970-01-01 00:00:00"))%>%
    dplyr::filter(obstime > "2017-02-04 00:00:00", 
                  is.na(run_id) == FALSE)%>%
    group_by(run_id, col, measurement)%>%
    mutate( obstime = as.POSIXct(obstime, origin = "1970/01/01 00:00:00"),
        hrs = difftime(obstime, first(obstime), units = "hours"),
        round_hrs = round(hrs))%>%
    dplyr::group_by(run_id, col, round_hrs,  measurement )%>%
    dplyr::summarise(obstime = first(obstime),
                    value = mean(value, na.rm = TRUE, trim = 0.2))%>%
    spread(key = measurement, value = value)%>%
    group_by(col, run_id)%>%
    dplyr::mutate(vol = Q * (as.numeric(round_hrs) - lag(as.numeric(round_hrs))),
                  vol = ifelse(is.na(vol) == TRUE, 0, vol),
                  vol = cumsum(vol))%>%
    left_join(., filter_details)





run_flows<-filter_run_data%>%
    dplyr::filter(col != "C3")%>%
    dplyr::group_by(run_id)%>%
    dplyr::summarise(V = mean(Q, na.rm = TRUE),
                     `Filtration rate (m/hr)` = round(V))



run_flow_difference<- filter_run_data%>%
    dplyr::filter(col != "C3")%>%
    dplyr::group_by(run_id, col)%>%
    dplyr::summarise(V = mean(Q, na.rm = TRUE),
                     #`Filtration rate (m/hr)` = round(V/0.5)*.5
                     `Filtration rate (m/hr)` = round(V))%>%
    select(-`Filtration rate (m/hr)`)%>%
    spread(key = col, value = V)%>%
    mutate(fl_dif_pct = (C1-C2)/C1 *100)





filter_run_data<- left_join(filter_run_data, run_flows)


# SELECT DATA FOR PROFILE PLOTS #####


turb_profile_data<-filter_run_data%>%
    # filter(col != "C3",
    #                                         `Filtration rate (m/hr)`<7,
    #                                         feed == "clar",
    #                                         run_id %in% runs_to_drop ==F,
    #                                         run_id != "17_MARL",
    #                                         upwash_rate != 20,
    #                                         upwash_rate <36,
    #                                         turb_data_quality == "",
    #                                         vol<400)%>%
    group_by(`Filtration rate (m/hr)`,upwash_label)%>%
    mutate(replicate = as.factor(as.numeric(as.factor(run_id))),
           temp_fct = cut(temp, breaks = c(0,8,12,20),
                          labels = c("Water temp <8C","8-10", "Water temp >9C")))%>%
    ungroup()%>%
    mutate(column_run = factor(interaction(run_id,col)),
           media_mat = as.factor(ifelse (media_label=="Filtralite HC 0.5-1mm & Filtralite NC 0.8-1.6mm", "Filtralite", "Sand-Anth")),
           blend = factor(blend_fct))%>%
    mutate(media_blend = interaction(media_mat,blend),
           scale_washrate = scale(upwash_rate),
           media_washrate = factor(interaction(media_mat,upwash_rate)),
           scale_vol = scale(vol),
           hr = as.numeric(round_hrs),
           media_blend_washrate = factor(interaction(media_mat,upwash_rate,blend)))%>%
    group_by(column_run)%>%
    mutate(diffturb = c(NA, diff(TURB)))%>%
    ungroup()%>%
    mutate(fl_norm_hl = DP * (5/Q),
           temp_norm_hl = fl_norm_hl * (1.5182/dyn_viscosity),
           letter = LETTERS[as.numeric(factor(interaction(upwash_label,blend_fct, lex.order = F)))])

#### AGGREGATE RUN STATISTICS ####


run_stats<- turb_profile_data%>%
    mutate(column_run = factor(interaction(run_id,col)))%>%
    dplyr::group_by(run_id, col,column_run, start_date, upwash_rate, upwash_label, media_label, blend_fct, temp_fct, upwash_only_time)%>%
    mutate(hl_acc = temp_norm_hl/vol)%>%
    dplyr::filter(is.finite(hl_acc)==T)%>%
    summarise(temp = mean(temp),
              temp_norm_hl = mean(temp_norm_hl, na.rm = T),
              norm_hl_acc = mean(hl_acc, na.rm = T),
              mean_turb = mean(TURB, na.rm = T),
              max_turb = max(TURB, na.rm = T),
              ufrv = max(vol, na.rm = T),
              run_time = max(round_hrs, na.rm = T))%>%
    mutate(upwash_empty_bed_vols = (upwash_rate/(3600/upwash_only_time))/1)





# VMF CALCULATIONS ####

## script to show performance against rinse as a proportion of vmf


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

run_vmf<- run_stats%>%
    dplyr::select(run_id, col, column_run,temp, upwash_rate, media_label)%>%
    left_join(., hyd_lkup)%>%
    mutate(ptcl_diameter = ifelse(grepl("Sand", media_label), 0.76, 0.76)/1000,
           media_density = ifelse(grepl("Sand", media_label), 2650, 1800))%>%
    mutate(Gal = Ga(particle_d90 = ptcl_diameter,
                    water_density = Density,
                    particle_density = media_density,
                    viscosity_abs = DynViscosity ),
           vmf_ms = vmf_wen_yu(viscosity_abs = DynViscosity,
                               water_density = Density,
                               particle_d90 = ptcl_diameter,
                               galileo_no = Gal),
           vmf_mh = vmf_ms*3600,
           rel_to_vmf = upwash_rate/vmf_mh)


# CBHL CALCS ####


cbhl<-turb_profile_data%>%
    dplyr::filter(round_hrs > 0,
                  round_hrs < 3)%>%
    group_by(column_run,run_id,media_mat,upwash_rate,temp, blend,media_label,upwash_only_time)%>%
    summarise(TURB = mean(TURB),
              Q = mean(Q),
              CBHL = median(DP,na.rm = T),
              NORM_CBHL = median(temp_norm_hl,na.rm = T))%>%
    left_join(.,run_vmf)%>%
    mutate(upwash_empty_bed_vols = (upwash_rate/(3600/upwash_only_time))/1,
           upwash_col_vols = (upwash_rate/(3600/upwash_only_time))/(2.2-0.5))

cbhl_ave<- cbhl%>%
    group_by(media_mat)%>%
    summarise(CBHL = mean(CBHL, na.rm = T))


# PARTICLE DATA ####

# select filter run data needed
filter_run_particles<- tbl(expdat, "filter_run_particles")%>%
    collect(n = Inf)%>%
    dplyr::mutate(obstime = as.POSIXct(obstime, origin="1970-01-01 00:00:00"))%>%
    dplyr::filter(obstime > "2017-02-04 00:00:00")%>%
    #collect(n = Inf)%>%
    group_by(run_id, col, measurement)%>%
    mutate(# obstime = as.POSIXct(obstime, origin = "1970/01/01 00:00:00"),
        hrs = difftime(obstime, first(obstime), units = "hours"),
        round_hrs = round(hrs))%>%
    dplyr::group_by(run_id, col, round_hrs,  measurement )%>%
    dplyr::summarise(value = mean(value, na.rm = TRUE, trim = 0.2))%>%
    dplyr::group_by(run_id, col, round_hrs )%>%
    dplyr::summarise(ptcls = sum(value, na.rm = TRUE))


ptcl_filter_runs<- filter_run_particles%>%
    dplyr::group_by(col, run_id)%>%
    count()%>%
    left_join(., filter_details)


particle_profile_data<- left_join(turb_profile_data, filter_run_particles)%>%
    group_by(run_id, col)%>%
    dplyr::filter(col!= "C3",
                    ptcls>5,
                  length(na.omit(ptcls))>15
                  )%>%
    mutate(rollmean_ptcls = zoo::rollmean(ptcls, 4, na.rm =T, align ="right",fill = NA),
           ptcls_dif4 = (rollmean_ptcls-lag(rollmean_ptcls, 4))/lag(rollmean_ptcls, 4),
           min_hr = hr[which.min(rollmean_ptcls)],
           in_brkthru = ifelse(hr>min_hr & ptcls_dif4 >= 0.1, 1, 0), # have particle counts increased >= 10% in 4 hrs
           in_brkthru = zoo::na.locf(in_brkthru),
           in_brkthru2 = zoo::rollmean(in_brkthru, 6, na.rm =T, align ="left",fill = NA), # is this increase persistent on average for next 6 hours?
           in_brkthru2 = zoo::na.locf(in_brkthru2),
           in_brkthru3 = ifelse(in_brkthru2>=4/6,1,0),
           in_brkthru4 = zoo::na.locf(in_brkthru3),
           brkthru = min(vol[in_brkthru4 == 1]),
           brkthru_duration = length(in_brkthru[in_brkthru==1]),
           remaining_run =length(in_brkthru[brkthru<vol])+1,
           brkthru_persistence = brkthru_duration/remaining_run,
           brkthru_persistence = ifelse(brkthru_persistence>1,1,brkthru_persistence),
           brkthru = ifelse(brkthru_persistence < 0.5,max(vol), brkthru ),
           brkthru = ifelse(is.infinite(brkthru)==T, max(vol), brkthru),
           ends_in_brkthru = last(in_brkthru)
    )

# SUMMARISE BREAKTHROUGH BVs ####

particle_breakthrough<-particle_profile_data%>%
    ungroup()%>%
    distinct(run_id, col, start_date,upwash_rate,upwash_only_time, upwash_label,
             media_label, blend_fct, 
             `Filtration rate (m/hr)`,brkthru,ends_in_brkthru,
             brkthru_duration, remaining_run,brkthru_persistence
    )



## CALCULATE HL RATE ####

head_loss_rate<- turb_profile_data%>%
    dplyr::filter(vol>0)%>%
    group_by(run_id, col)%>%
    mutate(vol_adj_norm_hl = (temp_norm_hl-min(temp_norm_hl))/vol*1000)%>%
    group_by(column_run,run_id,media_mat,upwash_rate,temp, blend,media_label,upwash_only_time)%>%
    summarise(vol_adj_norm_hl = mean(vol_adj_norm_hl))%>%
    left_join(.,run_vmf)%>%
    mutate(upwash_empty_bed_vols = (upwash_rate/(3600/upwash_only_time))/1,
           fct_bv = cut(upwash_empty_bed_vols, breaks = c(0,2.5,3.5,6)))


# GET PREVIOUS RINSE RATE ####
previous_rinse<- filter_details%>%
    dplyr::select(run_id, col, upwash_rate)%>%
    group_by(col)%>%
    mutate(previous_rinse = lag(upwash_rate))



## JOIN RUN DATA ####


run_data<- run_stats%>%
    left_join(., cbhl%>%dplyr::select(run_id,col, CBHL, NORM_CBHL, rel_to_vmf))%>%
    left_join(., particle_breakthrough%>%dplyr::select(run_id,col,brkthru,brkthru_duration, remaining_run, 
                                                       brkthru_persistence, ends_in_brkthru))%>%
    left_join(., head_loss_rate%>%dplyr::select(run_id,col,vol_adj_norm_hl))%>%
    mutate(grp = paste(
        ifelse(grepl("Filt",media_label),"FL", "SA"),
        gsub(" Blend", "",blend_fct)
    ))%>%
    ungroup()%>%
    mutate(start_date = as.POSIXct(dmy(start_date)))%>%
    left_join(., runs_with_issues)%>%
    dplyr::filter(run_issue == "INSTRUMENT FAILURE"| is.na(run_issue)==T,
                  feed =="clar"| is.na(feed)==T,
                  upwash_rate < 36,
                  col != "C3")%>%
    left_join(., previous_rinse)%>%
    mutate(censored = brkthru>= ufrv,
           censored = ifelse(is.na(censored)==T|censored==T, T,F),
           brkthru = ifelse(is.na(brkthru), ufrv, brkthru))%>%
    dplyr::filter(run_id %in% c("17_JULB", "17_JUNA","17_FEBA","17_FEBI","17_FEBH", "17_FEBG")==F,
                  run_time >10)
    


ggplot(run_data, aes(x = ufrv, y =brkthru, colour = brkthru_persistence, shape = censored))+
    geom_point()+
    geom_text(aes(label =run_id))




