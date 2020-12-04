# Script to update database



# load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(RSQLite)
library(RPostgreSQL)
library(DBI)
library(pracma)


# create database
expdat <- src_sqlite("DATA/roseberypilot.sqlite3", create = T)

rsqlite_conn<-dbConnect(RSQLite::SQLite(),  dbname= "DATA/roseberypilot.sqlite3")



src_tbls(expdat)

#list all data files
data_files<-list.files(path = "DATA/SITE A", full.names = TRUE, recursive = TRUE)



# drop run-log database table
dbSendQuery(rsqlite_conn,
            "DROP TABLE run_log")

## read run log sheet
run_log<- data_files[grep("filterrunlog", data_files)]%>%
                read.csv(.)%>%
                dplyr::mutate(start = dmy_hm(paste(start_date, start_time), tz = "Europe/London"),
                              end = dmy_hm(paste(end_date, end_time), tz = "Europe/London"),
                              flowstart = dmy_hm(paste(start_date, flowstart), tz = "Europe/London"))

##create database table
copy_to(expdat, run_log, name = "run_log",temporary = FALSE)


#Filter run times

##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE run_times")


## prep data
run_times<-run_log%>%
                    dplyr::group_by(run_id, col)%>%
                    dplyr::transmute(start = first(start),
                                     end = first(end),
                                     feed = feed,
                                     ptclstart = start - minutes(shift_ptcl_time),
                                     ptclend = end - minutes(shift_ptcl_time))




## copy table
copy_to(expdat, run_times, name = "run_times",temporary = FALSE)




# Squirrel data logger data

# Get the files names
sqrl<-data_files[grep("SQRL_EXP",data_files)]

sqrl_file0<- sqrl[grep("#00", sqrl)]
sqrl_file<- sqrl[-grep("#00", sqrl)]



## First apply read.csv, then rbind
sqrl_file <- do.call(rbind, lapply(sqrl_file, read.delim, stringsAsFactors = FALSE,  header = TRUE))%>%
    dplyr::transmute( obstime = dmy_hms(Date.Time, tz = "Europe/London"),
                      temp = as.numeric(TT2601...C.),
                      C1DP = as.numeric(PDI2601..Bar.),
                      C2DP = as.numeric(PDI2602..Bar.),
                      C3DP = as.numeric(PDI2603..Bar.),
                      C1TURB = as.numeric(QIT2603..FTU.),
                      C2TURB = as.numeric(QIT2602..FTU.),
                      C3TURB = as.numeric(QIT2601..FTU.),
                      C1Q = as.numeric(FIT2601..l.hr.)/1000/0.0176714586764426,
                      C2Q = as.numeric(FIT2602..l.hr.)/1000/0.0176714586764426,
                      C3Q = as.numeric(FIT2603..l.hr.)/1000/0.0176714586764426)

## First apply read.csv, then rbind
sqrl_file0 <- do.call(rbind, lapply(sqrl_file0,  read.delim, stringsAsFactors = FALSE,  header = TRUE, skip = 22))%>%
    dplyr::transmute( obstime = dmy_hms(Date.Time, tz = "Europe/London"),
                      temp = as.numeric(TT2601...C.),
                      C1DP = as.numeric(PDI2601..Bar.),
                      C2DP = as.numeric(PDI2602..Bar.),
                      C3DP = as.numeric(PDI2603..Bar.),
                      C1TURB = as.numeric(QIT2603..FTU.),
                      C2TURB = as.numeric(QIT2602..FTU.),
                      C3TURB = as.numeric(QIT2601..FTU.),
                      C1Q = as.numeric(FIT2601..l.hr.)/1000/0.0176714586764426,
                      C2Q = as.numeric(FIT2602..l.hr.)/1000/0.0176714586764426,
                      C3Q = as.numeric(FIT2603..l.hr.)/1000/0.0176714586764426)

sqrl<- bind_rows(sqrl_file,sqrl_file0)%>%
            dplyr::distinct()

rm(sqrl_file, sqrl_file0)

##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE sqrl")


#create database table
copy_to(expdat, sqrl, name = "sqrl",temporary = FALSE)



# particle  data
##list ptcl files
ptcl_day<-data_files[grep("PTCL DAILY",data_files)]

ptcl1_day<-ptcl_day[grep("C1",ptcl_day)]
ptcl2_day<-ptcl_day[grep("C2",ptcl_day)]
ptcl3_day<-ptcl_day[grep("C3",ptcl_day)]

ptcl1_day_mdy<-ptcl1_day[grep("MDY",ptcl1_day)]
ptcl2_day_mdy<-ptcl2_day[grep("MDY",ptcl2_day)]
ptcl3_day_mdy<-ptcl3_day[grep("MDY",ptcl3_day)]

ptcl1_day_dmy<-ptcl1_day[grep("DMY",ptcl1_day)]
ptcl2_day_dmy<-ptcl2_day[grep("DMY",ptcl2_day)]
ptcl3_day_dmy<-ptcl3_day[grep("DMY",ptcl3_day)]

ptcl1_day_hm<-ptcl1_day[grep("HM",ptcl1_day)]
ptcl2_day_hm<-ptcl2_day[grep("HM",ptcl2_day)]
ptcl3_day_hm<-ptcl3_day[grep("HM",ptcl3_day)]


##read ptcl files
ptcl1_day_dmy<-lapply(ptcl1_day_dmy, fread, skip = 1, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = dmy_hms(V1, tz = "Europe/London"),
                     instrument = "CNT01",
                     ch1 = V2,
                     ch2 = V3,
                     ch3 = V4,
                     ch4 = V5,
                     ch5 = V6,
                     ch6 = V7,
                     ch7 = V8,
                     ch8 = V9)

ptcl2_day_dmy<-lapply(ptcl2_day_dmy, fread, skip = 1, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = dmy_hms(V1, tz = "Europe/London"),
                     instrument = "CNT02",
                     ch1 = V2,
                     ch2 = V3,
                     ch3 = V4,
                     ch4 = V5,
                     ch5 = V6,
                     ch6 = V7,
                     ch7 = V8,
                     ch8 = V9)

ptcl3_day_dmy<-lapply(ptcl3_day_dmy, fread, skip = 1, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = dmy_hms(V1, tz = "Europe/London"),
                     instrument = "CNT03",
                     ch1 = V2,
                     ch2 = V3,
                     ch3 = V4,
                     ch4 = V5,
                     ch5 = V6,
                     ch6 = V7,
                     ch7 = V8,
                     ch8 = V9)

ptcl1_day_mdy<-lapply(ptcl1_day_mdy, fread, skip = 1, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = mdy_hms(V1, tz = "Europe/London"),
                     instrument = "CNT01",
                     ch1 = V2,
                     ch2 = V3,
                     ch3 = V4,
                     ch4 = V5,
                     ch5 = V6,
                     ch6 = V7,
                     ch7 = V8,
                     ch8 = V9)

ptcl2_day_mdy<-lapply(ptcl2_day_mdy, fread, skip = 1, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = mdy_hms(V1, tz = "Europe/London"),
                     instrument = "CNT02",
                     ch1 = V2,
                     ch2 = V3,
                     ch3 = V4,
                     ch4 = V5,
                     ch5 = V6,
                     ch6 = V7,
                     ch7 = V8,
                     ch8 = V9)

ptcl3_day_mdy<-lapply(ptcl3_day_mdy, fread, skip = 1, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = mdy_hms(V1, tz = "Europe/London"),
                     instrument = "CNT03",
                     ch1 = V2,
                     ch2 = V3,
                     ch3 = V4,
                     ch4 = V5,
                     ch5 = V6,
                     ch6 = V7,
                     ch7 = V8,
                     ch8 = V9)

ptcl1_day_hm<-lapply(ptcl1_day_hm, fread, skip = 1, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = dmy_hms(paste0(V1,":00", tz = "Europe/London")),
                     instrument = "CNT01",
                     ch1 = V2,
                     ch2 = V3,
                     ch3 = V4,
                     ch4 = V5,
                     ch5 = V6,
                     ch6 = V7,
                     ch7 = V8,
                     ch8 = V9)

ptcl2_day_hm<-lapply(ptcl2_day_hm, fread, skip = 1, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = dmy_hms(paste0(V1,":00"), tz = "Europe/London"),
                     instrument = "CNT02",
                     ch1 = V2,
                     ch2 = V3,
                     ch3 = V4,
                     ch4 = V5,
                     ch5 = V6,
                     ch6 = V7,
                     ch7 = V8,
                     ch8 = V9)

ptcl3_day_hm<-lapply(ptcl3_day_hm, fread, skip = 1, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = dmy_hms(paste0(V1,":00"), tz = "Europe/London"),
                     instrument = "CNT03",
                     ch1 = V2,
                     ch2 = V3,
                     ch3 = V4,
                     ch4 = V5,
                     ch5 = V6,
                     ch6 = V7,
                     ch7 = V8,
                     ch8 = V9)






##list ptcl files
ptcl_slow<-data_files[grep("PTCL SLOW",data_files)]

ptcl1_slow<-ptcl_slow[grep("cnt1",ptcl_slow)]
ptcl2_slow<-ptcl_slow[grep("cnt2",ptcl_slow)]
ptcl3_slow<-ptcl_slow[grep("cnt3",ptcl_slow)]


ptcl1_slow<-lapply(ptcl1_slow, fread, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = mdy_hms(paste(Date, Time, ":00"), tz = "Europe/London"),
                     instrument = "CNT01",
                     ch1 = `Cnts 1`,
                     ch2 = `Cnts 2`,
                     ch3 = `Cnts 3`,
                     ch4 = `Cnts 4`,
                     ch5 = `Cnts 5`,
                     ch6 = `Cnts 6`,
                     ch7 = `Cnts 7`,
                     ch8 = `Cnts 8`)


ptcl2_slow<-lapply(ptcl2_slow, fread, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = mdy_hms(paste(Date, Time, ":00"), tz = "Europe/London"),
                     instrument = "CNT02",
                     ch1 = `Cnts 1`,
                     ch2 = `Cnts 2`,
                     ch3 = `Cnts 3`,
                     ch4 = `Cnts 4`,
                     ch5 = `Cnts 5`,
                     ch6 = `Cnts 6`,
                     ch7 = `Cnts 7`,
                     ch8 = `Cnts 8`)

ptcl3_slow<-lapply(ptcl3_slow, fread, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = mdy_hms(paste(Date, Time, ":00"), tz = "Europe/London"),
                     instrument = "CNT03",
                     ch1 = `Cnts 1`,
                     ch2 = `Cnts 2`,
                     ch3 = `Cnts 3`,
                     ch4 = `Cnts 4`,
                     ch5 = `Cnts 5`,
                     ch6 = `Cnts 6`,
                     ch7 = `Cnts 7`,
                     ch8 = `Cnts 8`)




##list ptcl files
ptcl_slow_old<-data_files[grep("PTCL SLOLD",data_files)]

ptcl1_slow_old<-ptcl_slow_old[grep("cnt1",ptcl_slow_old)]
ptcl2_slow_old<-ptcl_slow_old[grep("cnt2",ptcl_slow_old)]
ptcl3_slow_old<-ptcl_slow_old[grep("cnt3",ptcl_slow_old)]


ptcl1_slow_old<-lapply(ptcl1_slow_old, fread, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = mdy_hms(paste(Date, Time, ":00"), tz = "Europe/London"),
                  instrument = "CNT01",
                  ch1 = `Cnts 1`,
                  ch2 = `Cnts 2`,
                  ch3 = `Cnts 3`,
                  ch4 = `Cnts 4`,
                  ch5 = `Cnts 5`,
                  ch6 = `Cnts 6`,
                  ch7 = `Cnts 7`,
                  ch8 = `Cnts 8`)


ptcl2_slow_old<-lapply(ptcl2_slow_old, fread, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = mdy_hms(paste(Date, Time, ":00"), tz = "Europe/London"),
                     instrument = "CNT02",
                     ch1 = `Cnts 1`,
                     ch2 = `Cnts 2`,
                     ch3 = `Cnts 3`,
                     ch4 = `Cnts 4`,
                     ch5 = `Cnts 5`,
                     ch6 = `Cnts 6`,
                     ch7 = `Cnts 7`,
                     ch8 = `Cnts 8`)

ptcl3_slow_old<-lapply(ptcl3_slow_old, fread, stringsAsFactors = FALSE)%>%
    dplyr::bind_rows()%>%
    dplyr::transmute(obstime = mdy_hms(paste(Date, Time, ":00"), tz = "Europe/London"),
                     instrument = "CNT03",
                     ch1 = `Cnts 1`,
                     ch2 = `Cnts 2`,
                     ch3 = `Cnts 3`,
                     ch4 = `Cnts 4`,
                     ch5 = `Cnts 5`,
                     ch6 = `Cnts 6`,
                     ch7 = `Cnts 7`,
                     ch8 = `Cnts 8`)





ptcls<- bind_rows(ptcl1_day_dmy,
                    ptcl1_day_mdy,
                    ptcl1_slow,
                  ptcl1_slow_old,
                  ptcl1_day_hm,
                    ptcl2_day_dmy,
                    ptcl2_day_mdy,
                    ptcl2_slow,
                  ptcl2_slow_old,
                  ptcl2_day_hm,
                    ptcl3_day_dmy,
                    ptcl3_day_mdy,
                    ptcl3_slow,
                  ptcl3_slow_old,
                  ptcl3_day_hm)%>%
        dplyr::group_by(instrument)%>%
        dplyr::arrange(obstime)%>%
        dplyr::ungroup()%>%
        dplyr::select(obstime, instrument, ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8)%>%
        dplyr::distinct(obstime, instrument, .keep_all = TRUE)%>%
        tidyr::gather(key = measurement, value = value, -instrument,-obstime)%>%
        dplyr::mutate(col = gsub("NT0","", instrument))







#create database table

ptcls<- ptcls%>%
    ungroup()%>%
    mutate(ind = seq(1,nrow(ptcls)),
           ind = cut(ind, breaks = seq(1, nrow(ptcls), 10000)))%>%
    group_by(ind)%>%
    nest()

##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE ptcls")


##create database table
copy_to(expdat, ptcls$data[[1]], name = "ptcls",temporary = FALSE)


for(i in(2:nrow(ptcls))){
    dbWriteTable(rsqlite_conn, "ptcls", ptcls$data[[i]], append=TRUE, row.names = FALSE)
}

Sys.sleep(60)



rm(ptcl1_day_dmy,
          ptcl1_day_mdy,
          ptcl1_slow,
          ptcl1_slow_old,
          ptcl1_day_hm,
          ptcl2_day_dmy,
          ptcl2_day_mdy,
          ptcl2_slow,
          ptcl2_slow_old,
          ptcl2_day_hm,
          ptcl3_day_dmy,
          ptcl3_day_mdy,
          ptcl3_slow,
          ptcl3_slow_old,
          ptcl3_day_hm,
            ptcls)










#######


# need to create a table of particle data shifted for each run
# create date and filter sequence, shift for sync, join to particle data, drop original particle times, save to db



particle_runs<- tbl(expdat, "run_log")%>%collect()%>%
    select(run_id, col, feed, start, end,flowstart, shift_ptcl_time)%>%
    mutate(start = as_datetime(start),
           end = as_datetime(end),
           flowstart = as_datetime(flowstart),
            flowstartp = flowstart- minutes(shift_ptcl_time),
           endp = end - minutes(shift_ptcl_time) )%>%
    na.omit()%>%
    mutate(obstime = map2(flowstart, end , .f = seq, by = "min"),
           obstimep = map2(flowstartp, endp, .f = seq, by = "min"))%>%
    unnest()%>%
    select(run_id, col,feed, obstime, obstimep, start, end)


##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE IF EXISTS filter_run_particle_index")

##create database index table
copy_to(expdat, particle_runs, name = "filter_run_particle_index",temporary = FALSE)


## table for part

dbSendQuery(rsqlite_conn, "DROP TABLE IF EXISTS filter_run_particles")

dbSendQuery(rsqlite_conn, "CREATE TABLE filter_run_particles AS SELECT
            filter_run_particle_index.col AS col,
            ptcls.instrument AS instrument,
            ptcls.measurement AS measurement,
            filter_run_particle_index.feed AS feed,
            filter_run_particle_index.obstime AS obstime,
            ptcls.value as value,
            filter_run_particle_index.run_id AS run_id,
            filter_run_particle_index.start AS runstart,
            filter_run_particle_index.end AS runend
            FROM ptcls LEFT JOIN filter_run_particle_index ON 
            (ptcls.obstime = filter_run_particle_index.obstimep AND
                ptcls.col = filter_run_particle_index.col)")





####


#make data longform

# transform into long form for querying



ptcls<- tbl(expdat, "filter_run_particles")%>%
    dplyr::select(obstime, instrument, measurement, value)%>%
    collect()%>%
    dplyr::filter(is.na(obstime)==F)

sqrl<- tbl(expdat, "sqrl")%>%collect(n =  Inf)%>%tidyr::gather(key = measurement, value = value, 2:11)%>%
                dplyr::transmute(obstime = obstime,
                                 instrument = "sqrl",
                                 measurement = measurement,
                                 value = value)%>%
    dplyr::filter(is.na(value)==F,
                  is.na(obstime)==F)
    
        



# convert pilot data to longform to help joining and filtering

long_expdat<- bind_rows(sqrl,ptcls)%>%
                dplyr::mutate(col = ifelse(substr(measurement, 1,2) == "C1",
                                           "C1",
                                           ifelse(substr(measurement, 1,2) == "C2",
                                                  "C2",
                                                  ifelse(substr(measurement, 1,2) == "C3",
                                                         "C3", NA))),
                              measurement = ifelse(is.na(col)== TRUE,
                                                   measurement,
                                                   substr(measurement, 3, nchar(measurement))))%>%
    
                mutate(obstime = as_datetime(obstime),
                       obstime = round_date(obstime, unit = "minute"))%>%
    dplyr::group_by(col, instrument, measurement, obstime)%>%
    dplyr::summarise(value = mean(value, na.rm = TRUE))



rm(sqrl,zeta,ptcls,raw_cond,raw_doc,clr_doc)

##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE long_expdat")


long_expdat<- long_expdat%>%
                ungroup()%>%
                mutate(ind = seq(1,nrow(long_expdat)),
                       ind = cut(ind, breaks = seq(1, nrow(long_expdat), 10000)))%>%
                group_by(ind)%>%
                nest()

##create database table
copy_to(expdat, long_expdat$data[[1]], name = "long_expdat",temporary = FALSE)


for(i in(2:nrow(long_expdat))){
    dbWriteTable(rsqlite_conn, "long_expdat", long_expdat$data[[i]], append=TRUE, row.names = FALSE)
}

Sys.sleep(60)

rm(long_expdat)





##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE filter_run_data")


dbSendQuery(rsqlite_conn,
            "CREATE TABLE filter_run_data AS SELECT long_expdat.col AS col, instrument, measurement,
obstime, value, run_id, run_times.start AS runstart, run_times.end AS runend FROM long_expdat LEFT JOIN run_times ON
            long_expdat.col = run_times.col AND
            long_expdat.obstime > run_times.start AND
            long_expdat.obstime < run_times.end")


##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE filter_run_particles")


dbSendQuery(rsqlite_conn,
            "CREATE TABLE filter_run_particles AS SELECT ptcls.col AS col, instrument, measurement,
obstime, value, run_id, run_times.start AS runstart, run_times.end AS runend
            FROM ptcls LEFT JOIN run_times ON
            ptcls.obstime > run_times.ptclstart AND
            ptcls.obstime < run_times.ptclend")







runs<- tbl(expdat, "filter_run_data")%>%
    filter(measurement == "TURB"| measurement == "Q")%>%
    select(run_id,col, measurement, obstime, value)%>%
    collect(n = Inf)%>%
    ungroup()%>%
    filter(is.na(value) == FALSE)%>%
    group_by(measurement, col, run_id)%>%
    filter(length(value) > 7)%>%
    distinct(run_id, col, obstime, .keep_all = TRUE)%>%
    mutate(value = hampel(value, k = 7)$y)%>%
    spread(key = measurement, value = value)

runsptcl<- tbl(expdat, "filter_run_particles")%>%
    collect(n = Inf)%>%
    dplyr::group_by(col, obstime, run_id)%>%
    dplyr::summarise(ptcls = sum(value, na.rm = TRUE))%>%
    dplyr::group_by(col, run_id)%>%
    dplyr::mutate(ptcls = ifelse(ptcls > 1, ptcls, NA))


cross_correlate<- function(df){
    cros_cor<- try(ccf(df$TURB,df$ptcls, na.action = na.pass, plot = FALSE, lag.max = 15))
    return(cros_cor)
}

max_cor_lag<- function(cros_cor){
    bestlag<- try(cros_cor[["lag"]][which.max(cros_cor[["acf"]])])
    bestlag<- ifelse(is.numeric(bestlag), bestlag, NA)
    return(bestlag)
}


apply_ptcl_lag<- function(df, lags){
    series2 <- ifelse(
        lags > 0, lag(df$ptcls, lags), lead(df$ptcls, abs(lags))
    )
    return(series2)
}

rm(runs_join)

runs_join<- left_join(runsptcl, runs)%>%
    group_by(run_id, col)%>%
    dplyr::arrange(obstime)%>%
    nest()%>%
    mutate(cros_cor = map(data, .f = cross_correlate),
           best_lag = map_dbl(cros_cor, .f = max_cor_lag),
           shift_dir = ifelse(best_lag > 0, "lag", "lead"),
           shift = as.integer(abs(best_lag)))%>%
    select(-cros_cor, - data)



runsptcl<- left_join(runsptcl, runs_join)%>%
    mutate(obstime = obstime + minutes(best_lag),
           instrument = "counter",
           measurement = "particles",
           value = ptcls)%>%
    ungroup()%>%
    select(col,instrument,measurement,obstime,value)


dbWriteTable(rsqlite_conn, "long_expdat", runsptcl, append=TRUE, row.names = FALSE)









rm(list = ls())

