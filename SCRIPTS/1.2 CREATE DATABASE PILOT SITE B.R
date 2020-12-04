# Script to update database


# load packages
library(tidyverse)
library(lubridate)
library(RSQLite)
library(DBI)
library(data.table)




# Script to create database


# create database
expdat <- src_sqlite("DATA/PilotSiteB.sqlite3", create = T)

rsqlite_conn<-dbConnect(RSQLite::SQLite(),  dbname= "DATA/PilotSiteB.sqlite3")



src_tbls(expdat)

#list all data files
data_files<-list.files(path = "DATA/SITE B", full.names = TRUE, recursive = TRUE)



# drop database table
dbSendQuery(rsqlite_conn,
            "DROP TABLE run_log")

# Filter run log
## Pilot run log
run_log<- data_files[grep("filterrunlog", data_files)]%>%
          read.csv(.)

##create database table
copy_to(expdat, run_log, name = "run_log",temporary = FALSE)


#Filter run times

##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE run_times")

## prep data
run_times<-run_log%>%dplyr::group_by(run_id, col)%>%
          dplyr::mutate(start = dmy_hms(paste0(start_date, " ", start_time,":00")),
                        end = dmy_hms(paste0(end_date, " ", end_time,":00")))%>%
          dplyr::select(run_id, col,start,end, run_issue)


## copy table
copy_to(expdat, run_times, name = "run_times",temporary = FALSE)



# Squirrel data logger data

# Get the files names
sqrl<-data_files[grep("SQUIRREL",data_files)]

sqrl_file0<- sqrl[grep("#00", sqrl)]
sqrl_file<- sqrl[-grep("#00", sqrl)]



## First apply read.csv, then rbind
sqrl_file <- do.call(rbind, lapply(sqrl_file, read.delim, stringsAsFactors = FALSE,  header = TRUE))%>%
          dplyr::transmute( obstime = dmy_hms(Date.Time),
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
          dplyr::transmute( obstime = dmy_hms(Date.Time),
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


##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE sqrl")


#create database table
copy_to(expdat, sqrl, name = "sqrl",temporary = FALSE)

write.csv(x = sqrl, "DATA/TIDY DATA/sqrl.csv")


# particle  data


##list ptcl files
ptcl_slow<-data_files[grep("PTCL",data_files)]

ptcl1_slow<-ptcl_slow[grep("C1",ptcl_slow)]
ptcl2_slow<-ptcl_slow[grep("C2",ptcl_slow)]
ptcl3_slow<-ptcl_slow[grep("C3",ptcl_slow)]


ptcl1_slow<-lapply(ptcl1_slow, fread, stringsAsFactors = FALSE)%>%
          dplyr::bind_rows()%>%
          dplyr::transmute(obstime = mdy_hms(paste(Date, Time, ":00")),
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
          dplyr::transmute(obstime = mdy_hms(paste(Date, Time, ":00")),
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
          dplyr::transmute(obstime = mdy_hms(paste(Date, Time, ":00")),
                           instrument = "CNT03",
                           ch1 = `Cnts 1`,
                           ch2 = `Cnts 2`,
                           ch3 = `Cnts 3`,
                           ch4 = `Cnts 4`,
                           ch5 = `Cnts 5`,
                           ch6 = `Cnts 6`,
                           ch7 = `Cnts 7`,
                           ch8 = `Cnts 8`)


ptcls<- bind_rows(ptcl1_slow,
                  ptcl2_slow,
                  ptcl3_slow)%>%
          dplyr::group_by(instrument)%>%
          dplyr::arrange(obstime)%>%
          dplyr::ungroup()%>%
          dplyr::distinct()


##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE ptcls")


#create database table
copy_to(expdat, ptcls, name = "ptcls",temporary = FALSE)

write.csv(x = ptcls, "DATA/TIDY DATA/ptcls.csv")




## Online flow cytometry data

dbSendQuery(rsqlite_conn,
            "DROP TABLE oncyt")

colnames(oncyt)

oncyt<- read.csv(file = "DATA/ONLINE_FCM.csv")%>%
          dplyr::mutate(obstime = ymd_hms(paste(Date,Time)))%>%
          dplyr::transmute(obstime = obstime,
                           col = col,
                           counts_ul = Concentration..Events.uL.,
                           p1_counts_ul = P1EventsPerul)


#create database table
copy_to(expdat, oncyt, name = "oncyt",temporary = FALSE)








#make data longform

# transform into long form for querying

ptcls<- ptcls%>%dplyr::select(obstime, instrument, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8)%>%
          tidyr::gather(key = measurement, value = value, -instrument,-obstime)


ptcls<- ptcls%>%dplyr::mutate(measurement = paste0(gsub("NT0","", instrument),measurement))


sqrl<-sqrl%>%tidyr::gather(key = measurement, value = value, 2:11)%>%
          dplyr::transmute(obstime = obstime,
                           instrument = "sqrl",
                           measurement = measurement,
                           value = value)


oncyt<-oncyt%>%tidyr::gather(key = measurement, value = value,-obstime,-col)%>%
          mutate(instrument = "oncyt",
                 measurement = paste0(col,measurement),
                 obstime = floor_date(obstime, unit = "minute"))



# convert pilot data to longform to help joining and filtering

long_expdat<- bind_rows(sqrl, ptcls)%>%
          dplyr::mutate(col = ifelse(substr(measurement, 1,2) == "C1",
                                     "C1",
                                     ifelse(substr(measurement, 1,2) == "C2",
                                            "C2",
                                            ifelse(substr(measurement, 1,2) == "C3",
                                                   "C3", NA))),
                        measurement = ifelse(is.na(col)== TRUE,
                                             measurement,
                                             substr(measurement, 3, nchar(measurement))),
                        obstime = floor_date(obstime, unit = "minute"))%>%
          dplyr::group_by(col, instrument, measurement, obstime)%>%
          dplyr::summarise(value = mean(value, na.rm = TRUE))

long_expdat<-bind_rows(long_expdat,oncyt)


##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE long_expdat")

##create database table
copy_to(expdat, long_expdat, name = "long_expdat",temporary = FALSE)




# send query to join runs and data

##drop table
dbSendQuery(rsqlite_conn,
            "DROP TABLE filter_run_data")


dbSendQuery(rsqlite_conn,
            "CREATE TABLE filter_run_data AS SELECT * FROM long_expdat LEFT JOIN run_times ON
            long_expdat.obstime > run_times.start AND
            long_expdat.obstime < run_times.end AND
            long_expdat.col = run_times.col")

rm(list = ls())
