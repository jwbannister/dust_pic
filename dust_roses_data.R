library(tidyverse)
library(lubridate)
library(sp)
library(rgdal)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)
path <- "~/code/dust_pic"
start_date <- as.Date('2016-01-01')
end_date <- Sys.Date()

#epa_sites <- c('Coso Junction'=1001, 'Olancha'=0021, 'Dirty Socks'=0022, 
#               'Stanley'=0026, 'Shell Cut'=0025, 'Mill Site'=0030, 
#               'Keeler'=1003, 'Lizard Tail'=0028, 'North Beach'=0029, 
#               'Lone Pine'=0004)
#epa_codes <- c('pm10'=81102, 'ws'=61103, 'wd'=61104)
#
#data_files <- list.files("./data", full.names=TRUE)
#
#epa_locs <- read.csv("./data/hourly_81102_2016.csv") %>% 
#    filter(Site.Num %in% epa_sites & State.Code==6 & County.Code==27) %>%
#    select('site_num'=Site.Num, 'latitude'=Latitude, 'longitude'=Longitude) %>% 
#    distinct()
#
#pm10_files <- data_files[grepl("81102", data_files)]
#pm10_df <- data.frame()
#for (fl in pm10_files){
#    print(fl)
#    df1 <- read.csv(fl)
#    names(df1) <- tolower(gsub("[.]", "_", names(df1)))
#    df2 <- df1 %>% 
#        filter(site_num %in% epa_sites & state_code==6 & county_code==27) %>%
#        select(site_num, date_gmt, time_gmt, 'pm10'=sample_measurement)
#    pm10_df <- rbind(pm10_df, df2)
#}
#
#wind_files <- data_files[grepl("WIND", data_files)]
#wind_df <- data.frame()
#for (fl in wind_files){
#    print(fl)
#    df1 <- read.csv(fl)
#    names(df1) <- tolower(gsub("[.]", "_", names(df1)))
#    df2 <- df1 %>% 
#        filter(site_num %in% epa_sites & state_code==6 & county_code==27) %>%
#        select(site_num, date_gmt, time_gmt, 'value'=sample_measurement, 
#               'code'=parameter_code)
#    wind_df <- rbind(wind_df, df2)
#}
#wind_df[wind_df$code==61103, ]$value <- 0.5144* wind_df[wind_df$code==61103, ]$value
#wind_df$key <- sapply(wind_df$code, function(x) ifelse(x==61103, 'ws', 'wd'))
#a <- wind_df %>% select(-code) %>% spread(key, value)

mfile_sites = c('Olancha', 'DirtySox', 'ShellCut', 'Stanley')

mfile_query <- paste0("SELECT datetime, site AS deployment, aspd AS ws, ", 
                      "dir AS wd, teom AS pm10 FROM archive.mfile_data ",
                      "WHERE datetime::date > '", start_date, "'::date ",
                      "AND site IN ('", paste(mfile_sites, collapse="', '"), "');")
mfile_df <- query_db("owenslake", mfile_query)

teom_old_query <- paste0("SELECT datetime, deployment, pm10_std_avg AS pm10 ", 
                        "FROM teom.avg_1hour_validated ", 
                        "WHERE datetime::date > '", start_date, "'::date ",
                        "AND deployment IN ('Haiwee', 'T2-1') ",
                        "AND NOT invalid")
teom_old_df <- query_db("owenslake", teom_old_query)

teom_new_query <- paste0("SELECT datetime, deployment, pm10_1hour_stp AS pm10 ", 
                        "FROM teom.hourly_validated ", 
                        "WHERE datetime::date > '", max(teom_old_df$datetime), "'::date ",
                        "AND deployment IN ('Haiwee', 'T2-1') ",
                        "AND NOT invalid")
teom_new_df <- query_db("owenslake", teom_new_query)

met_query <- paste0("SELECT m.datetime, i.deployment, m.ws_wvc AS ws, ",
                    "m.wd_wvc AS wd FROM teom.teom_analog_1hour m "
                    "JOIN instruments.deployments i ",
                    "ON i.deployment_id=m.deployment_id "
                    "WHERE datetime::date > '", start_date, "'::date ",
                    "AND deployment IN ('Haiwee', 'T2-1');")
met_df <- query_df("owenslake", met_query)




query1 <- paste0("SELECT * FROM teom.avg_1hour_validated_haiwee ",
                 "WHERE NOT invalid")
df1 <- query_db("owenslake", query1) %>%
    select(deployment, datetime, 'pm10'=pm10_std_avg)

query2 <- paste0("SELECT * FROM teom.hourly_validated_haiwee ", 
                 "WHERE NOT invalid")
df2 <- query_db("owenslake", query2) %>% 
    select(deployment, datetime, 'pm10'=pm10_1hour_stp)

df3 <- rbind(df1, df2)

query_met <- paste0("SELECT i.deployment, m.datetime, m.ws_2m AS ws, m.wd_6m ", 
                    "FROM met.met_1hour m JOIN instruments.deployments i ",
                    "ON m.deployment_id=i.deployment_id ", 
                    "WHERE i.deployment='Haiwee';")
met_df <- query_db("owenslake", query_met)

mfile_sites = c('Olancha'='Olancha', 'Dirty Socks'='DirtySox', 
                'Stanley'='Stanley', 'Shell Cut'='ShellCut')
query4 <- paste0("SELECT * FROM archive.mfile_data ",
                 "WHERE site in (", 
                 paste0(names(mfile_sites), collapse="', '"), "');")
df4 <- query_db("owenslake", query4) %>%
    select('deployment'=site, datetime, 'pm10'=teom, 'ws'=aspd, 'wd'=dir)


df1$site_num <- as.integer(df1$site_num)
df2 <- select(df1, site_num, aqs_parameter_desc, 
              date_gmt, hour_gmt='24_hour_gmt', sample_measurement, 
              units_of_measure, sample_duration, sample_frequency)
df3 <- filter(df2, site_num %in% epa_sites)
query1 <- paste0("SELECT DISTINCT i.deployment, ",
                 "st_y(st_transform(i.geom, 26911)) AS y, ",
                 "st_x(st_transform(i.geom, 26911)) AS x ",
                 "FROM instruments.deployments i ",
                 "WHERE i.deployment IN ('", 
                 paste0(names(epa_sites), collapse="', '"), "');")
xy = query_db("owenslake", query1)
xy = rbind(xy, data.frame('deployment'='Coso Junction', 'y'=3989840, 
                          'x'=414978))
xy$site_num = c()
for (i in c(1:nrow(xy))){
    xy$site_num[i] = epa_sites[xy$deployment[i]]
}
df4 <- left_join(df3, xy, by="site_num") %>%
    mutate(datetime = ymd_hms(paste(date_gmt, hour_gmt))) %>%
    select(deployment, y, x, datetime, pm10=sample_measurement) %>%
    filter(datetime %m-% seconds(1) >= start_date, 
           datetime %m-% seconds(1) < end_date)

airsci_sites <- c('T2-1', 'Haiwee')
query2 <- paste0("SELECT DISTINCT i.deployment, ",
                 "st_y(st_transform(i.geom, 26911)) AS y, ",
                 "st_x(st_transform(i.geom, 26911)) AS x, ",
                 "t.datetime, t.pm10_1hour_stp AS pm10 ", 
                 "FROM teom.teom_30min t ",
                 "JOIN instruments.deployments i ",
                 "ON t.deployment_id=i.deployment_ID ",
                 "WHERE i.deployment IN ('", 
                 paste0(airsci_sites, collapse="', '"), "') ", 
                 "AND (t.datetime - '1 second'::interval)::date ",
                 "BETWEEN '", start_date, "'::date AND '", end_date, "'::date ", 
                 "AND EXTRACT(MINUTE FROM t.datetime)=0;")
teom_30min <- query_db("owenslake", query2)
teom_30min$datetime <- teom_30min$datetime %m+% hours(8)
tz(teom_30min$datetime) <- 'UTC'

backfill_end <- min(teom_30min$datetime, na.rm=T) %m-% hours(9)
query3 <- paste0("SELECT DISTINCT i.deployment, ",
                 "st_y(st_transform(i.geom, 26911)) AS y, ",
                 "st_x(st_transform(i.geom, 26911)) AS x, ",
                 "t.datetime, t.pm10_std AS pm10 ", 
                 "FROM teom.teom_1min t ",
                 "JOIN instruments.deployments i ",
                 "ON t.deployment_id=i.deployment_ID ",
                 "WHERE i.deployment IN ('", 
                 paste0(airsci_sites, collapse="', '"), "') ", 
                 "AND (t.datetime - '1 second'::interval)::date ",
                 "> '", start_date, "'::date ",
                 "AND t.datetime < '", backfill_end, "';") 
teom_1min <- query_db("owenslake", query3)
teom_1min$datetime <- teom_1min$datetime %m+% hours(8)
tz(teom_1min$datetime) <- 'UTC'

