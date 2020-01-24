library(tidyverse)
library(lubridate)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)

# pull site locations from database
path <- "~/code/dust_pic/"
mfile_sites <- c("LP"="LonePine", "KE"="Keeler", "NB"="NorthBch", 
                 "LT"="LizardTl", "MS"="MillSite", "SC"="ShellCut", 
                 "DS"="DirtySox", "OL"="Olancha", "ST"="Stanley")
loc_list <- c('DS'='Dirty Socks', 'SC'='Shell Cut', 'ST'='Stanley', 
              'NB'='North Beach', 'LT'='Lizard Tail', 'OL'='Olancha', 
              'MS'='Mill Site', 'LP'='Lone Pine', 'KE'='Keeler') 
query2 <- paste0("SELECT DISTINCT i.deployment, ",
                 "st_y(st_transform(i.geom, 26911)) AS y, ",
                 "st_x(st_transform(i.geom, 26911)) AS x ",
                 "FROM instruments.deployments i ",
                 "WHERE i.deployment IN ('",
                 paste0(loc_list, collapse="', '"), "');")
site_labels <- query_db("owenslake", query2)
site_labels$abrv <- sapply(site_labels$deployment, function(x)
                           names(loc_list)[which(loc_list==x)])
site_labels$site <- sapply(site_labels$abrv, function(x) mfile_sites[x])

# read data from QC'd files provided by Maarten
df_teom <- openxlsx::read.xlsx(paste0(path, "data/",
                                      "TEOM_Model_00-17_Update_121117.xlsx"), 
                               sheet=2, startRow=5, colNames=T)
df_teom <- df_teom[ , c(2:13, 18)]
gather_teom <- df_teom %>% select(-Lookup) %>%
    gather(abrv, teom, c(-date, -hour)) 
gather_teom$date <- as.Date(gather_teom$date, origin='1899-12-30')
# Stanley sites used to be called something else
gather_teom[gather_teom$abrv=='AP', ]$abrv <- 'ST'

df_met <- openxlsx::read.xlsx(paste0(path, "data/",
                                      "TEOM_Model_00-17_Update_121117.xlsx"), 
                               sheet=4, startRow=5, colNames=T)
df_met <- df_met[ , 2:29]
gather_met <- df_met %>% select(-Lookup) %>%
    gather(key, value, c(-date, -hour)) %>%
    mutate(abrv=substr(key, 1, 2), var1=substr(key, 4, 5)) %>%
    select(-key) %>%
    spread(var1, value)
gather_met$date <- as.Date(gather_met$date, origin='1899-12-30')
# Stanley and North Beach sites used to be called something else
gather_met[gather_met$abrv=='DE', ]$abrv <- 'NB'
gather_met[gather_met$abrv=='AP', ]$abrv <- 'ST'
mfile_df <- left_join(gather_teom, gather_met, by=c('date', 'hour', 'abrv'))
colnames(mfile_df) <- tolower(colnames(mfile_df))
mfile_df$datetime <- as.POSIXct(paste0(as.Date(mfile_df$date, 
                                               format="%m/%d/%Y"), " ", 
                                       mfile_df$hour/100, ":00:00")) 
mfile_df[mfile_df==-9999] <- NA
mfile_df <- mfile_df %>% filter(year(datetime %m-% seconds(1))>=2012)

# gap fill Dirty Socks met data from Shell Cut
ds_gap <- filter(mfile_df, is.na(wd) & abrv=='DS')$datetime
gap_data0 <- filter(mfile_df, abrv=='SC' & datetime %in% ds_gap) %>%
    select(datetime, ws, wd)
gap_data0$abrv <- rep('DS', nrow(gap_data0))
mfile_df <- mfile_df %>% left_join(gap_data0, by=c("abrv", "datetime")) %>%
    mutate(ws=coalesce(ws.x, ws.y), wd=coalesce(wd.x, wd.y)) %>%
    select(-ws.x, -ws.y, -wd.x, -wd.y)
# gap fill Shell Cut met data from Dirty Socks
sc_gap <- filter(mfile_df, is.na(wd) & abrv=='SC')$datetime
gap_data0 <- filter(mfile_df, abrv=='DS' & datetime %in% sc_gap) %>%
    select(datetime, ws, wd)
gap_data0$abrv <- rep('SC', nrow(gap_data0))
mfile_df <- mfile_df %>% left_join(gap_data0, by=c("abrv", "datetime")) %>%
    mutate(ws=coalesce(ws.x, ws.y), wd=coalesce(wd.x, wd.y)) %>%
    select(-ws.x, -ws.y, -wd.x, -wd.y)
# gap fill Mill Site met data from Keeler
ms_gap <- filter(mfile_df, is.na(wd) & abrv=='MS')$datetime
gap_data0 <- filter(mfile_df, abrv=='KE' & datetime %in% ms_gap) %>%
    select(datetime, ws, wd)
gap_data0$abrv <- rep('MS', nrow(gap_data0))
mfile_df <- mfile_df %>% left_join(gap_data0, by=c("abrv", "datetime")) %>%
    mutate(ws=coalesce(ws.x, ws.y), wd=coalesce(wd.x, wd.y)) %>%
    select(-ws.x, -ws.y, -wd.x, -wd.y)

# gap fil Mill Site and Keeler met data from 1551
    query4 <- paste0("SELECT m.datetime AS datetime, m.ws_10m AS ws, ",
                     "M.wd_10m AS wd ",
                     "FROM met.met_1hour m ",
                     "JOIN instruments.deployments i ",
                     "ON i.deployment_id=m.deployment_id ",
                     "WHERE i.deployment='1551' ",
                     "AND EXTRACT(year FROM m.datetime)='2015';")
    gap_data2 <- query_db("owenslake", query4)
    gap_data2 <- rbind(gap_data2, gap_data2)
    gap_data2$abrv <- c(rep('MS', nrow(gap_data2)/2), rep('KE', nrow(gap_data2)/2))
    mfile_df <- mfile_df %>% left_join(gap_data2, by=c("abrv", "datetime")) %>%
        mutate(ws=coalesce(ws.x, ws.y), wd=coalesce(wd.x, wd.y)) %>%
        select(-ws.x, -ws.y, -wd.x, -wd.y)

mfile_df <- mfile_df %>% left_join(site_labels, by="abrv")
mfile_df <- mfile_df[!is.na(mfile_df$teom), ]

last_mfile <- as.character(max(as.Date(mfile_df$date)))
# fill with mfile data up to current date
query6 <- paste0("SELECT i.deployment, m.datetime, m.aspd AS ws, ",
                 "m.dir AS wd, m.teom ",
                 "FROM archive.mfile_data m ",
                 "JOIN instruments.deployments i ",
                 "ON m.deployment_id=i.deployment_id ",
                 "WHERE (m.datetime - '1 second'::interval)::date >= ",
                 "('", last_mfile, "'::date + '1 day'::interval) ", 
                 "AND i.deployment IN ('", paste(loc_list, collapse="', '"), "');")
db_mfile <- query_db("owenslake", query6) %>%
    left_join(site_labels, by="deployment")

mfile_df <- rbind(select(filter(mfile_df, date<'2017-07-01'), -date, -hour), 
                  filter(db_mfile, (datetime %m-% seconds(1))>='2017-07-01'))
mfile_df$date <- as.Date(mfile_df$datetime %m-% seconds(1), 
                         tz="America/Los_Angeles")
