library(tidyverse)
library(lubridate)
library(sp)
library(rgdal)
library(scales)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)

start_date <- as.Date('2017-10-01')
end_date <- as.Date('2018-06-30')

epa_xwalk <- c('NB'='0029', 'LT'='0028', 'KE'='1003', 'MS'='0030', 'SC'='0025', 
               'DS'='0022', 'ST'='0026', 'CJ'='1001', 'OL'='0021')
mfile_xwalk <- c("LP"="LonePine", "KE"="Keeler", "NB"="NorthBch", 
                 "LT"="LizardTl", "MS"="MillSite", "SC"="ShellCut", 
                 "DS"="DirtySox", "OL"="Olancha", "ST"="Stanley")
inst_xwalk <- c('DS'='Dirty Socks', 'SC'='Shell Cut', 'ST'='Stanley', 
              'NB'='North Beach', 'LT'='Lizard Tail', 'OL'='Olancha', 
              'MS'='Mill Site', 'LP'='Lone Pine', 'KE'='Keeler') 

teoms = c('SC', 'DS', 'ST', 'OL')

# make first attempt at data through EPA AQS website
# check EPA API service status
readLines("https://aqs.epa.gov/api/serviceAvailable", warn=F)
epa_codes <- c('pm10'='81102', 'ws_sc'='61101', 'wd_sc'='61102',
               'wd_rs'='61104', 'ws_rs'='61103')
url_query <- c()
tick <- 1
for (i in epa_xwalk[teoms]){
    for (j in epa_codes){
        url_query[tick] <- paste0("https://aqs.epa.gov/api/rawData", 
                                  "?user=jbannister@airsci.com", 
                                  "&pw=tealcrane32&format=DMCSV&param=", j, 
                                  "&state=06&county=027",
                                  "&bdate=", format(start_date, '%Y%m%d'),  
                                  "&edate=", format(end_date, '%Y%m%d'))
        tick <- tick + 1
    }
}
epa_df <- data.frame()
for (k in url_query){
    dat <- read_csv(k)
    dat <- dat[dat[1, 1] != 'END OF FILE', ]
    if (nrow(dat)>0) epa_df <- rbind(epa_df, dat)
}
names(epa_df) <- tolower(gsub(" ", "_", names(epa_df)))
epa_df <- filter(epa_df, site_num %in% epa_xwalk[teoms])
epa_df$datetime <- as.POSIXct(paste0(epa_df$date_gmt, " ", epa_df$'24_hour_gmt'),
                              tz="UTC")
epa_df$datetime <- format(epa_df$datetime, format="%Y-%m-%d %H:%M:%S", 
                          tz="America/Los_Angeles", usetz=TRUE)
epa_sum <- epa_df %>% group_by(site_num, parameter_code) %>%
    summarize(start= min(datetime, na.rm=F), 
              end = max(datetime, na.rm=T))

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





