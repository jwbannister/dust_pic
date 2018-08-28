library(tidyverse)
library(lubridate)
library(sp)
library(rgdal)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)
path <- "~/code/misc/haiwee/"
start_date <- as.Date('2016-01-01')
end_date <- Sys.Date()
epa_url <- paste0("https://aqs.epa.gov/api/rawData?user=jbannister@airsci.com", 
                  "&pw=tealcrane32&format=DMCSV&param=81102&state=06&county=027",
                  "&bdate=", format(start_date, '%Y%m%d'),  
                  "&edate=", format(end_date, '%Y%m%d'))
df1 <- read_csv(epa_url)
names(df1) <- tolower(gsub(" ", "_", names(df1)))
epa_sites = c('Coso Junction'=1001, 'Olancha'=21, 'Dirty Socks'=22, 
              'Stanley'=26, 'Shell Cut'=25)
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

