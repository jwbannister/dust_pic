library(tidyverse)
library(lubridate)
library(ggplot2)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)
path <- "~/code/dust_pic/"
source(paste0(path, "/dust_roses_functions.R"))

start_date <- as.Date("2012-01-01")
end_date <- as.Date("2018-01-01")

dfx <- epa_api_pm10_hourly(start_date, end_date)

df_1 <- dfx %>% filter(sample.duration=='1 HOUR') %>% 
    group_by(site, date) %>%
    do(pm10_24=round(sum(.$pm10, na.rm=T)/sum(!is.na(.$pm10)), 0), 
       n=sum(!is.na(.$pm10))) %>%
    mutate(pm10_24=unlist(pm10_24), n=unlist(n)) %>%
    filter(n >= 18) %>%
    select(-n)

df_24 <- dfx %>% filter(sample.duration == '24 HOUR') %>% 
    select(site, date, pm10_24 = pm10) %>% 
    rbind(., df_1) %>%
    filter(between(date, start_date, end_date - 1)) %>%
    mutate(year = year(date))

df_years <- df_24 %>% group_by(date, year) %>%
    filter(pm10_24 >= 150) %>%
    summarize(exceedances = length(pm10_24), max_pm10 = max(pm10_24)) %>%
    ungroup() %>% group_by(year) %>%
    summarize(exceedances = length(max_pm10), max_pm10 = max(max_pm10)) 

df_site_days <- df_24 %>% filter(pm10_24 >= 150) %>%
    arrange(site, date)

write.csv(df_years, "~/Desktop/shoreline_exceed_yearly_summary.csv", row.names=F)
write.csv(df_site_days, "~/Desktop/shoreline_exceeds.csv", row.names=F)


