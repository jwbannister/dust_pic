library(tidyverse)
library(circular)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)
path <- "~/code/dust_pic"
source(paste0(path, "/dust_roses_functions.R"))

available_sites <- c('DS'='Dirty Socks', 'SC'='Shell Cut', 'ST'='Stanley', 
    'NB'='North Beach', 'LT'='Lizard Tail', 'OL'='Olancha', 
    'MS'='Mill Site', 'LP'='Lone Pine', 'KE'='Keeler', 
    'HW'='Haiwee', 'CJ'='CoJu-696') 
map_areas <- list('South'=available_sites[c('DS', 'SC', 'ST', 'OL', 'HW', 'CJ')],
                  'Shoreline'=available_sites[c('DS', 'SC', 'ST', 'NB', 'LT',
                                                'OL', 'MS', 'KS')])
start_date <- as.Date("2011-01-01")
end_date <- as.Date("2018-12-31")
map_view <- 'Shoreline' #South or Shoreline

site_list <- map_areas[[map_view]]

district_avg_pm10 <- function(pm_dat){
    out_list <- vector(mode="list", length=2)
    names(out_list) <- c("avg_pm10", "n")
    backfill_pm10 <- (24 - sum(!is.na(pm_dat))) * 20
    out_list[['avg_pm10']] <- 
        round((backfill_pm10 + sum(pm_dat, na.rm=T))/24, 0)
    out_list[['avg_pm10']] <- sapply(out_list[['avg_pm10']], 
                                     function(x) ifelse(is.na(x), 0, x))
    out_list[['n']] <- sum(!is.na(pm_dat))
    return(out_list)
}

df1 <- pull_mfile_data(start_date, end_date, site_list)
df1 %>% group_by(deployment) %>% summarize(min(datetime), max(datetime))

df1$datetime <- as.POSIXct(df1$datetime, format="%Y-%m-%d %H:%M:%S")
df1 <- filter(df1, !is.na(datetime))
df1 <- df1[!duplicated(df1[ , 1:2]), ]

df2 <- df1
df2$date <- date(df2$datetime - 1)
df2 <- df2[!duplicated(data.frame(df2$datetime, df2$deployment)), ]
df2 <- filter(df2, pm10 > -15)
df2 %>% group_by(deployment) %>% summarize(min(date), max(date))

daily_sum <- df2 %>% group_by(date, deployment) %>%
    do(pm10_24=district_avg_pm10(.$pm10)[['avg_pm10']]) %>%
    mutate(pm10_24=unlist(pm10_24))
pm10_data_range <- daily_sum %>% group_by(deployment) %>% 
    summarize(min(date), max(date), max(pm10_24))
