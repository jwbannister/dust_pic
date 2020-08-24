library(tidyverse)
library(lubridate)
#df1 <- read_csv("~/Desktop/dump.csv")
# pull in df1 from shoreline_roses.R
days = c('3-4-17', '3-5-17', '3-18-17', '3-24-17', '3-27-17', '3-30-17', '3-31-17', '4-6-17', '4-7-17',
         '4-12-17', '4-13-17', '6-11-17', '8-31-17', '9-21-17', '10-8-17', '11-27-17', '12-16-17',
         '12-20-17', '2-10-18', '2-11-18', '2-18-18', '3-1-18', '3-2-18',  '3-27-18', '4-11-18',
         '4-16-18', '5-11-18', '12-5-18', '1-6-19', '2-12-19', '2-26-19', '4-9-19', '10-27-19',
         '1-16-20', '1-29-20','2-2-20', '2-3-20','5-3-20')
site_list <- c('Olancha', 'Dirty Socks', 'Stanley', 'Shell Cut', 'Mill Site', 'Keeler',
               'Lizard Tail', 'North Beach', 'Lone Pine')
val = 'pm10'
for (d in days){
    day = as.Date(d, "%m-%d-%y")
    df2 <- df1 %>% filter(deployment %in% site_list) %>%
        mutate(date1 = date(datetime %m-% seconds(1))) %>%
        filter(date1 == day) %>%
        select(datetime, deployment, val) %>%
        spread(deployment, val)
    for (s in site_list){
        if (s %in% names(df2)){
            next
        } else{
            df2[[s]] <- rep(NA, nrow(df2))
        }
    }
    df3 <- df2 %>% select(datetime, 'Olancha', 'Dirty Socks', 'Stanley', 'Shell Cut', 'Mill Site',
                          'Keeler', 'Lizard Tail', 'North Beach', 'Lone Pine')
    write.csv(df3, paste0("~/Desktop/tim/", d, "_", val, ".csv"), row.names=FALSE)
}
    
