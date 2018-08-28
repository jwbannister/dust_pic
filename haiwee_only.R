library(tidyverse)
library(lubridate)
library(sp)
library(rgdal)
library(scales)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)
path <- "~/code/misc/haiwee/"
start_date <- as.Date('2013-04-23') # install date for Haiwee monitor
end_date <- as.Date('2018-06-30')

query1 <- paste0("SELECT t.deployment, ",
                 "t.datetime, t.pm10_1hour_stp AS pm10 ", 
                 "FROM teom.hourly_validated_haiwee t ",
                 "WHERE (t.datetime - '1 second'::interval)::date ",
                 "BETWEEN '", start_date, "'::date AND '", end_date, "'::date ", 
                 "AND NOT t.invalid;")
df1 <- query_db("owenslake", query1)
attributes(df1$datetime)$tzone <- 'America/Los_Angeles'

backfill_end <- as.Date(min(df1$datetime, na.rm=T))
query2 <- paste0("SELECT t.deployment, ",
                 "t.datetime, t.pm10_std_avg AS pm10 ", 
                 "FROM teom.avg_1hour_validated_haiwee t ",
                 "WHERE (t.datetime - '1 second'::interval)::date ",
                 "BETWEEN '", start_date, "'::date AND '", backfill_end, "'::date ", 
                 "AND NOT t.invalid") 
df2 <- query_db("owenslake", query2)
attributes(df2$datetime)$tzone <- 'America/Los_Angeles'

df3 <- rbind(df1, df2) %>% filter(pm10>-15)
full_seq <- seq(min(df3$datetime), max(df3$datetime), by="hour", 
                tz="America/Los_Angeles")
df4 <- data.frame(deployment = rep("Haiwee", length(full_seq)), 
                  datetime = full_seq) %>%
    left_join(df3, by=c("deployment", "datetime"))
df4$date <- as.Date(df4$datetime %m-% seconds(1), tz="America/Los_Angeles")

daily_sum <- df4 %>% group_by(date) %>%
    do(pm10_24=round(sum(.$pm10, na.rm=T)/sum(!is.na(.$pm10)), 0), 
              n=sum(!is.na(.$pm10))) 
for (i in 2:3){
    daily_sum[ , i] <- unlist(daily_sum[ , i])
}
exceeds <- filter(daily_sum, pm10_24>=150)
for (j in 1:nrow(exceeds)){
    # calculate pm10 average with District backfill procedure
    exceeds$pm10_24[j] <- round(((exceeds$pm10_24[j]*exceeds$n[j])+
                           (20*(24-exceeds$n[j])))/24, 0)
}

# read in met data for exceedance days
df0 <- read_csv(paste0(path, "OL_Select_met.csv"))
df0$hour <- formatC(df0$hour/100, width=2, format="d", flag="0")
df0$datetime <- as.POSIXct(paste0(df0$date, " ", df0$hour, ":00"), 
                           format="%m/%d/%Y %H:%M")
plt_df <- df4 %>% filter(date %in% exceeds$date) %>%
    left_join(select(df0, datetime, ws_10m, wd_10m), by="datetime") %>%
    arrange(datetime)

#write.csv(exceeds, 
#          file=paste0(path, "haiwee_exceedance_days.csv"), row.names=F)
#write.csv(df4, 
#          file=paste0(path, "haiwee_hourly_data.csv"), row.names=F)

for (i in 1:nrow(exceeds)){
    dt <- exceeds$date[i]
    tmp <- plt_df %>% filter(date==exceeds$date[i], !is.na(pm10))
    ptitle <- paste0("Haiwee PM10 Rose, ", format(dt, "%m-%d-%Y"))
    psub <- paste0("24-hour PM10 = ", exceeds[exceeds$date==dt, "pm10_24"])
    ltitle=bquote('P'*M[10]~'('*mu*'g/'*m^3*')')
    p1 <- plot_rose(tmp, 'pm10', 'wd_10m', plot.title=ptitle, plot.sub.title=psub, 
                    legend.title=ltitle)
    png(filename=paste0(path, "/exceed_roses/haiwee_", dt, "_rose.png"), 
        width=6, height=6, units="in", res=300)
    print(p1)
    dev.off()
}
