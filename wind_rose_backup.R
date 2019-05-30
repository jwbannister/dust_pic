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
sf_1313 <- read.csv("~/Desktop/sf_1313.csv")
sf_1313$datetime <- as.POSIXct(sf_1313$datetime, tz='America/Los_Angeles')
sf_1313$date <- as.Date(sf_1313$datetime %m-% seconds(1), tz='America/Los_Angeles')
sf_1313$hour <- hour(sf_1313$datetime %m-% seconds(1))
hourly_1313 <- sf_1313 %>% group_by(date, hour) %>%
    summarize(sand_flux = sum(sand_flux))

start_date <- as.Date("2018-02-01")
end_date <- as.Date("2018-02-28")

site <- '1150'
query1 <- paste0("SELECT i.deployment, m.datetime, ",
                 "m.ws_10m AS ws, m.wd_10m AS wd ", 
                 "FROM met.met_1hour m ",
                 "JOIN instruments.deployments i USING(deployment_id) ",
                 "WHERE i.deployment='", site, "' ",
                 "AND m.datetime::date BETWEEN '", start_date, "'::date ",
                 "AND '", end_date, "'::date;")
df1 <- query_db("owenslake", query1)
df1$datetime <- as.POSIXct(as.character(df1$datetime), format="%Y-%m-%d %H:%M:%S", 
                           tz='America/Los_Angeles')
df1 <- filter(df1, !is.na(datetime))
df1 <- df1[!duplicated(df1[ , 1:2]), ]
df1$date <- as.Date(df1$datetime %m-% seconds(1), tz='America/Los_Angeles')
df1$hour <- hour(df1$datetime %m-% seconds(1))

df2 <- hourly_1313 %>% left_join(df1, by=c('date', 'hour'))

v <- 'wd'
data_continuity_plot <-df1 %>% ggplot(aes_string(x='datetime', y=v)) +
    geom_point() +
    facet_grid(rows=vars(deployment))

valueseq <- c(1, 2, 3, 4)
p1 <- plot_rose(df2, value='sand_flux', dir='wd', valueseq=valueseq,
                plot.title=paste0("Site 1313 Dust Rose"),
                plot.sub.title=paste0(start_date, " through ", end_date), 
                legend.title="Sand Flux (g/cm^2/hour)")
fl <- paste0("~/Desktop/dustrose_1313.pdf")
pdf(file=fl, width=8, height=10.5, paper="letter")
print(p1)
dev.off()
write.csv(arrange(df1, datetime), "~/Desktop/data_1313.csv", row.names=F)
