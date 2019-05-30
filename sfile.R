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

fl_list <- list.files("~/data/s_files/archive")
files_17 <- fl_list[sapply(fl_list, function(x) grepl("17", substr(x, 1, 2)))]
files_18 <- fl_list[sapply(fl_list, function(x) grepl("18", substr(x, 1, 2)))]
fls <- c(files_17, files_18)
template <- read.csv(paste0("~/data/s_files/archive/", fls[1]))
dfx <- data.frame(matrix(nrow=0, ncol=ncol(template)-3))
names(dfx) <- c('site', 'date', paste0('x', seq(0, 23)), 'volts')
for (i in fls){
    print(i)
    tmp <- read.csv(paste0("~/data/s_files/archive/", i))
    tmp1 <- filter(tmp, Site %in% c('9534', '9818')) %>%
        select(-Msng_Hrs, -Ratio, -Hr_Total)
    if (nrow(tmp1)>0){
        names(tmp1) <- names(df1)
        dfx <- rbind(dfx, tmp1)
    }
}
gb_sites <- filter(dfx, volts=='ok') %>% select(-volts)
gb_sites <- gb_sites %>% gather(key='hour', value='flux', -site, -date)
gb_sites$hour <- as.integer(gsub("x", "", gb_sites$hour))
write.csv(gb_sites, "~/Desktop/district_fluxes.csv", row.names=F)
daily_gb <- gb_sites %>% ungroup() %>%
    group_by(date, site) %>% summarize(sand_flux=sum(flux))

sf_1313 <- read.csv("~/Desktop/sf_1313.csv")
sf_1313$datetime <- as.POSIXct(sf_1313$datetime, tz='America/Los_Angeles')
sf_1313$date <- as.Date(sf_1313$datetime %m-% seconds(1), tz='America/Los_Angeles')
sf_1313$hour <- hour(sf_1313$datetime %m-% seconds(1))
hourly_1313 <- sf_1313 %>% group_by(date, hour) %>%
    summarize(sand_flux = sum(sand_flux))
daily_1313 <- hourly_1313 %>% ungroup() %>%
    group_by(date) %>% summarize(sand_flux=sum(sand_flux))

start_date <- as.Date("2017-01-01")
end_date <- as.Date("2018-12-31")

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

for (d in as.character(filter(daily_1313, sand_flux>1)$date)){
    tmp <- filter(df2, date==d)
    total <- round(filter(daily_1313, date==d)$sand_flux, 0)
    valueseq <- c(1, 2, 3, 4)
    p1 <- plot_rose(tmp, value='sand_flux', dir='wd', valueseq=valueseq,
                    plot.title=paste0("Site 1313 Dust Rose"),
                    plot.sub.title=paste0(d, ", Total Flux = ", total, " g/cm^2/day"), 
                    legend.title="Sand Flux (g/cm^2/hour)")
    fl <- paste0("~/Desktop/dustrose_1313_", d, ".pdf")
    pdf(file=fl, width=8, height=10.5, paper="letter")
    print(p1)
    dev.off()
}

high_gb <- arrange(daily_gb, desc(sand_flux))
for (i in seq(1, 10, 1)){
    gb_row <- high_gb[i, ]
    d <- gb_row$date
    d_char <- gsub("/", "-", d)
    s <- gb_row$site
    total <- round(gb_row$sand_flux, 0)
    tmp <- filter(gb_sites, site==s & date==d)
    tmp$date <- as.Date(tmp$date, format="%m/%d/%Y")
    tmp <- tmp %>% left_join(df1, by=c("date", "hour"))
    valueseq <- c(1, 2, 3, 4)
    p1 <- plot_rose(tmp, value='flux', dir='wd', valueseq=valueseq,
                    plot.title=paste0("Site ", s, " Dust Rose"),
                    plot.sub.title=paste0(d_char, ", Total Flux = ", total, 
                                          " g/cm^2/day"), 
                    legend.title="Sand Flux (g/cm^2/hour)")
    fl <- paste0("~/Desktop/dustrose_", s, "_", d_char, ".pdf")
    pdf(file=fl, width=8, height=10.5, paper="letter")
    print(p1)
    dev.off()
}
