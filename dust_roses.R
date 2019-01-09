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
start_date <- as.Date("2015-01-01")
end_date <- as.Date("2018-12-31")
map_view <- 'South' #South or Shoreline

site_list <- map_areas[[map_view]]
site_locs <- pull_site_locations(site_list)

yrs <- seq(year(start_date), year(end_date), 1)
prd <- vector(mode="list", length=length(yrs))
for (i in 1:length(yrs)){
    yr <- yrs[i]
    prd[[i]] <- c(as.Date(paste0(yr, "-01-01")), as.Date(paste0(yr, "-12-31")))
}

refresh_data <- TRUE
if (refresh_data){
    df1 <- data.frame(datetime=c(), deployment=c(), ws=c(), wd=c(), pm10=c())
    for (i in 1:length(prd)){
        print(year(prd[[i]][1]))
        epa_df <- load_hourly_epa_data(prd[[i]][1], prd[[i]][2], site_list)
        if (nrow(epa_df)>0){
            if (date(max(epa_df$datetime)-1) < as.Date(prd[[i]][2])){
                mfile_df <- pull_mfile_data(date(max(epa_df$datetime)-1), 
                                            prd[[i]][2], site_list)
                epa_df <- epa_df %>% filter(date(datetime-1)!=date(max(datetime)-1)) %>%
                    rbind(mfile_df)
            }
        } else{
            epa_df <- pull_mfile_data(prd[[i]][1], prd[[i]][2], site_list)
        }
        epa_df$datetime <- as.character(epa_df$datetime)
        teom_df <- pull_teom_data(prd[[i]][1], prd[[i]][2], site_list)
        dfx <- rbind(teom_df, epa_df)
        df1 <- rbind(df1, dfx)
    }
    save(df1, file=paste0(path, "/dust_pic_data.RData"))
} else{
    print("Using previously stored data...")
    load(paste0(path, "/dust_pic_data.RData"))
}
df1$datetime <- as.POSIXct(df1$datetime, format="%Y-%m-%d %H:%M:%S")
df1 <- filter(df1, !is.na(datetime))
df1 <- df1[!duplicated(df1[ , 1:2]), ]

v <- 'pm10'
data_continuity_plot <-df1 %>% ggplot(aes_string(x='datetime', y=v)) +
    geom_point() +
    facet_grid(rows=vars(deployment))

onlake_dir <- list('LP'=c(126, 176), 
                   'KE'=c(151, 296), 
                   'FR'=c(224, 345), 
                   'SC'=c(227, 33), 
                   'DS'=c(234, 50), 
                   'OL'=c(333, 39), 
                   'ST'=c(349, 230), 
                   'NB'=c(55, 250), 
                   'LT'=c(128, 288), 
                   'MS'=c(157, 333),
                   'T2'=c(0, 360), 
                   'HW'=c(344, 17), 
                   'CJ'=c(347, 12))
df2 <- df1 %>% rowwise() %>%
    mutate(onlake=angle_between(onlake_dir[[deployment]], wd)) %>%
    ungroup()
df2$date <- date(df2$datetime - 1)
df2 <- df2[!duplicated(data.frame(df2$datetime, df2$deployment)), ]
df2 <- filter(df2, pm10 > -15)
df2 <- filter(df2, !(is.na(datetime)))
df2 <- filter(df2, year(datetime - 1) < 2019)

check_days <- df2 %>% group_by(date, deployment) %>% 
    summarize(n=length(date)) %>% arrange(desc(n))

haiwee_daily <- df2 %>% group_by(date, deployment) %>% filter(deployment=='HW') %>%
    do(pm10_24=round(sum(.$pm10, na.rm=T)/sum(!is.na(.$pm10)), 0), 
              pm10_onlake=round(sum(filter(., onlake)$pm10, na.rm=T)/
                                sum(!is.na(.$pm10)), 0), 
              pm10_offlake=round(sum(filter(., !onlake)$pm10, na.rm=T)/
                                 sum(!is.na(.$pm10)), 0)) %>%
    mutate(pm10_24=unlist(pm10_24), pm10_onlake=unlist(pm10_onlake),
           pm10_offlake=unlist(pm10_offlake))
haiwee_daily$year <- year(haiwee_daily$date)

haiwee_sum <- haiwee_daily %>% ungroup() %>% group_by(year) %>%
    summarize(avg_pm10=mean(pm10_24, na.rm=T), 
              max_pm10=max(pm10_24, na.rm=T), 
              max_onlake=max(pm10_onlake, na.rm=T), 
              max_offlake=max(pm10_offlake, na.rm=T), 
              percent_data=length(pm10_24)/365)

valueseq <- c(10, 50, 100, 150)
for (yr in haiwee_sum$year){
    haiwee_continuity_plot <- haiwee_daily %>% filter(year==yr) %>%
        ggplot(aes(x=date, y=pm10_24)) +
        geom_point(color='blue') +
        ylab("24-Hour PM10") + xlab("Date")
    png(paste0("~/owens/coso_pm10/plots/p_", yr, ".png"), 
        height=2, width=6, res=300, units="in")
    print(haiwee_continuity_plot)
    dev.off()
    haiwee_rose <- df2 %>% mutate(year=year(date)) %>%
        filter(year==yr & !(is.na(wd))) %>%
        plot_rose(., value='pm10', dir='wd', valueseq=valueseq,
                  legend.title=bquote('P'*M[10]~'('*mu*'g/'*m^3*')'), 
                  plot.title=bquote('Haiwee Station P'*M[10]~'Rose'), 
                  plot.sub.title=yr, 
                  reverse.bars=T)
    png(paste0("~/owens/coso_pm10/plots/rose_", yr, ".png"), 
        height=6, width=6, res=300, units="in")
    print(haiwee_rose)
    dev.off()
}

daily_sum <- df2 %>% group_by(date, deployment) %>%
    do(pm10_24=avg_pm10(.$pm10)[['avg_pm10']]) %>%
    mutate(pm10_24=unlist(pm10_24))
if (map_view=='South'){
    epa_daily <- load_daily_epa_data(start_date, end_date, site_list)
    names(epa_daily)[3] <- 'pm10_24'
    daily_sum <- rbind(daily_sum, filter(epa_daily, deployment=='CJ' &
        date>max(filter(daily_sum, deployment=='CJ')$date)))
} 
daily_sum$year <- year(daily_sum$date)
    
daily_sum$flag.color <- if_else(daily_sum$pm10_24>150, "red", "black") 
exceeds <- filter(daily_sum, pm10_24>=150 & deployment %in% c('CJ'))
exceed_days <- sort(unique(exceeds$date))
print(exceed_days)

if (map_view=='Shoreline'){
    plot_range <- c('xmin'=404866, 'xmax'=426166, 'ymin'=4015506, 'ymax'=4052866)
    legnd_range <- c(plot_range[2]-4000, plot_range[2], plot_range[4]-8000, plot_range[4])
    logo_range <- c(plot_range[2]-5000, plot_range[2], plot_range[3], plot_range[3]+5000)
    text_range <- c(plot_range[2]-4000, plot_range[2], plot_range[4], plot_range[4]+2000)
} else if (map_view=='South'){
    plot_range <- c('xmin'=400866, 'xmax'=430166, 'ymin'=3988000, 'ymax'=4036000)
    legnd_range <- c(plot_range[2]-4000, plot_range[2], plot_range[3], plot_range[3]+8000)
    logo_range <- c(plot_range[1], plot_range[1]+5000, plot_range[3]-1000, plot_range[3]+3000)
    text_range <- c(plot_range[2]-7000, plot_range[2], plot_range[3]-3000, plot_range[3])
}

legnd <- build_legend(df2)
p2 <- background_map(plot_range, logo_range, legnd_range, legnd)

for (dt in as.character(exceed_days)){
    print(dt)
    roses <- create_roses(df2, dt)
    buffer <- 4000
    p3 <- p2 +
        ggtitle(substitute(paste("Hourly P",  M[10], " Roses for ", d),
                           list(d=format(as.Date(dt), "%m-%d-%Y"))),
                subtitle="CONFIDENTIAL ATTORNEY-CLIENT WORK PRODUCT")
    for (m in names(roses$grob)){
        label_data <- daily_sum %>% 
            filter(deployment==m & date==dt) %>%
            left_join(site_locs, by="deployment")
        p3 <- p3 +
            annotation_custom(roses$grob[[m]],
                              xmin=roses$center[[m]][1] - buffer,
                              xmax=roses$center[[m]][1] + buffer,
                              ymin=roses$center[[m]][2] - buffer,
                              ymax=roses$center[[m]][2] + buffer) +
            geom_label(data=label_data,
                       mapping=aes(x=x, y=y, label=pm10_24),
                       color=label_data$flag.color, size=3, 
                       label.padding=unit(0.15, "lines")) +
            geom_text(data=site_locs, 
                       mapping=aes(x=x, y=y, label=name),
                       size=4, nudge_y=-1000, family='mono', fontface='plain')
    }
    fl <- paste0("~/owens/coso_pm10/plots/", dt, "_map.pdf")
    pdf(file=fl, width=8, height=10.5, paper="letter")
    print(p3)
    dev.off()
}
save.image(paste0(path, "/image.RData"))