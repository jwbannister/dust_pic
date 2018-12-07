library(tidyverse)
library(circular)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(ggmap)
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

refresh_data <- FALSE
if (refresh_data){
    df1 <- data.frame(datetime=c(), deployment=c(), ws=c(), wd=c(), pm10=c())
    for (i in 1:length(prd)){
        print(year(prd[[i]][1]))
        epa_df <- load_epa_data(prd[[i]][1], prd[[i]][2], site_list)
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
        teom_df <- pull_teom_data(prd[[i]][1], prd[[i]][2], site_list)
        dfx <- rbind(teom_df, epa_df)
        df1 <- rbind(df1, dfx)
    }
    save(df1, file=paste0(path, "/dust_pic_data.RData"))
} else{
    print("Using previously stored data...")
    load(paste0(path, "/dust_pic_data.RData"))
}

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

check_days <- df2 %>% group_by(date, deployment) %>% 
    summarize(n=length(date)) %>% arrange(desc(n))

daily_sum <- df2 %>% group_by(date, deployment) %>%
    do(pm10_24=avg_pm10(.$pm10)[['avg_pm10']], n=avg_pm10(.$pm10)[['n']], 
              pm10_onlake=avg_pm10(filter(., onlake)$pm10)[['avg_pm10']], 
              n_onlake=avg_pm10(filter(., onlake)$pm10)[['n']], 
              pm10_offlake=avg_pm10(filter(., !onlake)$pm10)[['avg_pm10']], 
              n_offlake=avg_pm10(filter(., !onlake)$pm10)[['n']]) %>%
    mutate(pm10_24=unlist(pm10_24), n=unlist(n), 
           pm10_onlake=unlist(pm10_onlake), n_onlake=unlist(n_onlake), 
           pm10_offlake=unlist(pm10_offlake), n_offlake=unlist(n_offlake))
daily_sum$flag.color <- if_else(daily_sum$pm10_24>150, "red", "black") 

if (map_view=='South'){
    exceeds <- filter(daily_sum, pm10_24>=150 & deployment %in% c('OL', 'HW', 'CJ'))
} else if (map_view=='Shoreline'){
    exceeds <- filter(daily_sum, pm10_24>=150 & !(deployment %in% c('HW', 'CJ')))
}
    
exceed_days <- sort(unique(exceeds$date))
valueseq <- c(10, 50, 100, 150)
legend.data <- df2 %>% filter(deployment==df2$deployment[1])
legend.data$pm10[1] <- NA
legend.plot <- legend.data %>%
    plot_rose(., value='pm10', dir='wd', valueseq=valueseq,
              legend.title=bquote('P'*M[10]~'('*mu*'g/'*m^3*')'), 
              reverse.bars=T)
legnd <- g_legend(legend.plot)

shoreline <- pull_shoreline_polygon()

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

photo_back <- FALSE
if (photo_back){
    google_key = Sys.getenv("OWENS_MAPS_KEY")
    background <- photo_background(plot_range[1], plot_range[2], plot_range[3], 
                                   plot_range[4], zone='11N', key=google_key)
    p1 <- background + 
        geom_path(data=shoreline, mapping=aes(x=x, y=y, group=area_name))
} else{
    p1 <- ggplot(data=shoreline, mapping=aes(x=x, y=y)) +
        geom_path(mapping=aes(group=area_name))
}
p1 <- p1 +
    xlim(plot_range[1], plot_range[2]) +
    ylim(plot_range[3], plot_range[4]) +
    coord_equal() +
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          plot.title=element_text(size=12, hjust=0.5),
          plot.subtitle=element_text(hjust=0.5),
          panel.border=element_rect(color="black", fill="transparent"))
p2 <- p1 +
    annotation_custom(logo_grob, xmin=logo_range[1], xmax=logo_range[2],
                      ymin=logo_range[3], ymax=logo_range[4]) +
    annotation_custom(legnd, xmin=legnd_range[1], xmax=legnd_range[2],
                      ymin=legnd_range[3], ymax=legnd_range[4]) +
    annotation_custom(grid::textGrob(expression(paste("Site Label = 24-hour P", M[10])),
                                                gp=gpar(fontsize=10)),
                          xmin=text_range[1], xmax=text_range[2],
                          ymin=text_range[3], ymax=text_range[4])

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
    fl <- paste0(path, "/pdfs/", format(as.Date(dt), "%Y-%m-%d"),
                 map_view, "_exceedance.pdf")
    pdf(file=fl, width=8, height=10.5, paper="letter")
    print(p3)
    dev.off()
}

