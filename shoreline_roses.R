library(tidyverse)
library(circular)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)
path <- tempdir()
source("/Users/john/code/dust_pic/rose_map_functions.R")

mfile_names <- c('LP'='LonePine', 'KE'='Keeler', 'NB'='NorthBch', 
                 'LT'='LizardTl', 'MS'='MillSite', 'SC'='ShellCut', 
                 'DS'='DirtySox', 'OL'='Olancha', 'ST'='Stanley')
valid_names <- c('DS'='Dirty Socks', 'SC'='Shell Cut', 'ST'='Bill Stanley',
                 'NB'='North Beach', 'LT'='Lizard Tail', 'OL'='Olancha',
                 'MS'='Mill', 'LP'='Lone Pine', 'KE'='Keeler')
deployments <- c('DS'='Dirty Socks', 'SC'='Shell Cut', 'ST'='Stanley',
                 'NB'='North Beach', 'LT'='Lizard Tail', 'OL'='Olancha',
                 'MS'='Mill Site', 'LP'='Lone Pine', 'KE'='Keeler')

start_date <- as.Date('2017-01-01')
end_date <- as.Date('2020-12-01')

valid_df <-  pull_valid_data(start_date, end_date, valid_names)

mfile_df <- pull_mfile_data(start_date, end_date, mfile_names)

df1 <- valid_df %>% full_join(mfile_df, by=c('abrv', 'datetime'), suffix = c(".valid", ".mfile"))
for (x in c('date', 'ws', 'wd', 'pm10', 'onlake_wd', 'valid')){
    df1[[x]] <- coalesce(df1[[paste0(x, ".valid")]], df1[[paste0(x, ".mfile")]]) 
}
df1 <- df1 %>% select(datetime, abrv, date, ws, wd, pm10, onlake_wd, valid)
df1$deployment = sapply(df1$abrv, function(x) deployments[x])

mixed_valid <- df1 %>% group_by(date, abrv) %>%
    summarize(n_valid = sum(valid), n_notvalid = sum(!valid)) %>%
    filter(n_valid>0 & n_notvalid>0)
for (i in seq(1, nrow(mixed_valid))){
    df1 <- df1 %>% filter(!(date==mixed_valid$date[i] & abrv==mixed_valid$abrv[i] & !valid))
}

site_locs <- pull_site_locations(deployments)

exceeds <- df1 %>% group_by(date, abrv) %>%
    summarize(pm10_avg = sum(pm10)/length(pm10)) %>%
    filter(pm10_avg>=150)

exceeds %>% group_by(date) %>% summarize(no_sites_exceeding = length(pm10_avg))

plot_range <- c('xmin'=405000, 'xmax'=429000, 'ymin'=4012000, 'ymax'=4053000)
legnd_range <- c(plot_range[2]-4000, plot_range[2], plot_range[4]-4000, plot_range[4])
text_range <- c(plot_range[2]-5000, plot_range[2]-2000, plot_range[4]-8000, plot_range[4]-6000)
logo_range <- c(plot_range[2]-5000, plot_range[2], plot_range[3], plot_range[3]+5000)

value_seq <- c(20, 50, 100, 150)
legend_grob <- build_legend(df1, valueseq=value_seq)
logo_grob <- grob_logo("/Users/john/Documents/AIRSCI Logo.jpg")
p1 <- background_map(plot_range, logo_range, logo_grob, legnd_range, legend_grob)

for (dt in as.character(unique(exceeds$date))){
    print(dt)
    df2 <- filter(df1, date==dt)

    # one-off fix for missing wd data for Olancha on particular exceedance day
    for (i in seq(1, nrow(df2))){
        if (df2$abrv[i]=='OL'){
            df2$onlake_wd[i] <- TRUE
        }
    }

    onlake_count <- df2 %>% group_by(date, deployment, onlake_wd) %>%
        summarize(hrs = length(pm10)) %>%
        filter(!is.na(onlake_wd)) %>%
        spread(onlake_wd, hrs, fill=0)
    names(onlake_count) <- c('date', 'deployment', 'hrs_offlake', 'hrs_onlake')

    daily_sum <- df2 %>% group_by(date, deployment) %>%
        summarize(pm10_avg = round(sum(pm10)/length(pm10), 0))
    exceed_sites <- daily_sum %>% filter(pm10_avg>=150 & date==dt)

    breakdown <- df2 %>% 
        group_by(abrv) %>%
        mutate(n_hours = length(pm10)) %>% ungroup() %>%
        group_by(deployment, abrv, onlake_wd) %>%
        summarize(pm10 = round(sum(pm10)/mean(n_hours), 0)) %>%
        spread(onlake_wd, pm10, fill=0) %>%
        rename(onlake = 'TRUE', offlake = 'FALSE') %>%
        left_join(onlake_count, on=c('deployment')) %>%
        mutate(pm10_avg = (round(offlake + onlake, 0)))
    breakdown$flag_color <- if_else(breakdown$pm10_avg>150, "red", "black") 

    legend_text <- bquote("Site Label = 24-hour PM"[10])
    if (any(!df2$valid)){
        legend_text <- bquote(atop("Site Label = 24-hour PM"[10], "(estimate from preliminary data)"))
    }
    p2 <- p1 + annotation_custom(grid::textGrob(legend_text, gp=gpar(fontsize=10)),
                                 xmin=text_range[1], xmax=text_range[2],
                                 ymin=text_range[3], ymax=text_range[4])
    roses <- create_roses(df2, dt, value_seq)
    buffer <- 4000
    if (any(!df2$valid)){
        sub_title = "(from preliminary, unvalidated data)"
    } else{
        sub_title = ""
    }
    p3 <- p2 +
        ggtitle(substitute(paste("Hourly P",  M[10], " Roses for ", d),
                           list(d=format(as.Date(dt), "%m-%d-%Y"))),
        subtitle = sub_title)
    site_table <- breakdown %>% ungroup() %>% 
        filter(deployment %in% exceed_sites$deployment) %>%
        select(-deployment) %>% filter(date==dt) %>%
        left_join(site_locs, on="abrv") %>%
        select(deployment, onlake, offlake)
    site_table$deployment <- sapply(site_table$deployment, function(x) ifelse(x=='Olancha', 'Olancha*', x))
    site_table$onlake <- sapply(site_table$onlake, function(x) round(x, 0))
    site_table$offlake <- sapply(site_table$offlake, function(x) round(x, 0))
    names(site_table) <- c("Site", "Onlake", "Offlake")
    grob_theme <- gridExtra::ttheme_default(base_size=6, parse=T, 
                                 colhead=list(fg_params=list(parse=T)))
    site_table_grob <- gridExtra::tableGrob(site_table, rows=NULL, 
                                   theme=grob_theme)
    table_title_grob <- textGrob(label=bquote("Hourly "~'P'*M[10]~" by Direction "~'('*mu*'g/'*m^3*')'),
                                 just="center", gp=gpar(fontsize=6))
    for (m in names(roses$grob)){
        p3 <- p3 +
            annotation_custom(roses$grob[[m]],
                              xmin=roses$center[[m]][1] - buffer,
                              xmax=roses$center[[m]][1] + buffer,
                              ymin=roses$center[[m]][2] - buffer,
                              ymax=roses$center[[m]][2] + buffer)
    }
    label_data <- breakdown %>% ungroup() %>% select(-deployment) %>%
        filter(date==dt) %>% left_join(site_locs, by="abrv")
    label_data$y_offset <- rep(-1000, nrow(label_data))
    label_data[label_data$abrv=='LT', ]$y_offset <- 1000
    direction_grob <- textGrob(label="Offlake/Onlake directions defined in Owens Lake 2016 SIP (Table 7.2)",
                         just="center", gp=gpar(fontsize=6))
    p4 <- p3 + 
        annotation_custom(site_table_grob, 
                          xmin=plot_range['xmax'] - 4000,
                          xmax=plot_range['xmax'],
                          ymin=plot_range['ymin'] + 7000,
                          ymax=plot_range['ymin'] + 8000 + nrow(site_table)*1000) +
        annotation_custom(table_title_grob, 
                          xmin=plot_range['xmax'] - 4000,
                          xmax=plot_range['xmax'],
                          ymin=plot_range['ymin'] + 8500 + nrow(site_table)*1000,
                          ymax=plot_range['ymin'] + 9000 + nrow(site_table)*1000) +
        annotation_custom(direction_grob, 
                          xmin=plot_range['xmax'] - 10000,
                          xmax=plot_range['xmax'],
                          ymin=plot_range['ymin'] + 6000,
                          ymax=plot_range['ymin'] + 6500) +
        geom_label(data=label_data,
                   mapping=aes(x=x, y=y, label=pm10_avg),
                   color=label_data$flag_color, size=3, 
                   label.padding=unit(0.15, "lines")) +
        geom_text(data=label_data, 
                   mapping=aes(x=x, y=y+y_offset, label=deployment),
                   size=4, family='mono', fontface='plain')
    if ('Olancha*' %in% site_table$Site){
        olancha_grob <- textGrob(label="* Olancha onlake direction, as defined, contains known offlake emissions sources.",
                             just="center", gp=gpar(fontsize=6))
        p4 <- p4 + 
        annotation_custom(olancha_grob, 
                          xmin=plot_range['xmax'] - 12000,
                          xmax=plot_range['xmax'],
                          ymin=plot_range['ymin'] + 5000,
                          ymax=plot_range['ymin'] + 6000)
    }
    fl <- paste0(path, "/", dt, "_map.pdf")
    pdf(file=fl, width=8, height=10.5, paper="letter")
    print(p4)
    dev.off()

    fl2 <- paste0("/Users/john/code/dust_pic/output/", dt, "_map.png")
    png(file=fl2, width=8, height=10.5, units="in", res=300)
    print(p4)
    dev.off()

#    yesterday_map = paste0(path, "/", dt, "_yesterday.pdf")
#    yesterday_address = paste0("https://www.gbuapcd.org/Docs/OwensLake/Yesterday/CalPuff/archive/",
#                               substring(year(as.Date(dt)), 3), sprintf("%02d", month(as.Date(dt))),
#                               sprintf("%02d", day(as.Date(dt))), ".pdf")
#    download.file(yesterday_address, yesterday_map, method="libcurl")
}
fl_lst <- list.files(path)
out_file <- paste0("Shoreline_Summary_", start_date, "_to_", end_date, ".pdf")
system(paste0("/System/Library/Automator/Combine\\ PDF\\ Pages.action/Contents/Resources/join.py ",
              "-o /Users/john/code/dust_pic/output/", out_file, " ", path, "/*.pdf"))
