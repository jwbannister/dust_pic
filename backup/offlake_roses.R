library(tidyverse)
library(circular)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)
path <- "~/code/dust_pic/"
source(paste0(path, "/dust_roses_functions.R"))
onlake_dir <- list('LP'=c(126, 176), 
                   'KE'=c(151, 296), 
                   'FR'=c(224, 345), 
                   'SC'=c(227, 33), 
                   'DS'=c(234, 50), 
                   'OL'=c(333, 39), 
                   'ST'=c(349, 230), 
                   'NB'=c(55, 250), 
                   'LT'=c(128, 288), 
                   'MS'=c(157, 333)) 
dunes_dir <- c(296, 337)

site_list <- c('DS', 'SC', 'ST', 'NB', 'LT', 'OL', 'MS', 'KE')
site_locs <- pull_site_locations(site_list)

available_sites <- c('DS'='Dirty Socks', 'SC'='Shell Cut', 'ST'='Stanley', 
    'NB'='North Beach', 'LT'='Lizard Tail', 'OL'='Olancha', 
    'MS'='Mill Site', 'LP'='Lone Pine', 'KE'='Keeler', 
    'HW'='Haiwee', 'CJ'='CoJu-696') 

start_date <- as.Date("2016-10-01")
end_date <- as.Date("2018-06-31")
df1 <- pull_mfile_data(start_date, end_date, site_list)
df1 <- filter(df1, !is.na(datetime))
df1$site_label <- sapply(df1$deployment, function(x) available_sites[x])

df2 <- df1 %>% rowwise() %>%
    mutate(onlake=angle_between(onlake_dir[[deployment]], wd),
           dunes=angle_between(dunes_dir, wd) & deployment=='KE') %>%
    ungroup()
df2$date <- date(df2$datetime - 1)
df2 <- df2[!duplicated(data.frame(df2$datetime, df2$deployment)), ]
df2 <- filter(df2, pm10 > -15)

daily_sum <- df2 %>% group_by(date, deployment, site_label) %>%
    do(pm10_24=round(sum(.$pm10, na.rm=T)/sum(!is.na(.$pm10)), 0), 
              n=sum(!is.na(.$pm10)), 
              pm10_onlake=round(sum(filter(., onlake)$pm10, na.rm=T)/
                  sum(!is.na(filter(., onlake)$pm10)), 0), 
              n_onlake=sum(!is.na(filter(., onlake)$pm10)),
              pm10_dunes=round(sum(filter(., dunes)$pm10, na.rm=T)/
                  sum(!is.na(filter(., dunes)$pm10)), 0), 
              n_dunes=sum(!is.na(filter(., dunes)$pm10)),
              pm10_offlake=round(sum(filter(., !onlake)$pm10, na.rm=T)/
                  sum(!is.na(filter(., !onlake)$pm10)), 0), 
              n_offlake=sum(!is.na(filter(., !onlake)$pm10)))
for (i in 3:10){
    daily_sum[ , i] <- unlist(daily_sum[ , i])
}
exceeds <- filter(daily_sum, pm10_24>=150)
exceeds[is.na(exceeds)] <- 0
exceed_days <- sort(unique(exceeds$date))

owens <- pull_all_polygons() %>% filter(area_name!="Mono Lake")
shoreline <- pull_shoreline_polygon()

valueseq <- c(10, 50, 100, 150)
legend.data <- df2 %>% filter(deployment==df2$deployment[1])
legend.data$pm10[1] <- NA
legend.plot <- legend.data %>%
    plot_rose(., value='pm10', dir='wd', valueseq=valueseq,
              legend.title=bquote('P'*M[10]~'('*mu*'g/'*m^3*')'), 
              reverse.bars=T)
legnd <- g_legend(legend.plot)

p1 <- ggplot(data=shoreline, mapping=aes(x=x, y=y)) +
    geom_path(data=owens, mapping=aes(group=area_name), color='grey') +
    geom_path(mapping=aes(group=area_name)) +
#    geom_point(data=site_labels, mapping=aes(x=x, y=y)) +
    coord_equal()
info <- ggplot_build(p1)
xrange <- info$layout$panel_params[[1]]$x.range
yrange <- info$layout$panel_params[[1]]$y.range
p2 <- p1 +
    xlim(xrange[1] - 1000, xrange[2] + 1000) +
    ylim(yrange[1] - 1000, yrange[2] + 1000) +
    annotation_custom(legnd,
                      xmin=xrange[2] - 4000,
                      xmax=xrange[2],
                      ymin=yrange[2] - 8000,
                      ymax=yrange[2]) +
theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
#      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank(),
      plot.title=element_text(size=12))

daily_sum$flag.color <- if_else(daily_sum$pm10_24>150, "red", "black") 

for (dt in as.character(exceed_days)){
    print(dt)
    site_table <- filter(exceeds, date==dt) %>% 
        select(site_label, pm10_onlake, pm10_offlake, pm10_dunes)
    names(site_table) <- c("Site", "Onlake", "Offlake", "Keeler Dunes")
    site_table_grob <- gridExtra::tableGrob(site_table, rows=NULL, 
                                   theme=gridExtra::ttheme_minimal(base_size=6))
    roses <- list(grob=c(), center=c())
    for (j in site_table$Site){
        p_rose <- df2 %>% 
            filter(site_label==j & !is.na(pm10) & date==dt) %>%
            plot_rose_image_only(., value='pm10', dir='wd',
                                 valueseq=valueseq, reverse.bars=T)
        fl <- tempfile()
        png(filename=fl, bg="transparent")
        print(p_rose)
        dev.off()
        ras <- grid::rasterGrob(png::readPNG(fl), interpolate=TRUE)
        roses$grob[[j]] <- ras
        roses$center[[j]] <- c(filter(site_labels, deployment==j)$x,
                               filter(site_labels, deployment==j)$y)
    }

    buffer <- 4000
    p3 <- p2
    for (m in names(roses$grob)){
        label_data <- daily_sum %>% 
            filter(deployment==m & date==dt) %>%
            left_join(site_labels, by="deployment")
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
            geom_text(data=label_data,
                      mapping=aes(x=x, y=y, label=deployment),
                      nudge_y=-800, size=3)
    }

    p4 <- p3 +
        ggtitle(substitute(paste("Hourly P",  M[10], " Roses for ", d),
                           list(d=format(as.Date(dt), "%m-%d-%Y"))),
                subtitle="CONFIDENTIAL ATTORNEY-CLIENT WORK PRODUCT") +
        annotation_custom(grid::textGrob(
                             expression(
                                paste("Site Label = 24-hour P", M[10])),
                                         gp=gpar(fontsize=10)),
                          xmin=xrange[2] - 4000,
                          xmax=xrange[2],
                          ymin=yrange[2],
                          ymax=yrange[2] + 2000) +
        annotation_custom(logo_grob,
                          xmin=xrange[2] - 5000,
                          xmax=xrange[2],
                          ymin=yrange[1],
                          ymax=yrange[1] + 5000) +
        annotation_custom(site_table_grob, 
                          xmin=xrange[2] - 5500,
                          xmax=xrange[2],
                          ymin=yrange[1] + 5000,
                          ymax=yrange[1] + 6000 + nrow(site_table)*1000) +
        theme(plot.title=element_text(hjust=0.5),
              plot.subtitle=element_text(hjust=0.5),
              panel.border=element_rect(color="black", fill="transparent"))

    fl <- paste0(path, "pdfs/", format(as.Date(dt), "%Y-%m-%d"),
                 "_shoreline_exceedance.pdf")
    pdf(file=fl, width=8, height=10.5, paper="letter")
    print(p4)
    dev.off()
}
system(paste0("pdfunite ", path, "pdfs/*.pdf ", path, 
              "2015-present_shoreline_exceedances.pdf"))

