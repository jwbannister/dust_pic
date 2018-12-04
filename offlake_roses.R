library(tidyverse)
library(circular)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)
path <- "~/code/dust_pic/"
source(paste0(path, "offlake_roses_data.R"))
source(paste0(path, "offlake_roses_functions.R"))
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

angle_between <- function(angle_vec, angle){
    if (angle_vec[1]<angle_vec[2]){
        return(between(angle, angle_vec[1], angle_vec[2]))
    } else{
        return(between(angle, angle_vec[1], 360) |
               between(angle, 0, angle_vec[2]))
    }
}

df2 <- mfile_df %>% rowwise() %>%
    mutate(onlake=angle_between(onlake_dir[[abrv]], wd),
           dunes=angle_between(dunes_dir, wd) & deployment=='Keeler') %>%
    ungroup()

daily_sum <- df1 %>% group_by(date, deployment) %>%
    do(pm10_24=round(sum(.$teom, na.rm=T)/sum(!is.na(.$teom)), 0), 
              n=sum(!is.na(.$teom)), 
              pm10_onlake=round(sum(filter(., onlake)$teom, na.rm=T)/
                  sum(!is.na(filter(., onlake)$teom)), 0), 
              n_onlake=sum(!is.na(filter(., onlake)$teom)),
              pm10_dunes=round(sum(filter(., dunes)$teom, na.rm=T)/
                  sum(!is.na(filter(., dunes)$teom)), 0), 
              n_dunes=sum(!is.na(filter(., dunes)$teom)),
              pm10_offlake=round(sum(filter(., !onlake)$teom, na.rm=T)/
                  sum(!is.na(filter(., !onlake)$teom)), 0), 
              n_offlake=sum(!is.na(filter(., !onlake)$teom)))
for (i in 3:10){
    daily_sum[ , i] <- unlist(daily_sum[ , i])
}
exceeds <- filter(daily_sum, pm10_24>=150)
exceeds$class <- rep(NA, nrow(exceeds))
exceeds$dunes <- rep(NA, nrow(exceeds))
exceeds <- exceed_process(exceeds)

days <- exceeds %>% ungroup() %>% mutate(yr = year(date)) %>%
    group_by(yr) %>%
    summarize(exceedance_days = length(pm10_24))

write.csv(days, 
          file=paste0(path, "exceedance_days_2015-present.csv"), row.names=F)
write.csv(exceeds, 
          file=paste0(path, "site_exceedances_2015-present.csv"), row.names=F)

exceed_class <- exceeds %>% group_by(year(date)) %>%
    summarize(onlake=sum(class=='onlake'),
              offlake=sum(class=='offlake'), 
              mixed=sum(class=='mixed'), 
              total=length(pm10_24)) 
exceed_sum <- t(exceed_class)
colnames(exceed_sum) <- exceed_sum[1, ]
exceed_sum <- exceed_sum[-1, ]
    
keeler_dunes <- exceeds %>% filter(deployment=='Keeler') %>%
    group_by(year(date)) %>%
    summarize(total=length(pm10_24), 
              dunes=sum(dunes, na.rm=T))
exceed_sum <- rbind(exceed_sum, t(keeler_dunes)[3, ])
rownames(exceed_sum)[5] <- 'keeler_dunes'
write.csv(exceed_sum, file=paste0(path, "exceedance_summary_2015-present.csv"))

owens <- pull_all_polygons()
shoreline <- pull_shoreline_polygon()

exceed_days <- sort(unique(exceeds$date))
valueseq <- c(10, 50, 100, 150)
legend.data <- mfile_df %>% filter(deployment==mfile_df$deployment[1])
legend.data$teom[1] <- NA
legend.plot <- legend.data %>%
    plot_rose(., value='teom', dir='wd', valueseq=valueseq,
              legend.title=bquote('P'*M[10]~'('*mu*'g/'*m^3*')'), 
              reverse.bars=T)
legnd <- g_legend(legend.plot)

p1 <- ggplot(data=shoreline, mapping=aes(x=x, y=y)) +
    geom_path(data=owens, mapping=aes(group=area_name), color='grey') +
    geom_path(mapping=aes(group=area_name)) +
#    geom_point(data=site_labels, mapping=aes(x=x, y=y)) +
    coord_equal()
info <- ggplot_build(p1)
xrange <- info[[2]]$panel_ranges[[1]]$x.range
yrange <- info[[2]]$panel_ranges[[1]]$y.range
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
        select(deployment, pm10_onlake, pm10_offlake, pm10_dunes)
    names(site_table) <- c("Site", "Onlake", "Offlake", "Keeler Dunes")
    site_table_grob <- gridExtra::tableGrob(site_table, rows=NULL, 
                                   theme=gridExtra::ttheme_minimal(base_size=6))
    roses <- list(grob=c(), center=c())
    for (j in site_table$Site){
        p_rose <- mfile_df %>% 
            filter(deployment==j & !is.na(teom) & date==dt) %>%
            plot_rose_image_only(., value='teom', dir='wd',
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

