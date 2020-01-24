library(tidyverse)
library(circular)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)
path <- "~/code/dust_pic"
source(paste0(path, "/wind_roses_functions.R"))

mfile_xwalk <- c('DirtySox'='Dirty Socks', 'ShellCut'='Shell Cut', 
                 'Stanley'='Stanley', 'NorthBch'='North Beach', 
                 'LizardTl'='Lizard Tail', 'Olancha'='Olancha', 
                 'MillSite'='Mill Site', 'LonePine'='Lone Pine', 
                 'Keeler'='Keeler', 'ATower'='A Tower', 'T27'='T27')
mfile_sites <- query_db("owenslake", 
                        paste0("SELECT DISTINCT deployment, ",
                             "ST_X(ST_TRANSFORM(geom, 26911)) AS x, ",
                             "ST_Y(ST_TRANSFORM(geom, 26911)) AS y ",
                             "FROM instruments.deployments ",
                             "WHERE deployment IN ('",
                             paste(mfile_xwalk, collapse="', '"), 
                             "');"))
met_sites <- query_db("owenslake", 
                      paste0("SELECT DISTINCT deployment, ",
                             "ST_X(ST_TRANSFORM(geom, 26911)) AS x, ",
                             "ST_Y(ST_TRANSFORM(geom, 26911)) AS y ",
                             "FROM instruments.deployments ",
                             "WHERE deployment_id IN ",
                             "(SELECT DISTINCT deployment_id ",
                             "FROM met.met_1hour);"))
teom_sites <- query_db("owenslake", 
                      paste0("SELECT DISTINCT deployment, ",
                             "ST_X(ST_TRANSFORM(geom, 26911)) AS x, ",
                             "ST_Y(ST_TRANSFORM(geom, 26911)) AS y ",
                             "FROM instruments.deployments ",
                             "WHERE deployment_id IN ",
                             "(SELECT DISTINCT deployment_id ",
                             "FROM teom.teom_analog_1hour) ",
                             "AND NOT (deployment = 'Haiwee');"))

mfile_list <- c('ATower', 'LizardTl', 'Keeler', 'T27', 'LonePine', 
               'NorthBch')
met_list <- c('1150')
teom_list <- c('T29-4N', 'T29-4S')
site_locs <- rbind(filter(mfile_sites, deployment %in% mfile_xwalk[mfile_list]),
                   filter(met_sites, deployment %in% met_list), 
                   filter(teom_sites, deployment %in% teom_list))

mfile_data <- query_db("owenslake", 
                       paste0("SELECT datetime, site AS deployment, ",
                              "aspd AS ws, dir AS wd ",
                              "FROM archive.mfile_data ",
                              "WHERE site IN ('", 
                              paste(mfile_list, collapse="', '"), 
                              "');"))
met_data <- query_db("owenslake", 
                       paste0("SELECT m.datetime, i.deployment, ",
                              "m.ws_10m AS ws, m.wd_10m AS wd ",
                              "FROM met.met_1hour m ",
                              "JOIN instruments.deployments i ",
                              "ON i.deployment_id=m.deployment_id ",
                              "WHERE (m.datetime - '1 second'::interval)::date = '2019-05-16'::date ", 
                              "AND i.deployment IN ('", 
                              paste(met_list, collapse="', '"), 
                              "');"))
teom_data <- query_db("owenslake", 
                       paste0("SELECT t.datetime, i.deployment, ",
                              "t.ws_wvc AS ws, t.wd_wvc AS wd ",
                              "FROM teom.teom_analog_1hour t ",
                              "JOIN instruments.deployments i ",
                              "ON i.deployment_id=t.deployment_id ",
                              "WHERE i.deployment IN ('", 
                              paste(teom_list, collapse="', '"), 
                              "');"))

df1 <- rbind(mfile_data, met_data, teom_data)
a <- df1 %>% group_by(deployment) %>% summarize(start=min(datetime), end=max(datetime))
cutoff = c('1551'=2, '1552'=4, 'ATower'=10, 'Keeler'=11, 'LizardTl'=10, 
           'LonePine'=11, 'NorthBch'=9, 'T27'=6, 'T29-4S'=2, 'T29-4N'=2)
b <- a %>% left_join(data.frame('deployment'=names(cutoff), 'cutoff'=cutoff)) %>%
    mutate('stop'=start + years(cutoff), 'check'=(stop<end))

df2 <- df1 %>% left_join(select(b, deployment, stop)) %>%
    mutate(invalid = (datetime > stop)) %>%
    filter(!invalid & !(is.na(ws) | is.na(wd)) & ws>0) %>%
    select(-stop, -invalid)
data_count <- df2 %>% group_by(deployment) %>%
    summarize(data_count = length(ws))
df2 %>% group_by(deployment) %>% 
    summarize(start=min(datetime), end=max(datetime)) %>%
    left_join(data_count) %>%
    mutate(missing=difftime(end, start, units="hours") - data_count) 
df2$season <- sapply(df2$datetime, 
                     function(x) ifelse(between(month(x -1 ), 7, 9), 
                                        'summer', 'winter'))

seas <- 'summer'
if (seas != 'year'){
    df3 <- df2 %>% filter(season==seas)
} else{
    df3 <- df2
}

#v <- 'ws'
#data_continuity_plot <-df2 %>% ggplot(aes_string(x='datetime', y=v)) +
#    geom_point() +
#    facet_grid(rows=vars(deployment))

valueseq <- c(3, 6, 9, 12)
sites <- unique(df3$deployment)
roses <- vector(mode='list', length=length(sites))
names(roses) <- sites
for (site in sites){
    print(site)
    roses[[site]] <- vector(mode='list', length=2)
    names(roses[[site]]) <- c('grob', 'center')
    p_rose <- df3 %>% filter(deployment==site) %>%
            plot_rose_image_only(., value='ws', dir='wd',
                                     valueseq=valueseq, reverse.bars=T)
    fl <- tempfile()
    png(filename=fl, bg="transparent")
    print(p_rose)
    dev.off()
    ras <- grid::rasterGrob(png::readPNG(fl), interpolate=TRUE)
    roses[[site]]$grob <- ras
    if (site %in% site_locs$deployment){
        roses[[site]]$center <- c(filter(site_locs, deployment==site)$x,
                               filter(site_locs, deployment==site)$y)
    } else{
        st <- mfile_xwalk[which(names(mfile_xwalk)==site)]
        roses[[site]]$center <- c(filter(site_locs, deployment==st)$x,
                               filter(site_locs, deployment==st)$y)
    }
}

legnd <- build_legend(df3)
plot_range <- c('xmin'=404866, 'xmax'=426166, 'ymin'=4030000, 'ymax'=4052866)
legnd_range <- c(plot_range[2]-4000, plot_range[2], plot_range[4]-8000, plot_range[4])
logo_range <- c(plot_range[2]-5000, plot_range[2], plot_range[3], plot_range[3]+5000)
text_range <- c(plot_range[2]-4000, plot_range[2], plot_range[4], plot_range[4]+2000)
p2 <- background_map(plot_range, logo_range, legnd_range, legnd)

buffer <- 2000
for (m in names(roses)){
    p2 <- p2 +
        annotation_custom(roses[[m]]$grob,
                          xmin=roses[[m]]$center[1] - buffer,
                          xmax=roses[[m]]$center[1] + buffer,
                          ymin=roses[[m]]$center[2] - buffer,
                          ymax=roses[[m]]$center[2] + buffer) +
        geom_text(data=site_locs, 
                   mapping=aes(x=x, y=y, label=deployment),
                   size=4, nudge_y=-1000, family='mono', fontface='plain') +
        ggtitle(seas)
}
fl <- paste0("~/desktop/", seas, "_north_windroses.pdf")
pdf(file=fl, width=8, height=10.5, paper="letter")
print(p2)
dev.off()

# build up df3 with data as required
for (i in unique(df3$csc)){
    site <- i
    valueseq <- c(2, 4, 6, 8)
    p1 <- df3 %>% filter(csc==i) %>%
        plot_rose(., value='sand.flux', dir='wd', valueseq=valueseq, 
                            reverse.bars=T,
    plot.title=paste0("Site ", site, ", 05-16-2019"),
    legend.title = "Sand Flux (g/cm^2/hour)", 
    plot.sub.title = "(Wind data from met station 1150)")
    png(paste0("~/Desktop/", i, "_05-16-19_rose.png"), height=6, width=6, units="in", 
               res=300)
    print(p1)
    dev.off()
}
