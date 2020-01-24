library(jpeg)

data_cont <- function(df1, var){
    p1 <- ggplot(df1, aes_string(x='datetime', y=var)) +
        geom_point() +
        facet_grid(deployment ~ .)
    return(p1)
}

angle_between <- function(angle_vec, angle){
    if (angle_vec[1]<angle_vec[2]){
        return(between(angle, angle_vec[1], angle_vec[2]))
    } else{
        return(between(angle, angle_vec[1], 360) |
               between(angle, 0, angle_vec[2]))
    }
}

create_roses <- function(dat, dt, value_seq){
    roses <- list(grob=c(), center=c())
    for (j in unique(dat$abrv)){
        rose_data <- dat %>% 
            filter(abrv==j & !is.na(pm10) & !is.na(wd) & date==dt) 
        if (nrow(rose_data)>0){
            p_rose <- plot_rose_image_only(rose_data, value='pm10', dir='wd',
                                     valueseq=value_seq, reverse.bars=T)
            fl <- tempfile()
            png(filename=fl, bg="transparent")
            print(p_rose)
            dev.off()
            ras <- grid::rasterGrob(png::readPNG(fl), interpolate=TRUE)
        } else{
            ras <- grid::circleGrob(r=0.05, gp=grid::gpar(fill='grey'))
        }
        roses$grob[[j]] <- ras
        roses$center[[j]] <- c(filter(site_locs, abrv==j)$x,
                               filter(site_locs, abrv==j)$y)
    }
    return(roses)
}

pull_site_locations <- function(teom_sites){
    query2 <- paste0("SELECT DISTINCT i.deployment, ",
                     "st_y(st_transform(i.geom, 26911)) AS y, ",
                     "st_x(st_transform(i.geom, 26911)) AS x ",
                     "FROM instruments.deployments i ",
                     "WHERE i.deployment IN ('",
                     paste0(teom_sites, collapse="', '"), "');")
    site_locs <- query_db("owenslake", query2)
    site_locs$abrv <- sapply(site_locs$deployment, 
                                   function(x) names(teom_sites)[which(teom_sites==x)])
    return(site_locs)
}

pull_mfile_data <- function(start_date, end_date, teom_sites){
    print("Getting mfile data...")
    print(paste0(start_date, " through ", end_date))
    mfile_query <- paste0("SELECT datetime::text, site AS deployment, aspd AS ws, ", 
                          "dir AS wd, teom AS pm10 ",
                          "FROM archive.mfile_data ",
                          "WHERE (datetime - '1 second'::interval)::date ",
                          "BETWEEN '", start_date, "'::date AND '", end_date, "'::date ", 
                          "AND site IN ('", paste(teom_sites, collapse="', '"), "');")
    mfile_df <- query_db("owenslake", mfile_query)
    mfile_df$datetime <- as.POSIXct(mfile_df$datetime, format="%Y-%m-%d %H:%M:%S",
                              tz='America/Los_Angeles')
    mfile_df <- mfile_df %>% filter(pm10>-15) %>% filter(!is.na(datetime)) %>%
        filter(!is.na(pm10))
    mfile_df <- mfile_df[!duplicated(mfile_df[ , 1:2]), ]
    mfile_df$date <- date(mfile_df$datetime - 1)
    mfile_df$abrv <- sapply(mfile_df$deployment,
                            function(x) names(teom_sites)[which(teom_sites==x)])
    return(mfile_df)
}

background_map <- function(plot_range, logo_range, logo_grob, legnd_range, legend_grob){
    shoreline <- pull_shoreline_polygon()
    p1 <- ggplot(data=shoreline, mapping=aes(x=x, y=y)) + geom_path(mapping=aes(group=area_name))
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
              plot.subtitle=element_text(hjust=0.5))
    p2 <- p1 +
        annotation_custom(logo_grob, xmin=logo_range[1], xmax=logo_range[2],
                          ymin=logo_range[3], ymax=logo_range[4]) +
        annotation_custom(legend_grob, xmin=legnd_range[1], xmax=legnd_range[2],
                          ymin=legnd_range[3], ymax=legnd_range[4])
    return(p2)
}

build_legend <- function(df2, valueseq){
    legend.data <- df2 %>% filter(deployment==df2$deployment[1])
    legend.plot <- legend.data %>%
        plot_rose(., value='pm10', dir='wd', valueseq=valueseq,
                  legend.title=bquote('P'*M[10]~'('*mu*'g/'*m^3*')'), 
                  reverse.bars=T)
    legnd <- g_legend(legend.plot)
    return(legnd)
}

grob_logo <- function(file = "/Users/john/Documents/AIRSCI Logo.jpg"){
    img <- readJPEG(file)
    g <- rasterGrob(img, interpolate=T)
    return(g)
}
