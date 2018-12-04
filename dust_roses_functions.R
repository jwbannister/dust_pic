data_cont <- function(df1, var){
    p1 <- ggplot(df1, aes_string(x='datetime', y=var)) +
        geom_point(aes(color=abrv)) +
        facet_grid(abrv ~ .)
    print(p1)
}

angle_between <- function(angle_vec, angle){
    if (angle_vec[1]<angle_vec[2]){
        return(between(angle, angle_vec[1], angle_vec[2]))
    } else{
        return(between(angle, angle_vec[1], 360) |
               between(angle, 0, angle_vec[2]))
    }
}

rezone <- function(x){
    attributes(x)$tzone <- "America/Los_Angeles"
    return(x)
}

avg_pm10 <- function(pm_dat){
    out_list <- vector(mode="list", length=2)
    names(out_list) <- c("avg_pm10", "n")
    out_list[['avg_pm10']] <- round(sum(pm_dat, na.rm=T)/sum(!is.na(pm_dat)), 0)
    out_list[['avg_pm10']] <- sapply(out_list[['avg_pm10']], 
                                     function(x) ifelse(is.na(x), 0, x))
    out_list[['n']] <- sum(!is.na(pm_dat))
    return(out_list)
}

create_roses <- function(dat, dt){
    roses <- list(grob=c(), center=c())
    for (j in unique(df2$deployment)){
        print(j)
        rose_data <- df2 %>% 
            filter(deployment==j & !is.na(pm10) & date==dt) 
        if (nrow(rose_data)>0){
            p_rose <- plot_rose_image_only(rose_data, value='pm10', dir='wd',
                                     valueseq=valueseq, reverse.bars=T)
            fl <- tempfile()
            png(filename=fl, bg="transparent")
            print(p_rose)
            dev.off()
            ras <- grid::rasterGrob(png::readPNG(fl), interpolate=TRUE)
        } else{
            ras <- grid::circleGrob(r=0.05, gp=grid::gpar(fill='grey'))
        }
        roses$grob[[j]] <- ras
        roses$center[[j]] <- c(filter(site_locs, deployment==j)$x,
                               filter(site_locs, deployment==j)$y)
    }
    return(roses)
}
