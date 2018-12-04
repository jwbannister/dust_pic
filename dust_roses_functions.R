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

pull_site_locations <- function(teom_sites){
    query2 <- paste0("SELECT DISTINCT i.deployment, ",
                     "st_y(st_transform(i.geom, 26911)) AS y, ",
                     "st_x(st_transform(i.geom, 26911)) AS x ",
                     "FROM instruments.deployments i ",
                     "WHERE i.deployment IN ('",
                     paste0(teom_sites, collapse="', '"), "');")
    site_locs <- query_db("owenslake", query2)
    site_locs$deployment <- sapply(site_locs$deployment, 
                                   function(x) names(teom_sites)[which(teom_sites==x)])
    return(site_locs)
}

pull_mfile_data <- function(start_date, end_date, teom_sites){
    start_str <- paste0(start_date, " 01:00:00")
    end_str <- paste0(end_date %m+% days(1), " 00:00:00")
    print("Getting mfile data...")
    print(paste0(start_str, " through ", end_str))
    mfile_names <- c("LP"="LonePine", "KE"="Keeler", "NB"="NorthBch", 
                     "LT"="LizardTl", "MS"="MillSite", "SC"="ShellCut", 
                     "DS"="DirtySox", "OL"="Olancha", "ST"="Stanley")
    mfile_sites <- names(teom_sites)[names(teom_sites) %in% names(mfile_names)]
    mfile_query <- paste0("SELECT datetime, site AS deployment, aspd AS ws, ", 
                          "dir AS wd, teom AS pm10 FROM archive.mfile_data ",
                          "WHERE datetime BETWEEN '", 
                          start_str, "'::timestamp AND '", end_str, "'::timestamp ", 
                          "AND site IN ('", 
                          paste(mfile_names[mfile_sites], collapse="', '"), "');")
    mfile_df <- query_db("owenslake", mfile_query)
    mfile_df$deployment <- sapply(mfile_df$deployment, 
                                  function(x) names(mfile_names)[which(mfile_names==x)])
    return(mfile_df)
}

pull_teom_data <- function(yr, teom_sites){
    print("Getting LADWP TEOM data...")
    dwp_sites <- teom_sites[names(teom_sites) %in% c('HW')]
    # get teom data from 1 hour average view of old 1-min data if needed
    teom_old_query <- paste0("SELECT datetime, deployment, pm10_std_avg AS pm10 ", 
                            "FROM teom.avg_1hour_validated ", 
                            "WHERE EXTRACT(YEAR FROM datetime - '1 second'::interval)", 
                            "=", yr, " AND deployment IN ", 
                            "('", paste(dwp_sites, collapse="', '"), "') ",
                            "AND NOT invalid")
    teom_new_query <- paste0("SELECT datetime, deployment, pm10_1hour_stp AS pm10 ", 
                            "FROM teom.hourly_validated ", 
                            "WHERE EXTRACT(YEAR FROM datetime - '1 second'::interval)", 
                            "=", yr, " AND deployment IN ", 
                            "('", paste(dwp_sites, collapse="', '"), "') ",
                            "AND NOT invalid")
    teom_df <- data.frame(datetime=c(), deployment=c(), pm10=c(), ws=c(), wd=c())
    if (yr <= 2017){
        teom_old_df <- query_db("owenslake", teom_old_query)
        teom_df <- rbind(teom_df, teom_old_df)
    } 
    if (yr >= 2017){
        teom_new_df <- query_db("owenslake", teom_new_query)
        teom_df <- rbind(teom_df, teom_new_df)
    }
    teom_met_query <- paste0("SELECT m.datetime, i.deployment, m.ws_wvc AS ws, ",
                        "m.wd_wvc AS wd FROM teom.teom_analog_1hour m ", 
                        "JOIN instruments.deployments i ",
                        "ON i.deployment_id=m.deployment_id ", 
                        "WHERE EXTRACT(YEAR FROM datetime - '1 second'::interval)", 
                        "=", yr, " AND deployment IN ", 
                        "('", paste(dwp_sites, collapse="', '"), "');")
    teom_met <- query_db("owenslake", teom_met_query)
    teom_df <- left_join(teom_df, teom_met, by=c("datetime", "deployment"))
    teom_df$deployment <- sapply(teom_df$deployment, 
                                  function(x) names(dwp_sites)[which(dwp_sites==x)])
    return(teom_df)
}

load_epa_data <- function(teom_sites){
    print("Getting EPA site data...")
    epa_codes <- c('81102'='pm10', '61101'='ws_s', '61102'='wd_s', 
                   '61103'='ws_r', '61104'='wd_r')
    epa_xwalk <- c('NB'='0029', 'LT'='0028', 'KE'='1003', 'MS'='0030', 'SC'='0025', 
                   'DS'='0022', 'ST'='0026', 'CJ'='1001', 'OL'='0021')
    epa_sites <- names(teom_sites)[names(teom_sites) %in% names(epa_xwalk)]
    df1 <- read_csv("~/code/dust_pic/data/epa.csv", col_types="cDticdc")
    names(df1) <- c('deployment', 'date', 'hour', 'code', 'desc', 'value', 'units')
    df2 <- filter(df1, deployment %in% epa_xwalk[epa_sites])
    df2$deployment <- sapply(df2$deployment, 
                              function(x) names(epa_xwalk)[which(epa_xwalk==x)])
    df2$datetime_utc <- as.POSIXct(paste0(df2$date, " ", df2$hour), tz='UTC', 
                                 format="%Y-%m-%d %H:%M")
    df2$datetime <- sapply(df2$datetime_utc, rezone)
    df2$datetime <- as.POSIXct(df2$datetime, origin="1970-01-01")
    df2$date <- date(df2$datetime - 1)
    df2$year <- year(df2$datetime - 1)

    df3 <- df2 %>% 
        select(datetime, deployment, code, value) %>%
        spread(code, value)
    names(df3) <- sapply(names(df3), function(x) ifelse(x %in% names(epa_codes), 
                                                        epa_codes[x], x))
    if ('wd_r' %in% names(df3) & 'wd_s' %in% names(df3)){
        df3$wd <- coalesce(df3$wd_r, df3$wd_s)
        df3 <- select(df3, -wd_r, -wd_s)
    } else{
        names(df3)[which(grepl('wd', names(df3)))] <- 'wd'
    }
    if ('ws_r' %in% names(df3) & 'ws_s' %in% names(df3)){
        df3$ws <- coalesce(df3$ws_r, df3$ws_s)
        df3 <- select(df3, -ws_r, -ws_s)
    } else{
        names(df3)[which(grepl('ws', names(df3)))] <- 'ws'
    }
    return(df3)
}

pull_sand_flux <- function(d){
    query1 <- 
        paste0("SELECT sens.datetime, ic.deployment AS csc, ", 
           "CASE ", 
               "WHEN csc.sumpc_total > 0 ", 
               "THEN (sens.sumpc / csc.sumpc_total) * ", 
               "(COALESCE(csc.dwp_mass, csc.district_mass) / 1.2) ",
               "ELSE 0 ",
           "END AS sand_flux, ", 
           "flags.field_is_invalid(csc.sensit_deployment_id, 90, sens.datetime) AS invalid ",
           "FROM sensit.sensit_5min sens LEFT JOIN sandcatch.csc_summary csc ",
           "ON csc.sensit_deployment_id = sens.deployment_id ", 
           "JOIN instruments.deployments ic ",
           "ON csc.csc_deployment_id=ic.deployment_id ",
           "WHERE sens.datetime > csc.start_datetime ", 
           "AND sens.datetime <= csc.collection_datetime ", 
           "AND COALESCE(csc.dwp_mass, csc.district_mass) IS NOT NULL ",
           "AND (sens.datetime - '1 second'::interval)::date='", d, "'::date;")
    a <- query_db("owenslake", query1)
    attributes(a$datetime)$tzone <- 'America/Los_Angeles'
    return(a)
}

