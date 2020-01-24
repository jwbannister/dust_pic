data_cont <- function(df1, var){
    p1 <- ggplot(df1, aes_string(x='datetime', y=var)) +
        geom_point(aes(color=abrv)) +
        facet_grid(abrv ~ .)
    print(p1)
}

exceed_process <- function(exceeds){
    for (j in 1:nrow(exceeds)){
        if (exceeds$n_offlake[j]==0) exceeds$pm10_offlake[j] <- 0
        if (exceeds$n_onlake[j]==0) exceeds$pm10_onlake[j] <- 0
        if (exceeds$deployment[j]!='Keeler'){
            exceeds$n_dunes[j] <- NA
            exceeds$pm10_dunes[j] <- NA
        } else{
            if (exceeds$n_dunes[j]==0) exceeds$pm10_dunes[j] <- 0
            if (exceeds$pm10_dunes[j]>=150) exceeds$dunes[j] <- TRUE
            exceeds$pm10_dunes[j] <- round(((exceeds$pm10_dunes[j]*exceeds$n_dunes[j])+
                                      (20*(24-exceeds$n_dunes[j])))/24, 0)
        }
        # calculate pm10 average with District backfill procedure
        exceeds$pm10_24[j] <- round(((exceeds$pm10_24[j]*exceeds$n[j])+
                               (20*(24-exceeds$n[j])))/24, 0)
        exceeds$pm10_onlake[j] <- round(((exceeds$pm10_onlake[j]*exceeds$n_onlake[j])+
                                  (20*(24-exceeds$n_onlake[j])))/24, 0)
        exceeds$pm10_offlake[j] <- round(((exceeds$pm10_offlake[j]*exceeds$n_offlake[j])+
                                   (20*(24-exceeds$n_offlake[j])))/24, 0)
        if (exceeds$pm10_offlake[j]>=150 & exceeds$pm10_onlake[j]>=150){
            exceeds$class[j] <- "mixed" 
        } else if (exceeds$pm10_offlake[j]>=150){
             exceeds$class[j] <- "offlake"
        } else if (exceeds$pm10_onlake[j]>=150){
             exceeds$class[j] <- "onlake"
        } else{
            exceeds$class[j] <- "mixed" 
        }
    }
    return(exceeds)
}
