data_cont <- function(df1, var){
    p1 <- ggplot(df1, aes_string(x='datetime', y=var)) +
        geom_point(aes(color=abrv)) +
        facet_grid(abrv ~ .)
    print(p1)
}
