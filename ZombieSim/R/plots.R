plot.SZR <- function(data){
 p <- ggplot(data, aes(x = t)) +
    geom_line(aes(y  = S.t, color = "Susceptible")) +
    geom_line(aes(y  = Z.t, color = "Zombies")) +
    geom_line(aes(y  = R.t, color = "Removed")) +
    labs(color = "Legend") +
    scale_color_manual(values = legend.colors) +
    xlab("Time") +
    ylab("SZR Populations")
 
 return(p)
}