target_diagram <- function(data,
                           xlab = expression("RMSEc"%.%"sign("*sigma^"*"*")"),
                           ylab = 'MBE', 
                           type = 'quantile', cuts = seq(0.25, 1, .25),
                           ...){
    data <- prepareData(data)
    circle <- makeCircles(data, type, cuts)
    radius <- unique(circle$r)
    labels <- data.frame(x = 0, y = -radius,
                         lbs = signif(radius, 2))
    
    ggplot() + 
        geom_point(data = data,
                   aes_string(x = 'nrmsec*sign(difSD)',
                              y = 'nmbe', ...),
                   color = 'black', pch = 21) +
        geom_path(aes_string(x = 'x', y = 'y'),
                  data = circle, col = 'gray') +
        geom_vline(xintercept = 0, col = 'gray') +
        geom_hline(yintercept = 0, col = 'gray') +
        geom_text(aes_string(x = 'x', y = 'y',
                             label = 'lbs',
                             vjust = 1),
                  size = 3, data = labels) +
        xlab(xlab) + ylab(ylab) +                  
        coord_fixed() + theme_bw() 
}
