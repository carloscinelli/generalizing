#'@export
#' @importFrom graphics plot
plot.generalize <- function(x, ...){
  plot(x$samples, ...)
}


#'@export
#'@importFrom graphics hist
hist.generalize <- function(x, var = "p11s",
                            xlab = var,
                            breaks = 50,
                            main = "",
                            yaxt = "n",
                            ylab = "",
                            col = "lightgray",
                            lims = NULL,
                            ...){
  sim.bounds    <- do.call("rbind", x$samples)
  sim.bounds    <- as.data.frame(sim.bounds)

  if(is.null(lims)){
    lims <- range(sim.bounds[[var]])
  }

  hist(sim.bounds[[var]],
       breaks = breaks,
       xlim = lims,
       yaxt = yaxt,
       xlab = xlab,
       ylab = ylab,
       main = main,
       col = col,
       ...)
}
