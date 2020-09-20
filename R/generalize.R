#' Generalize experimental results
#'
#'
#' This function implements the methods discussed in Cinelli and Pearl (2020+).
#' The function generalizes experimental results from one or more source populations to a target population by leveraging
#' the invariance of probabilities of causation (Pearl, 1999).
#'
#'
#' The function works mainly as a convenience wrapper to \code{\link{rjags}} code, and uses the priors described in Cinelli and Pearl (2020+) for an illustration of the method.
#' Should users prefer to personalize the analyses with their own priors, they can extract the model code and modify it directly in \code{\link{rjags}}.
#'
#'@param sources Either a \code{\link{data.frame}} with one source population, or a \code{\link{list}} with two data.frames with two source populations. The data.frame must have a specific structure: it must have only one row, and at least four columns, including \code{N0}, \code{n0}, \code{N1} and \code{n1}. For an example of the format, see the example data in \code{\link{Aceh}}.
#'
#'@param target  A \code{\link{data.frame}} with one target population. The data.frame must have only one row, and at least two columns, \code{N0} and \code{n0}.
#'
#'@param monotonic Is monotonicity assumed? Default is \code{FALSE}.
#'
#'@param n.chains Number of parallel chains for the model passed to \code{\link{jags.model}}.
#'
#'@param n.adapt Number of iterations for adaptation passed to \code{\link{jags.model}}.
#'
#'@param n.iter Number of iterations to monitor passed to \code{\link{coda.samples}}.
#'
#'@param n.burnin Number of iterations for the burn-in phase, passed to \code{\link{update}}.
#'
#'
#'@return
#' An object of class \code{generalize} containing:
#' \describe{
#' \item{data}{A \code{\link{data.frame}} containing the data used for the analysis.}
#' \item{model.code}{A \code{\link{character}} vector containing the \code{\link{rjags}} code used for the analysis.}
#' \item{model}{An object of class \code{jags} returned from the function \code{\link{jags.model}} which can be used to generate dependent samples from the posterior distribution of the parameters.}
#' \item{samples}{An object of class \code{mcmc.list} returned from the function \code{\link{coda.samples}}. It contains the posterior samples of the parameters.}
#'
#' }
#'
#'@examples
#' # loads package
#' library(generalizing)
#'
#' # loads data
#' data("Aceh")
#' data("West.Java")
#' data("Sarlahi")
#'
#' #### ACEH to WEST JAVA
#'
#' # flat prior
#' Aceh_to_WJ   <- generalize(sources = Aceh, target = West.Java, n.iter = 1e5)
#'
#' # monotonic prior
#' Aceh_to_WJ_m <- generalize(sources = Aceh, target = West.Java,
#'                            monotonic = TRUE, n.iter = 1e5)
#'
#'
#' # posterior samples histograms
#' par(mfrow = c(1,2))
#' mark <- West.Java$n1/West.Java$N1
#' par(mfrow = c(1,2))
#'
#' ## hist flat prior
#' hist(Aceh_to_WJ, main = "Flat prior")
#' abline(v = mark, col = "red", lty = 2, lwd = 2)
#'
#' ## hist monotonic prior
#' hist(Aceh_to_WJ_m, main = "Monotonic prior")
#' abline(v = mark, col = "red", lty = 2, lwd = 2)
#'
#' #### ACEH + WEST JAVA to SARLAHI
#' AcehWJ_to_Sarlahi   <- generalize(sources = list(Aceh, West.Java),
#'                                   target = Sarlahi,
#'                                   n.iter = 1e5)
#' # posterior samples histograms
#' par(mfrow = c(1, 3))
#' mark <- Sarlahi$n1/Sarlahi$N1
#'
#' ## hist prob of sufficient for saving
#' hist(AcehWJ_to_Sarlahi, var = "PS01")
#'
#' ## hist prob of sufficient for harming
#' hist(AcehWJ_to_Sarlahi, var = "PS10")
#'
#' ## hist of P(Y1 = 1)
#' hist(AcehWJ_to_Sarlahi, var = "p11s")
#' abline(v = mark, col = "red", lty = 2, lwd = 2)
#'@references
#' C. Cinelli and J. Pearl. Generalizing Experimental Results by Leveraging Knowledge of Mechanisms. European Journal of Epidemiology.
#' Forthcoming (2020+)
#'
#'
#' J. Pearl. Probabilities of causation: three counterfactual interpretations and
#' their identification. Synthese, 121(1-2):93–149, 1999.
#'
#'
#'@import rjags
#'@importFrom stats update
#'@export
generalize <- function(sources, target, monotonic = FALSE,
                        n.iter = 1e4, n.adapt = 1e3, n.burnin = 1e4, n.chains = 4) {


  n.sources <- check_sources(sources)
  check_data(target, cols = c("N0", "n0"))

  if(!is.logical(monotonic)) stop("monotonic must be a logical argument: TRUE/FALSE")

  if (n.sources == 1) {
    if (monotonic) {

      model.code <- model_one_source_monotonic

      post <- c("PS01", "PS10",
                "p01" , "p01s",
                "p11" , "p11s")

    } else {

      model.code <- model_one_source

      post <- c("PS01" , "PS10",
                "PS01.l" , "PS01.u",
                "p01"  , "p01s",
                "p11"  , "p11s",
                "p11s.l", "p11s.u")
    }
  } else {
    model.code <- model_two_sources

    post <- c("PS01" , "PS10",
              "p01a"  , "p01b", "p01s",
              "p11a"  , "p11b", "p11s")
    if(monotonic){
      warning("Two source samples provided. Monotonic set to FALSE.")
    }
  }

  data <- build_data(sources, target, n = n.sources)

  ### Posterior samples bounds
  model  <- jags.model(textConnection(model.code),
                       data = data, n.chains = n.chains, n.adapt =  n.adapt, quiet = TRUE)
  ## burn-in
  update(model, n.iter = n.burnin)

  ## samples
  samples    <- coda.samples(model,
                             variable.names = post,
                             n.iter = n.iter)

  out <- list(
    data = data,
    model.code = model.code,
    model = model,
    samples = samples)

  class(out) <- "generalize"

  return(out)
}




build_data <- function(sources, target, n){

  if (n == 1){
    data <- list(N0  = sources$N0,
                 n0  = sources$n0,
                 N1  = sources$N1,
                 n1  = sources$n1,
                 N0s = target$N0,
                 n0s = target$n0)
  } else {

    data <-  list(N0a = sources[[1]]$N0,
                  n0a = sources[[1]]$n0,
                  N1a = sources[[1]]$N1,
                  n1a = sources[[1]]$n1,
                  N0b = sources[[2]]$N0,
                  n0b = sources[[2]]$n0,
                  N1b = sources[[2]]$N1,
                  n1b = sources[[2]]$n1,
                  N0s = target$N0,
                  n0s = target$n0)
  }
  return(data)

}


check_sources <- function(sources){

  is.df <- is.data.frame(sources)
  is.list <- is.list(sources) & !is.df

  if(is.df){
    check_data(sources)
    return(1)
  }

  if(is.list){
    n <- length(sources)
    if(n > 2) stop("The function currently handles at most two sources.")
    check_data(sources[[1]])
    check_data(sources[[2]])
    return(2)
  }
}

check_data <- function(data, cols = c("N0", "n0", "N1", "n1")){
  if(!is.data.frame(data)){
    stop("Data must be a data.frame.")
  }
  if(nrow(data)>1){
    stop("Data must have one row only.")
  }
  colnames <- names(data)
  all <- all(cols %in% colnames)
  if(!all){
    missing <- which(!cols %in% colnames)
    name <- deparse(substitute(data))
    stop(paste("Missing columns", paste(cols[missing], collapse = ", "), "in", name))
  }
}
