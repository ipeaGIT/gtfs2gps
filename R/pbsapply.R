
#' @title Process a function in parallel
#' @description Process a function in paralell using doSNOW.
#' This function is based on the function presented in
#' https://stackoverflow.com/questions/38199631/parsapply-and-progress-bar.
#' @param slaves Number of slaves.
#' @param progress Show progress bar?
#' @param X A vector with the objects to be used as single arguments to FUN.
#' @param FUN A function to be called with each value of X.
#' @param ... Other arguments passed to FUN after a value from X.
#' @export
pbSapply <- function(slaves, progress, X, FUN, ...) {
  cl <- snow::makeSOCKcluster(slaves)
  doSNOW::registerDoSNOW(cl)
  opts <- list()

  if(progress){
    pb <- utils::txtProgressBar(max = length(X))
    on.exit(close(pb))
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  }

  on.exit(snow::stopCluster(cl))
  foreach(i = X, .combine = 'rbind', .options.snow = opts) %dopar% {
    FUN(i, ...)
  }
}

