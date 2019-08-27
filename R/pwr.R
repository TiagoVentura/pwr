#' pwr
#'
#' \code{pwr} estimates the PWR model
#' @param \code{net}: an igraph network object.
#'
#' @param \code{y}: the dependent variable of the PWR model
#'
#' @param \code{X}: a matrix of covariates for the model.
#'
#' @param \code{bandwidht}: the bandwidth for the decay function applied to the connected paths. Default uses
#' the function \code{pwr_cv} to calculate the optimal bandwidth using leave-one-out cross validation
#'
#' @usage pwr(net, y, X, bandwidth="cv_pwr")
#' @return it returns a list with two objects:
#' \itemize{
#' \item a data frame with the local coefficients of the model
#' \item the output of \code{pwr_cv} in the case the bandwidth parameter is set to the default value, and the cross-validation is performed internally.
#' @examples
#' l <- pwr(net=net,y=log(V(net)$in_degree+1),
#'  X=as.matrix(log(V(net)$out_degree+1)))
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import igraph
#' @importFrom magrittr "%>%"
#' @export

pwr <- function(net, y,X, bandwidth="cv_pwr"){

  igraph::igraph_options(add.vertex.names=FALSE)
  N <- length(igraph::V(net))
  X <- as.matrix(X)

  if(igraph::is.igraph(net)==FALSE){
  warning("net should be a igraph object")
  }  else if(bandwidth=="cv_pwr"){

  cv <- pwr_cv(net=net, y=y, X=X, n=500)
  bandwidth <- cv$bandwidth
  N <- length(igraph::V(net))

  cwr.net <- array(0,dim=c(length(igraph::V(net)),ncol(X)+1))
  for(i in 1:N){
    d <- exp(-(c(c(.Call(igraph:::C_R_igraph_shortest_paths, net, as.numeric(i)-1, igraph::V(net)-1,3,1,1)))/bandwidth)^2)
    cwr.net[i,] <- coef(lm(y~X, weights=d))
    cwr.net <- as.data.frame(cwr.net)
    colnames(cwr.net) <- c("intercept", paste0("Beta_", 1:ncol(X)))
  }

  results <- list(coefficients=cwr.net, cv_band=cv)
  return(results)

  }  else{
    N <- length(igraph::V(net))
    cwr.net <- array(0,dim=c(length(igraph::V(net)),ncol(X)+1))
    for(i in 1:N){
      d <- exp(-(c(c(.Call(igraph:::C_R_igraph_shortest_paths, net, as.numeric(i)-1, igraph::V(net)-1,3,1,1)))/bandwidth)^2)
      cwr.net[i,] <- coef(lm(y~X, weights=d))

      cwr.net <- as.data.frame(cwr.net)
      colnames(cwr.net) <- c("intercept", paste0("Beta_", 1:ncol(X)))
    }

    return(cwr.net)

  }}


