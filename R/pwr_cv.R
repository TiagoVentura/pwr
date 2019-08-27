#' pwr_cv
#'
#' \code{pwr_cv} performs leave-one-out cross-validation to select the optimal bandwidth to the decay function used
#' in the PWR estimation.
#' @param \code{net}: an igraph network object.
#'
#' @param \code{y}: the dependent variable of the PWR model
#'
#' @param \code{X}: a matrix of covariates for the model.
#'
#' @param \code{bandwidht}: the values for the bandwidth search. Default searchs over the following values \code{exp(seq(from=-1, to=3, by=.05))}
#'
#' @param \code{n}: sample size for the CV search. We suggest the user to reduce the sample to 500 observations for
#' computational reasons. Default is the entire network.
#'
#' @usage pwr_cv(net, y, X, bandwidth=exp(seq(from=-1, to=3, by=.05)), n=lenght(V(net)))
#' @return it returns a list with two objects: the optimal bandwidth and a graph showing the selection
#' @examples
#' l <- cwr.cv_leaveoneout(net=net,y=log(V(net)$in_degree+1),
#'  X=as.matrix(log(V(net)$out_degree+1)), n=500)
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import igraph
#' @importFrom magrittr "%>%"
#' @export

pwr_cv <- function(net, y, X, bandwidth=exp(seq(from=-1, to=3, by=.05)),
                               n=length(igraph::V(net))) {


# making sure is a matrix

  X <- as.matrix(X)


# Selecting a sample from the total network

  sample_obs <- sample(length(igraph::V(net)), n) # must be n here

# Selecting y and x

  y_sample <- y[sample_obs]
  x_sample <- X[sample_obs, ]

# Calculating the distance matrix

  distance.w <- sapply(sample_obs, function(z)
    igraph::distances(net, v = igraph::V(net)[z],mode="all")[sample_obs])


# running the model with different bandwidths

  fitted_models_sample <-   purrr::map(bandwidth, ~ cwr.matrix_out(y=y_sample,
                                                                   x=x_sample,
                                                                   net=net, obs=sample_obs,
                                                                   bandwidth = .x,
                                                                   distance.w = distance.w))


# Getting the mse

  mse_values <- purrr::map_df(fitted_models_sample, ~ mse(data=.x, x=x_sample, y=y_sample)) %>%
    dplyr::mutate(bandwidth=bandwidth)



# Analytical solution
  res <- unname(lm(mse_values$mse~mse_values$bandwidth)$residuals)
  opt<-mse_values$bandwidth[which(res==min(res))]

#graph
  cv_graph <-  ggplot2::ggplot(mse_values, ggplot2::aes(y=mse, x=bandwidth)) +
    ggplot2::geom_point(size=3) +
    ggplot2::xlab("Bandwidth") + ggplot2::ylab("Mean Squared Error") +
    ggplot2::ggtitle("Plot for bandwidth selection") +
    ggplot2::theme_minimal() +
    ggplot2::geom_smooth(linetype="dashed", color="red") +
    ggplot2::geom_vline(xintercept =opt,
                        linetype="dashed", color="tomato2",
                        alpha=1)



# Outputs
  results <- list()
  results[["bandwidth"]] <- opt
  results[["cv_graph"]] <- cv_graph



  return(results)
}
