#' Twitter Network Data for Donal Trump
#'
#' A igraph object build after acccessing the Twitter API and collecting the network of
#' retweets to Donald Trump profile.
#'
#' @format A igraph object with data frame with 5771 vertices and 7229 edges.
#' The igraph also comes with the following features extracted directly from the Twitter API
#' \describe{
#'   \item{name}{name of the user who retweeted Trump}
#'   \item{ids}{id of the user who retweeted Trump}
#'   ...
#' }
#' @source \url{https://developer.twitter.com/}
"net"
