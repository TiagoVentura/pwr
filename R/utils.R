#' This function runs the leave-one-out cross validation

cwr.matrix_out <- function(y=y_sample,x=x_sample, net=net,
                           bandwidth=bandwidth, obs=sample_obs,
                           distance.w=distance.w){



  # Repeat here is a matrix. It was returning error when I had only one covariate
  x <- as.matrix(x)

  #weights
  matrix.w <- (exp(-distance.w/bandwidth)^2)
  # The trick here is to set the diag to zero.
  diag(matrix.w) <-0

  cwr.net <- t(sapply(1:ncol(matrix.w), function(z) coef(lm(y[-z]~ as.matrix(x[-z,]), # excluding the obs again
                                                weights= matrix.w[-z,z]))))
  cwr.net
}


#' MSE function

mse <- function(data, x=x_sample, y=y_sample){
  df <- data*cbind(1,x)
  df <- cbind(df, y_pred=rowSums(df), y)  %>%
        as.data.frame() %>%
        dplyr::mutate(error.sq=(y_pred-y)^2) %>%
        dplyr::select(y_pred, y, error.sq)

  df %>% dplyr::summarise(., mse=mean(.$error.sq, na.rm = TRUE))
  }



