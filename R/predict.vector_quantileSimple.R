#' @title predict.vector_quantileSimple
#' @description the prediction method for the vector_quantileSimple class.
#' @param model the fitted model data.
#' @param vec a numeric vector.
#' @param fixLowerThanFirst if values are lower than the first quantile (supose to be the lowest)
#' that values will be assigned to that quantile.
#' @param fixHigherThanLast if values are higher than the last quantile (supose to be the highest)
#' that values will be assigned the that quantile.
#' @return a character vector (or not).
#' @import dplyr
#' @export
predict.vector_quantileSimple<- function(model, vec, fixLowerThanFirst=TRUE, fixHigherThanLast=TRUE){
  data<- vec
  data.copy<- vec

  # will assign the values in the 'model' to the environment as variables.
  list2env(model, envir = .GlobalEnv)

  # fix quantiles lower than the first
  if ( fixLowerThanFirst==TRUE ){
    data.copy[ which(data <= quantiles[1]) ]<- glue::glue("({quantiles[1]}]")
  }
  # fix quantiles highes than the last
  if ( fixHigherThanLast==TRUE ){
    data.copy[ which(data >= quantiles[length(quantiles)]) ]<- glue::glue("[{quantiles[length(quantiles)]})")
  }
  # apply the quantilization itself
  for ( i in 2:(length(quantiles)) ){
    data.copy[ which(data > quantiles[i-1] & data <= quantiles[i]) ]<- glue::glue("]{quantiles[i-1]}, {quantiles[i]}]")
  }

  data.copy
}