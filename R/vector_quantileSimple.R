#' @title vector_quantileSimple
#' @description does a simple quantilization of the input vector.
#' @param vec a numeric vector.
#' @param quants the split values for the quantilization, must be velues between 0 and 1.
#' tip: you can use the 'seq' function to generate this.
#' @param quants.names the names to be applied to the quantiles.
#' @param fixLowerThanFirst if values are lower than the first quantile (supose to be the lowest)
#' that values will be assigned to that quantile.
#' @param fixHigherThanLast if values are higher than the last quantile (supose to be the highest)
#' that values will be assigned the that quantile.
#' @param ... options to be passed to stats::quantile (na.rm=TRUE is passed by default).
#' @return a character vector.
#' @import dplyr
#' @export
vector_quantileSimple<- function(vec, quants, quants.names=(quants*100) %>% as.character() %>% paste0(., '%'), fixLowerThanFirst=TRUE, fixHigherThanLast=TRUE, ...){
  data<- vec
  data.copy<- data

  quantiles<- stats::quantile(data,probs=quants,na.rm = TRUE, ...)
  names(quantiles)<- quants.names

  # fix quantiles lower than the first
  if ( fixLowerThanFirst==TRUE ){
    data.copy[ which(data <= quantiles[1]) ]<- names(quantiles)[1]
  }
  # fix quantiles highes than the last
  if ( fixHigherThanLast==TRUE ){
    data.copy[ which(data >= quantiles[length(quantiles)]) ]<- names(quantiles)[length(quantiles)]
  }
  # apply the quantilization itself
  for ( i in 2:(length(quantiles)) ){
    data.copy[ which(data <= quantiles[i] & data > quantiles[i-1]) ]<- names(quantiles)[i]
  }

  data.copy
}