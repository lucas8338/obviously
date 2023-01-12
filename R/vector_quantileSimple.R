#' @title vector_quantileSimple
#' @description does a simple quantilization (quantile classification) of the input vector.
#' @param vec a numeric vector.
#' @param quants the split values for the quantilization, must be velues between 0 and 1.
#' tip: you can use the 'seq' function to generate this.
#' @param quants.names the names to be applied to the quantiles.
#' @param ... options to be passed to stats::quantile (na.rm=TRUE is passed by default).
#' @return a list with params to be passed to predict method.
#' @import dplyr
#' @export
vector_quantileSimple<- function(vec, quants, quants.names=(quants*100) %>% as.character() %>% paste0(., '%'), ...){
  data<- vec
  data.copy<- data

  quantiles<- stats::quantile(data,probs=quants,na.rm = TRUE, ...)
  names(quantiles)<- quants.names

  result<- list('quantiles'=quantiles)

  class(result)<- 'vector_quantileSimple'

  result
}