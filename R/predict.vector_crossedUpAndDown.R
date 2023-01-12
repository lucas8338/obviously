#' @title predict.vector_crossedUpAndDown
#' @description predict method for the class 'vector_crossedUpAndDown'.
#' @param model the params from the fitted.
#' @param vec the vector to be applied
#' @return a character vector
#' @import dplyr
#' @export
predict.vector_crossedUpAndDown<- function(model,vec){
  data<- vec
  data.copy<- vec

  stopifnot("the length is different, then i supose the data are different,
  data need to be the 'same' to the fitted"=length(vec) == length(model[[1]]))

  for ( i in 2:(length(model)) ){
    cross<- model[[i]]
    crossedUps<- cross[['crossedUp']]
    crossedDns<- cross[['crossedDn']]
    crossedUps.name<- cross[['names']][[1]]
    crossedDns.name<- cross[['names']][[2]]
    # do work for crossedUps
    data.copy[crossedUps]<- crossedUps.name
    # do work for drossedDns
    data.copy[crossedDns]<- crossedDns.name
  }

  data.copy
}