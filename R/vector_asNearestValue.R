#' @title vector_asNearestValue
#' @description this will set the values of the input to the nearest value of the argument.
#' @param vec a numeric vector.
#' @param values a numeric vector with the values to be vec belong to.
#' @return a numeric vector.
#' @import dplyr
#' @export
vector_asNearestValue<- function(vec, values){
  vec.copy<- vec

  asNearestValue<- function(d){
    # this function returns the nearest value in the vector 'values'
    # of the input 'd'
    minIdx<- which.min(abs(d - values))
    values[[ minIdx ]]
  }

  vec.copy<- sapply(vec, FUN = asNearestValue)

  vec.copy
}