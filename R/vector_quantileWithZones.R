#' @title vector_quantileWithZones
#' @description do quantilization but will assign information above which zone the value is
#' in that moment.
#' @param vec a numeric vector.
#' @param quants values between 0 and 1 is the split for the quantilization.
#' @param quants.zones the names (prefixies) and values for the zones, is a list and its elements contains name and value
#' this function need at least two zones.
#' the condition to belong to a zone is '<=' the value. so if the parameter 'fixHigherThanLast.zone=FALSE'
#' is recommended the last zone have value Inf.
#' ZONES ARE PROFIXIES, so is recommended to add a separator to the names of the zones.
#' if for example you want a value lower than 0 belongs to a zone 'bellow0' and upper 0 bellongs to:
#' 'upper0' you can create two zones: list('bellow0_'=0, 'upper0_'=Inf), in this 'bellow0' will be
#' all values lower or EQUALY to 0, but lets say you want zero belongs to 'upper0' you can do this:
#' list( 'bellow0_'=0-1e-323', 'upper0_'=Inf ). '1e-323' is the lowest value allowed in double 64.
#' to check this do the test: '1e-323==0'.
#' @param quants.names the names to be applied to the quantiles (not considering zones yet).
#' @param fixLowerThanFirst param passed to 'vector_quantileSimple'.
#' @param fixHigherThanLast param passed to 'vector_quantileSimple'.
#' @param fixHigherThanLast.zone if values higher than the last zone should be considered as in last zone.
#' @return a character vector.
#' @import dplyr
#' @export
vector_quantileWithZones<- function(vec, quants, quants.zones, quants.names=(quants*100) %>% as.character() %>% paste0(., '%'), fixLowerThanFirst=TRUE, fixHigherThanLast=TRUE, fixHigherThanLast.zone=TRUE){
  data<- vec
  data.copy<- data

  data.copy<- vector_quantileSimple(data, quants = quants, quants.names = quants.names, fixLowerThanFirst = fixLowerThanFirst, fixHigherThanLast = fixHigherThanLast)

  # fix for the first zone
  selection<- which(data <= quants.zones[1])
  data.copy[ selection ]<- data.copy[ selection ] %>% paste0(names(quants.zones)[1], .)
  # fix for the last zone
  if ( fixHigherThanLast.zone==TRUE ){
    selection<- which(data >= quants.zones[length(quants.zones)])
    data.copy[ selection ]<- data.copy[ selection ] %>% paste0(names(quants.zones)[length(quants.zones)], .)
  }

  for ( i in 2:(length(quants.zones)) ){
    selection<- which(data <= quants.zones[i] & data > quants.zones[i-1])
    data.copy[ selection ]<- data.copy[ selection ] %>% paste0(names(quants.zones)[i], .)
  }

  data.copy
}