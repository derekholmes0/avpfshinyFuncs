#' Add new functions
#'
#' @name av_runShiny_addFunctions
#' @title Add experimental functions
#' @description Adds functions in this package
#' @returns Nothing
#' @details 
#' @importFrom alphavantagepf av_add_analytic
#' @export
av_runShiny_addFunctions <- function() {
  av_add_analytic("RCOR","rolling_correlations",helpstr="Rolling Correlations")
}
  
