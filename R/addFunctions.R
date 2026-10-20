#' Add new functions
#'
#' @name av_runShiny_addFunctions
#' @title Add experimental functions
#' @description Adds functions in this package
#' @param functions_directory Directory containing functions with inpput signatures to add
#' @returns Nothing
#' @details  Each file will be read in the specified directory and any function with a valid signature will be added to the list of available functions in the shiny app.  
#' The function signature is a call to the function with a single argument "signature" which returns a list of three items: 1) a short name for the function, 2) 
#' the function name, and 3) a help string for the function.
#' @importFrom alphavantagepf av_add_analytic
#' @export
av_runShiny_addFunctions <- function(fun_dir="c:/d/src/R/avpfShinyFuncs/avpfshinyFuncs/R") {
  message("av_runShiny_addFunctions v 0.2 ")

  allfiles <-list.files(fun_directory,pattern="*\\.r",ignore.case=TRUE,full.names=TRUE)

  is_fn_def_with_params <- function(e, params = c("todo", "rv")) {
    if (!(is.call(e) &&
          as.character(e[[1]]) %in% c("<-", "=", "<<-") &&
          is.call(e[[3]]) &&
          identical(e[[3]][[1]], as.name("function")))) {
      return(FALSE)
    }
    fn_formals <- e[[3]][[2]]          # pairlist of formal args
    identical(names(fn_formals), params)
  }

  check_fn <- function(thisfn) {
    exprs <- parse(thisfn)
    matching_exprs <- Filter(is_fn_def_with_params, as.list(exprs))
    fn_names <- sapply(matching_exprs, function(e) as.character(e[[2]]))
    sapply(fn_names, \(nm) {
      toadd <- do.call(nm,list("signature"))
      if( length(toadd) != 3 ) { # Add if signature is returned
        message_if_red(TRUE,paste0("Function ",nm," does not have a valid signature"))
      } else {
        nadded <- nadded+1
        message( av_add_analytic(toadd[[1]],toadd[[2]],helpstr=toadd[[3]]) )  
      }
    })
  }

  nadded<-0
  u1 <- sapply(allfiles, \(thisfn) check_fn(thisfn))
  return(paste("Added ",nadded," functions from",fun_dir))
}
  
