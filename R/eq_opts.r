# Implied VOlatilities

#' @name av_GPIV
#' @title Graph Implied Volatilites
#' @description Graph Implied Volatilites by Symbol, expiration code, and moneyness
#' @param todo Data structure containing  command and assets
#' @param rv Copy of internal av_runShiny() state
#' @returns List of output items displayed within to [av_runShiny()]
#' @seealso [av_runShiny_addFunctions()]
#' @details 
#' Graphs times series of Implied volatilties.  By default, 50 delta 1mo options are graphed, but can be overridden in the parmaeters following the 
#' `GPIV` function, or within the asset set as shown in the examples below:
#' @examples
#' \dontrun{
#' # Plot 50 delta calls 1 month out for three tickers
#' IBM;SPY;QQQ GPIV 
#' # Plot  25 delta calls 2 months out for three tickers
#' IBM;SPY;QQQ GPIV mo_2 C25 
#' # Plot 2 month options and 1 month 25 delta options for SPY, along with 1 month 50delta default
#' SPY,mo_2,C25;SPY,mo_1,C25;SPY GPiV
#' }
#' @import alphavantagepf
#' @import data.table
#' @importFrom FinanceGraphs fgts_dygraph
#' @importFrom stats cor quantile
#' @export
av_GPIV <- function(todo,rv) {
    if( is.character(todo) && todo=="signature" ) {
      return(list("GPIV","av_GPIV","Implied Volatility graphs"))
    }
    if(!exists("iv", envir=the_av)) {
        avsh_quick_message("Need Implied Vols for already in place; run av_add_options('iv') first")
        return() }
    toget <- optionset_to_DT(rv$assetline,s(rv$todoargs,sep=" "))
    toplot <- the_av$iv[toget,on=.(symbol,expcode,moneyn)][,.(variable, timestamp=ts,adjusted_close=iv)] |> narrowbydtstr(rv$dtstr_hist)
    cAssign("toplot;toget;rv;todo")
    if( nrow(toplot)<=0 ) {
      quick_message("Combination of options not found, check specification or inventory");
      iv_graph<-NULL
    }
    else { 
      iv_graph<- one_px_ts(toplot,rv,title="Implied Vols",extra_anno="") 
    }
    return(list("TS1"=iv_graph))
    }

#' @name av_GPIVS
#' @title Graph Implied Volatilite Spreads
#' @description Graph ATM Implied Volatilites
#' @param todo Data structure containing  command and assets
#' @param rv Copy of internal av_runShiny() state
#' @returns List of output items displayed within to [av_runShiny()]
#' @seealso [av_runShiny_addFunctions()]
#' @details 
#' Graphs Spread of first option less second option
#' @examples
#' \dontrun{
#' # Plot 50 delta calls 1 month out for three tickers
#' IBM;SPY GPIVS
#' }
#' @import alphavantagepf
#' @import data.table
#' @importFrom FinanceGraphs fgts_dygraph
#' @importFrom stats cor quantile
#' @export
av_GPIVS <- function(todo,rv) {
  if( is.character(todo) && todo=="signature" ) {
    return(list("GPIVS","av_GPISV","Implied Volatility spreads"))
  }
  if(!exists("iv", envir=the_av)) {
    avsh_quick_message("Need Implied Vols for already in place; run av_add_options('iv') first")
    return() }
  toget <- optionset_to_DT(rv$assetline,s(rv$todoargs,sep=" "))
  toplot <- the_av$iv[toget,on=.(symbol,expcode,moneyn)][,.(variable, timestamp=ts,adjusted_close=iv)] |> narrowbydtstr(rv$dtstr_hist)
  toplot_L <- toplot[variable==toget[1,]$variable,.(varlong=variable, timestamp,long=adjusted_close)]
  toplot_S <- toplot[variable==toget[2,]$variable,.(varshort=variable, timestamp,short=adjusted_close)]
  toplot <- toplot_L[toplot_s,on=.(timestamp)][!is.na(long) & !is.na(Short),][,
                .(timestamp,variable=paste0("(",varlong,")-(",varshort,")"),adjusted_close=long-short)]
  cAssign("toplot;toget;rv;todo")
  if( nrow(toplot)<=0 ) {
    quick_message("Combination of options not found, check specification or inventory");
    iv_graph<-NULL
  }
  else { 
    iv_graph<- one_px_ts(toplot,rv,title="Implied Vol Spreads",extra_anno="") 
  }
  return(list("TS1"=iv_graph))
}

#' @name av_OLAST
#' @title Summary of most recent term structure
#' @description Gives a tqble of most recent option term structures
#' @param todo Data structure containing  command and assets
#' @param rv Copy of internal av_runShiny() state
#' @returns List of output items displayed within to [av_runShiny()]
#' @seealso [av_runShiny_addFunctions()]
#' @details 
#' Uses precomeuted data from av_add_options("iv_nearest"). See data vignette for managing the options data stream.
#' @examples
#' \dontrun{
#' # Last term structures for IBM and SPY
#' IBM;SPY OLAST
#' }
#' @import alphavantagepf
#' @import data.table
#' @importFrom stats cor quantile
#' @export
av_OLAST <- function(todo,rv) {
  if( is.character(todo) && todo=="signature" ) {
    return(list("OLAST","av_OLAST","Last downloaded option term structure"))
  }
  if(!exists("iv_nearest", envir=the_av)) {
    avsh_quick_message("Need Implied Vol nearest set already in place; run av_add_options('iv_nearest') first")
    return() }
  av_rtn=list()
  toget <- optionset_to_DT(rv$assetline,s(rv$todoargs,sep=" "))
  opt_table <- the_av$iv_nearest[data.table(symbol=s(rv$assetline)), on=.(symbol)][order(symbol,dtoexp,type,expcode,ts)]
  opt_table <- opt_table[!data.table(moneyn=s("C0;P0;C90;P90")),on=.(moneyn)]
  lastts <- max(opt_table$ts)
  opt_last <- opt_table[ts==lastts,][order(symbol,dtoexp,expcode,delta)][,
              let(bapct=200*(ask-bid)/(ask+bid), vol_to_oi=volume/open_interest)]
  if( nrow(opt_last)<=0 ) {
    quick_message("Combination of options not found, check specification or inventory");
    return(list())
  }
  
  gt1a <- dcast(opt_last[,.(symbol,dtoexp,expcode,moneyn=paste0("iv_",moneyn),iv)],symbol + dtoexp + expcode ~ moneyn,value.var="iv")[order(symbol,dtoexp)]
  setcolorder(gt1a, paste0("iv_",s("P75;P50;P25;P10;P5;C5;C10;C25;C50;C75")), after="expcode")
  gt2a <- dcast(opt_last[,.(symbol,dtoexp,expcode,moneyn=paste0("k_",moneyn),strike)],symbol + dtoexp + expcode ~ moneyn,value.var="strike")[order(symbol,dtoexp)]
  setcolorder(gt2a, paste0("k_",s("P75;P50;P25;P10;P5;C5;C10;C25;C50;C75")), after="expcode")
  av_rtn[["GT1"]] <-
    gt1a[gt2a,on=.(symbol,dtoexp,expcode)] |> gt() |> gt.basetheme(digits=0) |>
            tab_spanner_delim(delim="_") |> 
              tab_style(style=list(cell_fill(color="lightcyan")), locations=cells_body(columns=ends_with("C50"))) |>
    tab_header(title="Term structure summary") |>
    tab_caption(paste0("Data for ",lastts))

  av_rtn[["GT2"]] <- 
    opt_last |> gt() |> gt.basetheme() |>
    cols_hide(columns=c(type,contractid,last,spot,gamma,vega,theta,rho,ITM,bid,ask,bid,ask_size,bid_size,ts)) |>
    tab_header(title="Current Term Structure All Data") |>
    tab_caption(paste0("Data for ",lastts))
  
  toplot=opt_last[dtoexp>=25,][,.SD[1],by=.(symbol,type,moneyn)][,let(absdelta=100*abs(delta), xlabel=paste0(type,":",strike))]
  av_rtn[["SCAT1"]] = fg_scatplot(toplot,"iv ~ absdelta + label:xlabel + color:symbol")
    # TOdo: Time series + levels
    return(av_rtn)
}


# -- Helper FUnctions
optionset_to_DT <- function(inline, default_args) {
  toget <- data.frame(x=s(inline)) |> tidyr::separate_wider_delim(x,",",names=s("symbol;expcode;moneyn"),too_many="drop",too_few="align_start")
  toget <- coalesce_DT_byentry(data.table(toget),data.table(expcode=fcoalesce(default_args[1],"MO_1"), moneyn=fcoalesce(default_args[2],"C50")))
  toget <- toget[,variable:=paste(symbol,expcode,moneyn)][]
  return(toget)  
}

