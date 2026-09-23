#' rolling_correlations
#'
#' @name rolling_correlations
#' @title Rolling correlation analysis
#' @description Rolling correlation analysis
#' @param todo Data structure containing  command and assets
#' @param rv Copy of internal av_runShiny() state
#' @returns List of output items
#' @seealso [av_runShiny_addFunctions()]
#' @details 
#' @import alphavantagepf
#' @import gt
#' @import data.table
#' @importFrom FinanceGraphs fgts_dygraph
#' @importFrom stats cor quantile
#' @export
rolling_correlations <- function(todo,rv) {
  symbol=rtn=adjusted_close=timestamp=var1=var2=rtn1=rtn2=rcorr=NULL
  tickers_to_get=strsplit(rv$assetline,";")[[1]]
  roll_window <- fcoalesce(as.numeric(rv$todoargs),22) # default to 22 day rolling correlation
  if(length(tickers_to_get)<3) { 
    avsh_quick_message("Need at least 3 tickers")
    return() }
  # Dont forget to add data in case it is needed
  av_add_px(equitylist=tickers_to_get)
  # Load the internal data store
  allpx <- av_load_shinydata("pxd")[data.table(symbol=tickers_to_get),on=.(symbol)]
  allpx <- allpx[,rtn:=c(NA_real_,diff(log(adjusted_close),1)), by=.(symbol)]
  newdtstr <- FinanceGraphs::extenddtstr(rv$dtstr_hist,begchg=-ceiling(31/22*roll_window))
  allpx <- allpx [,.(symbol,timestamp,rtn)] |> FinanceGraphs::narrowbydtstr(newdtstr)
  
  # Make pairwise data
  pairs <- CJ(var1=tickers_to_get,var2=tickers_to_get)[var1<var2,]
  corDT1<- allpx[,.(timestamp, var1=symbol, rtn1=rtn)][pairs,on=.(var1),allow.cartesian=TRUE]
  corDT<- allpx[,.(timestamp, var2=symbol, rtn2=rtn)][corDT1,on=.(var2,timestamp),allow.cartesian=TRUE]
  
  # Rolling correlations
  rollcor_DT <- corDT[,rcorr:=frollapply(.SD,roll_window,\(x) stats::cor(x$rtn1,x$rtn2),by.column=FALSE), by=.(var1,var2)]
  cornames <- c("corr_p25","corr_p50","corr_p75")
  rollcor_toplot <- rollcor_DT[, 
                               (cornames):=as.list(stats::quantile(.SD$rcorr,probs=c(0.25,0.5,0.75),na.rm=TRUE)), by=.(timestamp)][
                                 ,.SD, .SDcols=c("timestamp",cornames)]    
  rollcorr_dyg <- fgts_dygraph(rollcor_toplot,title=paste0("Rolling percentiles of ",roll_window," bd correlations"),
                               roller=1,events=rv$ts_events)
  # Overall correlations for period
  corDT <- corDT |> FinanceGraphs::narrowbydtstr(rv$dtstr_hist)
  allcorr <- corDT[,.(allcorr=stats::cor(rtn1,rtn2,use="pairwise.complete.obs")),by=.(var1,var2)]
  allcorr_gt <- dcast(allcorr,var1 ~ var2,value.var="allcorr") |> gt() |> 
    tab_header(title=paste0("Correlation matrix for ",rv$dtstr_hist)) |> 
    sub_missing(missing_text="--")
  # Last Percentiles
  corrpct <- rollcor_DT[,.(lastpctile=100*last(frank(rcorr,na.last=NA))/.N), by=.(var1,var2)]
  pctlast_gt <- dcast(corrpct,var1 ~ var2,value.var="lastpctile") |> gt() |> 
    tab_header(title=paste0("Last correlation percentile ",rv$dtstr_hist)) |> 
    fmt_number(decimals=1) |> sub_missing(missing_text="--")
  
  # Return list
  return(list("GT3L"=allcorr_gt,"TS1"=rollcorr_dyg,"GT3R"=pctlast_gt))
}

