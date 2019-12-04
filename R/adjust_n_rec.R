#' Adjust raster N recommendation
#' 
#' @export
#' 
adjust_N_recommendation <- function(n_rec,
                                    min_appl_rate=NULL,
                                    max_appl_rate=NULL){

  if(!is.null(min_appl_rate)){
    check_range('min_appl_rate',min_appl_rate,
                min_val=0)
    n_rec[n_rec < min_appl_rate] <- min_appl_rate
  }

  if(!is.null(max_appl_rate)){
    check_range('max_appl_rate',max_appl_rate,
                min_val=0)
    n_rec[n_rec < max_appl_rate] <- max_appl_rate
  }
  
  return(n_rec)
}