#' Estimates N Rich Strip and Farmer practice from actively mapped NDVI
#' 
#' @param active_ndvi a raster layer of actively mapped NDVI
#' @param n_rich_percentile percentile at which to calculate the summary
#'   value for the N Rich Strip (percent)
#' @param farmer_practice_percentile percentile at which to calculate the summary
#'   value for the N Rich Strip (percent)
#'   
#' @export
#' 
estimate_n_rich_strip_farmer_practice <- function(active_ndvi,
                                                  n_rich_percentile = 95,
                                                  farmer_practice_percentile = 50){

  pct_values <- quantile(active_ndvi,
                         probs = c(n_rich_percentile/100,
                                   farmer_practice_percentile/100))
  
  return(list(n_rich_strip=pct_values[1],
              farmer_practice=pct_values[2]))
}