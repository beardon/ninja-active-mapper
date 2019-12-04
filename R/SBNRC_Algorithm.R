#' Creates a variable rate nitrogen application image based on Nitrogen Rich Strip (n_rich_strip), Farmer/Target (farmer_practice), Growing Degree Days (GDD), and active_ndvi(Target).
#'
#' @param n_rich_strip (originally NRS) the average NDVI of the N-rich strip (unitless 0-1 scale)
#' @param farmer_practice (originally FT) the average NDVI of farmer practice adjacent to the N-rich strip  (unitless 0-1 scale)
#' @param GDD This parameter is the number of days with growing degrees greater than 0 since planting (1 to 260)
#' @param active_ndvi (originally Target) A raster image generated from the NinjaActiveMapper::krige function (unitless 0-1 scale)
#' @param min_appl_rate Minimum nitrogen application rate (lbs per acre) to be applied across the field (range would depend on crop, but probably 0 to 50?)
#' 
#' @return The SBNRC adjusted Nitrogen rate
#' @export
#' @examples
#' VRN(n_rich_strip=0.7,farmer_practice=0.54,GDD=85,active_ndvi,min_appl_rate=0)

generate_variable_rate=function(yld_fert_field,yld_ptnl_field,
                                c=0.024,d=0.60){

  #yld_ptnl_field (originally YP0); Yield potential of the field
  #response_index (originally RI); Response index
  #yld_ptnl_nRich (originally YPNR); Yield potential of N-rich strip
  #yld_fert_field (originally YPN); Yield of fertilized field
  #variable_rate_N(originally VRN); Variable rate of N to apply
  #(originally INSEY); update this when a good name/description is found
  #(originally INSEY_NR); update this when a good name/description is found
  
  variable_rate_N = (yld_fert_field - yld_ptnl_field) * c / d

  return(variable_rate_N)
}

#' Calculate yield potential
#' 
#' @export
#' 
yield_potential <- function(NDVI,GDD,A,b){

  check_range('GDD',GDD,
              ne_val=0,min_val=0)

  check_range('NDVI',NDVI,
              ne_val=0,min_val=0,max_val=1)
  
  yld_pot <- A*exp(NDVI/GDD*b)
  
  return(yld_pot)
}

#' Calculate yield potential
#' 
#' @export
#' 
fertilized_yield_potential <- function(yp_field,yp_N_rich,
                                       N_rich_strip,farmer_practice,
                                       slope,intercept){

  check_range('N_rich_strip',N_rich_strip,ne_val=0)

  check_range('farmer_practice',farmer_practice,
              ne_val=0,min_val=0,max_val=1)
  
  response_index <- slope*(N_rich_strip/farmer_practice)+intercept

  yp_fertilized <- yp_field*response_index
  
  # Cap fertilized yield potential at yield potential of N rich strip
  yp_fertilized[yp_fertilized>yp_N_rich] <- yp_N_rich
  
  yp_fertilized[yp_field > yp_N_rich] <- yp_field[yp_field > yp_N_rich]
  
  return(yp_fertilized)
}