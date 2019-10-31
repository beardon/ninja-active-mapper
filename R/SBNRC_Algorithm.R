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

generate_variable_rate=function(n_rich_strip=0.7,farmer_practice=0.54,GDD=85,active_ndvi=0.3,min_appl_rate=0){

  #yld_ptnl_field (originally YP0); Yield potential of the field
  #response_index (originally RI); Response index
  #yld_ptnl_nRich (originally YPNR); Yield potential of N-rich strip
  #yld_fert_field (originally YPN); Yield of fertilized field
  #variable_rate_N(originally VRN); Variable rate of N to apply
  #(originally INSEY); update this when a good name/description is found
  #(originally INSEY_NR); update this when a good name/description is found
  
  check_range('n_rich_strip',n_rich_strip,ne_val=0)
  check_range('farmer_practice',farmer_practice,
              ne_val=0,min_val=0,max_val=1)
  check_range('farmer_practice',farmer_practice,
              ne_val=0,min_val=0,max_val=1)
  check_range('GDD',GDD,
              ne_val=0,min_val=0)
  check_range('min_appl_rate',min_appl_rate,
              min_val=0)
  
  response_index=1.69*(n_rich_strip/farmer_practice)-0.6
  INSEY=active_ndvi/GDD
  INSEY_NR=n_rich_strip/GDD
  yld_ptnl_field=590*exp(INSEY*258.2)/1.12/60
  yld_fert_field=yld_ptnl_field*response_index
  yld_ptnl_nRich=590*exp(INSEY_NR*258.2)/1.12/60 
  yld_fert_field[yld_fert_field>yld_ptnl_nRich]=yld_ptnl_nRich
  variable_rate_N = (yld_fert_field - yld_ptnl_field) * 60 *.024 / .60
  variable_rate_N[variable_rate_N<min_appl_rate] <- min_appl_rate
  
  return(variable_rate_N)
}