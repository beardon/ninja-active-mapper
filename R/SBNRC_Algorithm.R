
#' Implementing the Wheat algorithm from SBNRC NRate VRN equations excel sheet from Dr. Arnall's email
#' Creates a variable rate nitrogen application image based on Nitrogen Rich Strip (n_rich_strip), Farmer/Target (farmer_practice), Growing Degree Days (GDD), and active_ndvi(Target).
#'
#' @param n_rich_strip (originally NRS) This value corresponds to the N-rich strip (90th percentile or average of NDVI).
#' @param farmer_practice (originally FT) This parameter is the farmer practice NDVI taken next to N-rich strip or field average. 
#' @param GDD This parameter is the number of days with growing degrees greater than 0 since planting.
#' @param active_ndvi (originally Target) This input is generated from the NinjaActiveMapper::krige function;a raster(image).
#' @param min_appl_rate This is a check to ensure that nitrogen is not applied higher than the yield potential of the N-rich strip.
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