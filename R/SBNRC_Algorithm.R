
#' Implementing the Wheat algorithm from SBNRC NRate VRN equations excel sheet from Dr. Arnall's email
#' Creates Nrate application based on Nitrogen Rich Strip (NRS), Farmer Target (FT), Growing Degree Days (GDD), and Target.
#'
#' @param NRS The kriged map of the field.
#' @return The pixel based N-Rate.
#' @export
#' @examples
#' VRN(NRS=kriged)

VRN=function(NRS=0.7,FT=0.54,GDD=85,Target=0.3){
 # NRS=0.7;FT=0.54;GDD=85;Target=0.3
  RI=1.69*(NRS/FT)-0.6
  INSEY=Target/GDD
  INSEY_NR=NRS/GDD
  YP0=590*exp(INSEY*258.2)/1.12/60
  YPN=YP0*RI
  YPNR=(YPN-YP0)*60*0.024/0.6
  #YPNR=590*exp(INSEY_NR*258.2) /1.12/60
  YPN[YPN>YPNR]=YPNR[YPN>YPNR]
  VRN = (YPN - YP0) * 60 *.024 / .60
  return(VRN)
}


# VRN=function(NDVI,GDD=85,YPNR,YP0,NRstrip,FT){
#     #FT = farmer/target
#   
#     #Use NFOA to determine N rate for field resolution no higher than 1m2, 2m2 is good:
#     #IF(YP0*RI < YPNR, YPN*RI, YPNR)-YP0 *60 lb/bu *.024 %N / .60%NUE
#     #translates to: if YPO*RI less than YPNR, use YPN*RI, else use YPNR( + use rest of equation)
#     #https://exceljet.net/excel-functions/excel-if-function
#     
#     #RI_HARVEST = 1.69 * (RI-NDVI) - 0.60
#     RI=NRstrip/FT
#   
#     INSEY = NDVI/GDD
#     
#     YP0 = (590 * exp(INSEY * 258.2)) / 1.12 / 60
#     
#     YPN = YP0*RI
#     YPN[YPN>YPNR] = YPNR
#     
# #    if(YPO*RI < YPNR){
# #       VRN_temp = (YPO*RI)
# #    } else {
# #       VRN_temp = YPNR
# #    }
#     
#      VRN = (YPN - YP0) * 60 *.024 / .60
#     
#     return(VRN)
# }
