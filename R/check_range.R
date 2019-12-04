check_range <- function(vname,val,
                        ne_val=NULL,min_val=NULL,max_val=NULL){
  err <- NULL

  if(! is.null(ne_val) && any((val == ne_val)[],na.rm=TRUE)){
    err <- str_c(vname," was defined as equal to ",ne_val,'.')
  }else if(! is.null(min_val) && any((val < min_val)[],na.rm = TRUE)){
    err <- str_c(vname," was defined as less than ",min_val,'.')
  }else if(! is.null(max_val) && any((val > max_val)[],na.rm=TRUE)){
    err <- str_c(vname," was defined as more than ",max_val,'.')
  }
  
  if(!is.null(err)){
    stop(err)
  }
  
  return(invisible())
  
}