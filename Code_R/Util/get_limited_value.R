get_limited_value = function(value){
  # INPUT  value  : une prédiction de note (nombre ou NA)
  # OUTPUT        : une prédiction comprise dans [1,10]
  if(is.na(value)){
    return(NA)
  }
  else if(value > 10){
    return(10)
  }
  else if(value < 1){
    return(1)
  }
  else{
    return(value)
  }
}