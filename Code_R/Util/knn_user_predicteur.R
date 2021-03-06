# Fonction pour obtenir la moyenne des notes d'un individu
get_meanRatings = function(User.ID, stat.Users){
  if(is.na(User.ID)){
    return(NA)
  }
  else{
    res = stat.Users$mean[stat.Users$User.ID == User.ID]
    return(res)
  }
}

# Fonction pour limiter la valeur de la pr�diction
limited_value = function(value){
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

knn_user_predicteur = function(weights, ratings, stat.Users, User.ID, predicteur, K_Neighbors){
  # INPUT   weights     : vecteur contenant le degr� de similarit� par plus proches voisins
  #         ratings     : vecteur contenant les notes des plus proches voisins
  #         stat.Users  : statistiques des utilsateurs
  #         User.ID      : identifiant de l'utilisateur
  #         predicteur  : la fonction de pr�diction
  #         K_Neighbors : les identifiants des plus proches voisins
  # OUTPUT              : la note pr�dite
  
  # Calcul des pr�dicteurs
  meanOfUser = as.numeric(stat.Users$mean[stat.Users$User.ID == User.ID])
  meanOfNeighbors = sapply(K_Neighbors,get_meanRatings, stat.Users)
  
  # Notation  : &a : pour les pr�dicteurs pond�r�s, la valeur est major�e/minor�e et le d�nominateur est la somme des valeurs absolues (a pour absolute)
  
  switch(predicteur, 
         'mean' = mean(ratings, na.rm = TRUE),
         'weighted' = limited_value(sum(ratings * weights, na.rm = TRUE)/ sum(weights, na.rm = TRUE)),
         'weighted-centered' = limited_value(meanOfUser + sum((ratings-meanOfNeighbors) * weights, na.rm = TRUE)/ sum(weights, na.rm = TRUE)),
         'weighted&a' = limited_value(sum(ratings * weights, na.rm = TRUE)/ sum(abs(weights), na.rm = TRUE)),
         'weighted-centered&a' = limited_value(meanOfUser + sum((ratings-meanOfNeighbors) * weights, na.rm = TRUE)/ sum(abs(weights), na.rm = TRUE))
  )
}