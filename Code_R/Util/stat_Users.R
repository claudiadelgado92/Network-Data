stat_Users = function(data.Ratings){
  # INPUT data.Ratings  : la base des notes
  # OUTPUT              : la base contenant les statistiques sur les notes attribu�es
  
  # Les statistiques pour un utilisateur donn� sont : 
  # - nb.Ratings  : le nombre de notes attribu�es ; 
  # - mean        : la moyenne de ses notes ; 
  # - sd          : l'�cart-type de ses notes ; 
  # - max         : la note maximale attribu�e ; 
  # - min         : la note minimale attribu�e ; 
  # - med         : la note m�diane
  
  User.ID = sort(unique(data.Ratings$User.ID))
  stat.Users = as.data.frame(User.ID)
  
  # D�termination du nombre de notes attribu�es
  stat.Users$nb.Ratings = tapply(data.Ratings$Book.Rating, data.Ratings$User.ID, length)
  
  # D�termination de la moyenne de chaque utilisateur
  stat.Users$mean = tapply(data.Ratings$Book.Rating, data.Ratings$User.ID, function(x) round(mean(x),2))
  
  # D�termination de l'�cart-type de chaque utilisateur
  stat.Users$sd = tapply(data.Ratings$Book.Rating, data.Ratings$User.ID, function(x) round(sd(x),2))
  
  # D�termination de la note maximale de chaque utilisateur
  stat.Users$max = tapply(data.Ratings$Book.Rating, data.Ratings$User.ID, function(x) round(max(x),2))
  
  # D�termination de la note minimale de chaque utilisateur
  stat.Users$min = tapply(data.Ratings$Book.Rating, data.Ratings$User.ID, function(x) round(min(x),2))
  
  # D�termination de la note m�diane de chaque utilisateur
  stat.Users$med = tapply(data.Ratings$Book.Rating, data.Ratings$User.ID, function(x) round(median(x),2))
  
  return(stat.Users)
  
}