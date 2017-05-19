deja_Vu = function(data.Ratings){
  #INPUT  data.Ratings : la base des notes 
  #OUTPUT list.DejaVu  : la liste comprenant pour chaque utilisateur les livres qu'il a notés
  
  vect.Users = sort(unique(data.Ratings$User.ID))
  list.DejaVu = lapply(vect.Users, function(x) sort(data.Ratings$ISBN[data.Ratings$User.ID == x]))
  
  return(list.DejaVu)
  
}