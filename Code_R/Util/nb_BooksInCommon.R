nb_BooksInCommon = function(data.Ratings){
  #INPUT  data.Ratings  : la base des notes
  #OUTPUT mat.InCommon  : la matrice contenant le nombre de films notés en commun pour chaque couple d'utilisateurs
  
  # Dimension du problème
  vect.Users = sort(unique(data.Ratings$User.ID))
  nb.Users = length(vect.Users) # nombre d'individus différents dans la data frame data.Ratings
  
  # Création de la matrice contenant le nombre de films notés en commun pour chaque couple d'utilisateurs
  mat.InCommon = matrix(NA, nrow = nb.Users, ncol = nb.Users) 
  
  cat(nb.Users)
  
  for (userIND1 in 1:(nb.Users-1)){
    

      cat(sprintf("|%0.f", userIND1))
    
    userID1 = vect.Users[userIND1]
    mat.BooksOfuserID1 = data.Ratings[data.Ratings$User.ID == userID1, c("ISBN", "Book.Rating")]
    mat.BooksOfuserID1 = mat.BooksOfuserID1[sort.list(mat.BooksOfuserID1[,1]),]
    
    for(userIND2 in (userIND1+1):nb.Users){
      
      userID2 = vect.Users[userIND2]
      mat.BooksOfuserID2 = data.Ratings[data.Ratings$User.ID == userID2, c("ISBN", "Book.Rating")]
      mat.BooksOfuserID2 = mat.BooksOfuserID2[sort.list(mat.BooksOfuserID2[,1]),]
      
      inCommon = length(intersect(mat.BooksOfuserID1$ISBN, mat.BooksOfuserID2$ISBN))
      
      mat.InCommon[userIND1, userIND2] = inCommon
      mat.InCommon[userIND2, userIND1] = inCommon 
    }
  }
  
  return(mat.InCommon)
}