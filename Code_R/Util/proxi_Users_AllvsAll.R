proxi_Users_AllvsAll = function(data.Ratings, similarity){
  #INPUT    data.Ratings  : la base des notes
  #         similarity    : la m�trique utilis�e pour calculer la similarit� ("pearson", "nrmse", "nmae", "RFP")
  #OUTPUT   mat.sim       : matrice contenant le degr� de similarit� entre les utilisateurs (sans filtre)
  
  # Dimension du probl�me
  vect.Users = sort(unique(data.Ratings$User.ID))
  nb.Users = length(vect.Users) # nombre d'individus diff�rents dans la data frame data.Ratings
  
  # Cr�ation de la matrice des similarit�s
  mat.sim = matrix(NA, nrow = nb.Users, ncol = nb.Users) 
  
  cat(nb.Users)
  
  for (userIND1 in 1:(nb.Users-1)){
    
    print(userIND1)
    #if(userIND1%%100==0){
    #  cat(sprintf("|%0.f", userIND1))
    #}
    User.ID1 = vect.Users[userIND1]
    mat.BooksOfUser.ID1 = data.Ratings[data.Ratings$User.ID == User.ID1, c("ISBN", "Book.Rating")]
    mat.BooksOfUser.ID1 = mat.BooksOfUser.ID1[sort.list(mat.BooksOfUser.ID1[,1]),]
    
    for(userIND2 in (userIND1+1):nb.Users){
      User.ID2 = vect.Users[userIND2]
      sim = proxi_Users(mat.BooksOfUser.ID1, User.ID2, data.Ratings, similarity)
      
      mat.sim[userIND1,userIND2] = sim
      mat.sim[userIND2,userIND1] = sim
    }
  }
  
  return(mat.sim)
}