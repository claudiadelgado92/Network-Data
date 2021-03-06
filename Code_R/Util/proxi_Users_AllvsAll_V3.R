proxi_Users_AllvsAll = function(data.Ratings, similarity){
  #INPUT    data.Ratings  : la base des notes
  #         similarity    : la m�trique utilis�e pour calculer la similarit� ("pearson", "nrmse", "nmae", "RFP")
  #OUTPUT   mat.sim       : matrice contenant le degr� de similarit� entre les utilisateurs (sans filtre)
  
  # Dimension du probl�me
  
  #fonction2
  test_V2 = function(userIND1, data.Ratings, similarity,vect.Users){
    #INPUT  userIND1            : l'identifiant du premier utilisateur
    #       data.Ratings        : la base des notes
    #       similarity          : la m�trique utilis�e pour calculer la similarit� ("RFP")
    
    print(userIND1)
    User.ID1 = vect.Users[userIND1]
    mat.BooksOfUser.ID1 = data.Ratings[data.Ratings$User.ID == User.ID1, c("ISBN", "Book.Rating")]
    mat.BooksOfUser.ID1 = mat.BooksOfUser.ID1[sort.list(mat.BooksOfUser.ID1[,1]),]
    
    t=sapply(vect.Users[(userIND1+1):nb.Users], proxi_Users_V2,mat.BooksOfUser.ID1, data.Ratings, similarity)
    return(t)
  }
  
  #fonction2
  proxi_Users_V2 = function(User.ID2,mat.BooksOfUser.ID1, data.Ratings, similarity){
    #INPUT  mat.BooksOfUser.ID1 : l'ensemble des (livres,notes) vus par le premier utilisateur ordonn� par l'ID des livres
    #       User.ID2             : l'identifiant du second utilisateur
    #       data.Ratings        : la base des notes
    #       similarity          : la m�trique utilis�e pour calculer la similarit� ("RFP")
    #OUTPUT                     : le degr� de similarit� en fonction des notes entre les deux utilisateurs
    
    # Matrice contenant les couples (livre, note) de l'utilisateur User.ID2 tri� par identifiant de livre
    mat.BooksOfUser.ID2 = data.Ratings[data.Ratings$User.ID == User.ID2, c("ISBN", "Book.Rating")]
    mat.BooksOfUser.ID2 = mat.BooksOfUser.ID2[sort.list(mat.BooksOfUser.ID2[,1]),]
    
    # Vecteur contenant les livres vus par User.ID1 et User.ID2
    vect.BooksInCommon = intersect(mat.BooksOfUser.ID1$ISBN, mat.BooksOfUser.ID2$ISBN)
    
    # Matrice contenant les notes des livres vus en commun par User.ID1 et User.ID2
    mat.BooksinCommon = matrix(NA,ncol = length(vect.BooksInCommon), nrow = 2)
    mat1=mat.BooksOfUser.ID1[mat.BooksOfUser.ID1$ISBN %in% vect.BooksInCommon,]
    mat2=mat.BooksOfUser.ID2[mat.BooksOfUser.ID2$ISBN %in% vect.BooksInCommon,]
    mat.BooksinCommon[1,] = mat1[match(unique(mat1$ISBN), mat1$ISBN),]$Book.Rating
    mat.BooksinCommon[2,] = mat2[match(unique(mat2$ISBN), mat2$ISBN),]$Book.Rating
    
    # Calcul des degr�s de similarit�
    switch(similarity,
           'RFP'     = cor(mat.BooksinCommon[1,],mat.BooksinCommon[2,], method="pearson") * log(length(vect.BooksInCommon))
    )
    
  }
  
  
  
  vect.Users = sort(unique(data.Ratings$User.ID))
  nb.Users = length(vect.Users) # nombre d'individus diff�rents dans la data frame data.Ratings
  
  # Cr�ation de la matrice des similarit�s
  mat.sim = matrix(NA, nrow = nb.Users, ncol = nb.Users) 
  
  cat(nb.Users)
  
  k=sapply(c(1:nb.Users),test_V2, data.Ratings, similarity,vect.Users)
  
  return(k)
}