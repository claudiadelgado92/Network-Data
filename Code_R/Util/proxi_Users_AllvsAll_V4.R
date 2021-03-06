proxi_Users_AllvsAll = function(data.Ratings,mat.InCommon, similarity="pearson"){
  #INPUT    data.Ratings  : la base des notes
  #         similarity    : la m�trique utilis�e pour calculer la similarit� ("pearson", "nrmse", "nmae", "RFP")
  #OUTPUT   mat.sim       : matrice contenant le degr� de similarit� entre les utilisateurs (sans filtre)
  
  # Dimension du probl�me
  vect.Users = sort(unique(data.Ratings$User.ID))
  nb.Users = length(vect.Users) # nombre d'individus diff�rents dans la data frame data.Rating
  nb.Books= length(unique(data.Ratings$ISBN))
  
  g <- graph.data.frame(data.Ratings)
  
  A <- get.adjacency(g, attr = "Book.Rating", names = T, sparse = F)
  A = A[1:nb.Users,]
  A = A[,(nb.Users+1):(nb.Users+nb.Books)]
  
  A[A==0]<-NA
  B=mat.InCommon
  B[B==0]<-NA
  
  A=t(A)
  mat.sim <- cor(A, use = "pairwise.complete.obs")
  
  if(similarity=="pearson")
  {
    return(mat.sim)
  }
  else if (similarity=="RFP"){
    return(mat.sim*log(B))
  }
  else
  {
    print('mauvaise similarit�')
    break
  }
}