K_nearest_neighbors = function(User.ID, ISBN, K, list.dejaVu, vect.Users, mat.sim){
  # INPUT   User.ID          : l'identifiant de l'utilisateur
  #         ISBN         : ID du livre dont on veut pr�dire la note  
  #         K               : le nombre de voisins
  #         list.dejaVu     : la liste des livres not�s par utilisateur
  #         vect.Users      : l'ensemble des utilisateurs
  #         mat.sim         : la matrice de similarit�
  # OUTPUT  mat.neighbors   : les identifiants des (au plus) K plus proches voisins pour le livre ISBN pour User.ID et leur degr� de similarit�
  
  # Plus sp�cifiquement, cette fonction retourne la matrice de taille Kx2 contenant les valeurs 
  # de vect.Users (ie les identifiants des utilisateurs), � partir des calculs de proximit� 
  # (au sens de la matrice de similarit�), de User.ID pour le livre ISBN. 
  # Rq : un utilisateur ne peut se pr�tendre un plus proche voisin d'un utilisateur X pour le livre Y
  # que s'il a not� le livre Y, d'o� l'utilit� de list.dejaVu.
  
  # La position de User.ID dans la matrice de similarit�
  userIND = which(vect.Users == User.ID)
  
  # G�n�ration de vect.Neighbors : vecteur contenant les plus proches voisins de User.ID au sens g�n�ral
  vect.Neighbors = order(mat.sim[userIND,], decreasing = TRUE)
  
  # G�n�ration de vect.Similarity : vecteur contenant les degr�s de similarit� de  tous les utilisateurs vis-�-vis de User.ID
  vect.Similarity = mat.sim[userIND,vect.Neighbors]
  nb.OtherUsers = length(vect.Similarity) #nombre maximal de plus proches voisins
  
  # Cr�ation de la matrice des plus proches voisins pour le livre ISBN (contient les identifiants et les coefficients de similarit�)
  mat.neighbors = as.data.frame(matrix(NA, nrow = K, ncol = 2))
  colnames(mat.neighbors) = c("neighbors", "similarities")
  
  userIND2 = 1 #indice dans vect.Neighbors
  nb.Neighbors = 0 #indice dans mat.neighbors
  h=0
  
  while((nb.Neighbors < K) & (userIND2 <= nb.OtherUsers)){
    print(h)
    h=h+1
    User.ID2 = vect.Neighbors[userIND2]
    if(ISBN %in% list.dejaVu[[userIND2]]){
      nb.Neighbors = nb.Neighbors+1
      mat.neighbors$neighbors[nb.Neighbors] = User.ID2
      mat.neighbors$similarities[nb.Neighbors] = vect.Similarity[userIND2]
    }
    userIND2 = userIND2+1
  }
  
  return(mat.neighbors)
}