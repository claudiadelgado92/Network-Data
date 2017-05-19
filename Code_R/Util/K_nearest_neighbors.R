K_nearest_neighbors = function(User.ID, ISBN, K, list.dejaVu, vect.Users, mat.sim){
  # INPUT   User.ID          : l'identifiant de l'utilisateur
  #         ISBN         : ID du livre dont on veut prédire la note  
  #         K               : le nombre de voisins
  #         list.dejaVu     : la liste des livres notés par utilisateur
  #         vect.Users      : l'ensemble des utilisateurs
  #         mat.sim         : la matrice de similarité
  # OUTPUT  mat.neighbors   : les identifiants des (au plus) K plus proches voisins pour le livre ISBN pour User.ID et leur degré de similarité
  
  # Plus spécifiquement, cette fonction retourne la matrice de taille Kx2 contenant les valeurs 
  # de vect.Users (ie les identifiants des utilisateurs), à partir des calculs de proximité 
  # (au sens de la matrice de similarité), de User.ID pour le livre ISBN. 
  # Rq : un utilisateur ne peut se prétendre un plus proche voisin d'un utilisateur X pour le livre Y
  # que s'il a noté le livre Y, d'où l'utilité de list.dejaVu.
  
  # La position de User.ID dans la matrice de similarité
  userIND = which(vect.Users == User.ID)
  
  # Génération de vect.Neighbors : vecteur contenant les plus proches voisins de User.ID au sens général
  vect.Neighbors = order(mat.sim[userIND,], decreasing = TRUE)
  
  # Génération de vect.Similarity : vecteur contenant les degrés de similarité de  tous les utilisateurs vis-à-vis de User.ID
  vect.Similarity = mat.sim[userIND,vect.Neighbors]
  nb.OtherUsers = length(vect.Similarity) #nombre maximal de plus proches voisins
  
  # Création de la matrice des plus proches voisins pour le livre ISBN (contient les identifiants et les coefficients de similarité)
  mat.neighbors = as.data.frame(matrix(NA, nrow = K, ncol = 2))
  colnames(mat.neighbors) = c("neighbors", "similarities")
  
  userIND2 = 1 #indice dans vect.Neighbors
  nb.Neighbors = 0 #indice dans mat.neighbors
  
  while((nb.Neighbors < K) & (userIND2 <= nb.OtherUsers)){
    User.ID2 = vect.Neighbors[userIND2]
    if(ISBN %in% list.dejaVu[[User.ID2]]){
      nb.Neighbors = nb.Neighbors+1
      mat.neighbors$neighbors[nb.Neighbors] = User.ID2
      mat.neighbors$similarities[nb.Neighbors] = vect.Similarity[userIND2]
    }
    userIND2 = userIND2+1
  }
  
  return(mat.neighbors)
}