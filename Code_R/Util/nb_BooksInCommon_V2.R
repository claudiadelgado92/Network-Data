nb_BooksInCommon = function(data.Ratings){
  #INPUT  data.Ratings  : la base des notes
  #OUTPUT mat.InCommon  : la matrice contenant le nombre de films not?s en commun pour chaque couple d'utilisateurs
  
  # formatage en graphe
  g <- graph_from_data_frame(data.Ratings)
  
  # transformation en graphe bipartite avec l'attribut "type"
  type <- (degree(g, mode="in")>0)
  g <- set_vertex_attr(g,name="type", value=type)
  cat(summary(V(g)$type))
  
  # Projection (pondérée) et récupération de la matrice d'adjacence
  usersgraph <- bipartite_projection(g, multiplicity = T, which = F)
  mat <- get.adjacency(usersgraph, type="both", attr="weight", names=TRUE)
  nb.Users=length(V(usersgraph)$name)
  mat.InCommon = matrix(mat, nrow = nb.Users, ncol = nb.Users) 
  return(mat.InCommon)
}