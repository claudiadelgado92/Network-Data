#
# Suite aux r�sultats obtenus gr�ce aux fonctions de pr�dictions naives (results_naivePredictionTest.tsv), 
# on choisit de prendre comme recommandation pour les diff�rents utilisateurs les films non vus ayant obtenus 
# les plus hautes moyennes car le pr�dicteur meanByMovie poss�de les erreurs minimales.
# On recommande les films qui ont d�pass� une certaine limite de visionnage nbMin.Ratings.
#
  
recommandation_meanByMovie = function (recap.Movies, list.dejaVu, userID, nb.Recommandations, nbMin.Ratings = 1){
  #INPUT  recap.Movies          : la base des films avec les statistiques
  #       list.dejaVu           : la liste des films not�s par utilisateur
  #       userID                : l'identifiant de l'utilisateur � qui l'on recommande
  #       nb.Recommandations    : nombre de films recommand�s
  #       nbMin.Ratings         : seuil de visionnage � partir duquel les films pourront �tre recommand�s
  #OUTPUT mat.Recommendations  : matrice contenant les identifiants de films recommand�s et la note pr�dite
  
  # G�n�ration du vecteur de films qu'userID a deja vu
  vect.DejaVu=list.dejaVu[[userID]]
  
  # Suppression des films deja visionn�s de stat.Movies
  recap.Movies = recap.Movies[!(recap.Movies$movieID %in% vect.DejaVu),]
  
  # Suppression des films n'ayant pas d�pass� le seuil de visionnage
  recap.Movies = recap.Movies[recap.Movies$nb.Ratings >= nbMin.Ratings,]
  
  # Tri de stat.Movies par ordre d�croissant vis-�-vis de la moyenne de chaque film        
  mat.Recommendations=recap.Movies[order(recap.Movies$mean,decreasing=TRUE), c("movieID","mean")]
  colnames(mat.Recommendations) = c("movieID", "prating")
  
  # Recommandation des nb.Recommandations films ayant les meilleurs moyennes  
  return(mat.Recommendations[1:nb.Recommandations,])
  
}