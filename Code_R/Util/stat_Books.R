stat_Books = function(data.Ratings){
  # INPUT data.Ratings  : la base des notes
  # OUTPUT              : la base contenant les statistiques sur les notes des livres
  
  # Les statistiques pour un livre donné sont : 
  # - nb.Ratings  : le nombre de notes reçues ; 
  # - mean        : la moyenne de ses notes ; 
  # - sd          : l'écart-type de ses notes ; 
  # - max         : la note maximale reçue ; 
  # - min         : la note minimale reçue ; 
  # - med         : la note médiane
  
  ISBN = sort(unique(data.Ratings$ISBN))
  l=length(ISBN)
  t=tapply(1:length(data.Ratings$ISBN), data.Ratings$ISBN, length)
  if(length(t)!=l)
  {
    add=names(t)[!(names(t) %in% sort(unique(data.Ratings$ISBN)))]
    for(j in 1:length(add))
    {
      ISBN[l+j]=add[j]
    }
  }
  stat.Books = as.data.frame(ISBN)
  # Détermination du nombre de notes reçues
  # veRIFICATION DE LA TAILLE pour la suite 

  stat.Books$nb.Ratings=tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, length)
  
  # Détermination de la moyenne de chaque livre
  stat.Books$mean = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(mean(x),2))
  
  # Détermination de l'écart-type de chaque livre
  stat.Books$sd = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(sd(x),2))
  
  # Détermination de la note maximale de chaque livre
  stat.Books$max = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(max(x),2))
  
  # Détermination de la note minimale de chaque livre
  stat.Books$min = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(min(x),2))
  
  # Détermination de la note médiane de chaque livre
  stat.Books$med = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(median(x),2))
  
  return(stat.Books)
  
}