stat_Books = function(data.Ratings){
  # INPUT data.Ratings  : la base des notes
  # OUTPUT              : la base contenant les statistiques sur les notes des livres
  
  # Les statistiques pour un livre donn� sont : 
  # - nb.Ratings  : le nombre de notes re�ues ; 
  # - mean        : la moyenne de ses notes ; 
  # - sd          : l'�cart-type de ses notes ; 
  # - max         : la note maximale re�ue ; 
  # - min         : la note minimale re�ue ; 
  # - med         : la note m�diane
  
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
  # D�termination du nombre de notes re�ues
  # veRIFICATION DE LA TAILLE pour la suite 

  stat.Books$nb.Ratings=tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, length)
  
  # D�termination de la moyenne de chaque livre
  stat.Books$mean = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(mean(x),2))
  
  # D�termination de l'�cart-type de chaque livre
  stat.Books$sd = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(sd(x),2))
  
  # D�termination de la note maximale de chaque livre
  stat.Books$max = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(max(x),2))
  
  # D�termination de la note minimale de chaque livre
  stat.Books$min = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(min(x),2))
  
  # D�termination de la note m�diane de chaque livre
  stat.Books$med = tapply(data.Ratings$Book.Rating, data.Ratings$ISBN, function(x) round(median(x),2))
  
  return(stat.Books)
  
}