naive_predictions = function(list.Datasets, train){
  # INPUT   list.Datasets : liste des sous-datasets de data.Ratings
  #         train         : num�ro de l'apprentissage
  # OUTPUT                : data frame contenant les pr�dictions 
  
  ### PREAMBULE
  
  # Bases d'apprentissage et de test
  nb.Datasets = length(list.Datasets)
  dataset_to_keep = (1:nb.Datasets)[(1:nb.Datasets) != train]
  
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  test.Ratings = list.Datasets[[train]]
  
  # G�n�ration des statistiques sur les donn�es de l'apprentissage
  stat.Users = stat_Users(train.Ratings)
  stat.Books = stat_Books(train.Ratings)
  
  # Dimension de la base de tests
  nb.Tests = dim(test.Ratings)[1]
  
  # El�ments pr�sents dans la base d'apprentissage
  vect.BooksInTrain = sort(unique(stat.Books$ISBN))
  vect.UsersInTrain = sort(unique(stat.Users$User.ID))
  
  ### PREDICTION
  
  #pr�diction aleatoire sur [1,10]
  test.Ratings$random_unif = round(runif(nb.Tests,1,10), 2)
  
  #pr�diction par la note moyenne des utilisateurs
  test.Ratings$meanOfUsers = round(mean(stat.Users$mean), 2)
  
  #pr�diction par la note moyenne des films
  test.Ratings$meanOfBooks = round(mean(stat.Books$mean), 2)
  
  #pr�diction par la note moyenne globale
  test.Ratings$mean = round(mean(train.Ratings$Book.Rating), 2)
  
  #pr�diction par la moyenne par utilisateur
  meanByUser = function(User.ID, stat.Users){
    if(User.ID %in% vect.UsersInTrain){
      return(stat.Users$mean[stat.Users$User.ID == User.ID])
    }
    else{
      return(NA)
    }
  }
  
  test.Ratings$meanByUser = sapply(test.Ratings$User.ID, meanByUser, stat.Users)
  
  #pr�diction par la moyenne par livre
  meanByBook = function(ISBN, stat.Books){
    if(ISBN %in% vect.BooksInTrain){
      return(stat.Books$mean[stat.Books$ISBN == ISBN])
    }
    else{
      return(NA)
    }
  }
  
  test.Ratings$meanByBook = sapply(test.Ratings$ISBN, meanByBook, stat.Books)
  
  return(test.Ratings)
  
}