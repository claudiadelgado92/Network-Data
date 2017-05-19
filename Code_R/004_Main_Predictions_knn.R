# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE -    3A DS - Statistical Analysis of Network Data
#    Sujet : Mapping d'un réseau littéraire à partir de notes données par différents lecteurs
#       Encadrant : Eric Kolaczyk
#       Etudiants : Damien Babet, Claudia Delgado, Gabriele Ranieri
#
#       Fichier : 004_Main_Predictions_knn.R
#       Description : résultats de la méthode knn pour la validation croisée et la base test
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



# ===================================== 1.PREAMBULE ===============================================

# Clean up
rm(list=ls()) 
cat("\014") 
setwd("C:/Users/cooky/Documents/ENSAE_3A/SAND")


# ====================== 2.CHARGEMENT DES BASES D'APPRENTISSAGE ET DE TEST ==========================

cat(sprintf("Les sous-bases proposés sont de taille : 5\n"))
nb.Datasets = 5

file_list.Datasets = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/list.Datasets.Train.Rdata")
load(file = file_list.Datasets)

# =================== 3.CHARGEMENT DES FONCTIONS ANNEXES  ================================

source("./Code_R/Util/K_nearest_neighbors.R")
source("./Code_R/Util/knn_user_predictions.R", encoding = 'UTF-8')
source("./Code_R/Util/knn_user_predicteur.R")
source("./Code_R/Util/get_limited_value.R")

library("hydroGOF")
library("zoo", lib.loc="~/R/win-library/3.3")

# ============================== 4.CHOIX DE PARAMETRES =========================================

Kmin = 25
Kmax = 50
Kpas = 1
seq_K = seq(Kmin, Kmax, by = Kpas)
nb.K = length(seq_K)

similarity_names = c("RFP") 
list.nbMin.InCommon = c(0)
method_names = c("weighted-centered&a")

# Calcul du nombre de prédicteurs
# Noms des familles de modèles : similarité_nbMin.InCommon_predicteur

nb.similarities = length(similarity_names)
nb.nbMin.InCommon = length(list.nbMin.InCommon)
nb.predicteurs = length(method_names)

mat.models = expand.grid(similarity_names, list.nbMin.InCommon, method_names, stringsAsFactors = FALSE)
colnames(mat.models) = c("similarity", "nbMin.InCommon", "predicteur")
mat.models$name = paste0(mat.models$similarity, "_", mat.models$nbMin.InCommon, "_", 
                         mat.models$predicteur)

nb.Models.byK = nb.similarities * nb.nbMin.InCommon * nb.predicteurs

# ====================== 5.CHARGEMENT DES TABLEAUX DE PREDICTION ====================================



result_error = as.data.frame(matrix(0, nrow=nb.Models.byK, ncol = nb.K))
rownames(result_error) = mat.models$name
colnames(result_error) = seq_K

# =================== 6.CALCUL DES TABLEAUX DE PREDICTION ================================

for(train in 1:nb.Datasets){ # pour chaque couple train/test de la validation croisée
  
  cat(sprintf("\n Calcul pour la sous-base : %0.f / %0.f \n", train, nb.Datasets))
  
  # Chargement des listes déjà vus
  file_list.dejaVu = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/train", train, "/list.dejaVu.Rdata")
  load(file = file_list.dejaVu)
  
  # Statistiques sur les utilisateurs
  file_stat.Users = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/train", train, "/stat.Users.csv")
  stat.Users = read.table(file = file_stat.Users, header=T, sep=';')
  
  # Statistiques sur les livres
  file_stat.Books = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/train", train, "/stat.Books.csv")
  stat.Books = read.table(file = file_stat.Books, header=T, sep=';')
  
  for(modelIND in 1:nb.Models.byK){
    
    cat(sprintf("\n Calcul pour le modèle : %s (%0.f / %0.f) \n",mat.models$name[modelIND], modelIND, nb.Models.byK))
    
    similarity = mat.models$similarity[modelIND]
    nbMin.InCommon = mat.models$nbMin.InCommon[modelIND]
    predicteur = mat.models$predicteur[modelIND]
    
    file_mat.sim = paste0("./CrossValidation/CV", nb.Datasets, "/train", train, "/mat.sim_", similarity, "_", nbMin.InCommon, ".csv")
    mat.sim = as.matrix(read.table(file = file_mat.sim, header=T, sep=';'))
    
    pred = knn_user_predictions(list.Datasets, train, seq_K, mat.sim, predicteur, list.dejaVu, stat.Users, stat.Books)
    colnames(pred) = c("User.ID", "ISBN", "Book.Rating", seq_K)
    write.table(pred, paste0("./Results/", repository, "/results_predictions_train", train, "_", similarity, nbMin.InCommon, "_", predicteur, ".csv"), col.names=NA, sep=";")
    
    for(kIND in 1:nb.K){
      k = seq_K[kIND]
      result_error[modelIND,kIND] = rmse(pred$Book.Rating, pred[,kIND+3])
    }
  }
  write.table(result_error, paste0("./Results/", repository, "/results_knn_userPredictionTest_train", train, ".csv"), col.names=NA, row.names = mat.models$name, sep=";")
}


# =================== 7.CALCUL DES TABLEAUX DE PREDICTION POUR LA BASE Test ================================

cat(sprintf("\n Calcul pour la base Test"))

#Adaptation du code pour prendre en compte la base Test
file_data.Test = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/data.Ratings.Test.Rdata")
load(file = file_data.Test)
train.Ratings = do.call("rbind", list.Datasets)
test.Ratings = data.Ratings.Test
list.Datasets = list(test.Ratings,train.Ratings)

# Chargement des listes déjà vus
file_list.dejaVu = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/Test/list.dejaVu.Rdata")
load(file = file_list.dejaVu)

# Statistiques sur les utilisateurs
file_stat.Users = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/Test/stat.Users.csv")
stat.Users = read.table(file = file_stat.Users, header=T, sep=';')

# Statistiques sur les livres
file_stat.Books = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/Test/stat.Books.csv")
stat.Books = read.table(file = file_stat.Books, header=T, sep=';')

for(modelIND in 1:nb.Models.byK){
  
  cat(sprintf("\n Calcul pour le modèle : %s (%0.f / %0.f) \n",mat.models$name[modelIND], modelIND, nb.Models.byK))
  
  similarity = mat.models$similarity[modelIND]
  nbMin.InCommon = mat.models$nbMin.InCommon[modelIND]
  predicteur = mat.models$predicteur[modelIND]
  
  file_mat.sim = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/Test/mat.sim_", similarity, "_", nbMin.InCommon, ".csv")
  mat.sim = as.matrix(read.table(file = file_mat.sim, header=T, sep=';'))
  
  pred = knn_user_predictions(list.Datasets, 1, seq_K, mat.sim, predicteur, list.dejaVu, stat.Users, stat.Books) #1 désigne la base Test
  colnames(pred) = c("User.ID", "ISBN", "Book.Rating", seq_K)
  write.table(pred, paste0("./Code_R/Results/results_predictions_Test_", similarity, nbMin.InCommon, "_", predicteur, ".csv"), col.names=NA, sep=";")
  
  for(kIND in 1:nb.K){
    k = seq_K[kIND]
    result_error[modelIND,kIND] = rmse(pred$Book.Rating, pred[,kIND+3])
  }
}

write.table(result_error, paste0("./Results/", repository, "/results_knn_userPredictionTest_Test.csv"), col.names=NA, row.names = mat.models$name, sep=";")
