# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE -    3A DS - Statistical Analysis of Network Data
#    Sujet : Mapping d'un réseau littéraire à partir de notes données par différents lecteurs
#       Encadrant : Eric Kolaczyk
#       Etudiants : Damien Babet, Claudia Delgado, Gabriele Ranieri
#
#       Fichier : 002_bis_Main_Preparation.R
#       Description : fonction principal pour les générer les bases de travail et de test
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ======================================== 1.PREAMBULE ============================================================

## Clean up
rm(list=ls()) 
cat("\014") 
setwd("C:/Users/cooky/Documents/ENSAE_3A/SAND")

library(igraph)
source("./Code_R/Util/stat_Users.R")
source("./Code_R/Util/stat_Books.R")
source("./Code_R/Util/deja_Vu.R")

source("./Code_R/Util/nb_BooksInCommon_v2.R")
source("./Code_R/Util/proxi_Users.R")
source("./Code_R/Util/proxi_Users_AllvsAll_V4.R")
source("./Code_R/Util/filtrer_similarite.R")


# ================================== 2.CHOIX DES PARAMETRES ===========================================================

# Pour la partie recommandation et validation-croisée
list.Similarities = c("RFP")
list.nbMin.InCommon = c(1)

# Pour la partie validation-croisée (il faut que la base de validation-croisée soit créée)
cat(sprintf("Les sous-bases proposés sont de taille : 5\n"))
nb.Tests = 5

# =============================== 3.PREPARATION DES FICHIERS POUR LA VALIDATION CROISEE ===========================

cat("Préparation des fichiers pour la validation croisée \n")


file_list.Datasets = paste0("./Code_R/CrossValidation/CV", nb.Tests, "/list.Datasets.Train.Rdata")
load(file = file_list.Datasets)


for(train in 1:nb.Tests){
  
  cat(sprintf("Début de la préparation : %0.f / %0.f \n", train, nb.Tests))
  
  dataset_to_keep = (1:nb.Tests)[(1:nb.Tests) != train]
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  
  cat("\t Création des bases de données des utilisateurs et leurs statistiques \n")
  stat.Users = stat_Users(train.Ratings)
  write.table(stat.Users, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/train", train, "/stat.Users.csv"), row.names = FALSE, sep=";")

  cat(" \t Création des bases de données des livres et leurs statistiques \n")
  stat.Books = stat_Books(train.Ratings)
  write.table(stat.Books, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/train", train, "/stat.Books.csv"), row.names = FALSE, sep=";")

  cat(" \t Création des listes des livres notés par utilisateur \n")
  list.dejaVu = deja_Vu(train.Ratings)
  save(list.dejaVu, file = paste0("./Code_R/CrossValidation/CV", nb.Tests, "/train", train, "/list.dejaVu.Rdata"))

    cat("\t Création de la matrice du nombre de livres notés en commun \n")
    mat.InCommon = nb_BooksInCommon(train.Ratings)
    write.table(mat.InCommon, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/train", train, "/mat.InCommon.csv"), row.names = FALSE, sep=";")
  
    cat(" \n \t Création des matrices des similarités \n")
    for(similarity in list.Similarities){
      cat(sprintf("\n \t \t Création de la matrice %s sans filtre\n", similarity))
      mat.sim0 = proxi_Users_AllvsAll(train.Ratings,mat.InCommon, similarity)
      write.table(mat.sim0, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/train", train, "/mat.sim_", similarity, "_0.csv"), row.names = FALSE, sep=";")

    #   for(nbMin.InCommon in list.nbMin.InCommon){
    #     cat(sprintf("\n \t \t Création de la matrice %s avec un seuil à %0.f \n", similarity, nbMin.InCommon))
    #     mat.sim_filtre = filtrer_similarite(mat.sim0, mat.InCommon, nbMin.InCommon)
    #     write.table(mat.sim_filtre, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/train", train, "/mat.sim_", similarity, "_", nbMin.InCommon, ".csv"), row.names = FALSE, sep=";")
    #    } 
     }
  
   
}

# =============================== 4.PREPARATION DES FICHIERS POUR LE BASE Test ===========================

cat("Préparation des fichiers pour la base Test\n")

file_list.Datasets = paste0("./Code_R/CrossValidation/CV", nb.Tests, "/list.Datasets.Train.Rdata")
load(file = file_list.Datasets)

train.Ratings = do.call("rbind", list.Datasets[1:nb.Tests])

cat("\t Création des bases de données des utilisateurs et leurs statistiques \n")
stat.Users = stat_Users(train.Ratings)
write.table(stat.Users, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/Test/stat.Users.csv"), row.names = FALSE, sep=";")

cat(" \t Création des bases de données des livres et leurs statistiques \n")
stat.Books = stat_Books(train.Ratings)
write.table(stat.Books, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/Test/stat.Books.csv"), row.names = FALSE, sep=";")

cat(" \t Création des listes des livres notés par utilisateur \n")
list.dejaVu = deja_Vu(train.Ratings)
save(list.dejaVu, file = paste0("./Code_R/CrossValidation/CV", nb.Tests, "/Test/list.dejaVu.Rdata"))

cat("\t Création de la matrice du nombre de livres notés en commun \n")
mat.InCommon = nb_BooksInCommon(train.Ratings)
write.table(mat.InCommon, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/Test/mat.InCommon.csv"), row.names = FALSE, sep=";")

 cat("\n \t Création des matrices des similarités \n")
 for(similarity in list.Similarities){
   cat(sprintf("\n \t \t Création de la matrice %s sans filtre\n", similarity))
   mat.sim0 = proxi_Users_AllvsAll(train.Ratings,mat.InCommon, similarity)
   write.table(mat.sim0, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/Test/mat.sim_", similarity, "_0.csv"), row.names = FALSE, sep=";")
   # for(nbMin.InCommon in list.nbMin.InCommon){
   #   cat("\n \t nbMin.InCommon =", nbMin.InCommon," \n")
   #   cat(sprintf("\n \t \t Création de la matrice %s avec un seuil à %0.f \n", similarity, nbMin.InCommon))
   #   mat.sim_filtre = filtrer_similarite(mat.sim0, mat.InCommon, nbMin.InCommon)
   #   write.table(mat.sim_filtre, paste0("./Code_R/CrossValidation/CV", nb.Tests, "/Test/mat.sim_", similarity, "_", nbMin.InCommon, ".csv"), row.names = FALSE, sep=";")
   # } 
 }


