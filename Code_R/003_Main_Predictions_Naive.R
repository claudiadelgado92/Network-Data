# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE -    3A DS - Statistical Analysis of Network Data
#    Sujet : Mapping d'un r�seau litt�raire � partir de notes donn�es par diff�rents lecteurs
#       Encadrant : Eric Kolaczyk
#       Etudiants : Damien Babet, Claudia Delgado, Gabriele Ranieri
#
#       Fichier : 003_Main_Predictions_Naive.R
#       Description : r�sultats des m�thodes naives pour la validation crois�e et la base test
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 
setwd("C:/Users/cooky/Documents/ENSAE_3A/SAND")

# =================== 2.CHARGEMENT DES FONCTIONS ANNEXES  ================================

source("./Code_R/Util/stat_Users.R")
source("./Code_R/Util/stat_Books.R")
source("./Code_R/Util/naive_predictions.R")
source("./Code_R/Util/stat_Users.R")

library("hydroGOF")
library("zoo", lib.loc="~/R/win-library/3.3")


# ====================== 3.CHARGEMENT DES BASES D'APPRENTISSAGE ET DE TEST ==========================

cat(sprintf("Les sous-bases propos�s sont de taille : 5\n"))
nb.Datasets = 5

file_list.Datasets = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/list.Datasets.Train.Rdata")
load(file = file_list.Datasets)

# =================== 4.GENERATION DES TABLEAUX DE PREDICTION ================================



predictor_names = c("random_unif", "meanOfBooks", "meanOfUsers", "mean", "meanByUser", "meanByBook")
nb.Predictors = length(predictor_names)

result_RMSE_cv = as.data.frame(matrix(0, nrow=nb.Predictors, ncol = nb.Datasets+1), row.names = predictor_names)
colnames(result_RMSE_cv) = c("Test.Base.1", "Test.Base.2", "Test.Base.3", "Test.Base.4", "Test.Base.5", "moyenne")
result_RMSE_test = as.data.frame(matrix(0, nrow=nb.Predictors, ncol = 1), row.names = predictor_names)

# =================== 5.CALCUL DES TABLEAUX DE PREDICTION ================================

for(train in 1:nb.Datasets){ # pour chaque couple train/test de la validation crois�e
  cat(sprintf("\n Calcul pour la base d'apprentissage : %.0f / %.0f", train, nb.Datasets))
  
  pred = naive_predictions(list.Datasets, train)
  
  for(model in predictor_names){
    result_RMSE_cv[model, train] = rmse(pred$Book.Rating, pred[,model])
  }
}

result_RMSE_cv$moyenne = round(rowMeans(result_RMSE_cv[,1:nb.Datasets],na.rm = TRUE), digits = 3)

write.table(result_RMSE_cv, paste0("./Code_R/Results/results_predictions_naive_base_train.csv"), col.names=NA, sep=";")


# =================== 6.CALCUL DES TABLEAUX DE PREDICTION POUR LA BASE TEST ================================

cat(sprintf("\n Calcul pour la base test"))

#Adaptation du code pour prendre en compte la base test
file_data.test = paste0("./Code_R/CrossValidation/CV", nb.Datasets, "/data.Ratings.Test.Rdata")
load(file = file_data.test)
train.Ratings = do.call("rbind", list.Datasets)
test.Ratings=data.Ratings.Test
list.Datasets=list(test.Ratings,train.Ratings)

pred = naive_predictions(list.Datasets, 1) #1 d�signe la base test
write.table(pred, paste0("./Code_R/Results/predictions_naive_base_test.csv"), col.names=NA, sep=";")

for(model in predictor_names){
  result_RMSE_test[model,1] = round(rmse(pred$Book.Rating, pred[,model]), digits = 5)
}

write.table(result_RMSE_test, paste0("./Code_R/Results/results_predictions_naive_base_test.csv"), col.names=NA, row.names = predictor_names, sep=";")
