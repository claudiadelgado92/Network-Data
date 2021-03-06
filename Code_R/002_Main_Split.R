# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE -    3A DS - Statistical Analysis of Network Data
#    Sujet : Mapping d'un r�seau litt�raire � partir de notes donn�es par diff�rents lecteurs
#       Encadrant : Eric Kolaczyk
#       Etudiants : Damien Babet, Claudia Delgado, Gabriele Ranieri
#
#       Fichier : 002_Main_Split.R
#       Description : fonction principal pour g�n�rer les sous-bases de travail des tests
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ======================================== 1.PREAMBULE =============================================

## Clean up
rm(list=ls()) 
cat("\014") 
setwd("C:/Users/cooky/Documents/ENSAE_3A/SAND")

# ============ 1.1. Chargement des fonctions =============================================
source("./Code_R/Util/split_data.R")

# ============ 1.2. Chargement des bases =============================================


data.Ratings = read.table(file = "./Data_Traitee/Data.csv", header=T, sep=',')
data.Ratings=data.Ratings[c("User.ID"  ,  "ISBN" ,       "Book.Rating")]


# ======================================== 2.DECOUPAGE DE LA BASE DES NOTES =============================================


nb.Tests = as.integer(readline(prompt = "Choisissez le nombre de sous-bases : "))


set.seed(42)

# G�n�ration d'un vecteur al�atoire
alea = runif(nrow(data.Ratings))

# Cr�ation de la base Test
data.Ratings.Test = subset(data.Ratings, (alea<quantile(alea,0.05)))

#Cr�ation de list.dataset servant � la cross validation

data.Ratings.Train=subset(data.Ratings, (alea>=quantile(alea,0.05)))

list.Datasets = split_data(data.Ratings.Train, nb.Tests)

save(list.Datasets, file = paste0("./Code_R/CrossValidation/CV", nb.Tests, "/list.Datasets.Train.Rdata"))
save(data.Ratings.Test, file = paste0("./Code_R/CrossValidation/CV", nb.Tests, "/data.Ratings.Test.Rdata"))
