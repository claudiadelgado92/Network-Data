# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 3A DS - Statistical Analysis of Network Data
#    Sujet : Mapping d'un r�seau litt�raire � partir de notes donn�es par diff�rents lecteurs
#       Encadrant : Eric Kolaczyk
#       Etudiants : Damien Babet, Claudia Delgado, Gabriele Ranieri
#
#       Fichier : 000_Data_Mining.R
#       Description : Exploration des donn�es
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

setwd("C:/Users/cooky/Documents/ENSAE_3A/SAND")
Ratings <- read.csv("./Data_Brute/BX-Book-Ratings.csv", header = T, sep=";")
Book <- read.csv("./Data_Brute/BX-Books.csv", header = T, sep=";")
Users <- read.csv("./Data_Brute/BX-Users.csv", header = T, sep=";")

dim(Users)

# Nombre d'utilisateurs ayant renseign� leur age
sum(Users$Age!="NULL")

# Nombre d'utilisateurs ayant renseign� leur localisation
length(unique(Users$Location))

# Nombre d'utilisateur ayant not� au moins un livre
length(unique(Ratings$User.ID))

# Nombre de notes �gale � 0
sum(Ratings$Book.Rating==0)
# Nombre de notes compris entre 1 et 5
sum((Ratings$Book.Rating<6)*((Ratings$Book.Rating>0)))
# Nombre de notes compris entre 6 et 10
sum(Ratings$Book.Rating>5)

# Nombre de livre ayant re�u au moins une note
length(unique(Ratings$ISBN))

# Nombre de livre pr�sent initialement dans la base et ayant re�u au moins une note
sum(Book$ISBN %in% Ratings$ISBN)

# Nombre de livre not� �tant pr�sen initialement dans la base et ayant re�u au moins une note
sum(Ratings$ISBN %in% Book$ISBN)

head(Ratings)
head(Book)

table(Ratings$Book.Rating)
