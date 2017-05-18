# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 3A DS - Statistical Analysis of Network Data
#    Sujet : Mapping d'un réseau littéraire à partir de notes données par différents lecteurs
#       Encadrant : Eric Kolaczyk
#       Etudiants : Damien Babet, Claudia Delgado, Gabriele Ranieri
#
#       Fichier : 001_Threshold_Pretreatment.R
#       Description : Fonction qui prétraite les données
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

library(plyr)

setwd("C:/Users/cooky/Documents/ENSAE_3A/SAND")
Ratings_v0 <- read.csv("./Data_Brute/BX-Book-Ratings.csv", header = T, sep=";")
Book_v0 <- read.csv("./Data_Brute/BX-Books.csv", header = T, sep=";")
Users_v0 <- read.csv("./Data_Brute/BX-Users.csv", header = T, sep=";")

## A ce stage on a :
##    278 858 utilisateurs (Users_v0)
##    271 379 livres (Books_V0)
##  1 048 575 notes (Ratings_v0)

# Note des lutilisateures prÈsent dans la base et dont le uvre est ausi prÈsent
Ratings_v1=Ratings_v0[Ratings_v0$ISBN %in% Book_v0$ISBN,]
Ratings_v2=Ratings_v1[Ratings_v1$User.ID %in% Users_v0$User.ID,]

# Livre ayant recu une note 
Book_v1=Book_v0[Book_v0$ISBN %in% Ratings_v2$ISBN,]

#Verification : les notes sont bien celles des livres prÈsnets dans la denriËre base
Ratings_v3=Ratings_v2[Ratings_v2$ISBN %in% Book_v1$ISBN,]

# Utilisateur ayant notÈ
Users_v1=Users_v0[Users_v0$User.ID %in% Ratings_v3$User.ID,]


## A ce stage on a :
##     83 643 utilisateurs (Users_v1)
##    257 832 livres (Books_V1)
##    941 144 notes (Ratings_v3)


#### On enleve les notes Ègales ??? 0 car elles ne reprÈsentent des utilisateurs qui ont
#### vu la page web du livre mais qui n'ont pas donnÈe de note


# Notes diffÈrents de 0 + utilisateurs et livre de la nouvelle base
Ratings_v4= Ratings_v3[Ratings_v3$Book.Rating!=0,]
Users_v2 = Users_v1[Users_v1$User.ID %in% Ratings_v4$User.ID,]
Book_v2 =Book_v1[Book_v1$ISBN %in% Ratings_v4$ISBN,]

## A ce stage on a :
##   61 876 utilisateurs (Users_v2)
##  142 032 livres (Books_V2)
##  351 863 notes (Ratings_v4)


# on A UN PROBLEME VCE CERTAINS LIVRE qui comporte des vorgules dans les descriptions => on le suspprimes
Book_v3=Book_v2[Book_v2$X=="",]
Book_v4=Book_v3[Book_v3$Image.URL.L!="",]
Book_v4=Book_v4[,c(1:8)]

## A ce stage on a :
##   61 876 utilisateurs (Users_v2)
##  142 026 livres (Books_V4)
##  351 863 notes (Ratings_v4)
write.table(Ratings_v4, "./Data_Traitee/Ratings_v4.csv",sep=";",col=NA,fileEncoding = "UTF-8")
write.table(Users_v2, "./Data_Traitee/Users_v4.csv",sep=";",col=NA,fileEncoding = "UTF-8")
write.table(Book_v4, "./Data_Traitee/Books_v4.csv",sep=";",col=NA,fileEncoding = "UTF-8")



## SEUIL de livre et d'utilisateurs => description de la perte

library(igraph)
g <- graph_from_data_frame(Ratings_v4)
type <- (degree(g, mode="in")>0)
g <- set_vertex_attr(g,name="type", value=type)
keepbooks <- V(g)[(degree(g, mode="in")>19)]
keepusers <- V(g)[(degree(g, mode="out")>4)]
g2 <- induced_subgraph(g,c(keepusers,keepbooks))
summary(V(g2)$type)
summary(g2)
# Le reseau reduit aux livres notes plus de 20 fois et aux utilisateurs
# ayant donne plus de 5 notes contient 11663 utilisateurs et 1873 livres.

# On reprend la demarche en contant les notes implicites :
g3 <- graph_from_edgelist(as.matrix(Ratings_v3[,-3]))
type3 <- (degree(g3, mode="in")>0)
keepbooks3 <- V(g3)[(degree(g3, mode="in")>19)]
keepusers3 <- V(g3)[(degree(g3, mode="out")>4)]
g4 <- induced_subgraph(g3,c(keepusers3,keepbooks3))
type4 <- (degree(g4, mode="in")>0)
summary(type4)
summary(g4)


books_6508=as.data.frame(keepbooks3$name)
colnames(books_6508)='ISBN'
write.table(books_6508, "./Data_Traitee/Books_6508.csv",sep=",",col=NA,fileEncoding = "UTF-8")

# On a maintenant 
# 6 508 livres, 
# 18 529 utilisateurs et 
# 295 989 ratings
# Des chiffres très proches de ceux de l'article.

# On revient sur les notes sans les 0 : g2

df_g2 <- as.data.frame(get.edgelist(g2))
colnames(df_g2)=c('User.ID','ISBN')
df_g2=merge(df_g2,Ratings_v4,by=c('User.ID','ISBN'))
write.table(df_g2, "./Data_Traitee/Data.csv",sep=",",col=NA,fileEncoding = "UTF-8")
