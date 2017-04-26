setwd("C:/Users/cooky/Documents/ENSAE_3A/SAND/Data_Brute")
Ratings_v0 <- read.csv("BX-Book-Ratings.csv", header = T, sep=";")
Book_v0 <- read.csv("BX-Books.csv", header = T, sep=";")
Book_v0bis <- read.csv("BX-Books2.csv", header = T, sep=";")
Users_v0 <- read.csv("BX-Users.csv", header = T, sep=";")

## A ce stage on a :
##    278 858 utilisateurs (Users_v0)
##    271 379 livres (Books_V0)
##  1 048 575 notes (Ratings_v0)

# Note des lutilisateures présent dans la base et dont le uvre est ausi présent
Ratings_v1=Ratings_v0[Ratings_v0$ISBN %in% Book_v0$ISBN,]
Ratings_v2=Ratings_v1[Ratings_v1$User.ID %in% Users_v0$User.ID,]

# Livre ayant recu une note 
Book_v1=Book_v0[Book_v0$ISBN %in% Ratings_v2$ISBN,]

#Verification : les notes sont bien celles des livres présnets dans la denrière base
Ratings_v3=Ratings_v2[Ratings_v2$ISBN %in% Book_v1$ISBN,]

# Utilisateur ayant noté
Users_v1=Users_v0[Users_v0$User.ID %in% Ratings_v3$User.ID,]


## A ce stage on a :
##     83 643 utilisateurs (Users_v1)
##    257 832 livres (Books_V1)
##    941 144 notes (Ratings_v3)


#### On enleve les notes égales à 0 car elles ne représentent des utilisateurs qui ont
#### vu la page web du livre mais qui n'ont pas donnée de note


# Notes différents de 0 + utilisateurs et livre de la nouvelle base
Ratings_v4= Ratings_v3[Ratings_v3$Book.Rating!=0,]
Users_v2 = Users_v1[Users_v1$User.ID %in% Ratings_v4$User.ID,]
Book_v2 =Book_v1[Book_v1$ISBN %in% Ratings_v4$ISBN,]

## A ce stage on a :
##   61 876 utilisateurs (Users_v2)
##  142 032 livres (Books_V2)
##  351 863 notes (Ratings_v4)


setwd("C:/Users/cooky/Documents/ENSAE_3A/SAND/Data_Traitee")


# on A UN PROBLEME VCE CERTAINS LIVRE qui comporte des vorgules dans les descriptions => on le suspprimes
Book_v3=Book_v2[Book_v2$X=="",]
Book_v4=Book_v3[Book_v3$Image.URL.L!="",]
Book_v4=Book_v4[,c(1:8)]
## A ce stage on a :
##   61 876 utilisateurs (Users_v2)
##  142 026 livres (Books_V4)
##  351 863 notes (Ratings_v4)
write.table(Ratings_v4, "Ratings.csv", col=NA, sep=";")
write.table(Ratings_v4, "Ratings2.csv", sep=";")
write.table(Book_v4, "Books.csv", col=NA, sep=";")

