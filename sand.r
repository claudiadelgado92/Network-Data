setwd("/Users/administrateur/Desktop/projet SAND/Data_Brute/")
Ratings_v0 <- read.csv("BX-Book-Ratings.csv", header = T, sep=";")
Book_v0 <- read.csv("BX-Books.csv", header = T, sep=";")
Users_v0 <- read.csv("BX-Users.csv", header = T, sep=";")

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


#### On enleve les notes Ègales ‡ 0 car elles ne reprÈsentent des utilisateurs qui ont
#### vu la page web du livre mais qui n'ont pas donnÈe de note


# Notes diffÈrents de 0 + utilisateurs et livre de la nouvelle base
Ratings_v4= Ratings_v3[Ratings_v3$Book.Rating!=0,]
Users_v2 = Users_v1[Users_v1$User.ID %in% Ratings_v4$User.ID,]
Book_v2 =Book_v1[Book_v1$ISBN %in% Ratings_v4$ISBN,]

## A ce stage on a :
##   61 876 utilisateurs (Users_v2)
##  142 032 livres (Books_V2)
##  351 863 notes (Ratings_v4)


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
# On a maintenant 
# 6 508 livres, 
# 18 529 utilisateurs et 
# 295 989 ratings
# Des chiffres très proches de ceux de l'article.

# projection
booksgraph <- bipartite_projection(g2,multiplicity = T,which = T)
usersgraph <- bipartite_projection(g2,multiplicity = T,which = F)

# clusters
c1 <- cluster_fast_greedy(booksgraph) # moins d'une minute
c2 <- cluster_fast_greedy(usersgraph) # environ 5 minutes
c1
c2
sizes(c1)
sizes(c2)
plot(c1, booksgraph) # trop long ! (plus de 10 minutes)

# On essaye d'exporter un graph lisible par Gephi
library(rgexf)
# construct the nodes and edges data for gexf conversion
nodes <- data.frame(ID = c(1:vcount(booksgraph)), NAME = V(booksgraph)$name)
edges <- as.data.frame(get.edges(booksgraph, c(1:ecount(booksgraph))))

# do the conversion
output <- write.gexf(nodes, edges)# ne termine pas !
print(output, "graph.gexf")
