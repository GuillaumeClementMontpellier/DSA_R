install.packages("igraph")
install.packages("dplyr")
library(dplyr)
library(igraph)

mails <- read.csv("C:/Users/guigu/Desktop/DSA/mails_perso.csv", sep=";")

network <- graph_from_data_frame(d=mails, directed=F) 

deg <- degree(network, mode="all")


list_deg <- lapply(split(deg,names(deg)),unname)

deg_fil <- list_deg[list_deg > 14]

length(deg_fil)

max_deg <- max(unlist(deg_fil))
min_deg <- min(unlist(deg_fil))

list_names <- names(deg_fil)

mails_fil <- mails[(mails$sender %in% list_names) & (mails$recipient %in% list_names), c(1, 2)]


# Count
mails_fils <- mails_fil %>% group_by(sender, recipient) %>% summarize(count=n())

mails_final <- as.data.frame(mails_fils)

max_edge <- max(mails_final$count)
min_edge <- min(mails_final$count)

mails_final$edgeWeight <- lapply(X = mails_final$count, FUN = function(arg_1) {
  min_edge_final <- 1
  max_edge_final <- 10
  (arg_1 - min_edge) * (max_edge_final - min_edge_final) / max_edge + min_edge_final
})

max(unlist(mails_final$edgeWeight))
min(unlist(mails_final$edgeWeight))


mails_final$color <- lapply(X = mails_final$count, FUN = function(arg_1) {
  min(max(arg_1, 0), 255)
})

network_fil <- graph_from_data_frame(d=mails_final, directed=F) 

deg_fil2 <- degree(network, mode="all")

spin_comm <- cluster_spinglass(network_fil, 
                          weights = mails_final$count, 
)

plot(
  spin_comm,
  network_fil,
  layout=layout.fruchterman.reingold,
  vertex.size = 8,
  edge.width=E(network_fil)$edgeWeight,
  vertex.label.color="firebrick4"
)




