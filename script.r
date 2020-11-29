# install.packages("igraph")
# install.packages("purrr")
library(dplyr)
library(igraph)
# library(readr)

# mails <- read_delim("C:/Users/guigu/Desktop/DSA/mails.csv", ";", escape_double = FALSE, trim_ws = TRUE)

mails <- read.csv("C:/Users/guigu/Desktop/DSA/mails_perso.csv", sep=";")

# mails_count <- mails %>% group_by(sender, recipient) %>% summarize(count=n())

# summary(mails)

network <- graph_from_data_frame(d=mails, directed=F) 

deg <- degree(network, mode="all")

# edc_comm <- cluster_edge_betweenness(network, 
#                                # weights = mails_count$count, 
# )
# 
# plot(
#   edc_comm,
#      network,
#      layout=layout.fruchterman.reingold,
#      # layout=layout.circle,
#   vertex.size = 8,
#   # edge.width=E(network)$count,
#   # edge.color="#999999",
#   # vertex.label.color="firebrick4"
# )


# names(deg[1])
# deg[1]

list_deg <- lapply(split(deg,names(deg)),unname)

# deg_fil <- list_deg
deg_fil <- list_deg[list_deg > 14]

length(deg_fil)

max_deg <- max(unlist(deg_fil))
min_deg <- min(unlist(deg_fil))

# mails$sender
list_names <- names(deg_fil)
list_names

# list <- list[list != "guillaume.clement"]

mails_fil <- mails[(mails$sender %in% list_names) & (mails$recipient %in% list_names), c(1, 2)]


# COunt
mails_fils <- mails_fil %>% group_by(sender, recipient) %>% summarize(count=n())

mails_final <- as.data.frame(mails_fils)

# mails_fil

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

# mails_fil

network_fil <- graph_from_data_frame(d=mails_final, directed=F) 

deg_fil2 <- degree(network, mode="all")

# deg_fil2

# ver_size <- lapply(X = deg_fil2, FUN = function(arg_1) {min(max(arg_1, 1), 20)})
# unlist(ver_size)

# par(bg="black")
# par(bg="white")
# plot(network_fil,
#      layout=layout.fruchterman.reingold,
#      vertex.size = 8,
#      edge.width=E(network_fil)$edgeWeight,                                 # Edge width, defaults to 1
#      edge.color="#999999"
# )


# clu <- components(network_fil)
# 
# groups(clu)

spin_comm <- cluster_spinglass(network_fil, 
                          weights = mails_final$count, 
)

edc_comm <- cluster_edge_betweenness(network_fil, 
                          # weights = mails_final$count, 
)

# comm

# comms <- communities(comm)
# comms

# comms
# 
# comms[1]
# comms[2]
# comms[3]
# comms[4]

plot(
  # spin_comm,
  network_fil,
  layout=layout.fruchterman.reingold,
  # layout=layout.sphere,
  vertex.size = 8,
  edge.width=E(network_fil)$edgeWeight,
  # edge.color="#999999",
  vertex.label.color="firebrick4"
)

plot(
  edc_comm,
  network_fil,
  layout=layout.fruchterman.reingold,
  # layout=layout.sphere,
  vertex.size = 8,
  edge.width=E(network_fil)$edgeWeight,
  # edge.color="#999999",
  vertex.label.color="firebrick4"
)


