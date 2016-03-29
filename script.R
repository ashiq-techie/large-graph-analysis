library("igraph")
edges = read.table("Wiki-Vote.txt")
graph = graph.data.frame(edges) #creating a graph
no.clusters(graph) #no. of components

#Calcuate Maximum Vertex In degree
max(degree(graph, mode="in"))
# In-degree distribution
plot(degree.distribution(graph, mode="in"), log="xy")

#Calcuate Maximum Vertex out degree
max(degree(graph, mode="out"))
# Out-degree distribution
plot(degree.distribution(graph, mode="out"), log="xy")

#To find Communities

#Finding the edge betweenness
#ebc <- edge.betweenness.community(graph, directed=T)
# 
# #Modularity for each merge
# mods <- sapply(0:ecount(graph), function(i){
#   graph2 <- delete.edges(graph, ebc$removed.edges[seq(length=i)])
#   clust <- clusters(graph2)$membership
#   modularity(graph,clust)
# })
# 
# #plot all modularities
# plot(mods, pch=20)
# 
# # giving different colors for identification of clusters
# graph2<-delete.edges(graph, ebc$removed.edges[seq(length=which.max(mods)-1)])
# V(graph)$color=clusters(graph2)$membership
# 
# #graph Layout
# graph$layout <- layout.fruchterman.reingold
# 
# plot(graph, vertex.label=NA)

#density and transitivity
graph.density(graph) 
transitivity(graph)

#greedy method
fcomm <- fastgreedy.community(as.undirected(graph))
#to find membership attribute
memb <- cutat(fcomm, steps=which.max(fcomm$modularity))
#layout for the graph
lay <- layout.drl(graph = graph)
empt <- graph.empty(n=vcount(graph))
colbar <- rainbow(5)
col <- colbar[memb+1]
col[is.na(col)] <- "gray"
#Plotting the community
plot(empt, layout=lay, vertex.size=1,vertex.label=NA, asp=FALSE,vertex.color=col,vertex.frame.color=col)

#Analysis - Who is highly influential in the network
sub_graph <- V(graph)[(degree(graph)/10)<20]
new_graph <- delete.vertices(graph = graph, sub_graph)
V(new_graph)$size<-degree(new_graph)/10
par(mai=c(0,0,1,0))
plot(new_graph, layout=layout.fruchterman.reingold,vertex.label.font=2, vertex.label.cex=V(new_graph)$size/37,vertex.color=col,vertex.frame.color=col)