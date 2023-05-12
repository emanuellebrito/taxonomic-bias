library(igraph)
library(bipartite)

first <- read.csv("abreu&vieira_2004.csv",  sep=";", dec = ",")

ppIg<-graph_from_incidence_matrix(first)
ppIg

ppIg<-graph_from_incidence_matrix(first, weighted = TRUE)
ppIg

V(ppIg)

edge_attr(ppIg)$weight

vertex_attr(ppIg)$color<-rep("#FFBF65", length(V(ppIg)))
vertex_attr(ppIg)$color[grep(pattern = "FALSE", vertex_attr(ppIg)$type)]<-"#00A5E3"
vertex_attr(ppIg)$color # checking the color vector

plot(ppIg)

plot(ppIg, vertex.color=vertex_attr(ppIg)$cor)

plot(ppIg, vertex.color=vertex_attr(ppIg)$cor,
     vertex.label=NA)

plot(ppIg, vertex.color=vertex_attr(ppIg)$cor,vertex.label=NA,
     vertex.size=igraph::degree(ppIg))

plot(ppIg, vertex.color=vertex_attr(ppIg)$cor,vertex.label=NA,
     vertex.size=2*igraph::degree(ppIg)) 

plot(ppIg, vertex.color=vertex_attr(ppIg)$cor,vertex.label=NA,
     vertex.size=2*igraph::degree(ppIg), edge.width=2)

plot(ppIg, vertex.color=vertex_attr(ppIg)$cor,vertex.label=NA,
     vertex.size=2*igraph::degree(ppIg),
     edge.width=(edge_attr(ppIg)$weight))


tiff('first.tif', w=3000, h=3000, units="px", res=600, compression = "lzw")
l<-layout_with_dh(ppIg)
plot(ppIg, vertex.color=vertex_attr(ppIg)$cor,vertex.label=NA,
     vertex.size=2*igraph::degree(ppIg),
     edge.width=(edge_attr(ppIg)$weight)/5, 
     edge.color="grey50", 
     edge.curved=0.3,
     layout=l)
dev.off()



