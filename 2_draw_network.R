library(igraph)
library(bipartite)

#set directory
setwd("C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 1 - Scientometric/data/pollination_webs")

#read data
first_net <- read.csv("clements&long_1923.csv",  sep=";", dec = ",", row.names = 1)

ppIg<-graph_from_incidence_matrix(first_net)
ppIg

ppIg<-graph_from_incidence_matrix(first_net, weighted = TRUE)
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


tiff('first_net.tif', w=3000, h=3000, units="px", res=600, compression = "lzw")
l<-layout_with_dh(ppIg)
plot(ppIg, vertex.color=vertex_attr(ppIg)$cor,vertex.label=NA,
     vertex.size=2*igraph::degree(ppIg)/4,
     edge.width=(edge_attr(ppIg)$weight)/5, 
     edge.color="grey50", 
     edge.curved=0.3,
     layout=l)
dev.off()



