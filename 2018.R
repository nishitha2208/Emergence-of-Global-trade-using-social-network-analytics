install.packages("tidyverse")
library(tidyverse)
Data18 <- read_csv("Edges.csv")
head(Data18)
install.packages("ggplot2")
library(ggplot2)
install.packages("igraph")
library(igraph)
install.packages("tnet")
library(tnet)
#library(sna)
#library(ergm)
Nodes18 <- read_csv("2018FINAL.csv")
head(Nodes18)
Data18m<-as.matrix(Data18[,1:2])
a = degree_w(Data18m)
plot(degree.distribution(a))

graph18<-graph_from_edgelist(Data18m,directed=T)
V(graph18)$name <- Nodes18$Label
V(graph18)$population <- Nodes18$Population
E(graph18)$weight <- Data18$Weight
plot(graph18, layout=layout.fruchterman.reingold, vertex.size=V(graph18)$population/10000000,edge.weight = E(graph18)$weight ,vertex.label = V(graph18)$name)
graph.density(graph18)
transitivity(graph18)
graph18_bc <- label.propagation.community(graph18) 
length(graph18_bc) #extract the number of communities detected
membership(graph18_bc) #extract community membership for nodes
sizes(graph18_bc) #extract size of each community
modularity(graph18_bc)
##DEGREE
degree_dist <- degree_distribution(graph18) #returns the percentage of nodes that has a degree of 0,1,2 till the highest degree in the network 
highest_degree <- length(degree_dist)-1
names(degree_dist) <- 0:highest_degree
degrees<- igraph ::degree(graph18) #degree centrality of all nodes
degrees_df <- as.data.frame(a)
ggplot(degrees_df,aes(degrees))+geom_histogram(bins=max(degrees),binwidth = 0.5) #plot degree distribution
degree(graph18, v="IND") 
igraph ::degree(graph18)[igraph ::degree(graph18)==max(igraph ::degree(graph18))] #find the node(s) with the highest degree centrality
##CLOSENESS
igraph ::closeness(graph18)
igraph ::closeness(graph18)[igraph ::closeness(graph18)==max(igraph ::closeness(graph18))]
##BETWEENNESS
igraph ::betweenness(graph18)
igraph ::betweenness(graph18)[igraph ::betweenness(graph18)==max(igraph ::betweenness(graph18))]
##EIGENVECTOR 
evcent(graph18)
evcent(graph18)$vector
evcent(graph18)$vector[evcent(graph18)$vector==max(evcent(graph18)$vector)]
centralization.degree(graph18)
centralization.degree(graph18)$centralization

degree_distribution(graph18,mode="in")
degree_distribution(graph18,mode="out")
degree_distribution(graph18,mode="all")

degree(graph18, mode="in") #in-degree centrality of all nodes
degree(graph18, mode="out")#out-degree centrality of all nodes
degree(graph18)
degree(graph18,mode="all")
degrees <- degree(graph18)
degrees_camp <- as.data.frame(degrees)
ggplot(degrees_camp,aes(degrees))+geom_histogram(bins=max(degrees),binwidth = 0.5) 

degree(graph18,mode="in")[degree(graph18,mode="in")==max(degree(graph18,mode="in"))]
degree(graph18,mode="out")[degree(graph18,mode="out")==max(degree(graph18,mode="out"))]
V(graph18)
E(graph18)
sw1 <- sample_smallworld(dim=1, size=161, nei=105, p=0.09) #p is the edge rewiring probability
#sw1
plot(sw1,layout=layout.circle)
ecount(sw1)
graph.density(sw1)
transitivity(sw1)
mean_distance(sw1)

##QAP test
library(sna)
library(ergm)
library(tidyverse)
library(igraph)
Data18adj <- read_csv(file="2018.csv") #read first row as header
dim(Data18adj)
Data18m <- as.matrix(Data18adj)

Data00adj <- read_csv(file="2000k.csv") #read first row as header
dim(Data00adj)
Data00m <- as.matrix(Data00adj)

flo.qap <- qaptest(list(Data18m,Data00m), gcor, g1=1, g2=2)
summary(flo.qap)
##ERGM 
library(intergraph)
Data18mn <- network(Data18m, directed=T,matrix.type = "edgelist")
Data18mn%v%'vertex.names'<- Nodes18$countryname
Data18mn%v%'population' <- Nodes18$Population
plot(Data18mn,label=Data18mn%v%'vertex.names',vertex.size=Data18mn%v%'population')

Data18.mod.1 <- ergm(Data18mn ~ edges + mutual + nodeocov('population') + absdiff('population'))	
summary(Data18.mod.1)		

##Data18.ergm <- ergm(df1m ~ edges + edgecov(dfm))

Data18.mod.2 <- ergm(Data18mn ~ edges + mutual + absdiff('population'))	
summary(Data18.mod.2)	

Data18.mod.3 <- ergm(Data18mn ~ edges + mutual + nodeocov('population'))	
summary(Data18.mod.3)	

Data18.mod.4 <- ergm(Data18mn ~ edges + gwesp(0.5, fixed = T) +  mutual + nodeicov('population') + absdiff('population'),control = control.ergm(seed=1))   
summary(Data18.mod.4)

##Export vs import 

