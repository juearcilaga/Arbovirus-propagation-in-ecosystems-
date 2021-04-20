
#Necessary libraries

library(readxl)
library(dplyr)
library(tidyr)
library('igraph')
library(ggplot2)
library(scales)


#Reading data
ecosystem<-read_excel("Supplementary tables.xlsx", sheet = "Table S1")
by_ecosystem <- lapply(ecosystem, function(x) gsub(" ", "_", x))
by_ecosystem<-data.frame(by_ecosystem)
by_ecosystem<-drop_na(by_ecosystem)

#Filtering data by ecosystem
Mangrove<-filter(by_ecosystem, Ecosystem=="Mangrove")
Peridomiciliar<-filter(by_ecosystem, Ecosystem=="Peridomiciliar_sites")
Mborders<-filter(by_ecosystem, Ecosystem=="Mangrove_borders")
Rice<-filter(by_ecosystem, Ecosystem=="Rice_fields")


#Adjacency matrix by ecosystem

Mangrove_mono<-data.frame(M=Mangrove[,1], H=Mangrove[,2], W=Mangrove[,6] )
colnames(Mangrove_mono)<-c("Mosquitoe","Host","weight")

Peridomiciliar_mono<-
                data.frame(M=Peridomiciliar[,1], H=Peridomiciliar[,2], W=Peridomiciliar[,6] )
colnames(Peridomiciliar_mono)<-c("Mosquitoe","Host","weight")

Mborders_mono<-data.frame(M=Mborders[,1], H=Mborders[,2], W=Mborders[,6] )
colnames(Mborders_mono)<-c("Mosquitoe","Host","weight")

Rice_mono<-data.frame(M=Rice[,1], H=Rice[,2], W=Rice[,6] )
colnames(Rice_mono)<-c("Mosquitoe","Host","weight")




#Mosquito-vertebrate integrative network

M_H<-by_ecosystem
colnames(M_H)<-c("M","H","Ecosystem", "MH", "M_by_env","weight_env")

#colnames(by_ecosystem)
# Original colnames:
#"Mosquito..M."                
#"Host..H."                   
#"Ecosystem"                   
#"M.fed.on.the.blood.of.H"    
#"M.Total.by.Ecosystem"        
#"X..M.fed.on.the.blood.of.H."

M_H_sumMH<-M_H %>% group_by(M, H) %>% summarise(sum=sum(as.numeric(MH)))
colnames(M_H_sumMH)<-c("M","H", "MH")

M_H_sumT<-M_H %>% group_by(M, H) %>% summarise(sum=sum(as.numeric(M_by_env)))
colnames(M_H_sumT)<-c("M","H", "MTotal")


#M_H_sumMH$H==M_H_sumT$H#same order

M_H_integrative<-data.frame(M_H_sumMH, M_H_sumT$MTotal)
colnames(M_H_integrative)<-c("M","H","MH","MTotal")

M_H_integrative<- 
          transform(M_H_integrative, P1= (M_H_integrative$MH /M_H_integrative$MTotal)*100)

M_H_integrative_net<-data.frame(M_H_integrative[1:2],M_H_integrative[5])
colnames(M_H_integrative_net)<-c("M","H","W")





#Virus-Mosquito Network

V_net<-read_excel("Supplementary tables.xlsx", sheet = "Table S3")
V_net <- lapply(V_net, function(x) gsub(" ", "_", x))#Remove space
V_net <-  lapply(V_net, function(x)gsub("_spp.","_spp" , x))
V_net <-  lapply(V_net, function(x)gsub("_aff.","" , x))
V_net <- lapply(V_net, function(x)gsub("\\(","" , x))#replace parenthesis
V_net <-  lapply(V_net, function(x)gsub(")","" , x))#Remove parenthesis
V_net <-  lapply(V_net, function(x)gsub("_Culex_","_" , x))
V_net <-  lapply(V_net, function(x)gsub("_Grabhamyia_","_" , x))
V_net <-  lapply(V_net, function(x)gsub("_Stegomyia_","_" , x))
V_net <-  lapply(V_net, function(x)gsub("_Melanoconion_","_" , x))
V_net <- lapply(V_net, function(x)gsub("_Splendens_section","" , x))

V_net <-data.frame(V_net)
V_net <-drop_na(V_net)


weights<-as.numeric(V_net[,7])
V_M_net<-data.frame(V=V_net[["Virus.detected"]],M=V_net[["Mosquito.species"]], W=weights)
colnames(V_M_net)<-c("V","M","W")




#Metadata for integrative network
extra_met<-read_excel("Supplementary tables.xlsx", sheet = "Table S4")
extra_met<-lapply(extra_met, function(x) gsub(" ", "_", x))#Remove space
extra_met<-data.frame(extra_met)
extra_met<-extra_met[order(extra_met$Element),]



component1<-Peridomiciliar_mono
colnames(component1)<-c("source","target","weight")

met_M<-data.frame(Element=unique(component1[,1]))
met_M<-filter(extra_met,  extra_met$Element %in% met_M$Element)

met_H<-data.frame(element=unique(component1[,2]))
met_H<-filter(extra_met,  extra_met$Element %in% met_H$element)

met_v<-unique(filter(V_M_net, V_M_net$M %in% met_M$Element))
met_v<-data.frame(element=unique(met_v$V))
met_v<-filter(extra_met,  extra_met$Element %in% met_v$element)

met<-rbind(met_M,met_H,met_v)

component2<-filter(V_M_net, V_M_net$M %in% met_M$Element)
colnames(component2)<-c("source","target","weight")

Peridomiciliar_V<-rbind(component1,component2)

Peridomiciliar_V_net<-graph_from_data_frame(d=Peridomiciliar_V,vertices=met, directed=T)
write_graph(Peridomiciliar_V_net, "Peridomiciliar_V_net.graphml", format =  "graphml")



component1<-Mangrove_mono
colnames(component1)<-c("source","target","weight")

met_M<-data.frame(Element=unique(component1[,1]))
met_M<-filter(extra_met,  extra_met$Element %in% met_M$Element)

met_H<-data.frame(element=unique(component1[,2]))
met_H<-filter(extra_met,  extra_met$Element %in% met_H$element)

met_v<-unique(filter(V_M_net, V_M_net$M %in% met_M$Element))
met_v<-data.frame(element=unique(met_v$V))
met_v<-filter(extra_met,  extra_met$Element %in% met_v$element)

met<-rbind(met_M,met_H,met_v)

component2<-filter(V_M_net, V_M_net$M %in% met_M$Element)
colnames(component2)<-c("source","target","weight")
Mangrove_V<-rbind(component1,component2)

Mangrove_V_net<-graph_from_data_frame(d=Mangrove_V,vertices=met, directed=T)
write_graph(Mangrove_V_net, "Mangrove_V_net.graphml", format =  "graphml")



component1<-Mborders_mono
colnames(component1)<-c("source","target","weight")

met_M<-data.frame(Element=unique(component1[,1]))
met_M<-filter(extra_met,  extra_met$Element %in% met_M$Element)

met_H<-data.frame(element=unique(component1[,2]))
met_H<-filter(extra_met,  extra_met$Element %in% met_H$element)

met_v<-unique(filter(V_M_net, V_M_net$M %in% met_M$Element))
met_v<-data.frame(element=unique(met_v$V))
met_v<-filter(extra_met,  extra_met$Element %in% met_v$element)

met<-rbind(met_M,met_H,met_v)

component2<-filter(V_M_net, V_M_net$M %in% met_M$Element)
colnames(component2)<-c("source","target","weight")
Mborders_V<-rbind(component1,component2)

Mborders_V_net<-graph_from_data_frame(d=Mborders_V,vertices=met, directed=T)
write_graph(Mborders_V_net, "Mborders_V_net.graphml", format =  "graphml")



component1<-Rice_mono
colnames(component1)<-c("source","target","weight")

met_M<-data.frame(Element=unique(component1[,1]))
met_M<-filter(extra_met,  extra_met$Element %in% met_M$Element)

met_H<-data.frame(element=unique(component1[,2]))
met_H<-filter(extra_met,  extra_met$Element %in% met_H$element)

met_v<-unique(filter(V_M_net, V_M_net$M %in% met_M$Element))
met_v<-data.frame(element=unique(met_v$V))
met_v<-filter(extra_met,  extra_met$Element %in% met_v$element)

met<-rbind(met_M,met_H,met_v)

component2<-filter(V_M_net, V_M_net$M %in% met_M$Element)
colnames(component2)<-c("source","target","weight")
Rice_mono_V<-rbind(component1,component2)

Rice_mono_V_net<-graph_from_data_frame(d=Rice_mono_V,vertices=met, directed=T)
write_graph(Rice_mono_V_net, "Rice_mono_V_net.graphml", format =  "graphml")





#Integrative_net
component1<-M_H_integrative_net
colnames(component1)<-c("source","target","weight")
component2<-V_M_net
colnames(component2)<-c("source","target","weight")
Integrative<-rbind(component1,component2)

Integrative_net <-graph_from_data_frame(d=Integrative,vertices=extra_met, directed=T)

#Metrics
net<-Integrative_net

closeness.cent <- closeness(net, mode="all")
closeness.cent <- data.frame(closeness.cent)

Page_Rank<-page_rank(net, algo ="prpack", weights = E(net)$weight)

V(net)$closeness.cent <- closeness(net, mode="all")
V(net)$degree.cent <- centr_degree(net, mode = "all")$res
V(net)$Page_Rank<-page_rank(net, algo ="prpack", weights = E(net)$weight)$vector

write_graph(Integrative_net, "net_integratitve.graphml", format =  "graphml")


#Did not use weights
shortestpaths<-shortest.paths(net, v=V(net), to=V(net), mode = "all")
shortestpaths<-data.frame(shortestpaths)

write.table(shortestpaths, "shortestpaths.txt", sep="\t")

paths<-get.shortest.paths(net, V(net), mode = "all")
paths<-unlist(paths, recursive = FALSE)

writeLines(as.character(paths),"paths.txt",sep="\n")

#is.connected(net)
average_path_length<-average.path.length(net, directed=TRUE, unconnected=FALSE)
writeLines(as.character(average_path_length),"average_path_length.txt",sep="\n")




#Generate random graph
g <- erdos.renyi.game(length(V(net)), edge_density(net, loops = FALSE), directed = TRUE)
degree_distribution(g)

Degree_Distribution <- igraph::degree(net, mode = "total")
hist(Degree_Distribution)

Degree_Distribution_random<- igraph::degree(g, mode = "total")
hist(Degree_Distribution_random)

  
png("Degree_distribution.png", width = 12, height = 6, units = "cm", res = 600, pointsize = 10)
  
  par(mfrow=c(1,2), mar=c(2,1,2,1), oma=c(1,2,1,1))
  
  hist(Degree_Distribution, xlab = "Node Degree",  col = "grey", border = "black", ylim=c(0,35), xlim = c(0,14), breaks = 14, main = "Observed",  cex.main= 0.9)
  
  hist(Degree_Distribution_random, col = "grey", border = "black", xlab = "Node Degree", ylim=c(0,35), xlim = c(0,14), breaks = 7, main = "Simulated", cex.main= 0.9)
  
  mtext("Degree distribution", outer = TRUE, cex = 1, font=2)
  #text(, "Degree distribution",cex=2,font=2,pos=3, offset=.2)
  
dev.off()





#Network visualizaion
c <-layout_in_circle(net)

V(net)[type=="Mosquito"]$color <- "tan3"
V(net)[type=="Virus"]$color <- "skyblue2"
V(net)[type=="Host"]$color <-"yellowgreen"
V(net)$size <-(Page_Rank$vector)*500


#Parameters to place labels around the circunference
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2* pi), range(x)))
}

n <- length(V(net)$name)
lab.locs <- radian.rescale(x=1:n, direction=-1,  start=0)


#Change edge color for Culex nigripalpus nodes

edgeseq = E(net)[weight>10]#Culex nigripalpus nodes 
others= E(net)[weight<10]#other nodes 

E(net)[edgeseq]$color="Indianred4"
E(net)[others]$color="black"
E(net)[weight>10]$weight<-5


png("Integrative_Network.png", width = 12, height = 12, units = "cm", res = 600, pointsize = 10)

par(mar=c(2,1,2,1))
plot(net, main= "Integrative Network", edge.arrow.size=.7, edge.width=E(net)$weight, vertex.label=V(net)$abreviation,vertex.label.family="Times", edge.color=E(net)$color, vertex.label.color="black", vertex.label.cex=0.7, vertex.label.dist=2.5, vertex.label.degree=lab.locs, layout=c)

legend(x=0.1,y=-1.3, c("Virus", "Host","Mosquito"),xjust=0.5, pch=21,col="#777777", pt.bg=c("skyblue2", "yellowgreen", "tan3"), pt.cex=1, cex=.8, bty="n", ncol=3)

dev.off()



