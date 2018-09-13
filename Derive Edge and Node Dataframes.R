library(stringr)
library(data.table)
library(igraph)
library(dplyr)
library(tidyverse)
library(networkD3)
library(threejs)
library(RColorBrewer)
library(visNetwork) 


data("phone.call2")

cs_data <- readRDS(file = "cs_cleaned_data.Rda")
external_data <- readRDS(file = "external_cleaned_data.Rda")
names(cs_data)
names(external_data)
external_data$course_forbidden_overlaps <- rep(NA, each = 32)
external_data$prereq <- rep(NA, each = 32)
external_data$coreq <- rep(NA, each = 32)
external_data$course_prereq_coreq <- rep(NA, each = 32)
data <- rbind(cs_data, external_data)

# some basic eda
data$course_distribution_categories <- sapply(data$course_distribution_categories, function(x){
  x <- gsub("[()]","", x)
})
# add a column to weight the nodes
data$number_of_seasons <- sapply(data$course_offerings, function(x){
  weight <- 0
  if(grepl("Fall",x)){
    weight <- weight + 1
  }
  if(grepl("Spring",x)){
    weight <- weight + 1
  }
  if(grepl("Summer",x)){
    weight <- weight + 1
  }
  return (weight)
})
# create node list

nodes <- data %>% select(-c("prereq", "coreq", "course_prereq_coreq"))
# hardcoding edgelist
raw_edge_data <- data %>% select(c("course_title_codes","prereq","coreq", "course_prereq_coreq"))

# extract all math courses with course numbers greater than 3000
# codes <- as.vector(as.character(raw_edge_data$course_title_codes))
# codes_greater_3000 <- codes[grepl("MATH",codes)]
# codes_greater_3000 <- codes_greater_3000[as.numeric(substr(codes_greater_3000,6, str_length(codes_greater_3000))) >= 3000]

new_nodes <- c()
create_edges_4_vectors <- function(edges_df, v1, v2, v3,v4, final, requisite){
  for (e1 in v1){
    for (e2 in v2){
      for (e3 in v3){
        for(e4 in v4){
        new_node <- paste(e1, "; ", e2, "; ", e3, "; ", e4, sep = "")
        edge1 <- c(e1, new_node, requisite )
        edge2 <- c(e2, new_node, requisite )
        edge3 <- c(e3, new_node, requisite )
        edge4 <- c(e4, new_node, requisite )
        edge5 <- c(new_node, final, requisite)
        edges_df <- rbind(edges_df,edge1,edge2,edge3,edge4,edge5)
        }
        }
      }
    }
  return (edges_df)
}
create_edges <- function(edges_df, v1, v2, v3, final, requisite){
  if(length(v3) == 0 & length(v1)!=0 & length(v2)==0){
    for (e1 in v1){
      edge1 <- c(e1, final, requisite )
      edges_df <- rbind(edges_df,edge1)
    }
  }
  else if(length(v3) == 0 & length(v1)!=0 & length(v2)!=0){
    for (e1 in v1){
      for (e2 in v2){
        new_node <- paste(e1, "; ", e2, sep = "")
        edge1 <- c(e1, new_node, requisite )
        edge2 <- c(e2, new_node, requisite )
        edge3 <- c(new_node, final, requisite)
        edges_df <- rbind(edges_df,edge1,edge2,edge3)
      }
    }
  }
  else{
    for (e1 in v1){
      for (e2 in v2){
        for (e3 in v3){
          new_node <- paste(e1, "; ", e2, "; ", e3, sep = "")
          edge1 <- c(e1, new_node, requisite )
          edge2 <- c(e2, new_node, requisite )
          edge3 <- c(e3, new_node, requisite )
          edge4 <- c(new_node, final, requisite)
          edges_df <- rbind(edges_df,edge1,edge2,edge3,edge4)
        }
      }
    }
  }
  return (edges_df)
}
courses_extract <-function(x){
  tokens <- unlist(strsplit(x,";"))
  tokens <- tokens[str_length(tokens)>=4]
  i <- 1
  word_vectors <- sapply(tokens, function(or_tokens){
    or_tokens <- unlist(strsplit(or_tokens,","))
    or_tokens <- as.vector(unlist(sapply(or_tokens, function(y){
      if(substr(y, 1, 1) == " "){
        return (substr(y,2,str_length(y)))
      }
      else(
        return (y)
      )
    })))
    return (or_tokens)
  })
  
  return (word_vectors)
}
list = list(0)
edges <- c("V1", "V2", "Relationship")
edges <- create_edges(edges, c("MATH 1110", "MATH 1910"),c(),c(),"CS 1112","Corequisite")
edges <- create_edges(edges, c("CS 1110", "CS 1112"),c(),c(),"CS 2024","Prerequisite")
edges <- create_edges(edges, c("CS 1110", "CS 1112"),c(),c(),"CS 2110","Prerequisite")
edges <- create_edges(edges, c("CS 2110"),c(),c(),"CS 2111","Corequisite")
edges <- create_edges(edges, c("CS 1110", "CS 1112"),c(),c(),"CS 2112","Prerequisite")
edges <- create_edges(edges, c("CS 2110", "CS 2112"),c(),c(),"CS 3110","Prerequisite")
edges <- create_edges(edges, c("CS 2800"),c(),c(),"CS 3110","Corequisite")
edges <- create_edges(edges, c("CS 1110", "CS 1112"),c("CS 2024"),c(),"CS 3410","Prerequisite")
edges <- create_edges(edges, c("CS 3110"),c(),c(),"CS 4110","Prerequisite")
edges <- create_edges(edges, c("CS 2110"),c(),c(),"CS 4154","Prerequisite")
edges <- create_edges(edges, c("MATH 2210", "MATH 2940"),c("MATH 3XXX"),c("CS 1110", "CS 1112"),"CS 4210","Prerequisite")
edges <- create_edges(edges, c("CS 2110", "CS 2112"),c("CS 2800"),c(),"CS 4320","Prerequisite")
edges <- create_edges(edges, c("CS 3410", "CS 3420"),c(),c(),"CS 4410","Prerequisite")
edges <- create_edges(edges, c("CS 4410"),c(),c(),"CS 4411","Corequisite")
edges <- create_edges(edges, c("CS 3410", "CS 3420", "ECE 3140"),c(),c(),"CS 4420","Prerequisite")
edges <- create_edges(edges, c("CS 2110", "CS 2112"),c(),c(),"CS 4620","Prerequisite")
edges <- create_edges(edges, c("CS 2110", "CS 2112"),c("CS 2800"),c(),"CS 4700","Prerequisite")
edges <- create_edges(edges, c("CS 2110"),c(),c(),"CS 4740","Prerequisite")
edges <- create_edges(edges, c("MATH 1920", "MATH 2220"),c("MATH 2940", "MATH 2210"),c("CS 1110", "CS 1112"),"CS 4750","Prerequisite")
edges <- create_edges(edges, c("BTRY 3010"), c("CS 4820"),c(),"CS 4775","Prerequisite")
edges <- create_edges(edges, c("BTRY 3080", "ECON 3130", "MATH 4710", "ENGRD 2700"), c("MATH 2940", "MATH 2210"), c("CS 2110", "CS 2112"),"CS 4780","Prerequisite")
edges <- create_edges(edges, c("CS 2800"), c(), c(),"CS 4810","Prerequisite")
edges <- create_edges(edges, c("PHYS 3316"), c("AEP 3620"), c(),"CS 4812","Prerequisite")
edges <- create_edges(edges, c("CS 2110", "CS 2112", "CS 3110"), c("CS 2800"), c(),"CS 4820","Prerequisite")
edges <- create_edges(edges, c("CS 2110", "CS 2112", "CS 3110"), c("CS 2800"), c(),"CS 4820","Prerequisite")
# new node
edges <- create_edges(edges, c("MATH 2210", "MATH 2220"),c("MATH 2230","MATH 2240"), c("MATH 1920",  "MATH 2940"),"MATH 2210; MATH 2220, MATH 2230; MATH 2240, MATH 1920; MATH 2940", "Prerequisite" )
# new node
edges <- create_edges(edges, c("CS 2800", "MATH 3320", "MATH 3340", "MATH 3360", "MATH 4340"),c(), c(),"CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340", "Prerequisite" )
edges <- create_edges(edges, c("MATH 2210; MATH 2220, MATH 2230; MATH 2240, MATH 1920; MATH 2940"),c("CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340"),c(), "CS 4860", "Prerequisite" )
edges <- create_edges(edges, c("CS 3110"), c(), c(),"CS 5110","Prerequisite")
edges <- create_edges(edges, c("CS 2110","CS 2112"),c(),c(),"CS 5112","Prerequisite")
edges <- create_edges(edges, c("CS 4110"),c("CS 6110"),c("CS 6410"),"CS 5114","Prerequisite")
edges <- create_edges(edges, c("CS 1110","CS 1112"),c(),c(),"CS 5306","Prerequisite")
edges <- create_edges(edges, c("CS 4410"),c(),c(),"CS 5414","Prerequisite")
edges <- create_edges(edges, c("ECE 4750","CS 4420"),c(),c(),"CS 5420","Prerequisite")
edges <- create_edges(edges, c("CS 1110","CS 1112"),c(),c(),"CS 5424","Prerequisite")
edges <- create_edges(edges, c("CS 2800","CS 4820"),c(),c(),"CS 5435","Prerequisite")
edges <- create_edges(edges, c("CS 5435"),c(),c(),"CS 5439","Corequisite")
edges <- create_edges(edges, c("CS 4410"),c(),c(),"CS 5450","Prerequisite")
edges <- create_edges(edges, c("CS 2110"),c(),c(),"CS 5620","Prerequisite")
edges <- create_edges(edges, c("CS 1110","CS 1112"),c(),c(),"CS 5682","Prerequisite")
edges <- create_edges(edges, c("CS 2110","CS 2112"),c(),c(),"CS 5740","Prerequisite")
edges <- create_edges(edges, c("MATH 1920", "MATH 2220"),c("MATH 2940", "MATH 2210"),c("CS 1110", "CS 1112"),"CS 5750","Prerequisite")
edges <- create_edges(edges,c("BTRY 3080", "ECON 3130", "MATH 4710", "ENGRD 2700"), c("MATH 2940", "MATH 2210"), c("CS 2110", "CS 2112"),"CS 5780","Prerequisite")
edges <- create_edges(edges,c("CS 2800"),c("CS 1110", "CS 1112"),c(),"CS 5785","Prerequisite")
edges <- create_edges(edges,c("CS 4110","CS 6110"),c(),c(),"CS 6113","Prerequisite")
edges <- create_edges(edges,c("CS 4110"),c("CS 6110"),c("CS 6410"),"CS 6114","Prerequisite")
edges <- create_edges(edges,c("MATH 4310"),c("MATH 4340"),c(),"CS 6210","Prerequisite")
edges <- create_edges(edges,c("CS 4410"),c(),c(),"CS 6410","Prerequisite")
edges <- create_edges(edges,c("CS 4410"),c(),c(),"CS 6450","Prerequisite")
edges <- create_edges(edges,c("MATH 2210", "MATH 2940"), c("CS 4820"),c(),"CS 6670","Prerequisite")
edges <- create_edges(edges,c("MATH 2210", "MATH 2940"), c("CS 4700", "CS 4740", "CS 4300", "CS 4780", "CS 4786"),c(),"CS 6742","Prerequisite")
edges <- create_edges(edges,c("CS 2800"), c(),c(),"CS 6764","Prerequisite")
edges <- create_edges(edges,c("CS 4780", "CS 5780", "CS 4786", "CS 5786", "CS 6780"), c(),c(),"CS 6783","Prerequisite")
edges <- create_edges(edges,c("CS 4780", "CS 4786"), c(),c(),"CS 6787","Prerequisite")
edges <- create_edges(edges,c("CS 4820"), c(),c(),"CS 6815","Prerequisite")
edges <- create_edges(edges,c("CS 4820"), c(),c(),"CS 6820","Prerequisite")
edges <- create_edges(edges,c("CS 4820"), c(),c(),"CS 6830","Prerequisite")
edges <- create_edges(edges,c("CS 6110"), c(),c(),"CS 7190","Prerequisite")
edges <- create_edges(edges,c("CS 4110"), c(),c(),"CS 7493","Prerequisite")
edges <- create_edges(edges,c("CS 1300"), c(),c(),"CS 2300","Prerequisite")
edges <- create_edges(edges,c("BTRY 3080", "ECON 3130", "MATH 4710", "ENGRD 2700"), c(),c(),"CS 2770","Prerequisite")
edges <- create_edges(edges,c("CS 2110", "CS 2112", "INFO 2450"), c(),c(),"CS 3152","Prerequisite")
edges <- create_edges(edges,c("INFO 3152", "ENGRC 3152"), c(),c(),"CS 3152","Corequisite")
edges <- create_edges(edges,c("CS 2110", "CS 2112"), c("CS 2300"),c(),"CS 3300","Prerequisite")
edges <- create_edges(edges,c("CS 2300"), c(),c(),"CS 3420","Prerequisite")
edges <- create_edges(edges,c("CS 3110"), c("CS 3420", "CS 3410"),c(),"CS 4120","Prerequisite")
edges <- create_edges(edges,c("CS 4121"), c(),c(),"CS 4120","Prerequisite")
edges <- create_edges(edges,c("CS 4120"), c(),c(),"CS 4121","Prerequisite")
edges <- create_edges(edges,c("CS 3152"), c("CS 3300", "CS 4620", "CS 4700", "CS 4758", "CS 5414"),c(),"CS 4152","Prerequisite")
edges <- create_edges(edges,c("MATH 2210", "MATH 2940"), c("MATH 3XXX"), c("CS 1110", "CS 1112"),"CS 4220","Prerequisite")
edges <- create_edges(edges,c("MATH 2210", "MATH 2940"), c("CS 2800"),c(),"MATH 2210, MATH 2940; CS 2800","Prerequisite")
# new node
edges <- create_edges(edges,c("MATH 2210, MATH 2940; CS 2800", "INFO 2940"), c("CS 2110", "CS 2112"),c(),"CS 4300","Prerequisite")
edges <- create_edges(edges,c("CS 4410"), c(),c(),"CS 4450","Prerequisite")
edges <- create_edges(edges, c("CS 2110", "CS 2112"),c("CS 2800"),c(),"CS 4670","Prerequisite")
edges <- create_edges_4_vectors(edges, c("CS 2800"), c("MATH 1910"), c("MATH 1920"), c("MATH 2210", "MATH 2940"),"CS 4850","Prerequisite")
edges <- create_edges(edges, c("INFO 2040"),c("CS 2800"),c("BTRY 3080", "ECON 3130", "MATH 4710", "ENGRD 2700"),"CS 4852","Prerequisite")
edges <- create_edges(edges, c("CS 1110", "CS 1112"),c(),c(),"CS 5094","Prerequisite")
edges <- create_edges(edges, c("CS 3110"),c("CS 3420", "CS 3410"),c(),"CS 5120","Prerequisite")
edges <- create_edges(edges, c("CS 5121"),c(),c(),"CS 5120","Corequisite")
edges <- create_edges(edges, c("CS 5120"),c(),c(),"CS 5121","Corequisite")
edges <- create_edges(edges, c("CS 2110","CS 2112"),c(),c(),"CS 5150","Prerequisite")
edges <- create_edges(edges, c("CS 4410"),c(),c(),"CS 5412","Prerequisite")
edges <- create_edges(edges, c("CS 4410"),c(),c(),"CS 5430","Prerequisite")
edges <- create_edges(edges, c("CS 5430"),c(),c(),"CS 5431","Corequisite")
edges <- create_edges(edges, c("CS 2110", "CS 2112"),c("CS 2800"),c(),"CS 5670","Prerequisite")
edges <- create_edges(edges, c("ORIE 5750", "CS 5785"), c("BTRY 3080", "ECON 3130", "MATH 4710", "ENGRD 2700"), c("MATH 2210", "MATH 2940"),"CS 5726","Prerequisite")
edges <- create_edges(edges, c("CS 4810"),c("CS 2800"),c(),"CS 5830","Prerequisite")
edges <- create_edges(edges, c("CS 3110"),c(),c(),"CS 6117","Prerequisite")
edges <- create_edges(edges, c("CS 4320"),c(),c(),"CS 6320","Prerequisite")
edges <- create_edges(edges, c("CS 4410"),c(),c(),"CS 6411","Prerequisite")
edges <- create_edges(edges, c("CS 2110"),c(),c(),"CS 6466","Prerequisite")
edges <- create_edges(edges, c("CS 4700"),c(),c(),"CS 6700","Prerequisite")
edges <- create_edges_4_vectors(edges, c("MAE 4180", "CS 4758"), c("BTRY 3080", "ECON 3130", "MATH 4710", "ENGRD 2700"), c("MATH 2210", "MATH 2940"), c("CS 2024", "CS 1110"),"CS 6751","Prerequisite")
edges <- create_edges(edges, c("CS 4820"),c(),c(),"CS 6850","Prerequisite")
edges <- create_edges(edges, c("CS 6860"),c(),c(),"CS 6861","Prerequisite")
edges <- data.frame(edges, stringsAsFactors = FALSE)
edges <- edges[-c(1),]
names(edges) <- c("V1", "V2", "Relationship")
edges <- setattr(edges, "row.names", 1:nrow(edges))

# cleaned up edges,
# have to attain new nodes from edge df

codes <- as.character(raw_edge_data$course_title_codes)
# edges$V1 <- as.vector(unlist(sapply(edges$V1, function(x){
#   return (gsub(";", " and",x))
# })))
# edges$V1 <- as.vector(unlist(sapply(edges$V1, function(x){
#   return (gsub(",", " or",x))
# })))
# edges$V2 <- as.vector(unlist(sapply(edges$V1, function(x){
#   return (gsub(";", " and",x))
# })))
# edges$V2 <- as.vector(unlist(sapply(edges$V1, function(x){
#   return (gsub(",", " or",x))
# })))
v1_unique <- unique(edges$V1)
v2_unique <- unique(edges$V2)
all <- unique(as.vector(c(as.vector(v1_unique), as.vector(v2_unique))))
new_nodes <- setdiff(all, codes)
new_nodes_df <- data.frame(course_title_codes = new_nodes, course_titles = rep(NA, length(new_nodes)), course_descriptions = rep(NA, length(new_nodes)),
                           course_offerings = rep(NA, length(new_nodes)), course_permissions = rep(NA, length(new_nodes)), course_forbidden_overlaps = rep(NA, length(new_nodes)),
                           course_distribution_categories = rep(NA, length(new_nodes)), number_of_seasons = rep(.5, length(new_nodes)))
nodes_df <- rbind(nodes, new_nodes_df)

nodes_df <- data.frame(unlist(sapply(nodes_df, as.character)), stringsAsFactors = FALSE)
nodes_df$number_of_seasons <- as.numeric(nodes_df$number_of_seasons)

names(nodes_df)[names(nodes_df) == 'course_title_codes'] <- 'id'
names(edges)[names(edges) == 'V1'] <- 'to'
names(edges)[names(edges) == 'V2'] <- 'from'
nodes_df$course_titles <- unlist(sapply(nodes_df$course_titles, function(x){
  if(is.na(x)){
    return ("")
  }
  else return (as.character(x))
}))
edges_color <- factor(edges$Relationship)
cl <- colors(distinct = TRUE)
set.seed(15887) # to set random generator seed
mycols_edges <-rainbow(length(levels(edges_color)))
edges_color <- as.character(factor(edges$Relationship, labels = mycols_edges))
g <- graph_from_data_frame(d = edges, vertices = nodes_df, directed = TRUE)
full_network <- saveRDS(g, file = "network.Rda")
nodes_vis <- data.frame(id = nodes_df$id, label = nodes_df$id,title = paste0("<p>",nodes_df$id,"<br>",nodes_df$course_titles,"</p>"))
visNetwork(nodes_vis, edges, height = "1000px", width = "1000px")  %>%
  visEdges(arrows = 'from', width = 2, color = edges_color)  %>% 
  visIgraphLayout()%>%
  visNodes(size = 25) %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE) %>%  
  visInteraction(keyboard = TRUE,
                 dragNodes = T, 
                 dragView = T, 
                 zoomView = T) %>%
  visPhysics(solver = "barnesHut")

# ec <- as.numeric(eigen_centrality(g)$vector)
# v <- nodes$number_of_seasons/3
# vertex_label <- nodes$id
# nodes$course_distribution_categories[15]<- NA
# nodes$course_distribution_categories[168]<- NA
# nodes$course_distribution_categories[246]<- NA
# 
# vertex_color <- factor(nodes$course_distribution_categories)
# cl <- colors(distinct = TRUE)
# set.seed(15887) # to set random generator seed
# mycols <-rainbow(length(levels(vertex_color)))
# vertex_color <- as.character(factor(nodes$course_distribution_categories, labels = mycols))
# class(vertex_color)
# vertex_color <- as.vector(unlist(sapply(vertex_color, function(x){
#   if(is.na(x)){
#      return ("white");
#   }
#   else{
#     return (as.character(x))
#   }
# })))
# 
# edges_color <- factor(edges$Relationship)
# cl <- colors(distinct = TRUE)
# set.seed(15887) # to set random generator seed
# mycols_edges <-rainbow(length(levels(edges_color)))
# edges_color <- as.character(factor(edges$Relationship, labels = mycols_edges))
# graphjs(g, vertex.size = (v), vertex.color <- vertex_color, 
#         vertex.label = vertex_label, bg = "black", layout= layout_on_sphere(g),
#         main="sphere layout")
# # Create new vector 'v' that is equal to the square-root of 'ec' multiplied by 5
# visNetwork(nodes, edges, width = "100%") %>%
#   visLayout(randomSeed = 12) 
# visNetwork(nodes, edges) %>% 
#   visIgraphLayout(layout = "layout_with_fr") %>% 
#   visEdges(arrows = "middle") %>%
#   visLayout(randomSeed = 1234) 
# # Plot threejs plot of graph setting vertex size to v
# 
# library("networkD3")
# edges.d3 <- data.frame(from=as.numeric(factor(edges$from))-1, 
#                     to=as.numeric(factor(edges$to))-1 )
# nodes.d3 <- cbind(idn=factor(nodes$id, levels=nodes$id), nodes) 
# 
# 
# forceNetwork(Links = edges.d3, Nodes = nodes.d3, Source="from", Target="to",
#              NodeID = "idn", Group = "course_distribution_categories",linkWidth = 1,
#              linkColour = "#afafaf", fontSize=12, zoom=T, legend=T,
#              Nodesize=8, opacity = 0.8, charge=-300, 
#              width = 1000, height = )
# 
# 
# names(nodes)
# 
