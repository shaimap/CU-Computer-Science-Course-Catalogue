library(stringr)
library(data.table)
library(igraph)
library(dplyr)
library(tidyverse)
library(networkD3)
library(threejs)
library(RColorBrewer)
library(visNetwork)


edge_metadata <- readRDS("edge_metadata.Rda")
node_metadata <- readRDS("node_metadata.Rda")

create_edges <- function(edges_df, list, final, requisite){
  if(length(list) == 1){
    v1 = list[[1]]
    for (e1 in v1){
      edge1 <- c(e1, final, requisite )
      edges_df <- rbind(edges_df,edge1)
    }
  }
  else if(length(list) == 2){
    v1 = list[[1]]
    v2 = list[[2]]
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
  else if (length(list) == 3){
    v1 = list[[1]]
    v2 = list[[2]]
    v3 = list[[3]]
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
  else if (length(list) == 4){
    v1 = list[[1]]
    v2 = list[[2]]
    v3 = list[[3]]
    v4 = list[[4]]
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
  }
  return (edges_df)
}
courses_extract <-function(x){
  tokens <- unlist(strsplit(x,";"))
  tokens <- tokens[str_length(tokens)>=4]
  i <- 1
  word_vectors <- lapply(tokens, function(or_tokens){
    or_tokens <- unlist(strsplit(or_tokens,","))
    or_tokens <- as.vector(unlist(sapply(or_tokens, function(y){
      if(substr(y, 1, 1) == " "){
        y <- (substr(y,2,str_length(y)))
      }
      if(substr(y, str_length(y),str_length(y)) == " "){
        y <- (substr(y,1,str_length(y)-1))
      }
      return (y)
    })))
    return (or_tokens)
  })
  
  return (word_vectors)
}

valid_dependents <- function(raw_edge_df, course_code){
  course_row <- raw_edge_df %>% filter(course_title_codes == course_code)
  prereq <- course_row$prereq
  coreq <- course_row$coreq
  if(is.na(prereq) & is.na(coreq)){
    return (FALSE)
  }
  else {
    return (TRUE)
  }
}
# assumes prereq and coreq exist
course_edges <- function(raw_edge_df, course_code){
  course_row <- raw_edge_df %>% filter(course_title_codes == course_code)
  prereq <- course_row$prereq
  coreq <- course_row$coreq
  edges <- c("V1", "V2", "Relationship")
  if(course_code == "CS 4860"){
    tokens1 <- list(c("MATH 2210"),c("MATH 2220"))
    edges <- create_edges(edges, tokens1, "MATH 2210; MATH 2220", "Prerequisite")
    tokens2 <- list(c("MATH 2230"), c("MATH 2240"))
    edges <- create_edges(edges, tokens2, "MATH 2230; MATH 2240", "Prerequisite")
    tokens3 <- list(c("MATH 1920"), c("MATH 2940"))
    edges <- create_edges(edges, tokens3, "MATH 1920; MATH 2940", "Prerequisite")
    tokens4 <- list(c("MATH 2210; MATH 2220", "MATH 2230; MATH 2240","MATH 1920; MATH 2940" ))
    edges <- create_edges(edges, tokens4, "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940))", "Prerequisite")
    tokens5 <- courses_extract("CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340")
    edges <- create_edges(edges, tokens5, "(CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340)", "Prerequisite")
    l1 = list(c("((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940))"),c("(CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340)"))
    edges <- create_edges(edges, l1, "CS 4860", "Prerequisite" )
  } else if(course_code == "CS 4300"){
    l1 = list(c("MATH 2210", "MATH 2940"),c("CS 2800"))
    edges <- create_edges(edges, l1, "(MATH 2210, MATH 2940; CS 2800)", "Prerequisite" )
    l2 = list(c("(MATH 2210, MATH 2940; CS 2800)", "INFO 2940"), c("CS 2110", "CS 2112"))
    edges <- create_edges(edges, l2, "CS 4300", "Prerequisite" )
  } else if(!is.na(prereq) & is.na(coreq)){
    tokens_prereq <- courses_extract(prereq)
    edges <- create_edges(edges, tokens_prereq, course_code, "Prerequisite")
  } else if(is.na(prereq) & !is.na(coreq)){
    tokens_coreq <- courses_extract(coreq)
    edges <- create_edges(edges, tokens_coreq, course_code, "Corequisite")
  } else{
    
    tokens_prereq <- courses_extract(prereq)
    prereq_bool <-length(tokens_prereq)==1 & tokens_prereq[[1]]==prereq 
    if(!prereq_bool){
    edges <- create_edges(edges, tokens_prereq, prereq, "Prerequisite")
    }
    tokens_coreq <- courses_extract(coreq)
    coreq_bool <- length(tokens_coreq)==1 & tokens_coreq[[1]]==coreq
    if(!coreq_bool){
    edges <- create_edges(edges, tokens_coreq, coreq, "Corequisite")
    }
    new_node = paste("(",prereq,"); (",coreq,")", sep = "")
    l1 = list(prereq)
    edges <- create_edges(edges, l1, new_node, "Prerequisite")
    l2 = list(coreq)
    edges <- create_edges(edges, l2, new_node, "Corequisite")
    l3 = list(new_node)
    edges <- create_edges(edges, l3, course_code, "Prerequisites and Corequisites")
  }
  edges <- data.frame(edges, stringsAsFactors = FALSE)
  edges <- edges[-c(1),]
  names(edges) <- c("to", "from", "Relationship")
  edges <- setattr(edges, "row.names", 1:nrow(edges))
  edges <- edges %>% filter(to != from)
  return (edges)
}

generate_course_info <- function(x){
  if(x == "MATH 3XXX"){
    return ("Any MATH course numbered 3000 and above")
  }
  else if(grepl(";|,", x)){
    return ("Combination of courses")
  }
  else{
    return("Not offered in the past year (2018)")
  }
}

# assumes edges is a huge dataframe
course_nodes <- function(original_nodes, edges){
  codes <- as.character(original_nodes$course_title_codes)
  v1_unique <- unique(edges$from)
  v2_unique <- unique(edges$to)
  all <- unique(as.vector(c(as.vector(v1_unique), as.vector(v2_unique))))
  new_nodes <- setdiff(all, codes)
  course_info <- as.vector(sapply(new_nodes, generate_course_info))
  new_nodes_df <- data.frame(course_title_codes = new_nodes, course_titles = as.vector(sapply(new_nodes, generate_course_info)), course_descriptions = rep(NA, length(new_nodes)),
                             course_offerings = rep(NA, length(new_nodes)), course_permissions = rep(NA, length(new_nodes)), course_forbidden_overlaps = rep(NA, length(new_nodes)),
                             course_distribution_categories = rep(NA, length(new_nodes)), number_of_seasons = rep(.5, length(new_nodes)), stringsAsFactors = FALSE)
  original_node_codes <- setdiff(all, new_nodes)
  original_nodes_selected <- original_nodes %>% filter(course_title_codes %in% original_node_codes)
  nodes_df <- rbind(original_nodes_selected, new_nodes_df)
  nodes_df <- data.frame(nodes_df, stringsAsFactors = FALSE)
  names(nodes_df)[names(nodes_df) == 'course_title_codes'] <- 'id'
  return (nodes_df)
}
display_label<- function(x){
  if(grepl(";|,", x)){
    return (NA)
  }
  else{
    return(as.character(x))
  }
}
check_prereq_coreq<- function(info, courses){
  tokens <- unlist(strsplit(info,";|[()]|,"))
  tokens <- tokens[str_length(tokens)>=4]
  tokens <- as.vector(unlist(sapply(tokens, function(y){
    if(substr(y, 1, 1) == " "){
      y <- (substr(y,2,str_length(y)))
    }
    if(substr(y, str_length(y),str_length(y)) == " "){
      y <- (substr(y,1,str_length(y)-1))
    }
    return (y)
  })))
  return (any(tokens %in% courses))
}

tokenize <- function(info){
  if(is.na(info)){
    return (c())
  }
  tokens <- unlist(strsplit(info,";|[()]|,"))
  tokens <- tokens[str_length(tokens)>=4]
  tokens <- as.vector(unlist(sapply(tokens, function(y){
    if(substr(y, 1, 1) == " "){
      y <- (substr(y,2,str_length(y)))
    }
    if(substr(y, str_length(y),str_length(y)) == " "){
      y <- (substr(y,1,str_length(y)-1))
    }
    return (y)
  })))
  return (tokens)
}
potential_courses <- function(raw_edge_df, course_code){
  non_na_prereq <- raw_edge_df %>% filter(!is.na(prereq))
  indices_prereq <- as.vector(sapply(non_na_prereq$prereq, function(x){
      return (check_prereq_coreq(x, course_code))
  }))
  prereq_rows <- non_na_prereq[indices_prereq, ]
  non_na_coreq <- raw_edge_df %>% filter(!is.na(coreq))
  indices_coreq <- as.vector(sapply(non_na_coreq$coreq, function(x){
      return (check_prereq_coreq(x, course_code))
      }))
  coreq_rows <- non_na_coreq[indices_coreq, ]
  rows <- rbind(prereq_rows,coreq_rows)
  return (rows)
}


outgoing_edges<- function(raw_edge_df, course_code){
    rows <- potential_courses(raw_edge_df, course_code)

  edges <- c("to", "from", "Relationship")
  
  edges <- apply(rows,1, function(x){
    course_code_target <- as.character(x["course_title_codes"])
    new_edges <- course_edges(rows, course_code_target)
    new <- rbind(edges, new_edges)
    new[-c(1),]
  })
  df <- do.call("rbind", edges)
  df <- setattr(df, "row.names", 1:nrow(df))
  return(df)
}

courses_taken_in_avail_course <- function(courses_taken, avail_course){
  if(is.na(avail_course)){
    return (TRUE)
  }
  else if(avail_course == "(MATH 2210, MATH 2940; CS 2800)"){
    return (courses_taken_in_avail_course(courses_taken, "MATH 2210, MATH 2940; CS 2800"))
  }
  else if(avail_course == "(MATH 2210, MATH 2940; CS 2800); CS 2110"){
    return (courses_taken_in_avail_course(courses_taken, "MATH 2210, MATH 2940; CS 2800")
            & courses_taken_in_avail_course(courses_taken, "CS 2110"))
  }
  else if(avail_course == "(MATH 2210, MATH 2940; CS 2800); CS 2112"){
    return (courses_taken_in_avail_course(courses_taken, "MATH 2210, MATH 2940; CS 2800")
            & courses_taken_in_avail_course(courses_taken, "CS 2112"))
  }
  else if(avail_course == "(CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340)"){
    return (courses_taken_in_avail_course(courses_taken, "CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340"))
  }
  else if(avail_course == "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940))"){
    return (courses_taken_in_avail_course(courses_taken, "MATH 2210; MATH 2220")
            | courses_taken_in_avail_course(courses_taken, "MATH 2230; MATH 2240")
            | courses_taken_in_avail_course(courses_taken, "MATH 1920; MATH 2940"))
  }
  else if(avail_course == "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)); (CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340)"){
    return (courses_taken_in_avail_course(courses_taken, "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940))")
            & courses_taken_in_avail_course(courses_taken, "(CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340)"))
  }
  else{
  tokens <- courses_extract(avail_course)
  for(i in 1:length(tokens)){
     x <- tokens[[i]]
    if("MATH 3XXX" %in% x){
      tokens[[i]] <- tokens[[i]][tokens[[i]]!="MATH 3XXX"]
      tokens[[i]] <- c(tokens[[i]], math_course_codes_greater_3000)
    }
  }
  for (v in tokens){
    if(!(any(v %in% courses_taken))){
      return (FALSE)
    }
  }
  return (TRUE)
  }
}
graph_object <- function(raw_edge_data, raw_nodes_data){
  codes <- raw_edge_data$course_title_codes
  codes <- codes[grepl("CS",codes)]
  edges <- lapply(codes, function(x){
    return (course_edges(raw_edge_data,x))
  })
  edges <- do.call("rbind", edges)
  nodes <- course_nodes(raw_nodes_data, edges)
  g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  return (g)
}

#create vis_plot based on edge list for courses that are prereq to this node
# multiple courses doesn't really work, i'll work on this part later
vis_net_plot <- function(raw_edge_df, raw_nodes_df, course_code, courses_taken, outgoing = FALSE){
  if(outgoing == FALSE){
    if(!valid_dependents(raw_edge_df, course_code)){
      return (NULL)
    }
    edges <- course_edges(raw_edge_df, course_code)
    if(ncol(edges) == 0){
      return (NULL)
    }
    nodes <- course_nodes(raw_nodes_df, edges)
    nodes$group <- as.vector(unlist(sapply(as.character(nodes$id), function(x){
        if (x == course_code){
          return ("Target Course")
        }
        else if(courses_taken_in_avail_course(courses_taken, x)){
         return ("You have taken this course!")
       }
        else{
         return ("You have yet to take this course!")
       }
    })))
    nodes_vis <- data.frame(id = nodes$id, 
                            label = sapply(nodes$id,display_label),
                            title = paste0("<p>",nodes$id,
                                           "<br>",nodes$course_titles,
                                           "<br>",nodes$group,"</p>"),
                            group = nodes$group)
    edges_vis <- data.frame(to = edges$to, 
                            from = edges$from, 
                            width = rep(2, nrow(edges)))
    vis <- visNetwork(nodes_vis, 
                      edges_vis, 
                      height = "700px", 
                      width = "1500px", 
                      background = "black")  %>%
    visEdges(arrows = 'from', width = 1)  %>% 
    visHierarchicalLayout(levelSeparation = 150, 
                      sortMethod = 'directed', 
                      blockShifting = FALSE, 
                      edgeMinimization = FALSE, 
                      direction = 'UD') %>%
    visNodes(size = 25, 
              font = list(face = 'Pt Sans', color = "white", bold = TRUE, size = 20)) %>%
    visOptions(highlightNearest = list( enabled = TRUE, degree = 1, algorithm = "hierarchical"), 
               selectedBy = "label") %>%  
    visPhysics(hierarchicalRepulsion = list(nodeDistance = 150)) %>%
      visInteraction(navigationButtons = TRUE)
    return (vis)
} else{
    new_courses <- potential_courses(raw_edge_df, course_code)
    if(nrow(new_courses) == 0){
      return (NULL)
    } else{
      new_course_codes <- new_courses[,"course_title_codes"]
      edges <- outgoing_edges(raw_edge_df, course_code)
      nodes <- course_nodes(raw_nodes_df, edges)
      nodes$group <- as.vector(unlist(sapply(as.character(nodes$id), function(x){
        if(courses_taken_in_avail_course(courses_taken, x)){
          return ("You have taken this course!")
        }
        else if (x %in% new_course_codes){
          return ("You fulfill at least one requirement for this course!")
        }
        else if (x == course_code){
          return ("Target Course")
        }
        else{
          return ("You have yet to take this course!")
        }
      })))
      nodes$shape <- rep("circularImage", nrow(nodes))
      nodes_vis <- data.frame(id = nodes$id, 
                              label = sapply(nodes$id,display_label),
                              title = paste0("<p>",nodes$id,
                                             "<br>",nodes$course_titles,
                                             "<br>",nodes$group,"</p>"),
                              group = nodes$group)
      edges_vis <- data.frame(to = edges$to, from = edges$from, width = rep(2, nrow(edges)),title = edges$Relationship)
      g <- graph_from_data_frame(edges,nodes,directed=TRUE)
      vis <- visNetwork(nodes_vis, edges_vis, height = "700px", width = "1500px", background = "black")  %>%
      visEdges(arrows = 'from', width = 1)  %>% 
      visIgraphLayout("layout_with_lgl", root = course_code) %>%
      visNodes(size = 25, font = list(face = 'Pt Sans', color = "white", bold = TRUE, size = 20)) %>%
      visOptions(highlightNearest = list( enabled = TRUE, degree = 1, algorithm = "hierarchical"), selectedBy = "label") %>%  
      visPhysics(hierarchicalRepulsion = list(nodeDistance = 200))%>%
        visInteraction(navigationButtons = TRUE)
    return (vis)
    }
  }
}


graph_object_multiple <- function(raw_edge_data, raw_nodes_data, courses_taken){
  original_codes <- raw_edge_data$course_title_codes
  raw_edge_data <- raw_edge_data %>% filter(!(is.na(prereq)&is.na(coreq)))
  codes <- raw_edge_data$course_title_codes
  codes <- codes[grepl("CS",codes)]
  edges <- lapply(codes, function(x){
    return (course_edges(raw_edge_data,x))
  })
  edges <- do.call("rbind", edges)
  for(c in courses_taken){
    edges <- rbind(edges, c("Source", c,"Prerequisite"))
  }
  nodes <- course_nodes(raw_nodes_data, edges)
  g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  return (g)
}

vis_net_paths_multiple <- function(raw_edge_data, raw_nodes_data, course_code, courses_taken){
    g <- graph_object_multiple(raw_edge_data, raw_nodes_data, courses_taken)
    original_edges <- data.frame((as_edgelist(g)), stringsAsFactors = FALSE)
    attributes <- data.frame((edge_attr(g)), stringsAsFactors = FALSE)
    original_edges <- cbind(original_edges,attributes)
    names(original_edges) <- c("to","from", "Relationship")
    original_edges <- unique(original_edges)
    shortest <- all_shortest_paths(g,
                                   from = c("Source"), 
                                   to = c(course_code), 
                                   mode = "out",
                                   weights = rep(1, gsize(g)))
    g_nodes <- data.frame(t(sapply(shortest$res, as_ids)), stringsAsFactors = FALSE)
    g_nodes <- unique(g_nodes)
    if(ncol(g_nodes) ==0){
      return (NULL)
    }
    edges <- apply(g_nodes, 1, function(x){
        convert_path_to_edges(raw_nodes = x, original_edges)
    })
    edges <- do.call("rbind", edges)
    edges <- setattr(edges, "row.names", 1:nrow(edges))
    nodes <- course_nodes(raw_nodes_data, edges)
    nodes$group <- as.vector(unlist(sapply(as.character(nodes$id), function(x){
        if (x == course_code){
          return ("Target Course")
        }
        else if(x == "Source"){
          return ("Source")
        }
        else if(courses_taken_in_avail_course(courses_taken, x)){
          return ("You have taken this course!")
        }
        else{
          return ("You have yet to take this course!")
        }
      })))
    nodes_vis <- data.frame(id = nodes$id, 
                            label = sapply(nodes$id,display_label),
                            title = paste0("<p>",nodes$id,
                                           "<br>",nodes$course_titles,
                                           "<br>",nodes$group,"</p>"),
                            group = nodes$group)
      edges_vis <- data.frame(to = edges$to, 
                              from = edges$from, 
                              width = rep(2, nrow(edges)),
                              title = edges$Relationship)
      # edges_vis$color <- color_palette[value]
      vis <- visNetwork(nodes_vis, 
                        edges_vis, 
                        height = "700px", 
                        width = "1500px", 
                        background = "black")  %>%
        visEdges(arrows = 'from', width = 1)  %>% 
        visHierarchicalLayout(levelSeparation = 150, 
                              sortMethod = 'directed', 
                              blockShifting = FALSE, 
                              edgeMinimization = FALSE, 
                              direction = 'UD') %>%
        visNodes(size = 25, 
                 font = list(face = 'Pt Sans', color = "white", bold = TRUE, size = 20)) %>%
        visOptions(highlightNearest = list( enabled = TRUE, degree = 1, algorithm = "hierarchical"), 
                   selectedBy = "label")%>%
        visPhysics(hierarchicalRepulsion = list(nodeDistance = 150),
                   stabilization = FALSE) %>%
        visInteraction(navigationButtons = TRUE)
      return (vis)
}
# vis_net_paths_multiple(edge_metadata, node_metadata, course_code = c("CS 4780"), courses_taken = c("CS 1110"))


vis_net_possible_courses <- function(raw_edge_df, raw_nodes_df, courses_taken){
  
  raw_edge_df_indices <- apply(raw_edge_df, 1, function(x){
    courses_taken_in_avail_course(courses_taken,as.character(x[2])) & courses_taken_in_avail_course(courses_taken,as.character(x[3]))
  })
  raw_edge_df <- raw_edge_df[raw_edge_df_indices,]
  codes <- (raw_edge_df%>% filter(!(is.na(prereq)&is.na(coreq))))$course_title_codes
  codes <- codes[grepl("CS",codes)]
  edges <- lapply(codes, function(x){
    return (course_edges(raw_edge_df,x))
  })
  edges <- do.call("rbind", edges)
  for(i in 1:length(courses_taken)){
    edges <- rbind(edges,c("Source",courses_taken[i], "Prerequisite"))
  }
  nodes <- course_nodes(raw_nodes_df,edges)
  nodes$group <- as.vector(unlist(sapply(as.character(nodes$id), function(x){
    if(x == "Source"){
      return ("Source")
    }
    else if (courses_taken_in_avail_course(courses_taken, x)){
      return ("You have taken this course!")
    }
    else if (x %in% codes){
      return ("You fulfill all requirements for this course!")
    }
    else{
      return("You have yet to take this course!")
    }
  })))
  nodes_vis <- data.frame(id = nodes$id, 
                            label = sapply(nodes$id,display_label),
                            title = paste0("<p>",nodes$id,
                                           "<br>",nodes$course_titles,
                                           "<br>",nodes$group,"</p>"),
                            group = nodes$group)
  edges_vis <- data.frame(to = edges$to, 
                            from = edges$from, 
                            width = rep(2, nrow(edges)),
                            title = edges$Relationship)
  vis <- visNetwork(nodes_vis, 
                      edges_vis, 
                      height = "1500px", 
                      width = "1500px", 
                      background = "black")  %>%
      visEdges(arrows = 'from', width = 1, color= list(opacity = .4))  %>% 
      visIgraphLayout("layout_with_lgl") %>%
      visNodes(size = 25, 
               font = list(face = 'Pt Sans', color = "white", bold = TRUE, size = 20)) %>%
      visOptions(highlightNearest = list( enabled = TRUE, degree = 1, algorithm = "hierarchical"), 
                 selectedBy = "label") %>%
      visPhysics(hierarchicalRepulsion = list(nodeDistance = 150)) %>%
    visInteraction(navigationButtons = TRUE)
    return (vis)
}

modal_popup_info <- function(node_metadata, input_node){
  g <- graph_object(raw_edge_data, raw_nodes_data)
  b <- betweenness(g)
  this_b <- as.numeric(b[which(names(b)==input_node)])
  row <- node_metadata %>% filter(course_title_codes == input_node)
  if(nrow(row)!= 0){
    attributes <- gsub("Distribution Category ", "",row$course_distribution_categories)
    attributes <- gsub("[()]", "", attributes)
    course_descriptions <- gsub("\n", "", row$course_descriptions)
    info <- paste("<center><b>",row$course_title_codes, "|",
                  row$course_titles,"</b></center><br>Course Distribution Categories: ",
                  attributes, "</br><br>Course Offerings: ",
                  row$course_offerings,
                  "</br><br>Forbidden Overlaps : ",
                  row$course_forbidden_overlaps,
                  "</br><br>Betweeness in overall computer science network : ",
                  this_b,
                  "</br><br>Course Description: ",
                  course_descriptions, "</br>")
    return (info)
  }
  else{
    return (NULL)
  }
}

