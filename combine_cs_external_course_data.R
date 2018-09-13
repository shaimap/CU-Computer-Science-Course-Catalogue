library(dplyr)
cs_data <- readRDS(file = "cs_cleaned_data.Rda")
external_data <- readRDS(file = "external_cleaned_data.Rda")
math_courses_greater_3000 <- readRDS(file = "math_course_codes_greater_3000.Rda")
math_course_codes_greater_3000 <- as.vector(math_courses_greater_3000[,"course_title_codes"])

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
raw_edge_data <- data %>% select(c("course_title_codes","prereq","coreq"))
raw_edge_data <- data.frame(sapply(raw_edge_data, as.character), stringsAsFactors = FALSE)
saveRDS(nodes, "node_metadata.Rda")
saveRDS(raw_edge_data, "edge_metadata.Rda")