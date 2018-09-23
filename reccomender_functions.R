library(recommenderlab)
library(dplyr)
library(tidyr)
library(proxy)
library(dbscan)
library(quanteda)
library(plotly)
n_threads <- max(1L, floor(RcppParallel::defaultNumThreads() - 1))
quanteda_options(threads = n_threads)

library(SnowballC)
library(tidytext)
library(textclean)

data <- readRDS(file = "clean_data_rec.RDS")
courses <- readRDS(file = "clean_courses_rec.RDS")
nn <- readRDS("nearest_neighbor.RDS")

# Recommender function based on courses_taken
course_recs <- function(courses_taken, data, courses, nn) {
  data$course_descriptions <- as.character(data$course_descriptions)
  courses_taken_descriptions <- c()
  for(course in courses_taken){
    course_description <- as.character(data[which(course == data$course_title_codes),]$course_descriptions)
    courses_taken_descriptions <- c(courses_taken_descriptions,course_description )
  }
  recommended_courses <- list()
  if (length(courses_taken_descriptions) != 0) {
    for (i in 1:length(courses_taken_descriptions)) {
      for (j in 1:nrow(courses)) {
        if (courses_taken_descriptions[i] == courses[j]) {
          course_descriptions_rec <- courses[nn$id[j,]]
          str <- as.character(sapply(course_descriptions_rec, function(x){
            course_code <- (data[which(x == data$course_descriptions),]$course_title_code)[1]
            course_title <- (data[which(x == data$course_descriptions),]$course_titles)[1]
            
            return (paste(course_code, "|", course_title))
          }))
          recommended_courses[[i]] <- data.frame(`Reccomended Courses`= str)
          break
        }
      }
    }
  } else {
    return("The user has not taken any courses!")
  }
  
  return(recommended_courses)
}

# Recommender function based on courses_taken
course_recs_vis <- function(courses_taken, data, courses, nn) {
  data$course_descriptions <- as.character(data$course_descriptions)
  courses_taken_descriptions <- c()
  for(course in courses_taken){
    course_description <- as.character(data[which(course == data$course_title_codes),]$course_descriptions)
    courses_taken_descriptions <- c(courses_taken_descriptions,course_description )
  }
  recommended_courses_dist <- list()
  if (length(courses_taken_descriptions) != 0) {
    for (i in 1:length(courses_taken_descriptions)) {
      for (j in 1:nrow(courses)) {
        if (courses_taken_descriptions[i] == courses[j]) {
          course_descriptions_rec <- courses[nn$id[j,]]
          course_codes_rec<- as.character(sapply(course_descriptions_rec, function(x){
            course_code <- (data[which(x == data$course_descriptions),]$course_title_code)[1]
            return (course_code)
          }))
          text <- as.character(sapply(course_descriptions_rec, function(x){
            course_code <- (data[which(x == data$course_descriptions),]$course_title_code)[1]
            course_title <- (data[which(x == data$course_descriptions),]$course_titles)[1]
            
            return (paste(course_code, "|", course_title))
          }))
          data_sim <- data.frame("Course Title Code" = course_codes_rec, "Cosine Similarity" = as.numeric(1-nn$dist[j,]), "Text" = text, stringsAsFactors = FALSE)
          data_sim <- data_sim %>% arrange(desc(Cosine.Similarity))
          p <- plot_ly(data_sim, x = ~Course.Title.Code, y = ~Cosine.Similarity, type = 'bar', text = ~Text,
                       marker = list(color = 'rgb(158,202,225)',
                                     line = list(color = 'rgb(8,48,107)',
                                                 width = 1.5))) %>%
            layout(title = paste("Cosine Similarity between Recommended Courses and",courses_taken[i]),
                   xaxis = list(title = "Recommended Courses"),
                   yaxis = list(title = "Cosine Similarity"))
          
          
          recommended_courses_dist[[i]] <- p
          break
        }
      }
    }
  } else {
    return("The user has not taken any courses!")
  }
  
  return(recommended_courses_dist)
}