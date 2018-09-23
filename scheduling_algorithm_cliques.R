library(dplyr)
library(stringr)
library(data.table)
library(igraph)
library(htmltools)


#data <- readRDS(file = "Preprocesses_course_time_data.Rda")

days_overlap <- function(d1, d2){
  ifelse(grepl("M",d1) & grepl("M",d2), TRUE,
  ifelse(grepl("T",d1) & grepl("T",d2), TRUE,
  ifelse(grepl("W",d1) & grepl("W",d2), TRUE,
  ifelse(grepl("R",d1) & grepl("R",d2), TRUE,
  ifelse(grepl("F",d1) & grepl("F",d2), TRUE,
  ifelse(grepl("Su",d1) & grepl("Su",d2), TRUE,
  ifelse(!grepl("Su",d1) & !grepl("Su",d2) & (grepl("S",d1)) & (grepl("S",d2)),TRUE,
  ifelse((grepl("SSu",d1) & grepl("S",d2)) | ((grepl("S",d1)) & (grepl("SSu",d2))), TRUE,
  FALSE))))))))
}
time_to_seconds<- function(t){
  t <- strsplit(t,":") %>% unlist()
  t <- (as.numeric(t[1])*3600) + (as.numeric(t[2])*60)
  return(t)
}
times_overlap <- function(s1,f1, s2, f2){
  s1 <- time_to_seconds(s1)
  f1 <- time_to_seconds(f1)
  s2 <- time_to_seconds(s2)
  f2 <- time_to_seconds(f2)
  return ((s1<=f2) & (f1>=s2))
}
not_overlap <- function(s1,f1,s2,f2,d1,d2){
  ifelse(days_overlap(d1,d2)==FALSE,TRUE,
         ifelse(times_overlap(s1,f1,s2,f2)==FALSE,TRUE, FALSE))
}
create_nodes<- function(inputted_courses,data){
  potential_courses <- apply(inputted_courses, 1, function(x){
    code <- as.character(x[1])
    type <- as.character(x[2])
    df <- data %>% filter(data$course_title_codes == code & data$class_type == type)
    df$course_title_codes <- as.character(df$course_title_codes)
    return (df)
  }) 
  potential_courses <- do.call("rbind", potential_courses)
  return (potential_courses$id)
}
create_edges_schedule<- function(inputted_courses,data){
  potential_courses <- apply(inputted_courses, 1, function(x){
    code <- as.character(x[1])
    type <- as.character(x[2])
    df <- data %>% filter(data$course_title_codes == code & data$class_type == type)
    df$course_title_codes <- as.character(df$course_title_codes)
    return (df)
  }) 
  potential_courses <- do.call("rbind", potential_courses)
  edges <- c("Course_ID1", "Course_ID2")
  n <- nrow(potential_courses)
  for (i in 1:n){
    course1 <- potential_courses[i,]
    other_courses <- potential_courses[-i,]
    s1 <- course1$class_start_time
    f1 <- course1$class_end_time
    d1 <- course1$class_days
    code1 <- course1$course_title_codes
    type1 <- course1$class_type
    for(j in 1:nrow(other_courses)){
      course2 <- other_courses[j,]
      s2 <- course2$class_start_time
      f2 <- course2$class_end_time
      d2 <- course2$class_days
      code2 <- course2$course_title_codes
      type2 <- course2$class_type
      if(not_overlap(s1,f1,s2,f2,d1,d2)==FALSE){
        edges <- edges
      }
      else if(code1 == code2 & type1 == type2){
        edges <- edges
      }
      else{
        edges <- rbind(edges,c(course1$id,course2$id))
      }
    }
  }
  edges <- data.frame(edges, stringsAsFactors = FALSE)
  edges <- edges[-c(1),]
  if(length(edges) <= 1){
    return (NULL)
  }
  else{
  names(edges) <- c("Course_ID1", "Course_ID2")
  edges <- setattr(edges, "row.names", 1:nrow(edges))
  edges <- edges %>% filter(Course_ID1 != Course_ID2)
  edges$Course_ID1<- as.numeric(edges$Course_ID1)
  edges$Course_ID2<- as.numeric(edges$Course_ID2)
  return (edges)
  }
}

graph_object_schedule <- function(inputted_courses,data){
  nodes <- create_nodes(inputted_courses,data)
  edges <- create_edges_schedule(inputted_courses,data)
  if(is.null(edges)){
    return (NULL)
  }
  else{
  g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  plot(g)
  return (g)
  }
}
importance_rating <- function(inputted_courses,schedule,morning){
  rating <- 0
  for(i in 1:nrow(schedule)){
    course <- schedule[i,]
    code <- as.character(course$course_title_codes)
    type <- course$class_type
    rating <- rating + (inputted_courses %>% filter((inputted_courses$course_title_codes == code)
                                         & (inputted_courses$class_type == type)))$weight
    s1 <- course$class_start_time
    f1 <- course$class_end_time
    if(morning == TRUE & time_to_seconds(f1) < time_to_seconds("12:00:00")){
      rating <- rating + 1
    } else if(morning == FALSE & time_to_seconds(s1) > time_to_seconds("12:00:00")){
      rating <- rating + 1
    }
    len1 <- nrow(inputted_courses %>% filter(inputted_courses$course_title_code==code))
    len2 <- nrow(schedule %>% filter(schedule$course_title_code==code))
    if(len1 == len2){
      rating <- rating + 10
    }
  }
  return (rating)
}

sort_by_importance <- function(inputted_courses,schedules,morning){
  schedule_ids <- 1:length(schedules)
  importance_ratings <- as.vector(sapply(schedules, function(sched){
    importance_rating(inputted_courses,sched, morning)
  }))
  ratings_by_id <- data.frame(schedule_ids,importance_ratings) %>% arrange(desc(importance_ratings))
  sorted_schedules <- list()
  for(i in 1:length(schedules)){
    sorted_schedules[[i]] <- schedules[[ratings_by_id$schedule_ids[i]]]
  }
  return (sorted_schedules)
}
start_time_non_24_hour <- function(time){
  split <- str_split(time,"-") %>% unlist()
  t <-gsub(" ", "", split[1])
  return (t)
}

attain_schedules <- function(inputs, data,morning){
  inputted_courses <- list()
  for(i in 1:nrow(inputs)){
    x <- inputs[i,]
    class_types <- (data %>% filter(data$course_title_codes == x$course_title_codes))$class_type %>% unique()
    course_title_codes <- rep(x$course_title_codes,length(class_types))
    weight <-rep(x$weight,length(class_types))
    inputted_courses[[i]] <- data.frame(course_title_codes, class_types, weight, stringsAsFactors = FALSE)
  }

  inputted_courses<- do.call("rbind",inputted_courses)
  g <- graph_object_schedule(inputted_courses,data)
  if(is.null(g)){
    df <- data.frame(data %>% filter(data$course_title_codes == inputted_courses$course_title_codes[1] & data$class_type == inputted_courses$class_type), stringsAsFactors = FALSE)
    schedules <- list()
    for(i in 1:nrow(df)){
      schedules[[i]] <- df[i,]
    }
    schedules <- sort_by_importance(inputted_courses,schedules,morning)
    return (schedules)
  } else{
    largest_cliques <- largest_cliques(g)
    schedules <- lapply(largest_cliques, function(clique){
      clique <- as.numeric(clique)
      nodes <- create_nodes(inputted_courses,data)
      indices <- nodes[clique]
      return (data %>% filter(data$id %in% indices))
    })
  schedules <- sort_by_importance(inputted_courses,schedules,morning)
  return (schedules)
  }
}
clean_schedule <- function(schedule){
  clean_sched <- data.frame("Course Title Code" = schedule$course_title_codes,
                            "Course Title"= schedule$course_titles,
                            "Class Type" = schedule$class_type,
                            "Class Locations" = schedule$class_locations,
                            "Class Days" = schedule$class_days,
                            "Class Time" = schedule$class_times,
                            "Registration Code" = schedule$class_numbers, 
                            stringsAsFactors = FALSE,
                            check.names = FALSE)
  return (clean_sched)
}
prettify_schedule <- function(schedule){
  start_times <- as.vector(sapply(schedule$class_times, start_time_non_24_hour))
  start_sort <- data.frame(time_ids = 1:length(start_times), 
                           start_times = sapply(schedule$class_start_time, time_to_seconds), 
                           stringsAsFactors = FALSE) %>% 
    arrange(start_times)
  sorted_start_times <- c()
  for(i in 1:length(start_times)){
    sorted_start_times[i] <- start_times[start_sort$time_ids[i]]
  }
  sorted_start_times <- unique(sorted_start_times)
  n <- length(sorted_start_times)
  calendar <- data.frame(class_start_times = sorted_start_times, 
                         Sunday = rep("",n), 
                         Monday = rep("",n), 
                         Tuesday = rep("",n),
                         Wednesday = rep("",n),
                         Thursday = rep("",n),
                         Friday = rep("",n),
                         Saturday = rep("",n), stringsAsFactors = FALSE, check.names = FALSE)
  for(i in 1:nrow(calendar)){
    for(j in 1:nrow(schedule)){
      x <- schedule[j,]
      if(grepl("SSu",x$class_days) & start_time_non_24_hour(x$class_times)==calendar[i,1]){
        entry <- paste0("<center><b>",as.character(x$course_title_codes)," ",
                      as.character(x$class_type),"</b><br/></center><center>",
                      as.character(x$class_location),"</center><center>",
                      as.character(x$class_times), "</center>")
        calendar[i,2] <- entry
        calendar[i,8] <- entry
      }
      if(grepl("Su",x$class_days) & !grepl("SSu",x$class_days)& start_time_non_24_hour(x$class_times)==calendar[i,1]){
        entry <- paste0("<center><b>",
                                   as.character(x$course_title_codes)," ",
                                   as.character(x$class_type),"</b><br/></center><center>",
                                   as.character(x$class_location),"</center><center>",
                                   as.character(x$class_times), "</center>")
        calendar[i,1] <- entry
      }
      if(grepl("S",x$class_days) & !grepl("Su",x$class_days)&start_time_non_24_hour(x$class_times)==calendar[i,1]){
        entry <- paste0("<center><b>",
                        as.character(x$course_title_codes)," ",
                        as.character(x$class_type),"</b><br/></center><center>",
                        as.character(x$class_location),"</center><center>",
                        as.character(x$class_times), "</center>")
        calendar[i,8] <- entry
      }
      if(grepl("M",x$class_days)&start_time_non_24_hour(x$class_times)==calendar[i,1]){
        entry <- paste0("<center><b>",
                        as.character(x$course_title_codes)," ",
                        as.character(x$class_type),"</b><br/></center><center>",
                        as.character(x$class_location),"</center><center>",
                        as.character(x$class_times), "</center>")
        calendar[i,3] <- entry
      }
      if(grepl("T",x$class_days)&start_time_non_24_hour(x$class_times)==calendar[i,1]){
        entry <- paste0("<center><b>",
                        as.character(x$course_title_codes)," ",
                        as.character(x$class_type),"</b><br/></center><center>",
                        as.character(x$class_location),"</center><center>",
                        as.character(x$class_times), "</center>")
        calendar[i,4] <- entry
      }
      if(grepl("W",x$class_days)&start_time_non_24_hour(x$class_times)==calendar[i,1]){
        entry <- paste0("<center><b>",
                        as.character(x$course_title_codes)," ",
                        as.character(x$class_type),"</b><br/></center><center>",
                        as.character(x$class_location),"</center><center>",
                        as.character(x$class_times), "</center>")
        calendar[i,5] <- entry
      }
      if(grepl("R",x$class_days)&start_time_non_24_hour(x$class_times)==calendar[i,1]){
        entry <- paste0("<center><b>",
                        as.character(x$course_title_codes)," ",
                        as.character(x$class_type),"</b><br/></center><center>",
                        as.character(x$class_location),"</center><center>",
                        as.character(x$class_times), "</center>")
        calendar[i,6] <- entry
      }
      if(grepl("F",x$class_days)&start_time_non_24_hour(x$class_times)==calendar[i,1]){
        entry <- paste0("<center><b>",
                        as.character(x$course_title_codes)," ",
                        as.character(x$class_type),"</b><br/></center><center>",
                        as.character(x$class_location),"</center><center>",
                        as.character(x$class_times), "</center>")
        calendar[i,7] <- entry 
      }
    }
  }
  calendar$class_start_times <- sapply(calendar$class_start_times, function(t){
    return (paste(str_sub(t,1,(str_length(t)-2)),str_sub(t,(str_length(t)-1),str_length(t))))
  })
  calendar <- setattr(calendar, "row.names", calendar$class_start_times)
  colnames(calendar) <- paste0("<center>", colnames(calendar),"</center>" )
  
  return (calendar %>% select(-1))
}
course_numbers <- function(schedule){
  str <- schedule$class_numbers[1]
  for(i in 2:length(schedule$class_numbers)){
    str <- paste(str, schedule$class_numbers[i], sep = ", ")
  }
  return (paste("Registration Codes:",str))
}
# inputs <- data.frame(course_title_codes = c("CS 3110", "CS 4700", "MATH 3320", "LING 1101"), weight = c(5,4,3,2), stringsAsFactors = FALSE)
# schedules <- attain_schedules(inputs, data, morning = FALSE)
# prettified_schedule <- prettify_schedule(schedules[[1]])
# course_numbers <- course_numbers(schedules[[1]])
# cleaned_sched <- clean_schedule(schedules[[1]])
