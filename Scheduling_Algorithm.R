library(dplyr)
library(stringr)
data <- readRDS(file = "Preprocesses_course_time_data.Rda")
id <- 1:nrow(data)
data$id <-id

# some constants of interest for the genetic algorithm
population_size <- 15
mutation_rate <- .1
crossover_rate <- .9
tournament_selection_size <- 3
num_of_elite_schedules <-1

instantiate_flags <-function(inputted_courses){
  rep(FALSE,nrow(inputted_courses))
}

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

# takes in a list of course codes and course types and weights in dataframe format and spits out non_overlapping row ids
# must input all lectures, labs, discussions for a particular class
# schedule_non_overlap<- function(courses, data){
#   potential_courses <- apply(courses, 1, function(x){
#     code <- as.character(x[1])
#     type <- as.character(x[2])
#     df <- data %>% filter(data$course_title_codes == code & data$class_type == type)
#     return (df)
#   })
#   n <- nrow(courses)
#   indices <- sample(x = n, size = n, replace = FALSE)
#   selected_courses <- c()
#   for (i in indices){
#     select <- potential_courses[[i]]
#     n2 <- nrow(select)
#     if(n2 != 0){
#       indices2 <- sample(x = n2, size = 1, replace = FALSE)
#       r1 <- select[indices2,]
#       s1 <- as.character(r1 %>% select("class_start_time"))
#       f1 <- as.character(r1 %>% select("class_end_time"))
#       d1 <- as.character(r1 %>% select("class_days"))
#       potential_courses <- lapply(potential_courses, function(y){
#         if(nrow(y)!=0){
#          indices3 <- apply(y, 1, function(b){
#             not_overlap(s1,f1,b[8],b[9],d1, b[3])
#          })
#           y <- y[indices3,]
#           return(y)
#         }
#         else{
#           return(y)
#         }
#       })
#       selected_courses <- c(selected_courses,as.character(r1 %>% select("id")))
#     } else{
#       selected_courses <- selected_courses
#     }
#   }
#   data %>% filter(id %in% selected_courses)
# }

# encode_courses <- function(randomized_course,inputted_courses){
#   str <- ""
#   for (i in 1:nrow(inputted_courses)){
#     row <- inputted_courses[i,]
#     code <- as.character(row[1])
#     type <- as.character(row[2])
#     length <- nrow(randomized_courses %>% filter(course_title_codes==code&class_type ==type))
#     if(length>0){
#       str <- paste(str,"1",sep = "")
#     } else{
#       str <- paste(str,"0",sep = "")
#     }
#   }
#   return (str)
# }

schedule <- function(inputted_courses,data){
  potential_courses <- apply(inputted_courses, 1, function(x){
    code <- as.character(x[1])
    type <- as.character(x[2])
    df <- data %>% filter(data$course_title_codes == code & data$class_type == type)
    return (df)
  })
  n <- nrow(inputted_courses)
  #indices <- sample(x = n, size = n, replace = FALSE)
  selected_courses <- c()
  for (i in 1:n){
    select <- potential_courses[[i]]
    n2 <- nrow(select)
    if(n2 != 0){
      indices2 <- sample(x = n2, size = 1, replace = FALSE)
      r1 <- select[indices2,]
      selected_courses <- c(selected_courses,as.character(r1 %>% select("id")))
    } else{
      selected_courses <- selected_courses
    }
  }
  data %>% filter(id %in% selected_courses)
}

population <- function(size,inputted_courses,data){
  population <- list()
  for (i in 1:size){
    sched <- schedule(inputted_courses,data)
    population[[i]] <- sched
  }
  return (population)
}

conflicts_score <- function(sched,i,morning){
  if(i == nrow(sched)){
    return (0)
  } else{
  class <- sched[i,]
  conflicts <- 0
  s1 <- as.character(class %>% select("class_start_time"))
  f1 <- as.character(class %>% select("class_end_time"))
  d1 <- as.character(class %>% select("class_days"))
  if(morning == TRUE & time_to_seconds(f1) > time_to_seconds("12:00:00")){
    conflicts <- conflicts + 1
  } else if(morning == FALSE & time_to_seconds(s1) < time_to_seconds("12:00:00")){
    conflicts <- conflicts + 1
  }
  else(
    conflicts <- conflicts
  )
  for(j in ((i+1):nrow(sched))){
    other_class <- sched[j,]
    s2 <- as.character(other_class %>% select("class_start_time"))
    f2 <- as.character(other_class %>% select("class_end_time"))
    d2 <- as.character(other_class %>% select("class_days"))
    if(not_overlap(s1,f1,s2,f2,d1,d2)==FALSE){
      # code <- other_class$course_title_codes %>% as.character()
      # type <- other_class$class_type %>% as.character()
      # class_weight <- inputted_courses %>% filter(inputted_courses$course_title_codes == code & inputted_courses$class_type == type)%>% select(weights) %>% as.numeric()
      conflicts <- conflicts + 1
    } else{
      conflicts <- conflicts
      }
  }
    return (conflicts)
  }
}
conflicts <- function(sched, morning){
  sum <- 0
  for (i in 1:nrow(sched)){
    sum <- sum + conflicts_score(sched, i, morning)
  }
  return (sum)
}
fitness_score <- function(sched, morning){
  sum <- 0
  for (i in 1:nrow(sched)){
    sum <- sum + conflicts_score(sched, i,morning)
    sched <- sched 
  }
  return (1/(sum+1))
}
sort_by_fitness <- function(pop,morning){
  schedule_ids <- 1:length(pop)
  fitness_scores <- as.vector(sapply(pop, function(sched){
    fitness_score(sched, morning)
  }))
  fitness_by_id <- data.frame(schedule_ids,fitness_scores) %>% arrange(desc(fitness_scores))
  sorted_pop <- list()
  for(i in 1:length(pop)){
    sorted_pop[[i]] <- pop[[fitness_by_id$schedule_ids[i]]]
  }
  return (sorted_pop)
}

selectTournamentPopulation <- function(pop,inputted_courses,data){
  tournamentPop <- population(tournament_selection_size,inputted_courses,data)
  indices <- sample(x = 1:length(pop), size = tournament_selection_size, replace = FALSE)
  for(i in tournament_selection_size){
    tournamentPop[[i]] <- pop[[indices[i]]]
  }
  return (tournamentPop)
}
crossoverSchedule <- function(s1,s2,inputted_courses,data){
  crossoverSched <- schedule(inputted_courses,data)
  for(i in 1:nrow(crossoverSched)){
    if(runif(n=1)>.5){
      crossoverSched[i,] <- s1[i,]
    }
    else{
      crossoverSched[i,] <- s2[i,]
    }
  }
  return (crossoverSched)
}
crossoverPopulation <- function(pop,inputted_courses,data, morning){
  crossoverPop <- population(length(pop),inputted_courses,data)
  for(i in 1:num_of_elite_schedules){
    crossoverPop[[i]] <- pop[[i]]
  }
  for(i in (num_of_elite_schedules+1):length(pop)){
    if(crossover_rate>runif(n=1)){
      s1 <- sort_by_fitness(selectTournamentPopulation(pop,inputted_courses,data),morning)[[1]]
      s2 <- sort_by_fitness(selectTournamentPopulation(pop,inputted_courses,data), morning)[[1]]
      crossoverPop[[i]] <- crossoverSchedule(s1,s2,inputted_courses,data)
    }
    else{
      crossoverPop[[i]] <- pop[[i]]
    }
  }
  return (crossoverPop)
}


mutateSchedule <- function(mutateSched, inputted_courses,data){
  Sched <- schedule(inputted_courses,data)
  for(i in nrow(mutateSched)){
    if(mutation_rate>runif(n=1)){
      mutateSched[i,] <- Sched[i,]
    }
    else{
      mutateSched[i,] <- mutateSched[i,]
    }
  }
  return (mutateSched)
}
mutatePopulation <- function(pop,inputted_courses,data){
  mutatePop <- population(length(pop),inputted_courses,data)
  for(i in 1:num_of_elite_schedules){
    mutatePop[[i]]<-pop[[i]]
  }
  for(i in (num_of_elite_schedules+1):length(pop)){
    mutatePop[[i]]<-mutateSchedule(pop[[i]],inputted_courses,data)
  }
  return (mutatePop)
}

evolve <- function(pop,inputted_courses,data,morning){
  return(mutatePopulation(crossoverPopulation(pop,inputted_courses,data,morning),inputted_courses,data))
}
driver <- function(inputted_courses,data,morning){
  generationNumber <- 0
  schedule_num <- 0
  print(paste("Generation #:", generationNumber))
  print(paste("Schedule #:", schedule_num))
  pop <- sort_by_fitness(population(population_size,inputted_courses,data),morning)
  for(sched in pop){
    schedule_num <- schedule_num +1
    print(paste("Schedule",schedule_num))
    print(sched)
    print(paste("Fitness",fitness_score(sched,morning)))
    print(paste("Conflicts",conflicts(sched,morning)))
  }
  while(generationNumber<=15){
    generationNumber <- generationNumber + 1
    print(paste("Generation:",generationNumber))
    pop <- sort_by_fitness(evolve(pop,inputted_courses,data,morning),morning)
    schedule_num <- 0
    for(sched in pop){
      schedule_num <- schedule_num +1
      print(paste("Schedule",schedule_num))
      print(sched)
      print(paste("Fitness",fitness_score(sched,morning)))
      print(paste("Conflicts",conflicts(sched,morning)))
    }
  }
  return (pop)
}
conflicts_score2 <- function(sched,i){
    class <- sched[i,]
    conflicts <- 0
    s1 <- as.character(class %>% select("class_start_time"))
    f1 <- as.character(class %>% select("class_end_time"))
    d1 <- as.character(class %>% select("class_days"))
    for(j in (1:nrow(sched))){
      other_class <- sched[j,]
      s2 <- as.character(other_class %>% select("class_start_time"))
      f2 <- as.character(other_class %>% select("class_end_time"))
      d2 <- as.character(other_class %>% select("class_days"))

      if(i == j){
        conflicts <- conflicts
      }
      else if(not_overlap(s1,f1,s2,f2,d1,d2)==FALSE){
        conflicts <- conflicts + 1
      } else{
        conflicts <- conflicts
      }
    }
    return (conflicts)
}
conflicting_classes <- function(sched){
  conflicts <- c()
  for (i in 1:nrow(sched)){
    conflicts_i <- conflicts_score2(sched, i)
    if(conflicts_i == 0){
      conflicts <- conflicts
    }
    else{
      this_class_id <- sched[i,]$id %>% as.character()
      conflicts <- c(conflicts,this_class_id)
    }
  }
  return (conflicts)
}
# 
choose_best_classes <- function(conflicting_ids, sched, inputted_courses){
  non_conflicting <-sched %>% filter(!(sched$id %in% conflicting_ids))
  conflicting <- sched %>% filter(sched$id %in% conflicting_ids)
  opt_class_ids <- list()
  opt <- c()
  for (i in 1: length(conflicting_ids)){
    c1 <- conflicting %>% filter(conflicting$id == conflicting_ids[i])
    code1 <- c1$course_title_codes %>% as.character()
    type1 <- c1$class_type %>% as.character()
    weight1 <- (inputted_courses %>% filter(inputted_courses$course_title_codes == code1 & inputted_courses$class_type == type1))$weight
    weights <-weight1
    opt_class_id <- c(conflicting_ids[i])
    other_conflicting_ids <- conflicting_ids[-i]
    other_conflicting <- conflicting %>% filter(conflicting$id != conflicting_ids[i])
    s1 <- c1$class_start_time
    f1 <- c1$class_end_time
    d1 <- c1$class_days
    for(j in 1:length(other_conflicting_ids)){
      c2 <- other_conflicting %>% filter(other_conflicting$id == other_conflicting_ids[j])
      code2 <- c2$course_title_codes %>% as.character()
      type2 <- c2$class_type %>% as.character()
      weight2 <- (inputted_courses %>% filter(inputted_courses$course_title_codes == code2 & inputted_courses$class_type == type2))$weight
      s2 <- c2$class_start_time
      f2 <- c2$class_end_time
      d2 <- c2$class_days
      if(not_overlap(s1,f1,s2,f2,d1,d2)){
        weights <- weights + weight2
        opt_class_id <- c(opt_class_id, other_conflicting_ids[j])
      }
      else{
        weights <- weights
        opt_class_id <- opt_class_id
      }
    }
    opt_class_ids[[i]]<- opt_class_id
    opt[i]<- weights
  }
  max_weight_index <- -1
  max_weight <- -1
  for(i in 1:length(conflicting_ids)){
    if(opt[i]> max_weight){
      max_weight <- opt[i]
      max_weight_index <- i
    }
    else{
      max_weight <- max_weight
      max_weight_index <- max_weight_index
    }
  }
  best_classes_ids <- opt_class_ids[[max_weight_index]]
  best_classes <- conflicting %>% filter(conflicting$id %in% best_classes_ids)
  return (rbind(non_conflicting, best_classes))
}

compile_raw_schedule <- function(sched,inputted_courses,data){
  conflicting_ids <- conflicting_classes(sched)
  conflicting_class <- sapply(1:nrow(sched), function(x){
    class_id <- sched[x, ] %>% select(id) %>% as.character()
    if(class_id %in% conflicting_ids){
      return (TRUE)
    } else{
      return (FALSE)
    }
  })
  sched <- cbind(sched, conflicting_class)
  return (sched)
}
scheds_unique <- function(scheds){
  sched_numbers <- c()
  for(i in 1:(length(scheds)-1)){
    s1 <- scheds[[i]]$id
    for(j in (i+1): length(scheds)){
      s2 <- scheds[[j]]$id
      if(all(s1 %in% s2)){
        print(i)
        print(j)
        sched_numbers <- c(sched_numbers,i)
      }
    }
  }
  names(scheds) <- as.character(1:length(scheds))
  return (scheds[!(names(scheds) %in% as.character(sched_numbers))])
}
prettify_scheds <- function(sched){
  times <- sched$class_start_time %>% unique()
  nice_sched <- data.frame(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
}
generate_multiple_raw_schedules <- function(inputted_courses,data, morning){
  scheds <- driver(inputted_courses,data, morning)
  new_scheds <-list()
  for(i in 1:length(scheds)){
    new_scheds[[i]] <- compile_raw_schedule(scheds[[i]],inputted_courses,data)
  }
  new_scheds <- scheds_unique(new_scheds)
  return (new_scheds)
}
## check over the ending part where we remove conflicting courses again, cannot work when we end up with a three-way conflict
scheds <- generate_multiple_raw_schedules(inputted_courses,data, morning = FALSE)


