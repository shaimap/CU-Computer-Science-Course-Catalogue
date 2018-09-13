library(dplyr)
library(ggmap)
library(stringr)

data <- readRDS(file = "course_time_data.Rda")
data <- unique(data)
data$instructors <- gsub("Instructors","",data$instructors)
generate_start_times <- function(time){
  split <- str_split(time,"-") %>% unlist()
  t <-gsub(" ", "", split[1])
  t <- paste(str_sub(t,1,(str_length(t)-2)),str_sub(t,(str_length(t)-1),str_length(t)))
  t <- format(strptime(t, "%I:%M %p"), format="%H:%M:%S")
  return (t)
}
generate_end_times <- function(time){
  split <- str_split(time,"-") %>% unlist()
  t <-gsub(" ", "", split[2])
  t <- paste(str_sub(t,1,(str_length(t)-2)),str_sub(t,(str_length(t)-1),str_length(t)))
  t <- format(strptime(t, "%I:%M %p"), format="%H:%M:%S")
  return (t)
}
start_times <- as.vector(sapply(data$class_times,generate_start_times))
end_times <- as.vector(sapply(data$class_times,generate_end_times))
data$class_start_time <- start_times
data$class_end_time <- end_times

data <- data[data$class_type !="Seven Week - First.",]
data <- data[data$class_type !="Seven Week - Second.",]
data <- data[data$class_days != "",]
data$class_numbers <- as.numeric(gsub("[^0-9]", "", data$class_numbers))
replace_loc <- function(x){
  if (grepl(" red",x)){
    return (c(gsub(" red","",x)))
  }
  if (grepl(" blue",x)){
    return (c(gsub(" blue","",x)))
  }
  if (grepl(" orange",x)){
    return (c(gsub(" orange","",x)))
  }
  if (grepl(" blue",x)){
    return (c(gsub(" blue","",x)))
  }
  if(grepl(" and Cornell Tech", x)){
    return (c(gsub(" and Cornell Tech","",x)))
  }
  if(grepl("Bill and Melinda Gates Hll",x)){
    return (c(x))
  }
  if(grepl("Noyes Community and Rec",x)){
    return (c(x))
  }
  if(grepl(" and ",x)){
    return (str_split(x," and "))
  }
  if(grepl(" or ",x)){
    return ((str_split(x," or ") %>% unlist())[1])
  }
  else{
    return (c(x))
  }
}
countrys_not_in_ithaca <- c("AAP in NYC", "Cornell Tech", "Geneva, NY", "Rome, Italy", "Washington, DC")
add_country <- function(x){
  v <- str_split(x," ") %>% unlist()
  y <- ""
  for( i in 1:(length(v)-1)){
    y <- paste(y,v[i])
  }
  y <- substr(y,2, str_length(y))
  ifelse(x == "Engineering in NYC", return ("NYC"),
  ifelse(grepl("Carpenter Hall", x), return ("Carpenter Hall, Ithaca, NY"),
  ifelse((!(x %in% countrys_not_in_ithaca) & !grepl("Bloomberg Center",x) & !grepl("Tata Innovation Center",x)),
    return (paste0(y, ", Ithaca, NY")),
    return (x))))
}

unique_loc <- data$class_locations %>% unique() %>% na.omit()

data$geocode_index <- as.vector(sapply(data$class_locations, function(x){

  for(i in 1:length(unique_loc)){
    if(is.na(x)){
      return (NA)
    }
    else if(x == unique_loc[i]){
      return (i)
    }
  }
}))
geocode_data <- data.frame(data$class_locations, data$geocode_index)
geocode_data <- lapply(geocode_data$data.class_locations,function(x){
  loc <- replace_loc(x)
})
geocodes <- lapply(unique_loc, function(x){
  out <- tryCatch(
    {
      l <- replace_loc(x)
      l <- lapply(l, add_country)
      return (as.vector(sapply(l, function(x){ggmap::geocode(x, output = "latlona",source = "google")})))
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", x))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", x))
      message("Here's the original warning message:")
      message(cond)
      if(grepl("OVER_QUERY_LIMIT", cond)){
        message("TRIAL 2")
        Sys.sleep(30)
        l <- replace_loc(x)
        l <- lapply(l, add_country)
        return (as.vector(sapply(l, function(x){ggmap::geocode(x, output = "latlona",source = "google")})))
      }
      else{
        return(NA)
      }
    },
    finally={
      message(paste("Processed URL:", x))
    }
  )
  return (out)
})
saveRDS(geocodes, "geocode_locs.Rda")
saveRDS(unique_loc, "unique_locs.Rda")
 id <- 1:nrow(data)
data$id <-id
saveRDS(data, file = "Preprocesses_course_time_data.Rda")