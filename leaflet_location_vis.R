library(leaflet)
library(htmltools)
# library(osrm)
library(dplyr)
library(data.table)
# library(sp)
# library(cartography)


map_locs <- function(schedule, geocodes,unique_locs){
  locs <- as.vector(schedule %>% select(class_locations))
  locs_index <- c() 
  for(i in 1:nrow(locs)){
    loc <- locs[i,]
    locs_index[i] <- which(unique_locs %in% loc)
  }
  geo <- geocodes[locs_index]
  l <- list()
  for(j in 1:length(geo)){
    x <- geo[[j]]
    if(is.na(x)){
      l[[j]] <- NA
    }
    else{
    y <- data.frame(t(x))
    df <- c("lng", "lat","index")
    for(i in length(y$lon)){
      df <- rbind(df, data.frame(lng = as.numeric(y$lon[[i]]), lat = as.numeric(y$lat[[i]]), index = j))
    }
    df <- df[-1,]
    df <- setattr(df, "row.names", 1:nrow(df))
    l[[j]] <- df
    }
  }
  geo_df <- data.frame(do.call("rbind",l))
  geo_df <- na.omit(geo_df)
  geo_df$lng <- as.numeric(geo_df$lng)
  geo_df$lat <- as.numeric(geo_df$lat)
  geo_df$index <- as.numeric(geo_df$index)
  return (geo_df)
  
}


create_map <- function(schedule, geocodes,unique_locs){
  indices <- c()
  for(i in 1:nrow(schedule)){
    row <- schedule[i,]
    if(is.na(row$class_locations)){
      indices <- c(indices, i)
    }
  }
  if(length(indices)==0){
    schedule <- schedule
  } else {
    schedule <- schedule[-indices,]
  }
  df <- map_locs(schedule,geocodes, unique_locs)
  vec <- paste0("<center><b>",as.character(schedule$course_title_codes[df$index])," ",
                as.character(schedule$class_type[df$index]),"</b><br/></center><center>",
                as.character(schedule$class_location[df$index]),"</center><center>",
                as.character(schedule$class_days[df$index]), " ",
                as.character(schedule$class_times[df$index]), "</center>")
  locs <- data.frame(lng = df$lng, lat = df$lat, label = vec)
  mp <- leaflet(locs) %>% addTiles() %>%addMarkers(clusterOptions = markerClusterOptions(),~lng, ~lat, popup = ~vec)
  return (mp)
  
}


