library(rvest)
library(stringr)
library(RCurl)
library(XML)
library(dplyr)
library(httr)
library(curl)
library(xml2)
library(purrr)


url1 <- read_html("https://classes.cornell.edu/browse/roster/FA18")
url2 <- read_html("https://classes.cornell.edu/browse/roster/SP18")
url3 <- read_html("https://classes.cornell.edu/browse/roster/SU18")
doc1 <- htmlParse(url1)
links1 <- xpathSApply(doc1, "//a/@href")
links1 <- na.omit(unique(as.vector(links1[grepl("/browse/roster/FA18/subject",links1)])))
doc2 <- htmlParse(url2)
links2 <- xpathSApply(doc2, "//a/@href")
links2 <- na.omit(unique(as.vector(links2[grepl("/browse/roster/SP18/subject",links2)])))
doc3 <- htmlParse(url3)
links3 <- xpathSApply(doc3, "//a/@href")
links3 <- na.omit(unique(as.vector(links3[grepl("/browse/roster/SU18/subject",links3)])))
subject_codes1<- url1 %>% html_nodes('li.browse-subjectcode') %>% html_text()
subject_codes2<- url2 %>% html_nodes('li.browse-subjectcode') %>% html_text()
subject_codes3<- url3 %>% html_nodes('li.browse-subjectcode') %>% html_text()
subject_codes <- c(subject_codes1,subject_codes2,subject_codes3)
links1 <- as.vector(sapply(links1, function(x){
  paste("https://classes.cornell.edu",x,sep="")
}))
links2 <- as.vector(sapply(links2, function(x){
  paste("https://classes.cornell.edu",x,sep="")
}))
links3 <- as.vector(sapply(links3, function(x){
  paste("https://classes.cornell.edu",x,sep="")
}))
links <- c(links1,links2,links3)

html_pages1 <- lapply(links1, function(x){
  read_html(x)
})
html_pages2 <- lapply(links2, function(x){
  read_html(x)
})
html_pages3 <- lapply(links3, function(x){
  read_html(x)
})
html_pages <- c(html_pages1,html_pages2,html_pages3)
course_title_codes1 <- as.vector(sapply(html_pages1, function(x){
  x %>%
  html_nodes('div.title-subjectcode') %>% 
  html_text()
}))
course_title_codes1 <- Reduce(c,course_title_codes1)
course_title_codes2 <- as.vector(sapply(html_pages2, function(x){
  x %>%
    html_nodes('div.title-subjectcode') %>% 
    html_text()
})) 
course_title_codes2 <- Reduce(c,course_title_codes2)

course_title_codes3 <- as.vector(sapply(html_pages3, function(x){
  x %>%
    html_nodes('div.title-subjectcode') %>% 
    html_text()
}))
course_title_codes3 <- Reduce(c,course_title_codes3)
course_title_codes <- c(course_title_codes1,course_title_codes2,course_title_codes3)

course_titles <- as.vector(sapply(html_pages, function(x){
    x %>%
    html_nodes('div.title-coursedescr') %>% 
    html_text()
}))

course_titles <- Reduce(c,course_titles)

course_urls1 <- lapply(subject_codes1, function(x){
  codes <- course_title_codes1[str_sub(course_title_codes1,1,(str_length(x)+1))==paste(x," ", sep = "")]
  matches <- regmatches(codes, gregexpr("[[:digit:]]+", codes))%>% unlist()
  urls <- as.vector(sapply(matches, function(y){
    return (paste("https://classes.cornell.edu/browse/roster/FA18/class/", x,"/",y, sep = ""))
  }))
  return (urls)
})
course_urls1 <- Reduce(c,course_urls1)
course_urls2 <- lapply(subject_codes2, function(x){
  codes <- course_title_codes2[str_sub(course_title_codes2,1,(str_length(x)+1))==paste(x," ", sep = "")]
  matches <- regmatches(codes, gregexpr("[[:digit:]]+", codes))%>% unlist()
  urls <- as.vector(sapply(matches, function(y){
    return (paste("https://classes.cornell.edu/browse/roster/SP18/class/", x,"/",y, sep = ""))
  }))
  return (urls)
})
course_urls2 <- Reduce(c,course_urls2)
course_urls3 <- lapply(subject_codes3, function(x){
  codes <- course_title_codes3[str_sub(course_title_codes1,3,(str_length(x)+1))==paste(x," ", sep = "")]
  matches <- regmatches(codes, gregexpr("[[:digit:]]+", codes))%>% unlist()
  urls <- as.vector(sapply(matches, function(y){
    return (paste("https://classes.cornell.edu/browse/roster/SU18/class/", x,"/",y, sep = ""))
  }))
  return (urls)
})

course_urls3 <- Reduce(c,course_urls3)
course_urls <- c(course_urls1,course_urls2,course_urls3)

# html_course_pages <- lapply(course_urls, function(x){
#   out <- tryCatch(
#     {
#         html_code <- read_html(x)
#         course_title_codes <- html_code %>% 
#           html_node('div.title-subjectcode') %>% 
#           html_text()
#         course_titles <- html_code %>% 
#           html_node('div.title-coursedescr') %>% 
#           html_text()
#         course_descriptions <- html_code %>% 
#           html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
#           html_text()
#         course_permissions <- html_code %>% 
#           html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(8) > span.catalog-permiss') %>% 
#           html_text()
#         course_prereq_coreq <- html_code %>% 
#           html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(8) > span.catalog-prereq') %>% 
#           html_text()
#         course_forbidden_overlaps <- html_code %>% 
#           html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(8) > span.catalog-forbid') %>% 
#           html_text()
#         course_distribution_categories <- html_code %>% 
#           html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
#           html_text()
#         course_credits <- html_code %>% 
#           html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
#           html_text()
#         course_offerings <-  html_code %>% 
#           html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(7) > span') %>%
#           html_text()
#         course_data <- data.frame(course_title_codes, course_titles, course_descriptions, course_offerings, course_permissions, 
#                                   course_prereq_coreq, course_forbidden_overlaps, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
#         return(course_data)
#     },
#     error=function(cond) {
#       message(paste("URL does not seem to exist:", x))
#       message("Here's the original error message:")
#       message(cond)
#       # Choose a return value in case of error
#       return(NA)
#     },
#     warning=function(cond) {
#       message(paste("URL caused a warning:", x))
#       message("Here's the original warning message:")
#       message(cond)
#       # Choose a return value in case of warning
#       return(NA)
#     },
#     finally={
#       message(paste("Processed URL:", x))
#       message("Some other message at the end")
#     }
#   )
#   return (out)
# })
# 
# saveRDS(html_course_pages,file="course_data.Rda")
# data <- readRDS(file="course_data.Rda")
# data <- do.call("rbind",data)
# data <- data[rowSums(is.na(data)) != ncol(data), ]
# # gotta make this data unique
# data <- unique(data)
# data <- data[!duplicated(data[,c('course_title_codes')]),]
# saveRDS(data, file = "final_webscraped_complete_course_data.Rda")

html_course_time_data <- lapply(course_urls1, function(x){
  out <- tryCatch(
    {
      html_code <- read_html(x)
      course_title_codes <- html_code %>%
        html_nodes('div.title-subjectcode') %>%
        html_text()
      course_titles <- html_code %>%
        html_nodes('div.title-coursedescr') %>%
        html_text()
      class_numbers <-html_code %>% 
        html_nodes('strong.tooltip-iws') %>%
        html_text()
      class_days <- html_code %>%
        html_nodes('span.pattern-only')%>%
        html_text()
      class_times <- html_code %>% 
        html_nodes("time.time") %>% 
        html_text()
      class_locations <- html_code %>% 
        html_nodes("a.facility-search") %>% 
        html_text()
      instructors <-html_code %>% 
        html_nodes('li.instructors') %>%
        html_text()
      class_type <- html_code %>% 
        html_nodes('em.tooltip-iws') %>%
        html_text()
      class_type <- class_type[!grepl("Week",class_type)]
      course_descriptions <- html_code %>% 
        html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
        html_text()
      course_distribution_categories <- html_code %>% 
        html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
        html_text()
      course_credits <- html_code %>% 
        html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
        html_text()
      if(length(class_times) == 0){
        return(NA)
      }
      else if(length(class_locations) ==0){
        course_data <- data.frame(class_numbers,class_days,class_times, instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
        course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)), 
                             course_titles = rep(course_titles,nrow(course_data)),
                             class_locations = rep(NA, nrow(course_data)), course_data)
        return(course_data)
        }
      else{
        course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
        course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                             course_titles = rep(course_titles,nrow(course_data)),
                             course_data)
        return(course_data)
        }
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
      # Choose a return value in case of warning
      return(NA)
    },
    finally={
      message(paste("Processed URL:", x))
    }
  )
  return (out)
})

saveRDS(html_course_time_data,file="course_time_data.Rda")
course_time_data <- readRDS(file="course_time_data.Rda")
course_time_data <- do.call("rbind",course_time_data)
course_time_data <- course_time_data[rowSums(is.na(course_time_data)) != ncol(course_time_data), ]
# gotta make this data unique
course_time_data <- unique(course_time_data)
# other data
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/3102")
course_title_codes <- "ARCH 3102"
course_titles <- "Design VI"
class_numbers <-c(4834)
class_days <- c("MWF")
class_times <- c("12:20pm - 4:25pm")
class_locations <- c(NA)
instructors <-c("InstructorsStaff")
class_type <- c("STU")
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/3308")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-c(1,8)]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
locations <- c(class_locations[1:2],NA, class_locations[3:5], NA, class_locations[6:7])
class_locations <- class_locations
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[-c(8)]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-c(1,8)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/4101")
course_title_codes <- "ARCH 4101"
course_titles <- "Design VII"
class_numbers <-c(4835,18337)
class_days <- c("MWF","MWF")
class_times <- c("12:20pm - 4:25pm","12:20pm - 4:25pm")
class_locations <- c(NA,NA)
instructors <-c("InstructorsStaff","InstructorsZissovici, J")
class_type <- c("STU","STU")
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/4102")
course_title_codes <- "ARCH 4102"
course_titles <- "Design VIII"
class_numbers <-c(4835,18337)
class_days <- c("MWF","MWF")
class_times <- c("12:20pm - 4:25pm","12:20pm - 4:25pm")
class_locations <- c(NA,NA)
instructors <-c("InstructorsStaff","InstructorsZissovici, J")
class_type <- c("STU","STU")
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/4408")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-c(1)]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-c(1)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/4509")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-c(1,7)]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- class_locations[-c(6)]
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[-c(1,7)]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-c(1,7)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/5101")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-c(3)]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_days <- class_days[-c(3,4)]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_times <- class_times[-c(3,4)]
class_locations <- c(NA,NA)
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[-c(3,4)]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-c(3)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/6308")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-c(1)]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1:2], NA, class_locations[3:5])
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[-c(1)]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-c(1)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/6408")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-c(1)]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-c(1)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/6509")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-c(1)]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[c(-1)]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-c(1)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ARCH/6605")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-c(1)]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations[4]<- NA
class_locations[5]<- "Sibley Hall 142"
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[c(-1)]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-c(1)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/BIOG/1250")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations[3:4] <- NA
class_locations[5:6] <-c("Balch Hall - Tatkon Ctr 3343","Plant Science Building 143")
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/BIOMI/4850")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations[3] <- NA
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/BIONB/2210")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
locations <- class_locations[1:5]
locations[6] <- NA
locations[7:13] <- class_locations[6:12]
locations[14] <- NA
class_locations <- locations
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/BIONB/3300")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-3]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-3]
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/BME/3300")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-3]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-3]
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/COGST/3300")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-3]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-3]
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/PSYCH/3300")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-3]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-3]
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CEE/3090")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-c("12246", "12586")
class_days <- c("TR","F")
class_times <- c("11:40am - 12:55pm","9:05am - 9:55am")
class_locations <- c("Hollister Hall 162","Hollister Hall 312")
instructors <-c("Samaranayake, S", "Hover, K")
class_type <- c("IND", "IND")
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/COMM/6310")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],sep = " or "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CRP/3850")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-5]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[-5]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-5]
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CS/1300")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CS/1300")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CS/1998")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-2]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[-2]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-2]
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CS/6410")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CS/6820")
course_title_codes <- "CS 6820"
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-c("11771", "17776")
class_days <- c("MWF","MWF")
class_days <- class_days[class_days != ""]
class_times <- c("2:30pm - 3:20pm","2:30pm - 3:20pm")
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],class_locations[4], sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[1:2]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CS/6830")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)

html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CS/7792")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[-3]
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_times <- class_times[-3]
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],class_locations[4],sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[-3]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CS/7794")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type, course_descriptions, course_distribution_categories, course_credits,stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/CS/7893")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ECE/4320")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations[3] <- NA
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/FDSC/4940")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-3]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_days <- class_days[-3]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- rep(NA,4)
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[-3]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
class_type <- class_type[-3]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/HIST/1650")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations,NA)
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/INFO/1300")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/INFO/6310")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/LAW/6764")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/MAE/4320")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations[3] <- NA
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/MAE/4780")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-3]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations[3] <- NA
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-3]
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/MAE/5780")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[1:8]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[1:8]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[-c(9,10)]

class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/MGMT/5010")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[1]
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_times <- class_times[1]
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- paste(class_locations[1],class_locations[2], sep = " and ")
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()

class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/MGMT/5010")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[1]
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_times <- class_times[1]
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- paste(class_locations[1],class_locations[2], sep = " and ")
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()

class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/MGMT/5245")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[1]
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_times <- class_times[1]
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- paste(class_locations[1],class_locations[2], sep = " and ")
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[1]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()

class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                      course_titles = rep(course_titles,nrow(course_data)),
                      course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/MSE/3030")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <-c(class_numbers,class_numbers[2])
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors[3] <- instructors[2]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- c(class_type,class_type[2])
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()

class_type <- class_type[!grepl("Week",class_type)]
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/MSE/5830")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <-c(class_numbers,class_numbers[2])
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors[3] <- instructors[2]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- c(class_type,class_type[2])

class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/NBA/6960")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1],paste(class_locations[2],class_locations[3],sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()

class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/NCC/5050")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[3:5]
class_days <- c("W","W","W")
class_days <- class_days[class_days != ""]
class_times <- c("8:40am - 11:25am","11:55am - 2:40pm","11:55am - 2:40pm")
class_locations <- rep("Breazzano Family Ctr 223",3)
instructors <-c("Hildreth, A", "Mish, R", "Hildreth, A")
class_type <- rep("LEC",3)

class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/NS/1160")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1:4],NA,class_locations[5:8])
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/NTRES/6940")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- c("F","MTWRF")
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_times <- class_times[1:2]
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1],paste(class_locations[2],class_locations[3], sep = " and "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[1:2]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/ORIE/6350")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations <- c(class_locations[1], paste(class_locations[2],class_locations[3],sep = " or "))
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/SYSEN/5100")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_numbers <- class_numbers[-4]
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days!=""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
class_locations[4]<-NA
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
instructors <- instructors[1:4]
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- c(class_type[1:3], class_type[5])
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/INFO/1300")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- html_code %>%
  html_nodes('span.pattern-only')%>%
  html_text()
class_days <- class_days[class_days != ""]
class_times <- html_code %>% 
  html_nodes("time.time") %>% 
  html_text()
class_locations <- html_code %>% 
  html_nodes("a.facility-search") %>% 
  html_text()
instructors <-html_code %>% 
  html_nodes('li.instructors') %>%
  html_text()
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/TECH/5000")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- c("R","R","R")
class_days <- class_days[class_days != ""]
class_times <- c("1:55pm - 3:10pm","3:20pm - 4:35pm","4:45pm - 6:00pm")
class_locations <- rep(paste("Bloomberg Center 161","Cornell Tech", sep = " and "),3)
instructors <-rep("Khaire, M",3)
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
html_code <- read_html("https://classes.cornell.edu/browse/roster/FA18/class/TECH/5100")
course_title_codes <- html_code %>%
  html_nodes('div.title-subjectcode') %>%
  html_text()
course_titles <- html_code %>%
  html_nodes('div.title-coursedescr') %>%
  html_text()
class_numbers <-html_code %>% 
  html_nodes('strong.tooltip-iws') %>%
  html_text()
class_days <- c("R","R","R")
class_days <- class_days[class_days != ""]
class_times <- c("1:55pm - 3:10pm","3:20pm - 4:35pm","4:45pm - 6:00pm")
class_locations <- rep(paste("Tata Innovation Center 141","Cornell Tech", sep = " and "),3)
instructors <-c("Pass, G","Inamoto, R","Inamoto, R")
class_type <- html_code %>% 
  html_nodes('em.tooltip-iws') %>%
  html_text()
class_type <- class_type[!grepl("Week",class_type)]
course_descriptions <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
course_distribution_categories <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
  html_text()
course_credits <- html_code %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > div.sections > div.group.heavy-left > ul.enroll-header > li.credit-info > p > span.credits') %>% 
  html_text()
course_data <- data.frame(class_numbers,class_days,class_times,class_locations,instructors,class_type,course_descriptions, course_distribution_categories, course_credits, stringsAsFactors = FALSE)
course_data <- cbind(course_title_codes = rep(course_title_codes,nrow(course_data)),
                     course_titles = rep(course_titles,nrow(course_data)),
                     course_data)
course_time_data <- rbind(course_data,course_time_data)
saveRDS(course_time_data, file = "course_time_data.Rda")