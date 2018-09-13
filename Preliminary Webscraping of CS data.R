library(rvest)
library(stringr)
# scraping computer science data from webpage

fall_2018_url <- 'https://classes.cornell.edu/browse/roster/FA18/subject/CS'
spring_2018_url <- 'https://classes.cornell.edu/browse/roster/SP18/subject/CS'
summer_2018_url <- 'https://classes.cornell.edu/browse/roster/SU18/subject/CS'
# Read HTML code from website
url <- read_html('https://classes.cornell.edu/browse/roster/FA18/subject/CS')
locations <- as.vector(html_nodes(url,'a.facility-search') %>% html_text())
class_number <- html_nodes(url,'strong.tooltip-iws')%>% html_text()
days_offered <- html_nodes(url, 'span.pattern-only')%>% html_text()
times_offered <- html_nodes(url, 'time.time')%>% html_text()
course_code <- html_nodes(url, 'div.title-subjectcode') %>% html_text()

course_info <- data.frame(course_title_codes = rep(course_code, n = 22),locations, days_offered,times_offered)
webpage_fall_2018 <- read_html(fall_2018_url)
course_title_codes_fall_2018 <- as.vector(html_nodes(webpage_fall_2018, 'div.title-subjectcode') %>% html_text())
course_titles_fall_2018 <- as.vector(html_nodes(webpage_fall_2018, 'div.title-coursedescr') %>% html_text())

webpage_spring_2018 <- read_html(spring_2018_url)
course_title_codes_spring_2018 <- as.vector(html_nodes(webpage_spring_2018, 'div.title-subjectcode') %>% html_text())
course_titles_spring_2018 <- as.vector(html_nodes(webpage_spring_2018, 'div.title-coursedescr') %>% html_text())


webpage_summer_2018 <- read_html(summer_2018_url)
course_title_codes_summer_2018 <- as.vector(html_nodes(webpage_summer_2018, 'div.title-subjectcode') %>% html_text())
course_titles_summer_2018 <- as.vector(html_nodes(webpage_summer_2018, 'div.title-coursedescr') %>% html_text())


# attain urls of individual cs pages
codes_fall_2018 <- substr(course_title_codes_fall_2018, 4, str_length(course_title_codes_fall_2018))
course_page_urls_fall_2018 <- as.vector(sapply(codes_fall_2018,function(x){
  paste('https://classes.cornell.edu/browse/roster/FA18/class/CS/',x, sep = "")
}))

codes_spring_2018 <- substr(course_title_codes_spring_2018, 4, str_length(course_title_codes_spring_2018))
course_page_urls_spring_2018 <- as.vector(sapply(codes_spring_2018,function(x){
  paste('https://classes.cornell.edu/browse/roster/SP18/class/CS/',x, sep = "")
}))

codes_summer_2018 <- substr(course_title_codes_summer_2018, 4, str_length(course_title_codes_summer_2018))
course_page_urls_summer_2018 <- as.vector(sapply(codes_summer_2018,function(x){
  paste('https://classes.cornell.edu/browse/roster/SU18/class/CS/',x, sep = "")
}))

course_title_codes <- as.vector(c(course_title_codes_fall_2018, course_title_codes_spring_2018, course_title_codes_summer_2018))
course_titles <- as.vector(c(course_titles_fall_2018, course_titles_spring_2018, course_titles_summer_2018))
# bind all urls and codes into 1 single dataframe
course_page_urls <- c(course_page_urls_fall_2018, course_page_urls_spring_2018, course_page_urls_summer_2018)
# attain various information from various cs pages
course_descriptions<- as.vector(sapply(course_page_urls, function(x){
  read_html(x) %>% 
  html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
  html_text()
}))
course_locations<-as.vector(sapply(course_page_urls, function(x){
  read_html(x) %>% 
  html_node('#class-subject-listing > div.class-listing > div:nth-child(3)') %>%
  html_text()
}))
course_offerings<- as.vector(sapply(course_page_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(7) > span') %>% 
    html_text()
}))

course_permissions<- as.vector(sapply(course_page_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(8) > span.catalog-permiss') %>% 
    html_text()
}))

course_prereq_coreq <- as.vector(sapply(course_page_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(8) > span.catalog-prereq') %>% 
    html_text()
}))

course_forbidden_overlaps <- as.vector(sapply(course_page_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(8) > span.catalog-forbid') %>% 
    html_text()
}))

course_distribution_categories <- as.vector(sapply(course_page_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
    html_text()
}))

# combine all attributes into 1 dataframe

cs_course_data <- data.frame(course_title_codes, course_titles, course_descriptions, course_offerings, course_permissions, 
                             course_prereq_coreq, course_forbidden_overlaps, course_distribution_categories)
saveRDS(cs_course_data, file = "cs_course_data.Rda")