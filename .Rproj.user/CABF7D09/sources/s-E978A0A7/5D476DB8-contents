library(rvest)
library(stringr)
library(dplyr)
extract_course_codes <- readRDS(file="extracted_courses.Rda")
extract_course_codes
# scraping computer science data from webpage

# gunna have to scrape all math courses
fall_math <- 'https://classes.cornell.edu/browse/roster/FA18/subject/MATH'
spring_math <- 'https://classes.cornell.edu/browse/roster/SP18/subject/MATH'
summer_math <- 'https://classes.cornell.edu/browse/roster/SU18/subject/MATH'

webpage_fall_2018 <- read_html(fall_math)
course_title_codes_fall_2018 <- as.vector(html_nodes(webpage_fall_2018, 'div.title-subjectcode') %>% html_text())
course_titles_fall_2018 <- as.vector(html_nodes(webpage_fall_2018, 'div.title-coursedescr') %>% html_text())

webpage_spring_2018 <- read_html(spring_math)
course_title_codes_spring_2018 <- as.vector(html_nodes(webpage_spring_2018, 'div.title-subjectcode') %>% html_text())
course_titles_spring_2018 <- as.vector(html_nodes(webpage_spring_2018, 'div.title-coursedescr') %>% html_text())


webpage_summer_2018 <- read_html(summer_math)
course_title_codes_summer_2018 <- as.vector(html_nodes(webpage_summer_2018, 'div.title-subjectcode') %>% html_text())
course_titles_summer_2018 <- as.vector(html_nodes(webpage_summer_2018, 'div.title-coursedescr') %>% html_text())


# attain urls of individual math pages
codes_fall_2018 <- substr(course_title_codes_fall_2018, 6, str_length(course_title_codes_fall_2018))
course_page_urls_fall_2018 <- as.vector(sapply(codes_fall_2018,function(x){
  paste('https://classes.cornell.edu/browse/roster/FA18/class/MATH/',x, sep = "")
}))

codes_spring_2018 <- substr(course_title_codes_spring_2018, 6, str_length(course_title_codes_spring_2018))
course_page_urls_spring_2018 <- as.vector(sapply(codes_spring_2018,function(x){
  paste('https://classes.cornell.edu/browse/roster/SP18/class/MATH/',x, sep = "")
}))

codes_summer_2018 <- substr(course_title_codes_summer_2018, 6, str_length(course_title_codes_summer_2018))
course_page_urls_summer_2018 <- as.vector(sapply(codes_summer_2018,function(x){
  paste('https://classes.cornell.edu/browse/roster/SU18/class/MATH/',x, sep = "")
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
math_course_data <- data.frame(course_title_codes, course_titles, course_descriptions, course_offerings, course_permissions, 
                            course_prereq_coreq, course_forbidden_overlaps, course_distribution_categories)
raw_data_fall<- math_course_data[1:61,]
raw_data_spring <- math_course_data[62:125,]
raw_data_summer <- math_course_data[126:133,]
diff_spring <- setdiff(raw_data_spring$course_title_codes, raw_data_fall$course_title_codes)
raw_data_spring <- raw_data_spring[raw_data_spring$course_title_codes %in% diff_spring, ]
diff_summer <- setdiff(raw_data_summer$course_title_codes, raw_data_fall$course_title_codes) %>% setdiff(raw_data_spring$course_title_codes)
raw_data_summer <- raw_data_summer[raw_data_summer$course_title_codes %in% diff_summer, ]
# bind the data back together
math_course_data <- rbind(raw_data_fall, raw_data_spring, raw_data_summer)
# extract all math courses with course numbers greater than 3000
greater_3000 <- function(x){
  code <- as.numeric(substr(x,6, str_length(x)))
  if(code >= 3000){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}

codes_greater_3000 <- filter(math_course_data, as.numeric(substr(course_title_codes,6, str_length(course_title_codes))) >= 3000)
saveRDS(codes_greater_3000, file = "math_course_codes_greater_3000.Rda")

math_courses <- extract_course_codes[grepl("MATH", extract_course_codes)]
extract_course_codes <- extract_course_codes[!grepl("MATH", extract_course_codes)]
math_courses <- math_courses[math_courses!="MATH 3XXX"]
other_math_courses <- math_course_data[math_course_data$course_title_codes %in% math_courses,]
remaining_urls <- c("https://classes.cornell.edu/browse/roster/FA18/class/PHIL/4310",
                   "https://classes.cornell.edu/browse/roster/SP18/class/ECE/3140",
                   "https://classes.cornell.edu/browse/roster/FA18/class/BTRY/3010",
                   "https://classes.cornell.edu/browse/roster/FA18/class/BTRY/3080",
                   "https://classes.cornell.edu/browse/roster/FA18/class/ECON/3130",
                   "https://classes.cornell.edu/browse/roster/FA18/class/ENGRD/2700",
                   "https://classes.cornell.edu/browse/roster/FA18/class/PHYS/3316",
                   "https://classes.cornell.edu/browse/roster/SP18/class/AEP/3620",
                   "https://classes.cornell.edu/browse/roster/FA18/class/ECE/4750",
                   "https://classes.cornell.edu/browse/roster/FA18/class/INFO/2450",
                   "https://classes.cornell.edu/browse/roster/FA18/class/ENGRD/2300",
                   "https://classes.cornell.edu/browse/roster/FA18/class/LING/1101",
                   "https://classes.cornell.edu/browse/roster/FA18/class/INFO/2040",
                   "https://classes.cornell.edu/browse/roster/FA18/class/ORIE/5750",
                   "https://classes.cornell.edu/browse/roster/SP18/class/INFO/3152",
                   "https://classes.cornell.edu/browse/roster/SP18/class/ENGRC/3152")
course_title_codes <- as.vector(sapply(remaining_urls, function(x){
  read_html(x) %>% 
    html_node('div.title-subjectcode') %>% 
    html_text()
}))
course_titles <- as.vector(sapply(remaining_urls, function(x){
  read_html(x) %>% 
    html_node('div.title-coursedescr') %>% 
    html_text()
}))
course_descriptions<- as.vector(sapply(remaining_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p.catalog-descr') %>% 
    html_text()
}))
course_offerings<- as.vector(sapply(remaining_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(7) > span') %>% 
    html_text()
}))

course_permissions<- as.vector(sapply(remaining_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(8) > span.catalog-permiss') %>% 
    html_text()
}))

course_prereq_coreq <- as.vector(sapply(remaining_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(8) > span.catalog-prereq') %>% 
    html_text()
}))

course_forbidden_overlaps <- as.vector(sapply(remaining_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(8) > span.catalog-forbid') %>% 
    html_text()
}))

course_distribution_categories <- as.vector(sapply(remaining_urls, function(x){
  read_html(x) %>% 
    html_node('#search-refresh > div.class-listing > div:nth-child(3) > p:nth-child(9) > span') %>% 
    html_text()
}))
other_courses_data <- data.frame(course_title_codes, course_titles, course_descriptions, course_offerings, course_permissions, 
                               course_prereq_coreq, course_forbidden_overlaps, course_distribution_categories)
external_courses <- rbind(other_math_courses,other_courses_data)
saveRDS(external_courses, file = "webscraped_external_courses.Rda")
