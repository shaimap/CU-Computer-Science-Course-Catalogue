library(rvest, dplyr)
url <- "http://www.cureviews.org/course/CS/4780"
html_code <- read_html(url)
rating <- html_code %>% html_node('<text x="80" y="87.27272727272727" text-anchor="middle" style="text-anchor: middle; fill: rgb(83, 178, 119); stroke: none; font-style: normal; font-variant: normal; font-weight: normal; font-stretch: normal; line-height: normal; fill-opacity: 1; font-size: 32px;">3.9</text>') %>% html_text()
rating <- html_code %>% html_node('div#text') %>% html_text()
rating %>%
  html_nodes("div") %>% 
  html_text()
#should change the title to hardcoding cureviews

courses <- rbind(c("CS 1110", 4.9, 2.8), 
             c("CS 1112", 4.0, 3.3),
             c("CS 1133", 2.0, 1.0),
             c("CS 1142", 4.0, 3.3),
             c("CS 1300", 4.2, 2.2),
             c("CS 1620", 3.0, 2.0),
             c("CS 1710", 2.2, 3.0),
             c("CS 1998", 3.0, 3.0),
             c("CS 2024", 4.0, 3.0),
             c("CS 2110", 3.9, 3.0),
             c("CS 2112", 4.4, 4.4),
             c("CS 2300", 3.5, 3.3),
             c("CS 2800", 2.3, 3.9),
             c("CS 2850", 3.9, 2.1),
             c("CS 3110", 3.6, 4.4),
             c("CS 3152", 5.0, 4.0),
             c("CS 3300", 3.8, 3.3),
             c("CS 3410", 3.4, 4.3),
             c("CS 3420", 3.3, 2.7),
             c("CS 4120", 5.0, 5.0),
             c("CS 4220", 4.0, 5.0),
             c("CS 4320", 3.4, 3.2),
             c("CS 4321", 3.0, 4.0),
             c("CS 4410", 1.3, 4.3),
             c("CS 4670", 5.0, 3.0),
             c("CS 4700", 1.5, 2.8),
             c("CS 4740", 2.3, 3.0),
             c("CS 4744", 2.0, 5.0),
             c("CS 4754", 1.5, 2.8),
             c("CS 4775", 5.0, 3.0),
             c("CS 4780", 5.0, 3.0),
             c("CS 4786", 1.7, 2.7),
             c("CS 4820", 4.7, 4.5),
             c("CS 4860", 4.0, 4.0),
             c("CS 5120",5.0, 5.0),
             c("CS 5320",3.4, 3.2))
courses <- data.frame(courses)
names(courses) <- c("course_title_codes", "quality rating", "difficulty rating")
courses$course_title_codes <- as.character(courses$course_title_codes)
