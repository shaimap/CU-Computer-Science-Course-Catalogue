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

#' Content-based filtering calculates item similarities based on course titles.
#' It breaks down terms in titles to calulate term-frequency. Measures the cosine
#' similarity and outputs the k-th nearest neighbors.



data <- readRDS(file = "Preprocesses_course_time_data.Rda")
data <- data %>% select(course_title_codes, course_titles, course_descriptions)
data <- data %>% unique()
saveRDS(data, "clean_data_rec.RDS")
# Clean the data
courses = data %>% 
  group_by(course_descriptions) %>% 
  select(course_descriptions) %>%
  unique() %>%
  as.matrix()

saveRDS(courses,"clean_courses_rec.RDS")


# Preprocess the data
ccourses <- courses %>%
  as_tibble() %>%
  mutate(id = 1:n()) %>%
  unnest_tokens(word, course_descriptions) %>%
  filter(!(word %in% stopwords("english"))) %>%
  mutate(word = replace_number(word, remove = T)) %>%
  mutate(word =  gsub("[[:punct:]]", " ", word)) %>%
  mutate(word = wordStem(word)) %>%
  mutate(word = replace_white(word)) %>%
  filter(word != "") %>%
  count(id, word)

course_dfm <- ccourses %>% cast_dfm(id, word, n)

# Calculate term frequency-inverse document frequency scores
course_dfm <- course_dfm %>% dfm_tfidf()

# Measure the cosine similarity
cos_dist <- course_dfm %>% textstat_simil(method = "cosine")
cos_dist <- 1 - cos_dist

# k-nearest neighbors
nn <- kNN(cos_dist, 5)
saveRDS(nn, file = "nearest_neighbor.RDS")
