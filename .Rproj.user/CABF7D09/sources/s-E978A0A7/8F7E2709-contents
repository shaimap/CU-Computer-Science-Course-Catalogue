library(dplyr)
library(keras)
complete_course_data <- readRDS(file = "final_webscraped_complete_course_data.Rda")
complete_course_data$course_credits<- gsub(" Credits\n","",complete_course_data$course_credits)      
complete_course_data$course_credits<- gsub(" Credit\n","",complete_course_data$course_credits)      
complete_course_data$course_credits<- gsub(" ","",complete_course_data$course_credits)
range_of_credits <- function(x){
  ifelse(grepl("-",x),NA,as.numeric(x))
}
complete_course_data$course_credits<- range_of_credits(complete_course_data$course_credits)

complete_course_data$technical_elective <- rep(NA, nrow(complete_course_data))
less_than_3000 <- function(x){
  match <- regmatches(x, gregexpr("[[:digit:]]+", x))%>% unlist()
  ifelse(match < 3000,0,NA)
}
complete_course_data$technical_elective <- less_than_3000(complete_course_data$course_title_codes)
indices_qualify <- which(complete_course_data$course_title_codes %in% c("AEM 4120","AEM 4210","BEE 4750","BIONB 3300",
                                                     "BTRY 3080","CEE 3040","CHEM 3570","CS 3152",
                                                     "CS 4999","EAS 3010","ECON 3030","ECON 3130",
                                                     "ECON 4020","ECE 3100","ENGRD 2700","LING 3333",
                                                      "MATH 4810","PHIL 4310","NBA 5550","ORIE 3120",
                                                     "ORIE 3150","ORIE 3300","ORIE 4800","PHIL 3310",
                                                     "PSYCH 3420"))
complete_course_data$technical_elective[indices_qualify] <- 1
indices_dont_qualify <- which(complete_course_data$course_title_codes %in% 
                                c("ART 3901", "INFO 3651","MAE 4610","NBA 5640","NCC 5500","ORIE 4990",
                                  "PSYCH 3800","AEM 3520","BEE 4890","CEE 3230","CEE 3610","ECON 4010",
                                  "HADM 4145","INFO 4120","MATH 4030","NCC 5560","NCC 5560","ORIE 4152",
                                  "PSYCH 3050","PSYCH 3250","GOV 3071","COMM 3020"))

complete_course_data$technical_elective[indices_dont_qualify] <- 0
other_indices_dont_qualify <- which(complete_course_data$course_credits < 3)
complete_course_data$technical_elective[other_indices_dont_qualify] <- 0
cee_indices_dont_qualify <- which(grepl("CE-",complete_course_data$course_distribution_categories))
complete_course_data$technical_elective[cee_indices_dont_qualify] <- 0
hist_indices_dont_qualify <- which(str_sub(complete_course_data$course_distribution_categories,1,4)=="HIST"|
                                     str_sub(complete_course_data$course_distribution_categories,1,4)=="ARTH")
complete_course_data$technical_elective[hist_indices_dont_qualify] <- 0


# We are not going to make this a network of prerequisite/ corequisite courses
MATH_1910 <- complete_course_data %>% filter(course_title_codes=="MATH 1910")
MATH_1920 <- complete_course_data %>% filter(course_title_codes=="MATH 1920")
CS_2800 <- complete_course_data %>% filter(course_title_codes=="CS 2800")
MATH_2940 <- complete_course_data %>% filter(course_title_codes=="MATH 2940")
CHEM_2090 <- complete_course_data %>% filter(course_title_codes=="CHEM 2090"|course_title_codes=="CHEM 2150")
CHEM_2080 <- complete_course_data %>% filter(course_title_codes %in% c("CHEM 2080","BTRY 3080", "ECON 3130", "MATH 2930", "MATH 4710", "PHYS 2214", 
                                               "PHYS 2218"))
PHYS_1112 <- complete_course_data %>% filter(course_title_codes %in% c("PHYS 1112","PHYS 1116"))
PHYS_2213<- complete_course_data %>% filter(course_title_codes %in% c("PHYS 2213","PHYS 2217"))
CS_1110 <- complete_course_data %>% filter(course_title_codes %in% c("CS 1110","CS 1112","CS 1114","CS 1115"))
ENGRI_courses <- complete_course_data %>% filter(grepl("ENGRI 1",course_title_codes))
ENGRD_courses <- complete_course_data %>% filter(grepl("ENGRD",course_title_codes))
FWS_courses <- complete_course_data %>% filter(grepl("FWS:",course_titles))
CA_liberal_studies_courses <- complete_course_data %>% filter(grepl("CA-",course_distribution_categories))
HA_liberal_studies_courses <- complete_course_data %>% filter(grepl("HA-",course_distribution_categories))
LA_liberal_studies_courses <- complete_course_data %>% filter(grepl("LA-",course_distribution_categories))
KCM_liberal_studies_courses <- complete_course_data %>% filter(grepl("KCM-",course_distribution_categories))
SBA_liberal_studies_courses <- complete_course_data %>% filter(grepl("SBA-",course_distribution_categories))
FL_liberal_studies_courses <- complete_course_data %>% filter(grepl("FL-",course_distribution_categories))
CE_liberal_studies_courses <- complete_course_data %>% filter(grepl("CE-",course_distribution_categories))

CS_3110 <- complete_course_data %>% filter(course_title_codes=="CS 3110")
CS_3410 <- complete_course_data %>% filter(course_title_codes %in% c("CS 3410","CS 3420","CS 3140"))
CS_4410 <- complete_course_data %>% filter(course_title_codes=="CS 4410")
CS_4820 <- complete_course_data %>% filter(course_title_codes=="CS 4820")
major_electives <- complete_course_data %>% filter(grepl("CS 4",course_title_codes)|grepl("CS 5",course_title_codes)|
                                                   grepl("CS 5",course_title_codes)|grepl("CS 6",course_title_codes)|
                                                   grepl("CS 7",course_title_codes))
major_electives <- major_electives %>% filter(!grepl("PLSCS",course_title_codes)|!grepl("VETCS",course_title_codes))
major_electives <- major_electives %>% filter(course_title_codes!="CS 4090"|course_title_codes!="CS 4998"|course_title_codes!="CS 4999")
major_electives_practicum <- major_electives %>% filter(grepl("Practicum",course_titles))

external_specializations<- complete_course_data %>% filter(!(course_title_codes %in% c("LING 4474","INFO 3300", "INFO 4300",
                                                                                     "INFO 4302", "INFO 5300")))
external_specializations<- external_specializations %>% filter(str_sub(external_specializations$course_title_codes,1,3)!="CS ")
external_specializations<- external_specializations %>% filter(!grepl("Independent Study",course_titles))
greater_3000 <- function(x){
match <- regmatches(x, gregexpr("[[:digit:]]+", x))%>% unlist()
return (match >3000)
}
external_specializations<- external_specializations %>% filter(greater_3000(course_title_codes))
external_specializations<- external_specializations %>% filter(course_credits>=3)

#use machine learning to predict technical electives
library("kernlab") 
library("caret") 
library("tm") 
library("dplyr") 
library("splitstackshape")
library("e1071")

codes <-  function(x){
  match <- regmatches(x, gregexpr("[[:digit:]]+", x))%>% unlist()
  return (as.numeric(match))
}
subjects <-  function(x){
  return (str_sub(x,1, str_length(x)-5))
}
#split the data into na and non na data
non_na_data <- complete_course_data[!is.na(complete_course_data$technical_elective),]
predictive_data <- complete_course_data[is.na(complete_course_data$technical_elective),]
# select columns of interest
names(modelling_data)
set.seed(100)
modelling_data <- non_na_data %>% filter(codes(non_na_data$course_title_codes)>3000)
#modelling_data <- modelling_data %>% filter((is.na(modelling_data$course_credits))|(modelling_data$course_credits)>=3 )
#modelling_data <- modelling_data %>% select(-c("course_offerings","course_permissions","course_forbidden_overlaps","course_credits"))
names(modelling_data)[which(names(modelling_data)=="course_title_codes")] <- "doc_id"
names(modelling_data)[which(names(modelling_data)=="course_descriptions")] <- "text"
in_training <- createDataPartition(modelling_data$technical_elective, p = .80,
                                   list = F, times = 1)

train_all <- modelling_data[in_training, ]
test_all <- modelling_data[-in_training, ]

train <- VCorpus(DataframeSource(train_all %>% select(-c("technical_elective"))))
train <- tm_map(train, content_transformer(stripWhitespace))
train <- tm_map(train, content_transformer(tolower))
train <- tm_map(train, content_transformer(removeNumbers))
train <- tm_map(train, content_transformer(removePunctuation))
train.dtm <- as.matrix(DocumentTermMatrix(train, control=list(wordLengths=c(1,Inf))))

test <- VCorpus(DataframeSource(test_all %>% select(-c("technical_elective"))))
test <- tm_map(test, content_transformer(stripWhitespace))
test <- tm_map(test, content_transformer(tolower))
test <- tm_map(test, content_transformer(removeNumbers))
test <- tm_map(test, content_transformer(removePunctuation))
test.dtm <- as.matrix(DocumentTermMatrix(test, control=list(wordLengths=c(1,Inf))))
train.df <- data.frame(train.dtm[,intersect(colnames(train.dtm), colnames(test.dtm))])
test.df <- data.frame(test.dtm[,intersect(colnames(test.dtm), colnames(train.dtm))])
train.df$corpus <- as.factor(train_all$technical_elective)
test.df$corpus <- as.factor(test_all$technical_elective)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

df.model<-ksvm(corpus~., trControl = trctrl, data= train.df, kernel="rbfdot")
df.pred<-predict(df.model, test.df)
con.matrix<-confusionMatrix(df.pred, test.df$corpus)

# achieved a model with 98% accuracy, yay!
# now lets apply model results to data

predicted_features <- predictive_data%>% select(-c("course_offerings","course_permissions","course_forbidden_overlaps","course_credits"))
names(predicted_features)[which(names(predicted_features)=="course_title_codes")] <- "doc_id"
names(predicted_features)[which(names(predicted_features)=="course_descriptions")] <- "text"
predict <- VCorpus(DataframeSource(predicted_features %>% select(-c("technical_elective"))))
predict <- tm_map(predict, content_transformer(stripWhitespace))
predict <- tm_map(predict, content_transformer(tolower))
predict <- tm_map(predict, content_transformer(removeNumbers))
predict <- tm_map(predict, content_transformer(removePunctuation))
predict <- as.matrix(DocumentTermMatrix(predict, control=list(wordLengths=c(1,Inf))))
predict.df <- data.frame(predict[,intersect(colnames(predict), colnames(predict))])
df.pred<-predict(df.model, predict.df)
results <- as.data.frame(df.pred)
rownames(results) <- rownames(predict.df)
df_predicted <- data.frame(technical_elective = as.numeric(as.character(results$df.pred)), course_title_codes = as.character(rownames(results)),stringsAsFactors = FALSE)

# join the data
predictive_data <- predictive_data %>% select(-c("technical_elective"))
predictive_data <- predictive_data %>% full_join(df_predicted,by = "course_title_codes")
original_data <- rbind(predictive_data,non_na_data)

technical_electives<- original_data %>% filter(technical_elective==1)

