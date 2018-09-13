library(stringr)
raw_data <- readRDS(file="cs_course_data.Rda")

# separate out the spring and summer data to eliminate duplicate courses
raw_data_fall<- raw_data[1:91,]
raw_data_spring <- raw_data[92:178,]
raw_data_summer <- raw_data[179:188,]
diff_spring <- setdiff(raw_data_spring$course_title_codes, raw_data_fall$course_title_codes)
raw_data_spring <- raw_data_spring[raw_data_spring$course_title_codes %in% diff_spring, ]
diff_summer <- setdiff(raw_data_summer$course_title_codes, raw_data_fall$course_title_codes) %>% setdiff(raw_data_spring$course_title_codes)
raw_data_summer <- raw_data_summer[raw_data_summer$course_title_codes %in% diff_summer, ]
# bind the data back together
raw_data <- rbind(raw_data_fall, raw_data_spring, raw_data_summer)
# pattern matching to clean up data columns

raw_data$course_permissions <- sapply(raw_data$course_permissions, function(x){
  if(grepl("CS 2110", x)){
    return ("Students who have not taken CS 2110/Engrd 2110, CS 2112, a CS course numbered 3000 or above, 
            or are affiliated with the computer science major.")
  }
  else if(grepl("College of Arts and Sciences", x)){
    return ("College of Engineering Students")
  }
  else if(grepl("Cornell Tech", x) & grepl("graduate", x)){
    return ("Cornell Tech Graduate Students")
  }
  else if(grepl("Cornell Tech", x)){
    return ("Cornell Tech Students")
  }
  else if(grepl("CS master of engineering", x)){
    return ("CS Master of Engineering Students")
  }
  else if(grepl("PhD|Ph.D.", x) & grepl("MS",x)){
    return ("Ph.D. Students, MS Students")
  }
  else if(grepl("PhD|Ph.D.", x)){
    return ("Ph.D. Students")
  }
  else if(grepl("robotics research", x)){
    return ("Students involved in Robotics Research")
  }
  else if(grepl("graduate", x) & grepl("senior", x)){
    return ("Graduate Students, Senior Students")
  }
  else if(grepl("graduate", x)){
    return ("Graduate Students")
  }
  else if(grepl("prefreshman", x)){
    return ("Prefreshman Students")
  }
  else{
    return (as.character(x))
  }
})

raw_data$course_offerings <- sapply(raw_data$course_offerings, function(x){
  x <- gsub("When Offered |\\.","", x)
})

# separate prereq and coreq columns here
corequisites <- as.vector(unlist(sapply(raw_data$course_prereq_coreq, function(x){
  if(grepl("Corequisite:", x)){
    return (gsub("Prerequisites/Corequisites Corequisite: |\\.","",x))
  }
  else{
    return (NA)
  }
}
)))
prerequisites <- unlist(sapply(raw_data$course_prereq_coreq, function(x){
  if(grepl("Prerequisite:", x) | grepl("Prerequisite for programmers:", x)){
    return (gsub("Prerequisites/Corequisites Prerequisite: |\\.","",x))
  }
  else{
    return (NA)
  }
})
)
# hard code coreqs
corequisites[2] <- "MATH 1110, MATH 1910"
corequisites[11] <- "CS 1110, CS 1112"
corequisites[13] <- "CS 2800"
corequisites[48] <- "CS 5435"
corequisites[96] <- "INFO 3152, ENGRC 3152"
corequisites[99] <- "CS 4121"
corequisites[111] <- "CS 5121"
corequisites[117] <- "CS 5430"

# now hard code prereqs
prerequisites[7] <- "CS 1110, CS 1112"
prerequisites[8] <- "CS 1110, CS 1112"
prerequisites[10] <- "CS 1110, CS 1112"
prerequisites[13] <- "CS 2110, CS 2112"
prerequisites[14] <- "CS 2110, CS 2112"
prerequisites[16] <- "CS 3110"
prerequisites[17] <- "CS 2110"
prerequisites[18] <- "MATH 2210, MATH 2940; MATH 3XXX; CS 1110, CS 1112"
prerequisites[19] <- "CS 2110, CS 2112; CS 2800"
prerequisites[21] <- "CS 3410, CS 3420"
prerequisites[23] <- "CS 3410, CS 3420, ECE 3140"
prerequisites[24] <- "CS 2110, CS 2112"
prerequisites[26] <- "CS 2110, CS 2112; CS 2800"
prerequisites[29] <- "MATH 1920, MATH 2220; MATH 2940, MATH 2210; CS 1110, CS 1112"
prerequisites[30] <- "BTRY 3010; CS 4820"
prerequisites[31] <- "BTRY 3080, ECON 3130, MATH 4710, ENGRD 2700; MATH 2940, MATH 2210; CS 2110, CS 2112"                           
prerequisites[32] <- "CS 2800"                           
prerequisites[33] <- "PHYS 3316, AEP 3620"                           
prerequisites[34] <- "CS 2110, CS 2112, CS 3110; CS 2800"                           
prerequisites[35] <- "(MATH 2210; MATH 2220, MATH 2230; MATH 2240, MATH 1920; MATH 2940); (CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340)"
prerequisites[38] <- "CS 3110"
prerequisites[39] <- "CS 2110, CS 2112"                           
prerequisites[40] <- "CS 4110; CS 6110; CS 6410"   
prerequisites[41] <- "CS 1110, CS 1112"                          
prerequisites[44] <- "CS 4410"
prerequisites[45] <- "ECE 4750, CS 4420"                           
prerequisites[46] <- "CS 1110, CS 1112"                          
prerequisites[47] <- "CS 2800, CS 4820"
prerequisites[53] <- "CS 1110, CS 1112"
prerequisites[55] <- "CS 2110, CS 2112"
prerequisites[56] <- "MATH 1920, MATH 2220; MATH 2940, MATH 2210; CS 1110, CS 1112"
prerequisites[57] <- "BTRY 3080, ECON 3130, MATH 4710, ENGRD 2700; MATH 2940, MATH 2210; CS 2110, CS 2112"
prerequisites[58] <- "CS 2800; CS 1110, CS 1112"
prerequisites[61] <- "CS 4110, CS 6110"
prerequisites[62] <- "CS 4110; CS 6110; CS 6410"
prerequisites[63] <- "MATH 4310, MATH 4340"
prerequisites[64] <- "CS 4410"
prerequisites[65] <- "CS 4410"
prerequisites[67] <- "MATH 2210, MATH 2940; CS 4820"
prerequisites[68] <- "MATH 2210, MATH 2940; CS 4700, CS 4740, CS 4300, CS 4780, CS 4786"
prerequisites[70] <- "CS 2800"
prerequisites[71] <- "CS 4780, CS 5780, CS 4786, CS 5786, CS 6780"
prerequisites[72] <- "CS 4780, CS 4786" 
prerequisites[73] <- NA
prerequisites[74] <- "CS 4820"
prerequisites[75] <- "CS 4820"
prerequisites[76] <- "CS 4820"
prerequisites[78] <- "CS 6110"
prerequisites[82] <- "CS 4410"
prerequisites[94] <- "CS 1300"
prerequisites[95] <- "BTRY 3080, ECON 3130, MATH 4710, ENGRD 2700"
prerequisites[96] <- "CS 2110, CS 2112, INFO 2450"
prerequisites[97] <- "CS 2110, CS 2112; CS 2300"
prerequisites[98] <- "ENGRD 2300"
prerequisites[99] <- "CS 3110; CS 3420, CS 3410"
prerequisites[101] <- "CS 3152; CS 3300, CS 4620, CS 4700, CS 4758, CS 5414"
prerequisites[102] <- "MATH 2210, MATH 2940; MATH 3XXX; CS 1110, CS 1112" 
prerequisites[103] <- "(MATH 2210, MATH 2940; CS 2800), INFO 2940; CS 2110, CS 2112" 
prerequisites[105] <- "CS 2110, CS 2112; CS 2800"
prerequisites[106] <- "LING 1101; CS 2110, CS 2112"
prerequisites[107] <- "CS 2800; MATH 1910; MATH 1920; MATH 2210, MATH 2940"
prerequisites[108] <- "INFO 2040; CS 2800; BTRY 3080, ECON 3130, MATH 4710, ENGRD 2700"
prerequisites[110] <- "CS 1110, CS 1112"
prerequisites[111] <- "CS 3110; CS 3420, CS 3410"
prerequisites[113] <- "CS 2110, CS 2112"
prerequisites[115] <- "CS 4410"
prerequisites[116] <- "CS 4410"
prerequisites[118] <- "CS 2800; CS 2110, CS 2112"
prerequisites[120] <- "CS 2800; CS 2110, CS 2112"
prerequisites[121] <- "ORIE 5750, CS 5785; BTRY 3080, ECON 3130, MATH 4710, ENGRD 2700; MATH 2210, MATH 2940"
prerequisites[122] <- "CS 2800; CS 4810"
prerequisites[126] <- "CS 4320"
prerequisites[130] <- "CS 4700"
prerequisites[132] <- "MAE 4180, CS 4758; BTRY 3080, ECON 3130, MATH 4710, ENGRD 2700; MATH 2210, MATH 2940; CS 2024, CS 1110"
prerequisites[134] <- "CS 6860"

raw_data$prereq <- prerequisites
raw_data$coreq <- corequisites
raw_data <- data.frame(sapply(raw_data, as.character))

raw_data$course_distribution_categories <- sapply(raw_data$course_distribution_categories, function(x){
  x <- gsub("Distribution Category |Course Attribute |(|)","", x)
})
raw_data$course_forbidden_overlaps <- sapply(raw_data$course_forbidden_overlaps, function(x){
  x <- gsub("Forbidden Overlaps Forbidden Overlap: ","", x)
})

# hardcode forbidden overlaps

raw_data$course_forbidden_overlaps[1] <- NA
raw_data$course_forbidden_overlaps[2] <- "CS 1112, CS 1114, CS 1115"
raw_data$course_forbidden_overlaps[13] <- "CS 3410, CS 3420"
raw_data$course_forbidden_overlaps[14] <- "CS 3110"
raw_data$course_forbidden_overlaps[35] <- "CS 4860, MATH 4810, MATH 4860, PHIL 4310"

# remove prereq_coreq column
raw_data$course_prereq_coreq <-
# save the cleaned data
saveRDS(raw_data, file = "cs_cleaned_data.Rda")

# now extract non cs courses from forbidden overlaps and prerequisites for additional webscraping
courses_extract <- c(as.vector(na.omit(raw_data$course_forbidden_overlaps)), 
                         as.vector(na.omit(raw_data$prereq)),
                         as.vector(na.omit(raw_data$coreq)))

courses_extract <- as.vector(unlist(sapply(courses_extract, function(x){
  tokens <- unlist(strsplit(x,",|;|[()]"))
  tokens <- tokens[str_length(tokens)>=4]
  tokens <- as.vector(unlist(sapply(tokens, function(y){
    if(substr(y, 1, 1) == " "){
      return (substr(y,2,str_length(y)))
    }
    else(
      return (y)
    )
  })
  ))
  return (tokens)
})))

courses_extract <- unique(courses_extract)
class(courses_extract)
# all non cs codes for courses we need to extract data from
courses_extract <- courses_extract[grepl("CS",courses_extract) == FALSE]
courses_extract
# construct edge matrix
# want to web scrape all courses that are not in the CS department as well but listed as forbidden overlaps, prereqs, or coreqs
# also want to scrap courses that are offered specially in spring or summer or winter 2018
saveRDS(courses_extract, file = "extracted_courses.Rda")
