library(stringr)
library(dplyr)
raw_data <- readRDS(file="webscraped_external_courses.Rda")

# bind the data back together
raw_data$course_offerings <- sapply(raw_data$course_offerings, function(x){
  x <- gsub("When Offered |\\.|(offered alternate years)|[()]| ","", x)
})
raw_data$course_distribution_categories <- sapply(raw_data$course_distribution_categories, function(x){
   x <- gsub("Distribution Category |Course Attribute |[()]| Breadth Requirement ","", x)
 })
raw_data <- unique(raw_data)
# save the cleaned data
saveRDS(raw_data, file = "external_cleaned_data.Rda")

# # separate prereq and coreq columns here
# corequisites <- sapply(raw_data$course_prereq_coreq, function(x){
#   if(grepl("Corequisite:", x)){
#     return (gsub("Prerequisites/Corequisites Corequisite: |\\.","",x))
#   }
#   else{
#     return (NA)
#   }
# }
# )
# prerequisites <- sapply(raw_data$course_prereq_coreq, function(x){
#   if(grepl("Prerequisite:", x) | grepl("Prerequisite for programmers:", x)){
#     return (gsub("Prerequisites/Corequisites Prerequisite: |\\.","",x))
#   }
#   else{
#     return (NA)
#   }
# }
# )
# # hard code coreqs
# corequisites[101] <- "AEP 4220"
# corequisites[108] <- "ENGRC 3152"
# # hard code prereqs
# 
# prerequisites[1] <- "MATH 1101"
# prerequisites[2] <- NA
# prerequisites[3] <- "MATH 1910"
# prerequisites[4] <- NA
# prerequisites[6] <- NA
# prerequisites[8] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940"
# prerequisites[9] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940"
# prerequisites[10] <- "MATH 1920"
# prerequisites[11] <- "MATH 2220, MATH 2230"
# prerequisites[12] <- "(MATH 2210; MATH 2220, MATH 2230; MATH 2240, MATH 1920; MATH 2940); (CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340); MATH XXXX"
# prerequisites[14] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940" 
# prerequisites[15] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940" 
# prerequisites[16] <- "MATH 4330" 
# prerequisites[17] <- "MATH 2210, MATH 2230, MATH 2940"
# prerequisites[18] <- "(MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)"
# prerequisites[19] <- "(MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)"
# prerequisites[20] <- "(MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)"
# prerequisites[21] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940"
# prerequisites[22] <- "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)); CS 2800"
# prerequisites[23] <- "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)); CS 2800"
# prerequisites[24] <- "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)); (MATH 2130; MATH 2310)"
# prerequisites[25] <- "(MATH 2210, MATH 2940); MATH 3XXX; (CS 1110, CS 1112)"
# prerequisites[26] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940"
# prerequisites[27] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940"
# prerequisites[28] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940"
# prerequisites[29] <- "(MATH 2210, MATH 2230, MATH 2310, MATH 2940); MATH 3XXX"
# prerequisites[30] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940"
# prerequisites[31] <- "MATH 3340, MATH 3360, MATH 4340, MATH 4500"
# prerequisites[32] <- "MATH 1920"
# prerequisites[33] <- "MATH 2220, MATH 2230"
# prerequisites[34] <- "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)); (CS 2800, MATH 3320, MATH 3340, MATH 3360, MATH 4340); MATH XXXX"
# prerequisites[38] <- "MATH 4140"
# prerequisites[41] <- "MATH 4340"
# prerequisites[43] <- "MATH 4310"
# prerequisites[44] <- "(MATH 4130, MATH 4140); (MATH 4310, MATH 4330); MATH 4350"
# prerequisites[45] <- "MATH 6510"
# prerequisites[46] <- "(MATH 4130; MATH 4140), MATH 6210"
# prerequisites[47] <- "MATH 6710; MATH 6370"
# prerequisites[57] <- "MATH 6730; MATH 6710"
# prerequisites[60] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940"
# prerequisites[61] <- "MATH 2210, MATH 2230, MATH 2310, MATH 2940"
# prerequisites[62] <- "MATH 3XXX, MATH 3XXX"
# prerequisites[64] <- "(MATH 2230; MATH 2240), MATH 3110, MATH 4130"
# prerequisites[65] <- "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)); MATH 2930"
# prerequisites[66] <- "(MATH 2210; MATH 2220); MATH 3XXX; (CS 1110, CS 1112)"
# prerequisites[67] <- "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940))"
# prerequisites[68] <- "MATH 4330"
# prerequisites[69] <- "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940))"
# prerequisites[70] <- "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)), (MATH 3040, MATH 3110, MATH 3340)"
# prerequisites[71] <- "((MATH 2210; MATH 2220), (MATH 2230; MATH 2240), (MATH 1920; MATH 2940)); MATH 3XXX"
# prerequisites[72] <- "MATH 4710; (MATH 2210, MATH 2940); MATH 1920"
# prerequisites[73] <- "MATH 4710, BTRY 3080, ORIE 3500, ECON 3130"
# prerequisites[74] <- "PHIL 2310, MATH 2810, MATH 3840"
# prerequisites[75] <- "MATH 4140"
# prerequisites[76] <- "MATH 4130, MATH 4140"
# prerequisites[77] <- "MAE 6750, MATH 6260"
# prerequisites[78] <- "MATH 6310"
# prerequisites[80] <- "MATH 4340"
# prerequisites[82] <- "MATH 4340; MATH 4530"
# prerequisites[83] <- "MATH 6510; MATH 6520"
# prerequisites[84] <- "MATH 6710"
# prerequisites[85] <- "BTRY 4090, MATH 6710"
# prerequisites[94] <- "MATH 2220, MATH 2230"
# prerequisites[95] <- "ENGRD 2300"
# prerequisites[96] <- NA
# prerequisites[97] <- "BTRY 3010; MATH 1910; MATH 1920"
# prerequisites[98] <- "(MATH 1110; MATH 1120); (ECON 1110, ECON 1120)"
# prerequisites[99] <- "MATH 1910, MATH 1920"
# prerequisites[100] <- "(PHYS 2214, PHYS 2218); (PHYS 1116; PHYS 2216); (MATH 2940, MATH 2210, MATH 2230)"
# prerequisites[101] <- "AEP 3610, PHYS 3316"
# prerequisites[102] <- "CS 3420, CS 3410"
# prerequisites[104] <- "CS 1110, CS 1112"
# prerequisites[107] <- "CS 2800; (CS 1110, CS 1112)"
# prerequisites[108] <- "(CS 2110, CS 2112), INFO 2450"
# 
# raw_data$prereq <- prerequisites
# raw_data$coreq <- corequisites
# 
# 
# raw_data$course_forbidden_overlaps <- sapply(raw_data$course_forbidden_overlaps, function(x){
#   x <- gsub("Forbidden Overlaps Forbidden Overlap: ","", x)
#   x <- gsub("due to an overlap in content, students will receive credit for only one course in the following group: ","", x)
#   x <- gsub("due to an overlap in content, students will receive credit for only one course in the following group: ","", x)
#   x <- gsub("Due to an overlap in content, students will receive credit for only one course in the following group: ","", x)
#   x <- gsub("Due to an overlap in content, students will not receive credit for both ","", x)
#   x <- gsub("due to an overlap in content, students will not receive credit for both ","", x)
#   x <- gsub("Due to an overlap in content, students will receive not receive credit for both ","", x)
#   x <- gsub("and ","", x)
#   x <- gsub(". For guidance in selecting an appropriate course, please consult First Steps in Math.","", x)
#   x <- gsub("\\.","", x)
#   
# })
# 
# raw_data$course_forbidden_overlaps[1] <- "MATH 1110, MATH 1106"
# raw_data$course_forbidden_overlaps[10] <- "BTRY 3080, ECON 3110, ECON 3130, MATH 4710"
# raw_data$course_forbidden_overlaps[11] <- "CS 4860, MATH 4810, MATH 4860, PHIL 4310"







