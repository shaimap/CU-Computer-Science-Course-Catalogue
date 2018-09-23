# CU-Computer-Science-Course-Catalogue
This web app is developed using R shiny and helps students devise course schedules based on classes they want to take as well as visualize course relationships.
Currently the web app has the following features:
- Scheduler enables a user to input their courses of interest, course importance ratings, and time preferences to retrieve all possible compatible schedules. The algorithm employs graph theory by linking compatible classes together and finding largest cliques from the resulting graph. Scheduler comes with an complimentary map that helps users visualize their class locations in order to help them better decide upon the optimal schedule.
- CS course visualizer enables a user to visualize prerequisite and corequisite relationships between courses. It queries the user for a course of interest as well as their course history and outputs visuals based on courses the users can potentially take, shortest course paths and prerequisites/corequisites of the course of interest.
- CS course recommendation engine which uses TFIDF and cosine similarity in order to recommend courses whose course descriptions are most similar to those courses the use has taken in the past. 

Note: I worked on alot of this project locally so the number of commits is not reflective of the effort that has gone into this project.


Link for web app is here: https://shaimaparveen.shinyapps.io/cu-computer-science-course-catalogue/
