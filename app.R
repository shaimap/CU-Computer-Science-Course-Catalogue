library("shiny")
library("shinydashboard")
library("shinydashboardPlus")
library("shinyjs")
library("DT")
library("leaflet")
library("shinyWidgets")
source("scheduling_algorithm_cliques.R")
source("leaflet_location_vis.R")
source("better_visualizations.R")
source("reccomender_functions.R")


data <- readRDS(file = "Preprocesses_course_time_data.Rda")
unique_locs <- readRDS(file = "unique_locs.Rda")
geocode_locs<- readRDS(file = "geocode_locs.Rda")
node_metadata <- readRDS(file = "node_metadata.Rda")
edge_metadata <- readRDS(file = "edge_metadata.Rda")
data_recs <- readRDS(file = "clean_data_rec.RDS")
courses_recs <- readRDS(file = "clean_courses_rec.RDS")
nn <- readRDS( "nearest_neighbor.RDS")
header <- dashboardHeader(

  # Create a tasks drop down menu
  dropdownMenu(
    type = "notifications",
    notificationItem(text = "Access the official cornell course catalogue!", 
                     href="https://classes.cornell.edu/browse/roster/FA18",
                     status = "success")
  )
)
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    # Create two `menuItem()`s, "Dashboard" and "Inputs"
    menuItem(
      tabName = "info",
      text = "Welcome",
      icon = icon("info")
    ),
    menuItem(
      tabName = "scheduler",
      text = "Schedule Maker",
      icon = icon("calendar")
    ),
    menuItem(
      tabName = "prereq_coreq", 
      text = "CS Relationship Visualizer",
      icon = icon("eye")
    ),
    menuItem(
      tabName = "rec_engine", 
      text = "CS Recommendation Engine",
      icon = icon("hand-o-right")
    )
  )
)
body <- dashboardBody(
  useShinyjs(),
  # Create a tabBox
  tabItems(
    tabItem(
      tabName = "info",
      div(id = 'logo',
          img(id = 'Cornell_University_seal.svg', src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/4/47/Cornell_University_seal.svg/768px-Cornell_University_seal.svg.png', height = '100px', align = 'right')),
      tags$h1(HTML("Welcome to the Unofficial CU Computer Science Course Catalogue!")),
      tags$h4(
        HTML("<br>This web app is a course catalogue specially tailored to CS students. Click on the different tabs and explore!</br>

             <br>Summary of Features:</br>
             <br><ul><li>A scheduler into which you can input and rate your courses to retrieve all possible nonconflicting course schedules.</li>
             <li>A map visualization at which your selected class sections occur.</li>
             <li>A CS course relationship visualizer which allows you to graphically observe relationships between different prerequisite and corequisite courses with respect to the courses you have taken and a course of interest.</li>
              <li>A CS course recommendation engine which recommends the top five best CS courses for you to take based on your course history.</li></ul>
             <br>Note: I am a student at Cornell and am not affiliated with the creation of the official course catalogue. Therefore, this app may be subject to errors.
             If you wish to learn more about the algorithms or notify me of any errors, consult me (Shaima Parveen) at sp822@cornell.edu. Code for this app is on github if you are interested.</br>")
      ),
      tags$h3(
        HTML("My Contact Info")
        ),
        socialButton(
          url = "https://www.linkedin.com/in/shaima-parveen/",
          type = "linkedin"
        ),
        socialButton(
          url = "https://github.com/shaimap",
          type = "github"
      )
  ),
    tabItem(
      tags$h1(HTML("Make a Schedule")),
      tabName = "scheduler",
      title = "Create a Schedule",
      fluidRow(boxPlus(title = "Courses of Interest",
                      width = 5,
                      closable = FALSE,
                      collapsible = TRUE,
                      status = "info",
                      footer = radioButtons(inputId = "time_pref",
                                            label = "Select your overall time preference.",
                                            choiceNames = c("Before noon","Afternoon"),
                                            choiceValues = c(TRUE, FALSE)),
                   selectizeInput("courses_selected", 
                           label = 'Select a course to add.', 
                           choices = paste(data$course_title_codes,"|", data$course_titles),
                           selected = NULL,
                           multiple = FALSE),
                   uiOutput("ratings"),
                   actionButton("course_add", 
                                label= "Add course",
                                icon = icon("plus"),
                                style="color: #fff; background-color: limegreen; border-color: limegreen")),
               boxPlus(
                      title = "Selected Courses", 
                      width = 7, 
                      closable = FALSE,
                      collapsible = TRUE,
                      status = "info",
                      footer = NULL,
                      footer_padding = FALSE,
                      dataTableOutput("course_show"),
                      actionButton("generate_scheds",
                      label= "Generate possible schedules",
                      style="color: #fff; background-color: steelblue; border-color: steelblue"))),
      fluidRow(boxPlus(title = "Schedules Generated",
                width = 12,
                footer = NULL,
                footer_padding = FALSE,
                closable = FALSE,
                status = "info",
                collapsible = TRUE,
                uiOutput("choose_sched"),
                DT::dataTableOutput("schedule"),
                textOutput("course_codes")),
                boxPlus(title = "Map of Class Locations",
                        closable = FALSE,
                        status = "info",
                        collapsible = TRUE,
                        width = 12,
                        footer = NULL,
                        footer_padding = FALSE,
                    leafletOutput("mymap")))),
  tabItem(
    tags$h1(HTML("<center>Visualize your course relationships</center>")),
    tabName = "prereq_coreq",
    title = "Visualize relationships between your courses",
    fluidRow(boxPlus(title = "Select your courses",
                        width = 12, 
                        closable = FALSE,
                        collapsible = TRUE,
                        status = "info",
                        footer = NULL,
                        footer_padding = FALSE,
                        selectizeInput("past_courses", 
                                       label = 'Which computer science courses have you taken in the past? Be sure to select all CS courses including the equivalent courses you have used AP or transfer credit for.', 
                                       choices = node_metadata$course_title_codes,
                                       selected = NULL,
                                       multiple = TRUE),
                        uiOutput("future_courses"),
                        actionButton("generate_vis",
                                     label= "Generate Visualizations",
                                     style="color: #fff; background-color: steelblue; border-color: steelblue"))),
    fluidRow(tabBox(width = 12,
                    tabPanel(title = "Shortest paths to target course",
                             textOutput("label1"), 
                             visNetworkOutput("shortest_path_vis", height = "1000px")),
                    tabPanel(title = "Target courses's prerequisites and corequisites",
                             textOutput("label2"),
                             visNetworkOutput("course_prereq",height = "1000px")),
                    tabPanel(title = "Target course's potential courses",
                             textOutput("label3"), 
                             visNetworkOutput("course_other",height = "1000px")),
                    tabPanel(title = "Courses you fulfill requirements for",
                             textOutput("label4"), 
                             visNetworkOutput("courses_possible",height = "1000px")
                    )))),
  tabItem(
    tags$h1(HTML("<center>Which courses should you take next?</center>")),
    tabName = "rec_engine",
    title = "Which courses should you take next?",
    fluidRow(boxPlus(title = "Select your courses",
                     width = 12, 
                     closable = FALSE,
                     collapsible = TRUE,
                     status = "info",
                     footer = NULL,
                     footer_padding = FALSE,
                     selectizeInput("past_courses1", 
                                    label = 'Which computer science courses have you taken in the past? Be sure to select all CS courses including the equivalent courses you have used AP or transfer credit for.', 
                                    choices = data_recs$course_title_codes,
                                    selected = NULL,
                                    multiple = TRUE),
                     actionButton("generate_recs",
                                  label= "Generate Recommendations",
                                  style="color: #fff; background-color: steelblue; border-color: steelblue"))),
    fluidRow(boxPlus(title = "Recommended Courses",
                     width = 12, 
                     closable = FALSE,
                     collapsible = TRUE,
                     status = "info",
                     footer = NULL,
                     footer_padding = FALSE,
                     uiOutput("choose_rec"),
                     DT::dataTableOutput("recs_datatable"),
                     br(),
                     br(),
                     plotlyOutput("recs_plot"))))))

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body)
server <- function(input, output,session) { 
  #CS course relationship visualizer
  output$future_courses <- renderUI({
    req(input$past_courses)
    remaining_courses <- node_metadata$course_title_codes[!node_metadata$course_title_codes %in% input$past_courses]
    selectizeInput("future_course_code",
                   label = 'Name one computer science course you would want to take or are curious about.',
                   choices = remaining_courses,
                   selected = NULL,
                   multiple = FALSE)
  })
  
  output$label1 <- renderText({
    req(input$past_courses)
    req(input$future_course_code)
    "Select a course node for closer inspection! Hover over course nodes to discover which courses you have or have not taken or could potentially take in the future."
  })
  
  shortest_paths <- eventReactive(input$generate_vis,{
    req(input$past_courses)
    req(input$future_course_code)
    vis_net_paths_multiple(edge_metadata, node_metadata, course_code = input$future_course_code, courses_taken = input$past_courses)
  })
  
  
  output$shortest_path_vis <- renderVisNetwork({
    req(input$past_courses)
    req(input$future_course_code)
    req(input$generate_vis)
    validate(
      need(shortest_paths(),"This visualization does not exist for your selected course combinations."
      )
    )
    shortest_paths()%>%
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click1', nodes.nodes[0]);
                ;}")
})
  
  observeEvent(input$click1,{
    req(input$past_courses)
    req(input$future_course_code)
    req(input$click1)
    req(input$generate_vis)
    info <- modal_popup_info(node_metadata, edge_metadata, input$click1)
    if(!is.null(info)){
      showModal(modalDialog(
        title = HTML(info)
      ))}})
  
  observeEvent(input$generate_vis,{
    req(input$past_courses)
    req(input$future_course_code)
    visNetworkProxy("shortest_path_vis") %>% visGetNodes()
  })
  
  output$vis1 <- renderDataTable({
    req(input$past_courses)
    req(input$future_course_code)
    if(!is.null(input$shortest_path_vis_nodes)){
      info <- data.frame(matrix(unlist(input$shortest_path_vis_nodes), ncol = dim(nodes)[1],
                                byrow=T),stringsAsFactors=FALSE)
      colnames(info) <- colnames(nodes)
      info
      
    }
  })
  
  output$label2 <- renderText({
    req(input$past_courses)
    req(input$future_course_code)
    "Select a course node for closer inspection! Hover over course nodes to discover which courses you have or have not taken or could potentially take in the future."
  })
  prereq_vis <- eventReactive(input$generate_vis,{
    req(input$past_courses)
    req(input$future_course_code)
    vis_net_plot(edge_metadata, node_metadata, course_code = input$future_course_code, courses_taken = input$past_courses, outgoing = FALSE)
  })
  output$course_prereq <- renderVisNetwork({
    req(input$past_courses)
    req(input$future_course_code)
    req(input$generate_vis)
    validate(
      need(prereq_vis(),"This visualization does not exist for your selected course combinations."
      )
    )
    prereq_vis()%>%
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click2', nodes.nodes[0]);
                ;}")
})
  
  
  observeEvent(input$click2,{
    req(input$past_courses)
    req(input$future_course_code)
    req(input$click2)
    req(input$generate_vis)
    info <- modal_popup_info(node_metadata, edge_metadata, input$click2)
    if(!is.null(info)){
      showModal(modalDialog(
        title = HTML(info)
      ))}})
  
  output$label3 <- renderText({
    req(input$past_courses)
    req(input$future_course_code)
    "Select a course node for closer inspection! Hover over course nodes to discover which courses you have or have not taken or could potentially take in the future."
  })
  
  other_vis <- eventReactive(input$generate_vis,{
    req(input$past_courses)
    req(input$future_course_code)
    vis_net_plot(edge_metadata, node_metadata, course_code = input$future_course_code, courses_taken = input$past_courses, outgoing = TRUE)
  })
  
  output$course_other <- renderVisNetwork({
    req(input$past_courses)
    req(input$future_course_code)
    req(input$generate_vis)
    validate(
      need(other_vis(), "This visualization does not exist for your selected course combinations."
      )
    )
    other_vis()%>%
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click3', nodes.nodes[0]);
                ;}")
  })
  
  observeEvent(input$click3,{
    req(input$past_courses)
    req(input$future_course_code)
    req(input$click3)
    req(input$generate_vis)
    info <- modal_popup_info(node_metadata, edge_metadata, input$click3)
    if(!is.null(info)){
      showModal(modalDialog(
        title = HTML(info)
      ))}})
  
  output$label4 <- renderText({
    req(input$past_courses)
    req(input$future_course_code)
    "Select a course node for closer inspection! Hover over course nodes to discover which courses you have or have not taken or could potentially take in the future."
  })
  
  possible_vis <- eventReactive(input$generate_vis,{
    req(input$past_courses)
    req(input$future_course_code)
    vis_net_possible_courses(edge_metadata, node_metadata, courses_taken = input$past_courses)
  })
  output$courses_possible <- renderVisNetwork({
    req(input$past_courses)
    req(input$future_course_code)
    req(input$generate_vis)
    validate(
      need(possible_vis(),"This visualization does not exist for your selected course combinations."
      )
    )
    possible_vis()%>%
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click4', nodes.nodes[0]);
                ;}")
  })
  
  observeEvent(input$click4,{
    req(input$past_courses)
    req(input$future_course_code)
    req(input$click4)
    req(input$generate_vis)
    info <- modal_popup_info(node_metadata, edge_metadata,input$click4)
    if(!is.null(info)){
      showModal(modalDialog(
        title = HTML(info)
      ))}})
  

  # schedule maker
  rv <- reactiveValues(i=0, weight = c(), course_title_codes = c(), gen = 0, select = NULL)
  output$ratings <- renderUI({
    if (input$courses_selected!= ""){
      x <- str_split(input$courses_selected[length(input$courses_selected)]," ") %>% unlist()
      input_course <- paste(x[1],x[2])
      sliderInput("rating_slide", 
                paste("Rate",input_course,
                "on a scale of 1 to 10 in terms of how much you value this course.", sep = " "),
                min = 1, max = 10,value = 10)}
  })
  
  df <- eventReactive(input$course_add | !is.null(input$select_button), {
    req(input$courses_selected)
    req(input$course_add)
    x <- str_split(input$courses_selected," ") %>% unlist()
    input_course <- paste(x[1],x[2])
    if(!is.null(input$select_button)){
      if(is.null(rv$select)){
        selectedRow <- as.numeric(substr(as.character(strsplit(input$select_button, "_")[[1]][2]),1,1))
        rv$weight <- rv$weight[-selectedRow]
        rv$course_title_codes <- rv$course_title_codes[-selectedRow]
        rv$select <- input$select_button
      }
      else if(rv$select != input$select_button){
        selectedRow <- as.numeric(substr(as.character(strsplit(input$select_button, "_")[[1]][2]),1,1))
        rv$weight <- rv$weight[-selectedRow]
        rv$course_title_codes <- rv$course_title_codes[-selectedRow]
        rv$select <- input$select_button
      }
      else if(input_course %in% rv$course_title_codes){
        rv$weight <- rv$weight
        rv$course_title_codes <- rv$course_title_codes
        rv$select <- rv$select
      }
      else{
        rv$weight <- c(rv$weight,input$rating_slide)
        rv$course_title_codes <- c(rv$course_title_codes, input_course)
        rv$select <- rv$select
      }
    }
    else if(input_course %in% rv$course_title_codes){
      rv$weight <-rv$weight
      rv$course_title_codes <-rv$course_title_codes
    }
    else{
      rv$weight <- c(rv$weight,input$rating_slide)
      rv$course_title_codes <- c(rv$course_title_codes, input_course)
    }
    df <- data.frame(course_title_codes = rv$course_title_codes, weight = rv$weight, stringsAsFactors = FALSE)
    return (df)
  })
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }

  output$course_show <- renderDataTable({
    req(input$course_add)
    if(nrow(df())==0){
      return (NULL)
    }
    course_titles <- c() 
    for(i in 1:length(df()$course_title_codes)){
      course <- df()$course_title_codes[i]
      course_titles[i] <- as.character(unique(data$course_titles[data$course_title_codes %in% course]))
    }
    buttons <- list()
    
    for(i in 1:nrow(df())){
      buttons[[i]] <- shinyInput(actionButton, 1, paste0("button_",i),
                                                      label = "", 
                                                      icon = icon("minus"), 
                                                      style="color: #fff; background-color: red; border-color: red",
                                                      onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
    }
    starblocks <- list()
    for(i in 1:nrow(df())){
      starblocks[[i]] <- as.character(starBlock(maxstar = 10, grade = df()$weight[i], color = "lightblue"))
    }
    new_df <- data.frame("Course" = paste(df()$course_title_codes, "|", course_titles))
    new_df$`Importance Rating` <- starblocks
    new_df$` ` <- buttons
    
    datatable(new_df, 
              rownames = NULL,
              options = list(dom = 't',scrollY = TRUE, bSort = FALSE), 
              extensions = list("Scroller")) %>% 
              formatStyle(columns = c("Course","Importance Rating", " "), backgroundColor = "white", color = "black")%>% 
              formatStyle(column = " ", textAlign = 'right')
    })
  


  scheds <- eventReactive(input$generate_scheds,{
    req(input$time_pref)
    req(input$generate_scheds)
    if(!is.null(df())){
    schedules <-  attain_schedules(df(), data, morning = input$time_pref)
    return (schedules)
    }
    else{
      return (NULL)
    }
  })
  pretty_sched <- reactive({
    req(input$time_pref)
    req(input$generate_scheds)
    req(input$sched_number)
    if(!is.null(df())){
    p <- prettify_schedule(scheds()[[as.numeric(input$sched_number)]])
    return (p)
    }
    else{
      return (NULL)
    }
  })
  output$choose_sched <- renderUI({
      req(input$time_pref)
      req(input$generate_scheds)
      if(!is.null(df())){
      selectInput("sched_number", paste("Select from",length(scheds()),"Potential Schedules in Order of Optimality!"),
                 choices = 1:length(scheds()),multiple = FALSE, selected = 1, width = '1400px')
      }
      else{
        return (NULL)
      }
  })
  output$schedule<- DT::renderDataTable({
    req(input$time_pref)
    req(input$generate_scheds)
    req(input$sched_number)
    if(!is.null(df())){
    datatable(pretty_sched(),
              width = 12,
              extensions = 'Buttons',
              options = list(dom = 'tB',
                             ordering = FALSE,
                             rowCallback = JS("function(r,d) {$(r).attr('height', '75px')}"),
                             initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '20px'});}"),
                             buttons = c('copy', 'csv', 'excel')),
              class = 'cell-border stripe', 
              callback = JS("table.on('mouseenter', 'td', function() {
                           Shiny.onInputChange('hoverIndexJS', this.innerHTML);});
                           return table;"),
              escape = FALSE, selection=list(target="cell")
              )
      
    }
    else{
      return (NULL)
    }
  })
  observeEvent(input$schedule_cell_clicked$row,{
                 req(input$schedule_cell_clicked$row)
                 req(input$schedule_cell_clicked$col)
                 
                 str <- pretty_sched()[input$schedule_cell_clicked$row,input$schedule_cell_clicked$col]
                 if(str!= ""){
                 v1 <- str_split(str, ">") %>% unlist()
                 s1 <- v1[3]
                 v2 <- str_split(s1, "<") %>% unlist()
                 s2 <- v2[1]
                 words <- str_split(s2, " ") %>% unlist()
                 code <- paste(words[1], words[2])
                 class <- words[3]
                 row <- scheds()[[as.numeric(input$sched_number)]] %>% filter(course_title_codes == code & class_type == class)
                 attributes <- gsub("Distribution Category ", "",row$course_distribution_categories)
                 attributes <- gsub("[()]", "", attributes)
                 info <- paste("<center><b>",row$course_title_codes, row$class_type, "|",
                               row$course_titles,"</b></center><br>Registration Code: ",
                               row$class_number, "</br><br>Course Credits: ",
                               row$course_credits, "</br><br>Course Attribute: ",
                               attributes, "</br><br>Course Description: ",
                               row$course_descriptions, "</br>")
                showModal(modalDialog(
                title = HTML(info)
                ))}
  })
  output$mymap <- renderLeaflet({
    req(input$time_pref)
    req(input$generate_scheds)
    req(input$sched_number)
    if(!is.null(df())){
    create_map(scheds()[[as.numeric(input$sched_number)]], geocode_locs,unique_locs)
    }
    else{
      return (NULL)
    }
  })
  output$course_codes <- renderText({
    req(input$time_pref)
    req(input$generate_scheds)
    req(input$sched_number)
    if(!is.null(df())){
      course_numbers(scheds()[[as.numeric(input$sched_number)]])
    }
    else{
      return (NULL)
    }
  })
  
  #course recs
  output$choose_rec <- renderUI({
    req(input$generate_recs)
    selectInput("courses_taken_recs",
                   label = 'Select a course for which you want to see your recommendations.',
                   choices = input$past_courses1,
                   selected = input$past_courses1[1],
                   multiple = FALSE)
  })
  
  rec_courses <- eventReactive(input$courses_taken_recs,{
    req(input$past_courses1)
    req(input$generate_recs)
    req(input$courses_taken_recs)
    return (course_recs(input$past_courses1, data_recs, courses_recs, nn))
  })
  rec_courses_vis <- eventReactive(input$courses_taken_recs,{
    req(input$past_courses1)
    req(input$generate_recs)
    req(input$courses_taken_recs)
    return (course_recs_vis(input$past_courses1, data_recs, courses_recs, nn))
  })
  
  output$recs_datatable <- renderDataTable({
    req(input$past_courses1)
    req(input$generate_recs)
    req(input$courses_taken_recs)
    index <- which(input$past_courses1 == input$courses_taken_recs)
    course_rec <- rec_courses()[[index]]
    colnames(course_rec)[1] <- "Click on the different courses to find out more!"
    datatable(course_rec, 
              options = list(dom = 't',scrollY = TRUE, bSort = FALSE), 
              extensions = list("Scroller"))
  })
  
  output$recs_plot <- renderPlotly({
    req(input$past_courses1)
    req(input$generate_recs)
    req(input$courses_taken_recs)
    index <- which(input$past_courses1 == input$courses_taken_recs)
    vis <- rec_courses_vis()[[index]]
    vis
  })
  observeEvent(input$recs_datatable_cell_clicked$row,{
    req(input$recs_datatable_cell_clicked$row)
    req(input$recs_datatable_cell_clicked$col)
    index <- which(input$past_courses1 == input$courses_taken_recs)
    course_rec <- rec_courses()[[index]]
    str <- course_rec[input$recs_datatable_cell_clicked$row,input$recs_datatable_cell_clicked$col]
    v <- str_split(str, " ") %>% unlist()
    course_code <- paste(v[1],v[2])
    row <- data_recs %>% filter(course_title_codes==course_code)
    if(nrow(row)!= 0){
      course_descriptions <- gsub("\n", "", row$course_descriptions)
      info <- paste("<center><b>",row$course_title_codes, "|",
                    row$course_titles,"</b></center><br>Course Description: ",
                    course_descriptions, "</br>")
      showModal(modalDialog(
        title = HTML(info)))
    }
    else{
      showModal(modalDialog(
        title = HTML(NULL)))
    }
  })
  
  
}
shinyApp(ui, server)