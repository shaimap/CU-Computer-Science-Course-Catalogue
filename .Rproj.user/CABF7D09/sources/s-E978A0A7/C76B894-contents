library("shiny")
library("shinydashboard")
library("shinydashboardPlus")
library("DT")
library("leaflet")
library("shinyWidgets")
source("scheduling_algorithm_cliques.R")
source("leaflet_location_vis.R")
source("better_visualizations.R")


data <- readRDS(file = "Preprocesses_course_time_data.Rda")
unique_locs <- readRDS(file = "unique_locs.Rda")
geocode_locs<- readRDS(file = "geocode_locs.Rda")
node_metadata <- readRDS(file = "node_metadata.Rda")
edge_metadata <- readRDS(file = "edge_metadata.Rda")
header <- dashboardHeader(

  # Create a tasks drop down menu
  dropdownMenu(
    type = "tasks",
    taskItem(
      text = "Mission Learn Shiny Dashboard",
      value = 10
    )
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
    )
  )
)
body <- dashboardBody(
  # Create a tabBox
  tabItems(
    tabItem(
      tabName = "info",
      div(id = 'logo',
          img(id = 'Cornell_University_seal.svg', src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/4/47/Cornell_University_seal.svg/768px-Cornell_University_seal.svg.png', height = '100px', align = 'right')),
      tags$h2(HTML("Welcome to the Unofficial CU Computer Science Course Catalogue!")),
      tags$div(
        HTML("<br>This web app is a course catalogue specially tailored to CS students. Click on the different tabs and explore!</br>

             <br>Summary of Features:</br>
             <br><ul><li>A scheduler into which you can input and rate your courses to retrieve all possible course schedules, and map visualizations of locations at which the courses occur.</li>
             <li>A CS course relationship visualizer which allows you to graphically observe relationships between different prerequisite and corequisite courses with respect to the courses you have taken and a course of interest.</li></ul>
             <br>Note: I am just a student at Cornell and am not affiliated with the creation of the official course catalogue. I know that this app may be subject to errors.
             If you wish to learn more about the algorithms or notify me of any errors, consult me (Shaima Parveen) at sp822@cornell.edu.</br>")
      )
  ),
    tabItem(
      tags$h2(HTML("Make a Schedule")),
      tabName = "scheduler",
      title = "Create a Schedule",
      fluidRow(column(width = 4,
                  box(title = "Courses of Interest",
                   width = NULL,
                   selectizeInput("courses_selected", 
                           label = 'Select a course to add.', 
                           choices = data$course_title_codes,
                           selected = NULL,
                           multiple = FALSE),
                   uiOutput("ratings"),
                   actionButton("course_add", 
                                label= "Add course")),
                   box(title = "Time Preference",
                       width = NULL,
                       radioButtons(inputId = "time_pref",
                       label = "Select your overall time preference.",
                       choiceNames = c("Before noon","Afternoon"),
                       choiceValues = c(TRUE, FALSE)))),
               column(width = 8, 
                      box(title = "Selected Courses", 
                      width = NULL,
                      dataTableOutput("course_show"),
                      actionButton("generate_scheds",
                      label= "Generate possible schedules"),
                      actionButton("clear_sched",
                      label = "Clear all courses")))),
      fluidRow(column(width = 12, 
                box(title = "Schedules Generated",
                width = NULL,
                uiOutput("choose_sched"),
                DT::dataTableOutput("schedule"),
                textOutput("course_codes")),
                box(title = "Map of Class Locations",
                    width = NULL,
                    leafletOutput("mymap", height = "750px"))))),
    tabItem(
      tags$h2(HTML("<center>Visualize your course relationships</center>")),
      tabName = "prereq_coreq",
      title = "Visualize relationships between your courses",
      fluidRow(column(width = 12,
                      box(title = "Select your courses",
                          width = NULL,
                          selectizeInput("past_courses", 
                                         label = 'Which computer science courses have you taken in the past? Be sure to select all CS courses including the equivalent courses you have used AP or transfer credit for.', 
                                         choices = node_metadata$course_title_codes,
                                         selected = NULL,
                                         multiple = TRUE),
                          uiOutput("future_courses"),
                          actionButton("generate_vis",
                                       label= "Generate Visualizations")))),
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
                             ))))))

ui <- dashboardPage(
  header = dashboardHeader(),
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
    "Select a course node for closer inspection! Hover over course nodes to discover which courses you have or have not taken or could potentially take in the future."
  })

  shortest_paths <- eventReactive(input$generate_vis,{
    vis_net_paths_multiple(edge_metadata, node_metadata, course_code = input$future_course_code, courses_taken = input$past_courses)
  })


  output$shortest_path_vis <- renderVisNetwork({
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
    req(input$click1)
    req(input$generate_vis)
    info <- modal_popup_info(node_metadata, input$click1)
    if(!is.null(info)){
      showModal(modalDialog(
        title = HTML(info)
      ))}})
  
  observeEvent(input$generate_vis,{
    visNetworkProxy("shortest_path_vis") %>% visGetNodes()
  })
  
  output$vis1 <- renderDataTable({
    if(!is.null(input$shortest_path_vis_nodes)){
      info <- data.frame(matrix(unlist(input$shortest_path_vis_nodes), ncol = dim(nodes)[1],
                                byrow=T),stringsAsFactors=FALSE)
      colnames(info) <- colnames(nodes)
      info
      
    }
  })
    
  output$label2 <- renderText({
    "Select a course node for closer inspection! Hover over course nodes to discover which courses you have or have not taken or could potentially take in the future."
  })
  prereq_vis <- eventReactive(input$generate_vis,{
    vis_net_plot(edge_metadata, node_metadata, course_code = input$future_course_code, courses_taken = input$past_courses, outgoing = FALSE)
  })
  output$course_prereq <- renderVisNetwork({
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
    req(input$click2)
    req(input$generate_vis)
    info <- modal_popup_info(node_metadata, input$click2)
    if(!is.null(info)){
      showModal(modalDialog(
        title = HTML(info)
      ))}})
  
  output$label3 <- renderText({
    "Select a course node for closer inspection! Hover over course nodes to discover which courses you have or have not taken or could potentially take in the future."
  })
  
  other_vis <- eventReactive(input$generate_vis,{
    vis_net_plot(edge_metadata, node_metadata, course_code = input$future_course_code, courses_taken = input$past_courses, outgoing = TRUE)
    })
  
  output$course_other <- renderVisNetwork({
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
    req(input$click3)
    req(input$generate_vis)
    info <- modal_popup_info(node_metadata, input$click3)
    if(!is.null(info)){
      showModal(modalDialog(
        title = HTML(info)
      ))}})
  
  output$label4 <- renderText({
    "Select a course node for closer inspection! Hover over course nodes to discover which courses you have or have not taken or could potentially take in the future."
  })
  
  possible_vis <- eventReactive(input$generate_vis,{
    vis_net_possible_courses(edge_metadata, node_metadata, courses_taken = input$past_courses)
  })
  output$courses_possible <- renderVisNetwork({
    req(input$generate_vis)
    print(possible_vis())
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
    req(input$click4)
    req(input$generate_vis)
    info <- modal_popup_info(node_metadata, input$click4)
    if(!is.null(info)){
      showModal(modalDialog(
        title = HTML(info)
   ))}})

  # schedule maker
  rv <- reactiveValues(i=0, weight = c(), course_title_codes = c(), clear = 0, gen = 0)
  output$ratings <- renderUI({
    if (input$courses_selected!= ""){
      
      sliderInput("rating_slide", 
                paste("Rate",input$courses_selected[length(input$courses_selected)],
                "on a scale of 1 to 10 in terms of how much you value this course.", sep = " "),
                min = 1, max = 10,value = 10)}
  })
  
  df <- eventReactive(input$course_add | input$clear_sched,{
    req(input$courses_selected)
    req(input$course_add)
    if(input$courses_selected %in% rv$course_title_codes){
      rv$weight <-rv$weight
      rv$course_title_codes <-rv$course_title_codes
    }
    else{
    rv$weight <- c(rv$weight,input$rating_slide)
    rv$course_title_codes <- c(rv$course_title_codes, input$courses_selected)
    }
    df <- data.frame(course_title_codes = rv$course_title_codes, weight = rv$weight, stringsAsFactors = FALSE)
    if(rv$clear == input$clear_sched){
      print(rv$clear)
      print(input$clear_sched)
      return (df)
    } else{
      rv$clear <- rv$clear + 1
      rv$weight <- NULL
      rv$course_title_codes <- NULL
      return (NULL)
    }
  })

  output$course_show <- renderDataTable({
    req(input$course_add)
    course_titles <- c() 
    for(i in 1:length(df()$course_title_codes)){
      course <- df()$course_title_codes[i]
      course_titles[i] <- as.character(unique(data$course_titles[data$course_title_codes %in% course]))
    }
    new_df <- data.frame("Course Title Code" = df()$course_title_codes, 
                         "Course Title" = course_titles,
                         "Importance Rating" = df()$weight, check.names = FALSE)
    datatable(new_df, options = list(dom = 't',scrollY = TRUE), extensions = list("Scroller"))
    })
  
  scheds <- eventReactive(input$generate_scheds,{
    req(input$time_pref)
    req(input$generate_scheds)
    print(paste("rvgen:",rv$generate))
    print(paste("gen:",input$generate_scheds))
    if(!is.null(df())){
    schedules <-  attain_schedules(df(), data, morning = input$time_pref)
    return (schedules)
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
  })
  output$choose_sched <- renderUI({
      req(input$time_pref)
      req(input$generate_scheds)
      if(!is.null(df())){
      selectInput("sched_number", paste("Select from",length(scheds()),"Potential Schedules in Order of Optimality!"),
                 choices = 1:length(scheds()),multiple = FALSE, selected = 1, width = '1400px')
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
  })
  output$course_codes <- renderText({
    req(input$time_pref)
    req(input$generate_scheds)
    req(input$sched_number)
    if(!is.null(df())){
    course_numbers(scheds()[[as.numeric(input$sched_number)]])
    }
  })
}
shinyApp(ui, server)