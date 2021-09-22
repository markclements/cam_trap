start_up_UI <- function(id) {
  ns <- NS(id)
  tagList(
  verbatimTextOutput(ns("one")),
  shinyDirButton(id = ns("upload_new"), 
                 label = "use new directory", 
                 title = "Upload")
  )
}

start_up_server <- function(id, rv, input) {
  moduleServer(id, function(input, output, session) {
    observe({
      if(!file.exists("session.RDS")) {
        print("not found")
        output$one <- renderPrint({
          print("No previous directory found")
        })
      } else {
        session <- readRDS("session.RDS")
        if (!is.null(session$names)) rv$names <- session$names
        else rv$names <- character()
        output$one <- renderPrint({
          print(glue("The last directory used was {session$dir}"))
        })
      }
    })

    dir <- reactive({
    volumes <- c(Home = fs::path_home(),
                 "R Installation" = R.home(),
                 getVolumes()())
    shinyDirChoose(input = input,
                   id = "upload_new",
                   roots = volumes,
                   session = session,
                   restrictions = system.file(package = "base"))
    new_dir <- parseDirPath(volumes, input$upload_new)
  
  
  if(file.exists("session.RDS")){
    old_dir <- readRDS("session.RDS")$dir
    return(old_dir)
  }
  else return(new_dir)
    
    
    })
    
    observe({
      req(dir())
      if (file.exists(str_c(dir(), "data.RDS", sep = "/"))) {
        temp <- readRDS(str_c(dir(), "data.RDS", sep = "/"))
      
        if (!is.null(temp$df)) {
          rv$annotations <- tibble()
        } else {
          rv$annotations <- temp$annotations ## tibble with annotations
        }
    
        if (is.null(temp$control_file)) {
          rv$control_file <- determine_capture_intervals(dir(), 15)
        } else {
          rv$control_file <- temp$control_file
        }
      } else {
        rv$temp <- character() ## for downstream stuff?
        rv$annotations <- tibble()
        #rv$names <- character()
        rv$control_file <- determine_capture_intervals(dir(), 15)
      }
      # addResourcePath("temp",dir())
    })
    
    return(reactive({dir()}))

  })
}
