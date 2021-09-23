start_up_UI <- function(id) {
  ns <- NS(id)
  tagList(
  selectInput(
       inputId = ns("direc"),
       label = "Directories List (sorted by most recent)",
       choices = c(""),
       width = "100%"
    ),
    actionButton(
        inputId = ns("load_dir"),
        label = "Directory from List"
    ),
    shinyDirButton(
        id = ns("upload_new"),
        label = "New Directory",
        title = "New Directory"
    ),
    verbatimTextOutput(
        outputId = ns("out")
    )
  )
}

start_up_server <- function(id, rv, input) {
  moduleServer(id, function(input, output, session) {
    ## session.RDS is a file that contains the path to the last used directory and the list of project species names 
    ## it is saved on exit from the app: see shut_down_module.R
    ## it is intended to make it easier to remember where I left off, but it is quite fragile in design. 
    
    observe({
      if (file.exists(here("session.RDS"))) {
         rv$session_file <- readRDS("session.RDS")
          isolate({
           rv$dirs_list <- rv$session_file$dir
          })
      }
    })

    observeEvent(input$upload_new, {
        volumes <- c(Home = fs::path_home(),
                    "R Installation" = R.home(),
                    getVolumes()())

        shinyDirChoose(
            input = input,
            id = "upload_new",
            roots = volumes,
            session = session,
            restrictions = system.file(package = "base")
            )

        rv$dir <- parseDirPath(volumes, input$upload_new)
    })

    observeEvent(input$load_dir, {
        rv$dir <- input$direc
    })

    observe({
       updated_list <- c(rv$dir, rv$dirs_list)
       rv$dirs_list <- updated_list[!duplicated(updated_list)]
       updateSelectInput(
            inputId = "direc",
            choices = rv$dirs_list
        )
    })

    output$out <- renderPrint({
        glue("{rv$dir}")
    })
    observe({
      req(rv$dir)
      if (file.exists(glue("{rv$dir}/data.RDS"))) {
        temp <- readRDS(glue("{rv$dir}/data.RDS"))
      
        if (!is.null(temp$df)) {
          rv$annotations <- tibble()
        } else {
          rv$annotations <- temp$annotations ## tibble with annotations
        }
    
        if (is.null(temp$control_file)) {
          rv$control_file <- determine_capture_intervals(rv$dir, 15)
        } else {
          rv$control_file <- temp$control_file
        }
      } else {
        rv$temp <- character() ## for downstream stuff?
        #rv$names <- character()
        rv$control_file <- determine_capture_intervals(rv$dir, 15)
        if (!is.null(rv$control_file)) {
          rv$control_file %>%
          pull(image) -> imgs
          rv$annotations <- imap(setNames(imgs,imgs), ~tibble(species = as.character(NA), distance = as.numeric(NA), notes = as.character(NA)))
        }
      }
      # addResourcePath("temp",dir())
    })
   })
}
