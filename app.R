library(shiny)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(fs)
library(exifr)
library(magick)
library(shinyFiles)
library(reactable)
library(glue)
library(shinyWidgets)
#library(taxize)
#library(wikitaxa)


source("R/input_folder_module.R")
source("R/start_up_module.R")
source("R/display_image_sequence_module.R")
source("R/control_image_sequence_module.R")
source("R/image_records_module.R")
source("R/determine_capture_interval.R")
source("R/shut_down_module.R")
source("R/delete_record.R")
source("R/display_table_module.R")
source("R/update_interval.R")
source("R/update_interval_module.R")
source("R/pick_images_module.R")

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  navbarPage(
    fluid = TRUE,
    title = "CamTrapApp",
    tabPanel(
      title = "Upload",
      start_up_UI("start")
    ),
    tabPanel(
      title = "Annotate",
      div(
        id = "sequence_nav",
        actionButton(
          inputId = "backward",
          label = "Back"
        ),
        textOutput(
          outputId = "out"
        ),
        actionButton(
          inputId = "forward",
          label = "Next"
        )
      ),
      fluidRow(
        column(
          width = 8,
          display_image_sequence_UI("img")
        ),
        column(
          width = 4,
          display_image_sequence_control_UI("img_seq"),
          hr(),
          image_records_UI("image_records"),
          hr(),
          display_table_UI("table")
        )
      )
    ),
    tabPanel(
      title = "Update Interval",
      update_interval_ui("update")
    )
  )
)
 

server <- function(input,output,session){
  
  
  rv <- reactiveValues(event = 1)
  ## rv$event = session events
  ## rv$control_file 
  ## rv$annotations 
  ## rv$names  
  ## rv$record record number
  

  
  start_up_server(
    id = "start",
    rv = rv,
    input = input
  ) -> dir
  
  display_image_sequence_server(
    id = "img",
    image = image,
    dir = dir
  )
  
  display_image_sequence_control_server(
    id = "img_seq",
    rv = rv
  ) -> image
  
  image_records_server(
    id = "image_records",
    rv = rv
  )
  
  shut_down_server(
    id = "shut_down",
    rv = rv,
    dir = dir,
    input = input
  )
  
  display_table_server(
    id = "table", 
    rv = rv
  )
  
  update_interval_server(
    id = "update",
    rv = rv
  )
  
  
  #
  # Move this into a module = capture event navigation UI and server
  #
  capture_events <- reactive({
    req(rv$control_file)
    rv$control_file %>% pull(event) %>% unique() -> x
    rv$event <- 1 ## for switching to a new image directory 
    return(x)
  })
  
  observeEvent(input$forward,{
    req(rv$event)
    if(rv$event >=1 & rv$event < max(capture_events())){
      rv$event <- rv$event + 1
      rv$record <- NULL
    } 
    
  })
  
  observeEvent(input$backward,{
    req(rv$event)
    if(rv$event > 1 & rv$event <=  max(capture_events())) {
      rv$event <- rv$event - 1
      rv$record <- NULL
    }
  })
  
  output$out <- renderText({
    if (is.null(rv$control_file)) return("No files")
    glue::glue("Capture sequence {rv$event} of {max(capture_events())}")
  })
#
#   
# 

  
  
      
}

shinyApp(ui,server)



