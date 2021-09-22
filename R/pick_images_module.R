pick_images_ui <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      inputId = ns("pick_image"),
      label = NULL, 
      choices = character(0),
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3"
        ), 
      multiple = TRUE
    )
  )
}

pick_images_server <- function(id, rv, add) {
    moduleServer(id, function(input, output, session) {
   
   
      
      images <- reactive({
                  
                  if(!is.null(rv$control_file)){
                  rv$control_file %>% 
                    filter(event == rv$event) %>% 
                    pull(image) %>% 
                    unlist() -> images
                    
                  return(images)
                  }
                })
        
      
      
      observe({ 
        
        if (!is.null(rv$record)){
          
          rv$annotations %>% 
            filter(event == rv$event &
                     record == rv$record) %>% 
            pull(images) %>% 
            unlist() -> current_record
          
          
            updatePickerInput(
              session = session,
              inputId = "pick_image",
              selected = current_record,
              choices = images()
            )
        }
        else {
          updatePickerInput(
            session = session,
            inputId = "pick_image",
            selected = character(0),
            choices = images()
          )
        }
      })
      
      observeEvent(add(),{
        updatePickerInput(
          session = session,
          inputId = "pick_image",
          selected = character(0),
          choices = images()
        )
      })
    
      selected <- reactive({
        if(is.null(input$pick_image)) return(NA_character_)
        else return(input$pick_image)
      })
      return(selected)
    })
  
}

