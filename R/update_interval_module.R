### one problem with this is that does not adjust for number of 
### annotations associated with the images in the interval because 
### we do not record or associate the specific image with an observation
### really only a problem when going to a smaller interval. 
### Perhaps we could just raise an alert that you need to go back 
### and manually edit the observations after splitting the interval. 

update_interval_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(
      inputId = ns("new_interval"),
      label = "choose a new capture interval (total minutes)",
      value = 15,
      min = 0.000001
    ),
    actionButton(
      inputId = ns("update_interval"), 
      label = "update interval"
    )
  )
}

update_interval_server <- function(id, rv) {
    moduleServer(id, function(input, output, session) {
      
      observeEvent(input$update_interval,{
        temp <- update_interval(
          rv$control_file,
          rv$annotations,
          input$new_interval
        )
        
      rv$annotations <- temp$annotations
      rv$control_file <- temp$control_file
        
      })
      
    })
}