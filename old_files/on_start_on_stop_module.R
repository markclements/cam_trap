on_start_on_stop_UI <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("one"))
}

on_start_on_stop_server <- function(id, dir, rv, input) {
  moduleServer(id, function(input, output, session) {
   output$one <- renderPrint({dir()})
   })
  
  observe({
  if (file.exists(str_c(dir(), "data.RDS", sep = "/"))) {
    d <- readRDS(str_c(dir(), "data.RDS", sep = "/")) ## error: can not open connection
    rv$d <- d$inputs$`image_list-image_list`
    rv$df <- d$df
    rv$names <- d$names ## possibly related to saving a reactive() object
  } else {
    rv$d <- character()
    rv$df <- tibble()
    rv$names <- character()
  }
})

  
  onStop(function() {
    cat("stopping")
    
    isolate({
      if(length(rv$df)>0){
        saveRDS(
          list(
            df = tibble(rv$df),
            names = rv$names,
            inputs = reactiveValuesToList(input)
          ),
          file = str_c(dir(), "data.RDS", sep = "/")
        )
      }  
    })
  })
  
}
