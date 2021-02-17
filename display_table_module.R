display_table_UI <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("table"))
}

display_table_server <- function(id, rv, curr_img) {
  moduleServer(id, function(input, output, session) {
    
    output$table <- renderReactable({
      if (is.null(curr_img)) return() 
      
      else {
        
      
      x <- fs::path_file(curr_img())
      reactable(rv$df %>% filter(image == x),
                resizable = TRUE, 
                showPageSizeOptions = TRUE,
                selection = "single", 
                onClick = "select", 
                striped = TRUE, 
                highlight = TRUE)
      }
      })
    
    
    selected <- reactive(getReactableState("table","selected"))
    return(selected)
    
    
  })    
}