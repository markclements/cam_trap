display_table_UI <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("table"))
}

display_table_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    output$table <- renderReactable({
      if (is.null(rv$annotations)) return() 
      
      else {
        
        rv$annotations %>% 
          filter(event == rv$event) %>%
          select(id, distance, images, notes) %>%
          mutate(images = map_int(images, ~length(.))) %>%
          ungroup() -> table
        
      reactable(table,
                resizable = TRUE, 
                showPageSizeOptions = TRUE,
                selection = "single", 
                onClick = "select", 
                striped = TRUE, 
                highlight = TRUE,
                bordered = TRUE)
      }
      })
    
    observe({
      if (is.null(rv$annotations)) return() 
    rv$record <- getReactableState("table", "selected")
    })
    
    observeEvent(rv$event,{
      updateReactable(
        outputId = "table",
        selected = NA 
      )
    })
  })    
}