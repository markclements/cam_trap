display_table_UI <- function(id) {
  ns <- NS(id)
  #reactableOutput(ns("table"))
   rHandsontableOutput(ns("table"))
}

display_table_server <- function(id, rv, image) {
  moduleServer(id, function(input, output, session) {


    observeEvent(input$table, {
      rv$annotations[[image()]] <- hot_to_r(input$table)
    })

    table <- reactive({
      table <- rv$annotations[[image()]]
      return(table)
    })

    output$table <- renderRHandsontable({
    #output$table <- renderReactable({
      if (is.null(table())) return()

      else {
        rhandsontable(table(), stretch = "all")

        # rv$annotations %>% 
        #   filter(event == rv$event) %>%
        #   select(id, distance, images, notes) %>%
        #   mutate(images = map_int(images, ~length(.))) %>%
        #   ungroup() -> table

      # reactable(table,
      #           resizable = TRUE, 
      #           showPageSizeOptions = TRUE,
      #           selection = "single", 
      #           onClick = "select", 
      #           striped = TRUE, 
      #           highlight = TRUE,
      #           bordered = TRUE)
      
      }
      })
    # observe({
    #   if (is.null(rv$annotations)) return()
    # rv$record <- getReactableState("table", "selected")
    # })
    # observeEvent(rv$event,{
    #   updateReactable(
    #     outputId = "table",
    #     selected = NA
    #   )
    # })
  })
}