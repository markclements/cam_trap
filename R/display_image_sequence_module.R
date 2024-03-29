display_image_sequence_UI <- function(id){
  ns <- NS(id)
  #uiOutput(ns("img"))
  imageOutput(ns("img"))
}

display_image_sequence_server <- function(id, image, rv) {
  moduleServer(id, function(input, output, session) {

  # output$img <- renderUI({
  #   
  #   tags$img(src=glue::glue("temp/{image()}"), height = "650px")
  # 
  # })
    output$img <- renderImage({
        req(rv$dir)
      list(
        src = glue("{rv$dir}/{image()}"),
        width = "100%",
        height = "auto"
      )
    }, deleteFile = FALSE
   )
  })
}