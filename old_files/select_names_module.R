select_names_UI <- function(id) {
  ns <- NS(id)
 fileInput(
   inputId = ns("select_names"),
   label = "Upload species names from another session?",
   accept = ".RDS")
}

select_names_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
  
    observe({
      req(input$select_names)
      ext <- tools::file_ext(input$select_names$name)
      ### need some way to validate and prevent crashes here by uploading the wrong file type. 
      d <- readRDS(input$select_names$datapath)
      #print(d$names)
      rv$names <- d$names
    })
      
  # observe({
  #   if (file.exists(str_c(dir(), "data.RDS", sep = "/"))) {
  #     d <- readRDS(str_c(dir(), "data.RDS", sep = "/")) ## error: can not open connection
  #     rv$d <- d$inputs$`image_list-image_list`
  #     rv$df <- d$df
  #     rv$names <- d$names ## possibly related to saving a reactive() object
  #   } else {
  #     rv$d <- character()
  #     rv$df <- tibble()
  #     rv$names <- character()
  #   }
  # })
  
  })
}

