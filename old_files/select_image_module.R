select_image_UI <- function(id) {
  ns <- NS(id)
  selectInput(ns("image_list"),
    "select",
    choices = NA,
    size = 10,
    selectize = F
  )
}

select_image_server <- function(id, dir, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      x <- fs::dir_ls(
        dir()
      ) %>%
        fs::path_file()

      if (length(rv$d)!=0) {
        selected <- rv$d
      } else {
        selected <- x[1]
      }

      updateSelectInput(session,
        "image_list",
        choices = x,
        selected = selected
      )
    })


    reactive(
      str_c(dir(), input$image_list, sep = "/")
    )
  })
}