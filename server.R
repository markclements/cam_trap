server <- function(session, input, output) {
  rv <- reactiveValues()

  if (file.exists("inputs.RDS")) {
    d <- readRDS("inputs.RDS") ## error: can not open connection
    rv$df <- d$df
    rv$names <- d$names ## possibly related to saving a reactive() object
  } else {
    d <- NULL
    rv$df <- NULL
    rv$names <- NULL
  }


  callModule(
    module = input_folder_server,
    id = "directory"
  ) -> dir

  select_image_server(
    id = "image_list",
    dir,
    d
  ) -> curr_img ## returned as reactive

  callModule(
    module = plot_image_server,
    id = "img",
    curr_img
  )

  # callModule(module = zoom_image_server,
  #            id = "img_zoom",
  #            curr_img,
  #            reactive(input$img_brush))

  annotate_image_server(
    id = "img_ann",
    curr_img,
    selected,
    rv
  )
  
  apply_last_server(
    id = "apply_last",
    curr_img = curr_img,
    dir = dir,
    rv = rv
  )

  display_table_server(
    id = "disp_table",
    rv = rv,
    curr_img
  ) -> selected ## returned as reactive

  onStop(function() {
    cat("stopping")

    isolate({
      saveRDS(
        list(
          df = tibble(rv$df),
          names = rv$names,
          inputs = reactiveValuesToList(input)
        ),
        file = "inputs.RDS"
      )
    })
  })
}
