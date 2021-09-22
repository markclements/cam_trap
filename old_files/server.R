server <- function(session, input, output) {
  rv <- reactiveValues()


  callModule(
    module = input_folder_server,
    id = "directory"
  ) -> dir

  on_start_on_stop_server(
    id = "on_start",
    rv = rv,
    dir = dir,
    input = input
  )
  
  select_names_server(
    id = "select_names",
    rv = rv
  )
  
  select_image_server(
    id = "image_list",
    dir = dir,
    rv = rv
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

  
}
