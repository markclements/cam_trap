ui <- navbarPage(
  fluid = TRUE,
  title = "CamTrapApp",
  tabPanel(
    title = "Import",
    column(
      width = 12,
      input_folder_UI("directory"),
      on_start_on_stop_UI("on_start"),
      select_names_UI("select_names")
    )
  ),
  #### end import
  tabPanel(
    title = "Annotate",
    fluidRow(
      column(
        fluidRow(
        tags$head(
          tags$style(
            type = "text/css",
            "#img-img img {max-width: 100%; width: 100%}"#; height: auto}"
          ),
          tags$script(src = "wheelzoom.js")
        )
      ),
        style = "background-color: black; height: 800px",
        align = "center",
        width = 8,
        plot_image_UI("img")
      ),
      column(
        style = "background-color: grey",
        width = 4,
        fluidRow(
          style = "height: 800px",
        h3("Observations"),
        select_image_UI("image_list"),
        actionButton("zoom", "zoom") %>% tagAppendAttributes(onclick = "wheelzoom(document.querySelectorAll('img'));"),
        annotate_image_UI("img_ann"),
        apply_last_UI("apply_last"),
        display_table_UI("disp_table")
      )
      )
    )
    # ,
    # fluidRow(
    #   h3("Table"),
    #   # display_table_UI("disp_table")
    # )
  ) #### end annotate
)