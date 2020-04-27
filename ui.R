ui <- navbarPage(
  fluid = FALSE,
  title = "CamTrapApp",
  tabPanel(title = "Import",
           column(width = 3,
                  input_folder_UI("directory"))),
  #### end import
  tabPanel(
    title = "Annotate",
    column(
      style = "background-color: grey",
      align = "center",
      width = 9,
      plot_image_UI("img")
    ),
    
    column(
      style = "background-color: coral",
      width = 3,
      h3("Observations"),
      select_image_UI("image_list"),
      selectInput("new", label = "new", choices = c("new", "old"))
    )
  ) #### end annotate
)