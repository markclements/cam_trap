plot_image_UI <- function(id) {
  ns <- NS(id)
  
  imageOutput(
    ns("img"),
    #height = "auto",
    #width = "100%"#,
    # click = "img_click",
    # brush = brushOpts(
    #   id = "img_brush",
    #   resetOnNew = TRUE,
    #   clip = FALSE
   # ),
    #dblclick = "img_dblclick"
  )  
  
}

plot_image_server <- function(input, output, session, curr_img) {
  
  ### /home/mark/cam_trap/image = dir()
  ### curr_img = file.JPG
  
    
  
  output$img <- renderImage({
    #fs::dir_ls(dir())[1] %>%
    #input$image_list %>%
    
      image_read(curr_img()) %>%
     # image_resize("x600") %>%
      image_write(tempfile(fileext = 'jpg'), format = 'jpg') -> d
    
    #print(image_read(d) %>% image_info(.))
    
    list(id="img",
         src = d,
        # width = "800px",
        # height = "600px",
         alt = "This is alternate text")
    
  }, deleteFile = TRUE)
  
}