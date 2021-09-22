zoom_image_UI <- function(id) {
  ns <- NS(id)
  
  imageOutput(ns("zoom_img"),
              #height = "500px"
              width = "100%")
}

zoom_image_server <- function(input, output, session, dir, brush) {
  observeEvent(brush(), {
    output$zoom_img <- renderImage({
      width = brush()$xmax - brush()$xmin
      height = brush()$ymax - brush()$ymin
      
      dir() %>%
        image_read(.) %>%
        image_resize("x600") %>%
        image_crop(geometry = geometry_area(
          width = width,
          height = height,
          x_off = brush()$xmin,
          y_off = brush()$ymin
        )) %>%
        image_resize("500x500") %>%
        image_write(tempfile(fileext = 'jpg'), format = 'jpg') -> d
      
      list(src = d,
           #width = 600,
           # height = "600px",
           alt = "This is alternate text")
      
    }, deleteFile = TRUE)
    
    showModal(modalDialog(
      size = "m",
      easyClose = TRUE,
      fluidRow(
        style = "overflow-x: scroll;
                            border: 2px double grey;",
        column(12, align = "center",
               zoom_image_UI("img_zoom"))
      )
    ))
  })
  
}