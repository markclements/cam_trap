select_image_UI <- function(id) {
  ns <- NS(id)
  selectInput(ns("image_list"),
              "select",
              choices=NA,
              size = 5,
              selectize = F)
}

select_image_server <- function(input, output, session, dir) {
  
  observe({
    
    x<-fs::dir_ls(dir())
    
    updateSelectInput(session,
                      "image_list",
                      choices=x)
  })
  
  curr_img<-reactive(input$image_list)
  
  return(curr_img)
}