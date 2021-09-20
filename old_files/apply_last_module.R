## some comments
apply_last_UI <- function(id) {
  ns <- NS(id)
  actionButton(
    inputId = ns("apply_last"),
    label = "apply last"
  )
}

apply_last_server <- function(id, dir, curr_img, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns ### for name-spacing the UI components of the modal
      
    observeEvent(input$apply_last, {
      
      all_img <- fs::dir_ls(
        dir()
      ) %>%
        fs::path_file()
      
      cur_img <- fs::path_file(curr_img())
      
      last_img <- all_img[which(all_img==cur_img)-1]
      
      
      last <- rv$df %>%
        filter(image == last_img) %>%
       # slice_tail() %>% 
        mutate(image = cur_img)
      
      rv$df <- bind_rows(rv$df, last)
      
      
    })
    
  })
}  

#bind_rows(inputs$df, inputs$df %>% slice_tail()) 
