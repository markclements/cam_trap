
server <- function(session, input, output) {
    
    if (file.exists("inputs.RDS")){
        d<-readRDS("inputs.RDS")
        print(d)
    }
     
    else print("d is null")
    
    callModule(module = input_folder_server,
               id = "directory") -> dir
    
    callModule(module = select_image_server,
               id = "image_list",
               dir) -> curr_img
    
    callModule(module = plot_image_server,
               id = "img",
               curr_img)
    
    callModule(module = zoom_image_server,
               id = "img_zoom",
               curr_img,
               reactive(input$img_brush))
    

    
    onStop(function() {
        cat("stopping")
        
        isolate({
            saveRDS(list(directory=dir,
                         current_image=curr_img,
                         inputs=reactiveValuesToList(input)) , file = 'inputs.RDS')
        })
        
    })
    
 }
