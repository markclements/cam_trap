input_folder_UI<-function(id){
  ns <- NS(id)
  
  shinyDirButton(id = ns("directory"), 
                 label = "Browse...", 
                 title = "Upload")
}



input_folder_server<-function(input,output,session){
  
  volumes <- c(Home = fs::path_home(), 
               "R Installation" = R.home(), 
               getVolumes()())
  
  shinyDirChoose(input = input,
                 id = "directory", 
                 roots = volumes, 
                 session = session, 
                 restrictions = system.file(package = "base"))
  
  dir<-reactive({
     
     parseDirPath(volumes,input$directory)
   
     })
  
  
return(dir)
  
}
