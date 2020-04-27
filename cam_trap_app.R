
#### Camera Trap app ####
### 

library(shiny)
library(tidyverse)
library(fs)
library(exifr)
library(magick)
library(shinyFiles)
library(mime)
library(taxize)

#### APP 1 ####
# uses data.frame to store annotations #

ui<-navbarPage(fluid = TRUE,
               title ="My App",
  tabPanel(title="set up",
           shinyDirButton("directory", "Chose directory", "Upload"),
           textInput(inputId = "station_id",label="Station id", value = NA)
           ),             
  tabPanel(title = "image analysis",             
  column(width=3,
         numericInput(inputId = "image_num",label = "Record Number",value = 1,step=1,min=1),
         actionButton(inputId = "add_record",label="+ Record"),
         actionButton(inputId = "rem_record",label="- Record"),
         h5("Number of records for this image"),
         textOutput("num_records"),
         #textInput(inputId = "species",label = "Species",placeholder = "enter scientific name"),
         selectizeInput(inputId = "species",label="Species", choices=NA,options=list(create = TRUE)),
         numericInput(inputId = "num_individuals",label="number of individuals",value = NA)
         
      ),
  column(width=9,
            fluidRow(style="overflow-x: scroll;
                     border: 2px double grey;",
              # h4("Image"),
               imageOutput("plot",
                           # width = "100%",
                            height = "500px")
              ),
         fluidRow(
         h5("Zoom image"),
         sliderInput(inputId = "zoom",min = 500,max=2000,value = 500,label="")
         )
         )
  ),
    tabPanel(title="data" ,         
            fluidRow(verbatimTextOutput("dir")
                  ) 
              )
          )
server<-function(input,output,session){
  
  rv<-reactiveValues()
  
  ### choose directory and load information about file contents. 
  ### save info on images and data files (csv's).
  
  volumes <- c(Home = fs::path_home(), 
               "R Installation" = R.home(), 
               getVolumes()())
  
  shinyDirChoose(input, 
                 "directory", 
                 roots = volumes, 
                 session = session, 
                 restrictions = system.file(package = "base"))
  
  observeEvent(input$directory,{
    
    dir<-(parseDirPath(volumes, 
                       input$directory))
    
    if (!is_empty(dir)){
      dir_ls(dir) %>%
        keep(~str_detect(guess_type(.),"image"))->rv$img
      
      dir_ls(dir) %>%
        keep(~str_detect(guess_type(.),"csv"))->rv$csv  
      
      ### check for a data storage file (from a previous session), if none exist create a new datafile
      if (is_empty(rv$csv)){
        tibble(image_name=rv$img,species=NULL)->rv$data_file
        
        #### FIX THIS, data file name saved is TOO GENERIC!!!! ####    
        write_csv(rv$data_file,path = str_c(dir,"data.csv",sep = "/"))
      }
      ### if data file exists, read into memory
      else rv$data_file<-read_csv(rv$csv) 
      
      ### get the first image to display to user   
      rv$image<-as.character(rv$data_file[input$image_num,"image_name"])
    }
  })
  
  observeEvent(input$image_num,{
    
    if (!is_empty(rv$data_file)){
      image<-as.character(rv$data_file[input$image_num,"image_name"])
      
      if (image != rv$image) rv$image<-image
      
    }
    
  })
  
  
  ### prevent user from viewing files that are not in the directory ###
  observe({
    if (!is.null(rv$data_file)) {
      updateNumericInput(session=session,inputId = "image_num",max=rv$data_file %>% summarise(n=n()) %>% as.integer())
    }
  })
  
  ### add records to images ###
  observeEvent(input$add_record,{
    rv$data_file %>%
      add_row(image_name=.[[input$image_num,"image_name"]],
              .after =input$image_num)->rv$data_file
    
    updateNumericInput(session=session,inputId = "image_num", value = as.integer(input$image_num)+1)
  })
  
  
  ### remove records from images ###
  observeEvent(input$rem_record,{
    rv$data_file %>%
      filter(image_name==as.character(.[input$image_num,"image_name"]))%>%
      summarise(n=n()) %>%
      as.integer()->cur_num
    
    if(cur_num>1){
      
      rv$data_file %>%
        slice(-input$image_num)->rv$data_file
      updateNumericInput(session=session,inputId = "image_num", value = as.integer(input$image_num)-1)
    }
  })
  
  
  ### report number of records per image to user ###
  output$num_records<-renderText({
    
    if (is.null(rv$data_file)) "No Records for this image"
    
    else{
      rv$data_file %>%
        filter(image_name==as.character(.[input$image_num,"image_name"]))%>%
        summarise(n=n()) %>%
        as.integer()
    }
  })
  
  #### change values in data frame!!! #### 
  # observe({
  #   if(!is_empty(rv$data_file)){
  #     rv$data_file[input$image_num,"species"]<-input$species ### isolate input$species, wait for advance to next record?
  #   }
  # })
  
 
### display image to user, allow zoom and enhance!   
   output$plot<-renderImage({
    
    file<-rv$image 
    
    width  <- session$clientData$output_myImage_width
    height <- input$zoom
    
    list(
      src = file,
      width = width,
      height = height,
      alt = "This is alternate text"
    )
    
  },deleteFile = FALSE)

#### get station id from image using OCR
#### update text box input 
#### FIX: allow user to enter own value and overide OCR value.   
  
  # observe({  
  #   
  #   file<-rv$image 
  #   
  #   if (!is.null(file)){
  #   image_read(file) %>%
  #     image_info()%>%
  #     select(height)->height
  #   
  #   image_read(file) %>%
  #     image_crop(paste0("+0+",round(0.94*height))) %>% ## height from image info
  #     image_convert(type="grayscale") %>%
  #     image_modulate(brightness = 120) %>%
  #     image_enhance %>%
  #     image_median() %>%
  #     image_contrast() %>%
  #     image_ocr() %>% 
  #     str_extract("Camera\\d{1,}")->x
  #   
  #     updateTextInput(session = session,
  #               inputId = "station_id",
  #               value = x)  
  #   }
  # })
  
  ## print something in app ##
  output$dir <- renderPrint({
 rv$data_file
  })
  
  output$dir2 <- renderPrint({
  })
  
}

shinyApp(ui,server)

##### Doodle Space ####

### species (scientific name)
### number of individuals
### distance from camera
### station id (GPS coord?) via OCR but allow user to override
### date (from image metadata)
### time (from image metadata)
### notes or comments 


### look up scientific names using common names ### 

comm2sci("humming bird", db = "ncbi", itisby = "search", simplify = TRUE)

taxize::use_entrez()

### get date and  time of image ### 
exifr::read_exif("Desktop/image/11070008.JPG") %>%
  select(CreateDate) %>% View()

### ocr grab camera num, temperature ###
image_read("Desktop/image/11070008.JPG") %>%
  image_info() %>%
  select(height)->height

image_read("Desktop/image/11070011.JPG") %>%
  image_crop(paste0("+0+",round(0.94*height))) %>% ## height from image info
  image_convert(type="grayscale") %>%
  image_modulate(brightness = 120) %>%
  image_enhance %>%
  image_median() %>%
  image_contrast() %>%
  image_ocr() %>% 
  str_extract("Camera\\d{1,}")


dir_ls("Desktop/image/") %>%
  path_file()

dir_ls("Desktop/image/") %>%
  path_dir()

dir_ls("Desktop/image/") %>%
  path_ext()

dir_ls("Desktop/image/") %>%
  file_info()




#### APP 2 ####
# uses lists and json to store image annotations # 

library(shiny)
library(tidyverse)
library(fs)
library(exifr)
library(magick)
library(shinyFiles)
library(mime)
library(taxize)
library(jsonlite)
library(shinyWidgets)

ui<-navbarPage(fluid = TRUE,
               title ="My App",
               tabPanel(title="set up",
                    h4("Edit Project"),  
                      column(width=6,
                        fluidRow(
                          column(width=3,     
                            p("Project directory")
                          ),
                          column(width=7,
                            verbatimTextOutput("dir",placeholder = F)
                          ),
                          column(width=2,
                            shinyDirButton("directory", "Browse...", "Upload")
                           )
                        ),
                        h6("Number of image files "),
                        #verbatimTextOutput("dir2",placeholder =  F),
                        textInput(inputId = "station_id",label="Station id", value = NA)
                        )
               ),             
               tabPanel(title = "image analysis",             
                        column(width=3,
                               uiOutput("img"),
                               actionButton(inputId = "prev_img",label="Prev"),
                               actionButton(inputId = "next_img",label="Next"),
                               h5("Zoom image"),
                               sliderInput(inputId = "zoom",min = 500,max=2000,value = 500,label=""),
                              # selectInput(inputId = "phototype",label="",choices = NA,selected = NA),
                               pickerInput(inputId = "phototype",
                                           label="",
                                           choices = c("animal","blank"),
                                           options = list(title="nothing selected")),
                              
                               uiOutput("animal_id"),
                               uiOutput("species_id")
                        ),
                        column(width=9,
                               fluidRow(style="overflow-x: scroll;
                                        border: 2px double grey;",
                                        # h4("Image"),
                                        imageOutput("plot",
                                                    # width = "100%",
                                                    height = "500px")
                                        
                                ),
                               verbatimTextOutput("dir2",placeholder =  F) 
                               
                        )
),
tabPanel(title="data" ,         
         fluidRow(
          
         ) 
)
)


server<-function(input,output,session){
  
  
  
  ### choose directory and load information about file contents. 
  ### save info on images and data files (csv's).
  
  volumes <- c(Home = fs::path_home(), 
               "R Installation" = R.home(), 
               getVolumes()())
  
  shinyDirChoose(input, 
                 "directory", 
                 roots = volumes, 
                 session = session, 
                 restrictions = system.file(package = "base"))
  
  observeEvent(input$directory,{
    
    rv$dir<-(parseDirPath(volumes, 
                       input$directory))

    if (!is_empty(rv$dir)){
      dir_ls(rv$dir) %>%
        keep(~str_detect(guess_type(.),"image")) %>%
        path_file() %>%
        as.character()->img
      
      rv$img<-tibble(imageid=img,
                     date_time=NA,
                     phototype=NA,
                     identification=NA)
    
    }
     else {
       return(NULL)
       }
  })

  output$img<-renderUI({
   # print("render ui")
    isolate(choices<-rv$img$imageid)
    selectInput(inputId = "img",
                label = "Current image",
                choices = choices
                )
  })
  
  rv<-reactiveValues(img_num=1)

  observeEvent(input$next_img,{
   # print("next image")
    if (rv$img_num==length(rv$img$imageid)) rv$img_num<<-length(rv$img$imageid)
    else rv$img_num<<-rv$img_num+1
    
    updateSelectInput(session=session,
                      inputId="img",
                      selected=rv$img$imageid[rv$img_num])
  })
  
  observeEvent(input$prev_img,{
    #print("prev img")
    if (rv$img_num==1) rv$img_num<<-1
    else rv$img_num<<-rv$img_num-1
    
    updateSelectInput(session=session,
                      inputId="img",
                      selected=rv$img$imageid[rv$img_num])
    
  })
  
  observeEvent(input$img,{
   # print("input image")
    rv$img_num<<-which(rv$img$imageid==input$img)
  })
  
  observeEvent(input$img,{
   selected<-rv$img$phototype[rv$img_num]
   #print("update select")
   updatePickerInput(session = session,
                     inputId = "phototype",
                     selected = selected)
   
  })
  
  
  observeEvent(input$phototype,{
    if (!is.null(rv$img)){
      #print("update data")
      #print(input$phototype)
      if (input$phototype == "") rv$img$phototype[rv$img_num]<-NA
      else rv$img$phototype[rv$img_num]<-input$phototype
    }
  })
  
  observe({  #### store animal id data
    if(!is.null(rv$img)){ 
      
      if(!is.na(rv$img[rv$img_num,]$identification)){
      
    isolate(ls<-rv$img[rv$img_num,]$identification[[1]]["speciesid"])
    #isolate(<-rv$img[rv$img_num,]$identification[[1]]["count"])
  
    #print(spp)   
    #print(cnt)
            
    if(!is.null(ls)){
     
    spp<-map(1:length(ls),~{
        input[[paste0("species",.)]]
       })
    
    cnt<-map(1:length(ls),~{
        input[[paste0("count",.)]]
      })  
    if(!is.null(spp)){
    print(tibble(spp=as.character(spp),cnt=cnt))
    rv$img[rv$img_num,]$identification<-list(tibble(spp=as.character(spp),cnt=cnt)) 
      }
    }
      }
    }
      
  })
 
  
  
  observeEvent(c(input$img,input$phototype,rv$img),{
    if (!is.null(rv$img)){
      cur_phototype<-rv$img$phototype[rv$img_num]
      cur_id_stat<-rv$img$identification[rv$img_num]
      ### if user switches the phototype to animal, create a id data frame for that image
      if (is.na(cur_id_stat) & !is.na(cur_phototype)){
        if (cur_phototype=="animal"){
        rv$img[rv$img_num,]$identification<-list(tibble(speciesid=NA,count=NA))
        }
      }
      ### if use user views an image that has animals and there is a id data frame
      ### allow user to see current valuesstored for that image 
      if (!is.na(cur_id_stat) & cur_phototype=="animal"){
        print("update current val")
        spp<-rv$img[rv$img_num,]$identification[[1]]["speciesid"]
        output$species_id<-renderUI({
          map(1:length(spp),
          ~column(11,  
              pickerInput(inputId = paste0("species",.),
                          label = paste0("species",.),
                          choices = c("new","old"),
                          selected = isolate(spp),
                          options = list(title="species name")),
              numericInput(inputId = paste0("count",.),
                           label="",
                           min = 1,
                           value = 1,
                           max=100)
          )) %>% tagList(.)
        })
      }
      else {
        output$species_id<-renderUI({
        p("no animals present")
          }) 
      } 
      ### if user switches back to blank, remove id data frame
      if (!is.na(cur_id_stat) & cur_phototype=="blank"){
        #print(!is.na(cur_id_stat))
        rv$img$identification[rv$img_num]<-NA
      }
  }

  })
  
  ## display image to user, allow zoom and enhance!
  output$plot<-renderImage({
    file<-glue::glue({rv$dir},"/",{input$img})
    
    width  <- session$clientData$output_myImage_width
    height <- input$zoom
    #print("render image")
    
        list(
          src = file,
          width = width,
          height = height,
          alt = "This is alternate text"
        )
    },deleteFile = FALSE
  )
  
  #renderUI()

  output$dir <- renderPrint({
    if (is_empty(rv$dir)) return("choose a directory")
    else rv$dir
  })
  
  output$dir2 <- renderPrint({
    if (is_empty(rv$dir)) return("choose a directory")
    else str(rv$img)
  })
  
}

shinyApp(ui,server)

x<-fromJSON(
  '{"ImageID":["xxxxc"],
  "ImageDateTime": ["today"],
  "PhotoType": ["animal"],
  "PhotoTypeIdentifications": {
    "PhotoTypeIdentifiedBy": ["ME"]
  },
  "ImageIdentifications": {
    "Identification": {
      "SpeciesScientificName": ["cow","horse"],
      "Count": ["3","5"]
    }
  }
}') 


x$ImageIdentifications$Identification<-tibble(SpeciesScientificName=c("cow","horse"),Count=c(3,2))  

x %>% toJSON(pretty = T) %>% fromJSON() %>% toJSON(.,pretty = T)

# "Image": {
#   "ImageID": [" "],
#   "ImageDateTime": [" "],
#   "PhotoType": [" "],
#   "PhotoTypeIdentifications": {
#     "PhotoTypeIdentifiedBy": [" "]
#   },
#   "ImageIdentifications": {
#     "Identification": {
#       "SpeciesScientificName": [" "],
#       "Count": [" "]
#     }
#   }
# }


img<-data.frame(image_id=c("xxxy","ffffr","zzzz"),
            date=c("today","today","today"),
            phototype=NA,
            identification=NA,
            stringsAsFactors = F)

img %>% as_tibble(.)->img

img[img$image_id=="xxxy",]$phototype<-"animal"

img[img$image_id=="xxxy",]$identification<-list(tibble(species=c("cow","dog"),
                                                  count=c(2,3),
                                                  comment=c(NA,NA),
                                                  distance=c(10,5)))

img[img$image_id=="ffffr",]$identification<-list(tibble(species=NA,
                                                       count=NA,
                                                       comment=NA,
                                                       distance=NA))

img[img$image_id=="zzzz",]$identification<-list(list(species=c("B","C","D"),count=NA))

  
str(img)
  
img[img$image_id=="ffffr",]$identification[[1]][["species"]]

img[img$image_id=="zzzz",]$identification[[1]][["species"]]

img[2,]$identification<-list(tibble(c("A","B")))




img %>% toJSON(pretty = T) %>% fromJSON() %>% str()


tibble(spp=as.character(list("C")))

# $CameraTrapMetadataStandard
# $CameraTrapMetadataStandard$ProjectID
# [1] " "
# 
# $CameraTrapMetadataStandard$ProjectName
# [1] " "
# 
# $CameraTrapMetadataStandard$ProjectObjectives
# [1] " "
# 
# $CameraTrapMetadataStandard$ProjectDesign
# [1] " "
# 
# $CameraTrapMetadataStandard$CountryCode
# [1] " "
# 
# $CameraTrapMetadataStandard$PublishDate
# [1] " "
# 
# $CameraTrapMetadataStandard$ProjectDataAccessandUseConstraints
# [1] " "
# 
# $CameraTrapMetadataStandard$PrincipalInvestigators
# $CameraTrapMetadataStandard$PrincipalInvestigators$PrincipalInvestigator
# $CameraTrapMetadataStandard$PrincipalInvestigators$PrincipalInvestigator$PrincipalInvestigatorName
# [1] " "
# 
# $CameraTrapMetadataStandard$PrincipalInvestigators$PrincipalInvestigator$PrincipalInvestigatorEmail
# [1] " "
# 
# 
# 
# $CameraTrapMetadataStandard$ProjectContacts
# $CameraTrapMetadataStandard$ProjectContacts$ProjectContact
# $CameraTrapMetadataStandard$ProjectContacts$ProjectContact$ProjectContactName
# [1] " "
# 
# $CameraTrapMetadataStandard$ProjectContacts$ProjectContact$ProjectContactEmail
# [1] " "
# 
# 
# 
# $CameraTrapMetadataStandard$OrganizationName
# [1] " "
# 
# $CameraTrapMetadataStandard$CameraDeploymentID
# [1] " "
# 
# $CameraTrapMetadataStandard$CameraSiteName
# [1] " "
# 
# $CameraTrapMetadataStandard$CameraDeploymentBeginDate
# [1] " "
# 
# $CameraTrapMetadataStandard$CameraDeploymentEndDate
# [1] " "
# 
# $CameraTrapMetadataStandard$DeploymentLocationID
# [1] " "
# 
# $CameraTrapMetadataStandard$QuietPeriodSetting
# [1] " "
# 
# $CameraTrapMetadataStandard$ActualLatitude
# [1] " "
# 
# $CameraTrapMetadataStandard$ActualLongitude
# [1] " "
# 
# $CameraTrapMetadataStandard$CameraMake
# [1] " "
# 
# $CameraTrapMetadataStandard$Bait
# [1] " "
# 
# $CameraTrapMetadataStandard$Feature
# [1] " "
# 
# $CameraTrapMetadataStandard$CameraStatus
# [1] " "
# 
# $CameraTrapMetadataStandard$Other
# [1] " "
# 
# $CameraTrapMetadataStandard$ImageSequence
# $CameraTrapMetadataStandard$ImageSequence$ImageSequenceID
# [1] " "
# 
# $CameraTrapMetadataStandard$ImageSequence$ImageSequenceBeginTime
# [1] " "
# 
# $CameraTrapMetadataStandard$ImageSequence$ImageSequenceEndTime
# [1] " "
# 
# $CameraTrapMetadataStandard$ImageSequence$SequenceIdentificationsBy
# $CameraTrapMetadataStandard$ImageSequence$SequenceIdentificationsBy$sequenceIdentifiedBy
# [1] " "
# 
# 
# $CameraTrapMetadataStandard$ImageSequence$SequenceIdentifications
# $CameraTrapMetadataStandard$ImageSequence$SequenceIdentifications$Identification
# $CameraTrapMetadataStandard$ImageSequence$SequenceIdentifications$Identification$SpeciesScientificName
# [1] " "
# 
# $CameraTrapMetadataStandard$ImageSequence$SequenceIdentifications$Identification$Count
# [1] " "
# 
# 
# 
# 
# $CameraTrapMetadataStandard$Image
# $CameraTrapMetadataStandard$Image$ImageID
# [1] " "
# 
# $CameraTrapMetadataStandard$Image$ImageDateTime
# [1] " "
# 
# $CameraTrapMetadataStandard$Image$PhotoType
# [1] " "
# 
# $CameraTrapMetadataStandard$Image$PhotoTypeIdentifications
# $CameraTrapMetadataStandard$Image$PhotoTypeIdentifications$PhotoTypeIdentifiedBy
# [1] " "
# 
# 
# $CameraTrapMetadataStandard$Image$ImageIdentifications
# $CameraTrapMetadataStandard$Image$ImageIdentifications$Identification
# $CameraTrapMetadataStandard$Image$ImageIdentifications$Identification$SpeciesScientificName
# [1] " "
# 
# $CameraTrapMetadataStandard$Image$ImageIdentifications$Identification$Count
# [1] " "

comm2sci("wild turkey",db = "eol",simplify = T)


library(shiny)
library(tidyverse)



library(shiny)
library(tidyverse)

ui <- fluidPage(
        fluidRow( 
          column(4,
            p("toy example"), 
            selectInput(inputId = "row_id",label = "",selected = 1,choices = c("NA")),
            actionButton(inputId = "add",label="add data frame"),
            numericInput(inputId = "rows",label = "",value = 1,min=1)
          ),
        column(5,uiOutput("names")
          )  
        ),
        fluidRow(
          column(6,
            p("df"),
            verbatimTextOutput("df")
          ),
          column(6,
            p("nested df"),
            verbatimTextOutput("nested_df")
          )
        )
  )

server <- function(input, output, session) {
  
rv<-reactiveValues(df=tibble(image_id=1:5,ident=NA))

observe({
  isolate(choices<-unlist(rv$df$image_id))
  updateSelectInput(session=session,
                    inputId = "row_id",
                    choices = choices)
})



observe({

  if (!is.na(rv$df[input$row_id,]$ident)){
      table<-input$row_id  
      values<-rv$df[input$row_id,]$ident[[1]]$a
      
      rv$df[input$row_id,]$ident[[1]]$a<<-map(1:length(values),~input[[paste0("values",.,table)]])
   }
})



output$names<-renderUI({
  print("ui update")

  table<-input$row_id 
  values<-rv$df[input$row_id,]$ident[[1]]$a
  
  map2(.x=1:length(values),.y=values,~textInput(inputId = paste0("values",.x,table),
                                 label = paste0("values",.,table),
                                 value = .y)
  )
})

observeEvent(input$add,{
  rv$df[input$row_id,]$ident<-list(tibble(a=rep(NA,input$rows),b=NA))
})

output$df<-renderPrint(reactiveValuesToList(input))

output$nested_df<-renderPrint(str(rv$df[input$row_id,]$ident[[1]]))
  
}

shinyApp(ui, server)

image_read("image/11070008.JPG") %>%
  image_resize("x600")%>%
  image_crop(geometry = geometry_area(height=558,
                                 width=57,
                                 x_off=25,
                                 y_off=526))


$xmin
[1] 25

$xmax
[1] 57

$ymin
[1] 526

$ymax
[1] 558
