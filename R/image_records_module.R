## some comments
image_records_UI <- function(id) {
  ns <- NS(id)
  div(
    # fluidRow(
    #   column(
    #     width = 3,
    #     textOutput(ns("species_record")),
    #   ),
    #   column(
    #     width = 9,
    #     div(
    #       id = "record_control",
    #       actionButton(
    #         ns("backward"),
    #         label = "Previous Record"
    #       ),
    #       actionButton(
    #         ns("forward"),
    #         label = "Next Record"
    #       )
    #     )
    #   )
    # ),
    fluidRow(
      column(
        width = 3,
        p("Species ID")
      ),
      column(
        width = 9,
        selectInput(
          ns("species"),
          label = NULL,
          choices = c("bear", "moose")
        ),
      )
    ),
    tags$details(
      tags$summary("Modify Species ID List"),
      div(
        id = "list_species",
        textInput(
          ns("add_species_text"),
          label = NULL
        ),
        div(
          id = "list_button",
          actionButton(
            ns("add_species_bttn"),
            label = "add species"
          ),
          actionButton(
            ns("rem_species_bttn"),
            label = "remove species"
          )
        )
      )
    ),
    fluidRow(
      div(
        id = "species_distance",
        column(
          width = 3,
          p("Distance (m)")
        ),
        column(
          width = 9,
          numericInput(
            ns("distance"),
            label = NULL,
            value = 1,
            min = 0,
            max = 100,
            step = 1
          )
        )
      )  
    ),
    fluidRow(
      column(
        width = 3,
        p("Notes")
      ),
      column(
        width = 9,
        textAreaInput(
          ns("notes"),
          label = NULL
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        p("Observation Images")
      ),
      column(
        width = 9,
        pick_images_ui(ns("pick_images"))
      )
    ),
    fluidRow(
      br(),
      div(
        id = "record_buttons",
        actionButton(
          inputId = ns("add_record"),
          label = "add record"
        ),
        actionButton(
          inputId = ns("update_record"),
          label = "update record"
        ),
        actionButton(
          inputId = ns("remove_record"),
          label = "delete record"
        )
     )
    )
  )
}

image_records_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    pick_images_server(
      id = "pick_images",
      rv = rv,
      add = reactive({input$add_record})
    ) -> images
    

    observe({
      updateSelectInput(
        session = session,
        inputId = "species",
        choices = rv$names
      )
    })
    
    ## rv$control_file  is reactive value
    ##
    ## tibble(image, date_time, event)
    ##
    ## rv$annotations is a reactive value
    ##
    ## tibble(event = rv$event,
    ##       event_begin = date_time[first] no? 
    ##       event_end = date_time[last],   no? 
    ##       id = input$species,
    ##       images = list(images),
    ##       record = record number
    ##       distance = input$distance, 
    ##       notes = input$notes)
    ##
    
    
    ## use left_join to process annotations event # and date_time from control file? 
    
    
    ### need 
    ### 
    ### 
    ### 
    ###  
    
    
    observeEvent(input$add_record,{
      if(length(rv$annotations) < 1){
        rv$annotations <- tibble( ## new set of annotations
          event = rv$event,
          id = input$species,
          distance = input$distance,
          notes = input$notes) %>%
          mutate(record = 1) %>%
          #mutate(images = glue_collapse(glue("{images()}"), ","))
          mutate(images = list(images()))
      
      } else {
          rv$annotations <- bind_rows(
            rv$annotations,
            tibble(
              event = rv$event,
              id = input$species,
              distance = input$distance,
              notes = input$notes) %>%
              #mutate(images = glue_collapse(glue("{images()}"), ","))
              mutate(images = list(images()))
          ) %>%
            group_by(event) %>%
            mutate(record = 1:n()) %>%
            ungroup()
      }
      
      updateSelectInput(session = session, inputId = "species", selected = " ")
      updateNumericInput(session = session, inputId = "distance", value = NA_real_)
      updateTextInput(session = session, inputId = "notes", value = " ")
      
    })
    
    species_record <- reactive({
      if (length(rv$annotations) < 1) return() 
      
      rv$annotations %>% 
        filter(event == rv$event) %>% 
        pull(record) -> records
      
      if (length(records) >= 1) { 
        rv$record <- length(records)
        return(records)
      }
      if (length(records) < 1){ 
        rv$record <- NULL
        return()
      }  
    })
    
    
    
## update records
    
    observeEvent(input$update_record,{
      if(!is.null(species_record())){
      rv$annotations %>%
        filter(event == rv$event &
               record == rv$record) %>% 
        mutate(id = input$species,
               distance = input$distance,
               notes = input$notes) %>% 
          #mutate(images = glue_collapse(glue("{images()}"), ","))
          mutate(images = list(images())) -> updated
      
      rv$annotations %>%
        filter(event != rv$event |
               record != rv$record) %>% 
        bind_rows(., updated) %>% 
        arrange(event, record) -> rv$annotations
      }
      
      updateSelectInput(session = session, inputId = "species", selected = " ")
      updateNumericInput(session = session, inputId = "distance", value = NA_real_)
      updateTextInput(session = session, inputId = "notes", value = " ")
      
    })
    
    observeEvent(input$remove_record,{
     
      if(is.null(species_record())) return()
      
      rv$annotations <- delete_record(df = rv$annotations,
                                      ev = rv$event,
                                      re = rv$record)
    })
    
    # observeEvent(input$forward,{
    #   req(rv$record)
    #   if(rv$record >= 1 & rv$record < max(species_record())) rv$record <- rv$record + 1
    # })
    # 
    # observeEvent(input$backward,{
    #   req(rv$record)
    #   if(rv$record > 1 & rv$record <=  max(species_record())) rv$record <- rv$record - 1  
    # })
    # 
    # output$species_record <- renderText({
    #   if (is.null(species_record())) return("No records") ###
    #   glue::glue("Record {rv$record} of {max(species_record())}")
    # })
    
    observe({
      if (!is.null(rv$record)){
      rv$annotations %>% 
        filter(event == rv$event &
               record == rv$record) -> current_record
        
      
      updateSelectInput(session = session, inputId = "species", selected = current_record$id)
      updateNumericInput(session = session, inputId = "distance", value = current_record$distance)
      updateTextInput(session = session, inputId = "notes", value = current_record$notes)
      }
      else{
      
        updateSelectInput(session = session, inputId = "species", selected = " ")
        updateNumericInput(session = session, inputId = "distance", value = NA_real_)
        updateTextInput(session = session, inputId = "notes", value = " ")
          
      }
      
    })
    
    observeEvent(input$add_species_bttn,{
        rv$names <- c(input$add_species_text,rv$names) %>% str_sort() 
        #updateSelectInput(session = session, inputId = "species")
    })
    
    observeEvent(input$rem_species_bttn,{
      rv$names <- rv$names[rv$names != input$species]
      
    })
    
    
  })
}