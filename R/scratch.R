readRDS("session.RDS")
install.packages("excelR")





ui <- fluidPage(

excelOutput("table")

)

server <- function(input, output, session) {

    rv <- reactiveValues()

    observe({
   
    rv$table_data <- tibble(
            species = rep(c("dog","cat"), 10),
            distance = 1:20, 
            notes = c("some notes brah")
            )
    })
        output$table <- renderExcel({
        
            isolate({excelTable(rv$table_data, autoFill = TRUE)})
        
        })

        observeEvent(input$table,{
            rv$table_data <- excel_to_R(input$table)
                
                print(rv$table_data)
        })

}
shiny::runApp(list(ui = ui, server = server))

ui <- fluidPage(
    selectInput(
       inputId = "direc",
       label = "Directories List (sorted by most recent)",
       choices = c(""),
       width = "100%"
    ),
    actionButton(
        inputId = "load_dir",
        label = "Directory from List"
    ),
    shinyDirButton(
        id = "upload_new",
        label = "New Directory",
        title = "New Directory"
    ),
    verbatimTextOutput(
        outputId = "out"
    )
)
server <- function(input, output, session) {

    rv <- reactiveValues()

    

   observeEvent(input$upload_new, {
        volumes <- c(Home = fs::path_home(),
                    "R Installation" = R.home(),
                    getVolumes()())

        shinyDirChoose(
            input = input,
            id = "upload_new",
            roots = volumes,
            session = session,
            restrictions = system.file(package = "base")
            )

        rv$dir <- parseDirPath(volumes, input$upload_new)
    })

    observeEvent(input$load_dir, {
        rv$dir <- input$direc
    })

    output$out <- renderPrint({
        glue("{rv$dir}")
    })
    onStop(function() {
      #cat("stopping")
        isolate({
          saveRDS(
            list(
              dir = c(rv$dir, rv$session_file$dir),
              names = as.character(rv$names)
            ),
            file = here("session.RDS")
          )
        })
      })

}
shiny::runApp(list(ui = ui, server = server))



dirs <- c("one", "one", "two", "three", "four", "one")

dirs[!duplicated(dirs)]
