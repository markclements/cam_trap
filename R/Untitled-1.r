readRDS("session.RDS")
install.packages("excelR")





ui <- fluidPage(

excelOutput("table")

)

server <- function(input, output, session){

    table_data <- tibble(
            species = rep(c("dog","cat"), 10),
            distance = 1:20, 
            notes = c("some notes brah")
            )

        output$table <- renderExcel({
        excelTable(table_data, autoFill = TRUE)
        })

        observeEvent(input$table,{
            table_data <- excel_to_R(input$table)
                if(!is.null(table_data)){
                print(table_data)
            }
        })

}


ui <- fluidPage(


)

server <- function(input, output, session){

    rv <- reactiveValues()  

    observe({
        if(file.exists("session.RDS")){
           session_file <- readRDS("session.RDS")
           print(session_file) 
        }
        
    })

}
