shut_down_UI <- function(id) {
  ns <- NS(id)
}

shut_down_server <- function(id, dir, rv, input) {
  moduleServer(id, function(input, output, session) {
    

    onStop(function() {
      
      # isolate({
      #   if(length(rv$annotations) > 0 | length(rv$contol_file) > 0){
      #     saveRDS(
      #       list(
      #         annotations = tibble(rv$annotations),
      #         control_file = tibble(rv$control_file),
      #         inputs = reactiveValuesToList(input)
      #       ),
      #       file = here(glue("rv$dir}/data.RDS"))
      #     )
      #   })
        isolate({
          saveRDS(
            list(
              dir = rv$dirs_list,
              names = as.character(rv$names)
            ),
            file = here("session.RDS")
          )
        })
    })
  })
}  