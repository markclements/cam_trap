## some comments
annotate_image_UI <- function(id) {
  ns <- NS(id)
  actionButton(
    inputId = ns("img"),
    label = "annotate"
  )
}

annotate_image_server <- function(id, curr_img, selected, rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$img, {
      ns <- session$ns ### for name-spacing the UI components of the modal

      showModal(
        modalDialog(
          size = "m",
          easyClose = TRUE,
          fluidRow(
            selectizeInput(
              inputId = ns("species"),
              label = "species",
              choices = c(),
              options = list(create = TRUE)
            ),
            textInput(ns("distance"), "distance"),
            textAreaInput(ns("notes"), "notes")
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              inputId = ns("up_img"),
              label = "update"
            )
          )
        )
      )
      updateSelectizeInput(
        session = session,
        inputId = "species",
        choices = c("",rv$names),
        selected = NULL
      )
    })

  observe({
      rv$names <- unique(c(input$species, rv$names)) %>% sort()
    
      rv$names <- unique(c(input$species_update, rv$names)) %>% sort()

  # print(rv$names)
})

    observeEvent(input$up_img, {
      removeModal()

      x <- fs::path_file(curr_img())

      if (is.null(rv$df)) {
        rv$df <- tibble(image = x, id = input$species, distance = input$distance, notes = input$notes)
      } else {
        rv$df <- bind_rows(rv$df, tibble(image = x, id = input$species, distance = input$distance, notes = input$notes))
      }
    })

    observeEvent(selected(), {
      ns <- session$ns ### for name-spacing the UI components of the modal

      showModal(
        modalDialog(
          size = "m",
          easyClose = TRUE,
          fluidRow(
            selectizeInput(
              inputId = ns("species_update"),
              label = "species",
              choices = c(),
              options = list(create = TRUE)
            ),
            textInput(ns("distance_update"), "distance"),
            textAreaInput(ns("notes_update"), "notes")
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              inputId = ns("update_record"),
              label = "update"
            ),
            actionButton(
              inputId = ns("delete_record"),
              label = "delete"
            )
          )
        )
      )

      x <- fs::path_file(curr_img())

      rv$df %>%
        filter(image == x) %>%
        slice(selected()) -> record

      updateSelectizeInput(
        session = session,
        inputId = "species_update",
        choices = rv$names,
        selected = record$id
      )

      updateTextInput(
        session = session,
        inputId = "distance_update",
        value = record$distance
      )

      updateTextAreaInput(
        session = session,
        inputId = "notes_update",
        value = record$notes
      )
    })

    observeEvent(input$update_record, {
      removeModal()

      x <- fs::path_file(curr_img())


      rv$df %>%
        mutate(row_num = 1:n()) %>%
        filter(image == x) %>%
        slice(selected()) %>%
        pull(row_num) -> row

      rv$df %>%
        slice(-row) -> df

      rv$df <- bind_rows(
        df,
        tibble(
          image = x,
          id = input$species_update,
          distance = input$distance_update,
          notes = input$notes_update
        )
      )
    })


    observeEvent(input$delete_record, {
      removeModal()

      x <- fs::path_file(curr_img())


      rv$df %>%
        mutate(row_num = 1:n()) %>%
        filter(image == x) %>%
        slice(selected()) %>%
        pull(row_num) -> row

      rv$df %>%
        slice(-row) -> rv$df
    })
  })
}