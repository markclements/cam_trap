display_image_sequence_control_UI <- function(id) {
  ns <- NS(id)
  div(
    id = "image_seq",
    fluidRow(
      column(
        width = 3,
        textOutput(
          outputId = ns("image_num")
        )
      ),
      column(
        width = 9,
        sliderTextInput(
          inputId = ns("image_sequence"),
          label = NULL,
          grid = FALSE,
          hide_min_max = TRUE,
          choices = c("one", "two")
        )
      )
    )
  )
}

display_image_sequence_control_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    images_in_this_sequence <- reactive({
      req(rv$control_file)

      rv$control_file %>%
        filter(event == rv$event) %>%
        pull(image)
    })

    # observeEvent(images_in_this_sequence(),{
    #   updateSliderInput(
    #     session = session,
    #     inputId = "image_sequence",
    #     value = 1,
    #     max = length(images_in_this_sequence())
    #   )
    # })


    observeEvent(images_in_this_sequence(), {
      choices <- images_in_this_sequence()

      if (length(choices) < 2) choices <- c(choices, choices)

      updateSliderTextInput(
        session = session,
        inputId = "image_sequence",
        choices = choices,
        selected = choices[1]
      )
    })

    output$image_num <- renderText({
      # cc<-c("one","two","three")
      # which(cc=="three")
      img_num <- which(images_in_this_sequence() == input$image_sequence)

      glue::glue("Image {img_num} of {length(images_in_this_sequence())}")
    })

    reactive(
      # images_in_this_sequence()[[input$image_sequence]]
      input$image_sequence
    )
  })
}