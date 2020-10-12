dropdown_select <- function(id, choices, h4_label = "Type") {
  ns <- NS(id)
  tagList(
    h4(paste0("Select Vessel ,", h4_label, ":")),
    dropdown_input(ns("dropdown"), choices = choices)
  )
}

dropdown_srv <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$dropdown, {
        new_shipnames <- filter(ships, ship_type == input$dropdown) %>%
          pull(ship_name) %>%
          unique()
        update_dropdown_input(session, "dropdown_name", choices = new_shipnames, value = "")
      })
    }
  )
}







counterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      count <- reactiveVal(0)
      observeEvent(input$button, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      count
    }
  )
}