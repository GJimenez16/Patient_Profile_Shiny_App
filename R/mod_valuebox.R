mod_valuebox_ui <- function(id, has_icon = TRUE) {
  ns <- NS(id)
  
  if(has_icon == TRUE) {
    value_box(
      title = textOutput(ns("title")),
      value = textOutput(ns("value")),
      showcase = uiOutput(ns("icon"))
    )
  } else {
    value_box(
      title = textOutput(ns("title")),
      value = textOutput(ns("value"))
    )
  }
}

mod_valuebox_server <- function(id, title, value, icon) {
  
  moduleServer(id, function(input, output, session) {
    output$title <- renderText(title)
    output$value <- renderText(value)
    
    output$icon <- renderUI({
      if (!is.null(icon)) bs_icon(icon)
    })
  })
}