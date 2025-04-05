library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(DT)
library(future)
library(promises)
library(random.cdisc.data)
library(plotly)



# Random adam datasets
data <- list(
  adsl = random.cdisc.data::cadsl,
  adae = random.cdisc.data::cadae,
  adlb = random.cdisc.data::cadlb,
  advs = random.cdisc.data::cadvs
)



ui <- page_navbar(
  
  title = "Patient Profile",
  
  # Overview of Patient Profile -- demographics, disposition
  nav_panel(
    title = "Overview",
    h2(paste("Study", unique(data$adsl$STUDYID))),
    
    
    layout_column_wrap(
      value_box(title = "Total Patients", 
                value = n_distinct(data$adsl$USUBJID),
                showcase = bs_icon("person-standing")),
      
      value_box(title = "Completed treatments", 
                value = nrow(subset(data$adsl, EOSSTT == "COMPLETED")),
                showcase = bs_icon("check-circle-fill")),
      
      value_box(title = "On going treatments", 
                value = nrow(subset(data$adsl, EOSSTT == "ONGOING")),
                showcase = bs_icon("clipboard2-pulse-fill"))
    ),
    
    card(
      card_header("Age distribution by Sex"),
      card_body(plotlyOutput("agedist"))
    ),
    
    card(
      card_header("Race Distribution"),
      card_body(plotlyOutput("racedist"))
    )
    
    
    
    
  ),
  
  # Tab for deeper dive by selecting one patient
  nav_panel(
    title = "Patient Overview",
    selectInput(inputId = "subjectid",label = "Choose Subject", choices = unique(data$adsl$SUBJID)),
    
    page_navbar(
      title = "",
      
      nav_panel(
        "Overview",
        card(
          card_header("Age, sex, race")
        ),
        card(
          card_header("Disposition, start/end date")
        ),
        card(
          card_header("Key flags")
        )
      ),
      
      nav_panel(
        "Adverse Events Timeline",
        card(
          card_header("Gantt chart")
        )
      ),
      
      nav_panel(
        "Labs",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("labfilter", "Choose labs", choices = data$adlb$LBTEST, multiple = TRUE)
          ),
          plotOutput("labplot")
        )
      )
    )
  )
)

server <- function(input,output, session) {
  
  output$agedist <- renderPlotly({
    
    fig <- plot_ly(
      data$adsl,
      x = ~AGE,
      color = ~SEX,
      type = "histogram"
    ) %>% 
      layout(
        xaxis = list(title = "Age"),
        yaxis = list(title = "Count"),
        barmode = "stack"
      )
    
  })
  
  output$racedist <- renderPlotly({
    
    fig <- plot_ly(
      data$adsl,
      y = ~RACE,
      type = "histogram",
      orientation = "h"
    ) %>% 
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
}


shinyApp(ui, server)
