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
    title = "General Overview",
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
        
        layout_column_wrap(
          value_box(title = "Age", 
                    value = textOutput("patient_age")),
          value_box(title = "Sex", 
                    value = textOutput("patient_sex")),
          value_box(title = "Race", 
                    value = textOutput("patient_race"))
        ),
        
        layout_column_wrap(
          value_box(title = "Date of first exposure to treatment (dd-mm-yyyy)", 
                    value = textOutput("patient_trtsdtm")),
          value_box(title = "Date of last exposure to treatment (dd-mm-yyyy)", 
                    value = textOutput("patient_trtedtm")),
          value_box(title = "End of Study Status", 
                    value = textOutput("patient_eosstt")),
          value_box(title = "Reason for Discontinuation", 
                    value = textOutput("patient_dcsreas"))
          
        ),
        
        card(
          card_header("Adverse Events"),
          card_body(DTOutput("ae_table"))
        )
      ),
      
      nav_panel(
        "Adverse Events Timeline",
        card(
          card_header("AE Timeline"),
          card_body(plotlyOutput("ae_gantt"))
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
  
  filtered_adae <- reactive({
    
    req(input$subjectid)
    data$adae %>% 
      filter(input$subjectid == SUBJID)
    
  })
  
  # General Overview panel
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
  
  
  # Patient Overview panel
  output$ae_table <- renderDT({

    filtered_adae() %>%
      select(SUBJID,
             TRT01SDTM,
             TRT02SDTM,
             AETERM,
             AETOXGR,
             AESEV,
             AEACN,
             AEOUT,
             ASTDY,
             AENDY,
             AERELNST,
             AEACNOTH
             )
  })
  
  output$patient_age <- renderText({
    paste(last(filtered_adae()$AGE))
  })
  
  output$patient_sex <- renderText({
    paste(last(filtered_adae()$SEX))
  })
  
  output$patient_race <- renderText({
    paste(last(filtered_adae()$RACE))
  })
  
  output$patient_trtsdtm <- renderText({
    paste(format(last(filtered_adae()$TRTSDTM), "%d-%m-%Y"))
  })
  
  output$patient_trtedtm <- renderText({
    paste(format(last(filtered_adae()$TRTEDTM), "%d-%m-%Y"))
  })
  
  output$patient_eosstt <- renderText({
    paste(last(filtered_adae()$EOSSTT))
  })
  
  output$patient_dcsreas <- renderText({
    paste(last(filtered_adae()$DCSREAS))
  })
  
  
  # Gantt chart
  output$ae_gantt <- renderPlotly({
    
    data_plot <- filtered_adae() %>% 
      mutate(ASTDTM = as.Date(ASTDTM, format = "%m/%d/%Y"),
             AENDTM = as.Date(AENDTM, format = "%m/%d/%Y"))
    
    fig <- plot_ly()
    
    color_mapping <- c("1" = "green",
                       "2" = "yellow",
                       "3" = "orange",
                       "4" = "red",
                       "5" = "darkred")
    
    fig <- plot_ly()
    
    for(i in 1:(nrow(data_plot) - 1)) {
      fig <- add_trace(fig,
                       x = c(data_plot$ASTDTM[i], data_plot$AENDTM[i]),
                       y = c(i, i),
                       mode = "lines",
                       line = list(width = 30,
                                   color = color_mapping[as.character(data_plot$AETOXGR[i])]),
                       showlegend = FALSE,
                       hoverinfo = "text",
                       # Custom hover text
                       text = paste("Adverse Event:", data_plot$AETERM[i], "<br>",
                                    "Severity:", data_plot$AESEV[i], "<br>",
                                    "Grade:", data_plot$AETOXGR[i], "<br>",
                                    "Duration:", data_plot$AENDTM[i] - data_plot$ASTDTM[i]),
                       evaluate = TRUE
      )
    }
    
    fig <- layout(
      fig,
      yaxis = (list(showgrid = FALSE,
                    tickmode = "array",
                    tickvals = 1:nrow(data_plot),
                    ticktext = unique(data_plot$AETERM)))
    )
    
    fig
    
  })
}


shinyApp(ui, server)
